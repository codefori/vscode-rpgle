import path = require('path');
import { CompletionItem, CompletionItemKind, CompletionParams, InsertTextFormat, Position, Range } from 'vscode-languageserver';
import { documents, getWordRangeAtPosition, parser, prettyKeywords } from '.';
import Cache from '../../../../language/models/cache';
import Declaration from '../../../../language/models/declaration';
import * as ileExports from './apis';
import skipRules from './linter/skipRules';
import * as Project from "./project";
import { getInterfaces } from './project/exportInterfaces';
import Parser from '../../../../language/parser';
import { Token } from '../../../../language/types';

const completionKind = {
	function: CompletionItemKind.Interface,
	struct: CompletionItemKind.Struct
};

const eol = `\n`;

export default async function completionItemProvider(handler: CompletionParams): Promise<CompletionItem[]> {
	const items: CompletionItem[] = [];
	const lineNumber = handler.position.line;

	const trigger = handler.context?.triggerCharacter;
	const currentPath = handler.textDocument.uri;
	const document = documents.get(currentPath);

	if (document) {
		const doc = await parser.getDocs(currentPath, document.getText());
		if (doc) {
			const isFree = (document.getText(Range.create(0, 0, 0, 6)).toUpperCase() === `**FREE`);

			// If they're typing inside of a procedure, let's get the stuff from there too
			const currentProcedure = doc.procedures.find((proc, index) =>
				proc.range.start && proc.range.end &&
				lineNumber >= proc.range.start &&
				(lineNumber <= proc.range.end + 1 || index === doc.procedures.length - 1) &&
				currentPath === proc.position.path
			);

			const currentLine = document.getText(Range.create(
				handler.position.line,
				0,
				handler.position.line,
				200
			));

			// This means we're just looking for subfields in the struct
			if (trigger === `.`) {
				const cursorIndex = handler.position.character;
				let tokens = Parser.lineTokens(isFree ? currentLine : currentLine.length >= 7 ? ``.padEnd(7) + currentLine.substring(7) : ``, 0, 0, true);

				if (tokens.length > 0) {
					
					// We need to find the innermost block we are part of
					tokens = Parser.fromBlocksGetTokens(tokens, cursorIndex);

					// Remove any tokens after the cursor
					tokens = tokens.filter(token => token.range.end <= cursorIndex);

					// Get the possible variable we're referring to
					let tokenIndex = Parser.getReference(tokens, cursorIndex);

					let currentDef: Declaration | undefined;

					for (tokenIndex; tokenIndex < tokens.length; tokenIndex++) {
						if (tokens[tokenIndex] === undefined || [`block`, `dot`, `newline`].includes(tokens[tokenIndex].type)) {
							continue;
						}

						const word = tokens[tokenIndex].value?.toUpperCase();

						if (!word) break;

						if (currentDef) {
							if (currentDef.subItems && currentDef.subItems.length > 0) {
								currentDef = currentDef.subItems.find(subItem => subItem.name.toUpperCase() === word);
							}

						} else {
							currentDef = [
								// First we search the local procedure
								currentProcedure && currentProcedure.scope ? currentProcedure.scope.parameters.find(parm => parm.name.toUpperCase() === word && parm.subItems.length > 0) : undefined,
								currentProcedure && currentProcedure.scope ? currentProcedure.scope.structs.find(struct => struct.name.toUpperCase() === word && struct.keyword[`QUALIFIED`]) : undefined,
								currentProcedure && currentProcedure.scope ? currentProcedure.scope.constants.find(struct => struct.subItems.length > 0 && struct.name.toUpperCase() === word) : undefined,
	
								// Then we search the globals
								doc.structs.find(struct => struct.name.toUpperCase() === word && struct.keyword[`QUALIFIED`]),
								doc.constants.find(constants => constants.subItems.length > 0 && constants.name.toUpperCase() === word)
							].find(x => x); // find the first non-undefined item

							if (currentDef && currentDef.subItems.length > 0) {
								// All good!
							} else {
								currentDef = undefined;
							}
						}
					}

					if (currentDef && currentDef.subItems.length > 0) {
						items.push(...currentDef.subItems.map(subItem => {
							const item = CompletionItem.create(subItem.name);
							item.kind = CompletionItemKind.Property;
							item.insertText = subItem.name;
							item.detail = prettyKeywords(subItem.keyword);
							item.documentation = subItem.description + ` (${currentDef.name})`;
							return item;
						}));
					}
				}
			} else {
				// Normal defines and all that.....
				const upperLine = currentLine.toUpperCase();
				if (Project.isEnabled && (upperLine.includes(`/COPY`) || upperLine.includes(`/INCLUDE`))) {
					const localFiles = await Project.getIncludes(currentPath);

					items.push(...localFiles.map(file => {
						const basename = path.basename(file.uri);

						const item = CompletionItem.create(basename);
						item.kind = CompletionItemKind.File;
						item.insertText = `'${file.relative}'`;
						item.detail = file.relative;
						return item;
					}));

				} else if (currentLine.trimStart().startsWith(`//`)) {
					items.push(...skipRules);

				} else {
					const expandScope = (localCache: Cache) => {
						for (const subItem of localCache.parameters) {
							const item = CompletionItem.create(subItem.name);
							item.kind = CompletionItemKind.TypeParameter;
							item.insertText = subItem.name;
							item.detail = [`parameter`, prettyKeywords(subItem.keyword)].join(` `);
							item.documentation = subItem.description;
							items.push(item);
						}

						for (const procedure of localCache.procedures) {
							if (procedure.prototype && localCache.procedures.some(proc => proc.name.toUpperCase() === procedure.name.toUpperCase() && !proc.prototype)) {
								continue; // Skip if the actual procedure is already defined
							}

							const item = CompletionItem.create(procedure.name);
							item.kind = procedure.prototype ? CompletionItemKind.Interface : CompletionItemKind.Function;
							item.insertTextFormat = InsertTextFormat.Snippet;
							item.insertText = `${procedure.name}(${procedure.subItems.map((parm, index) => `\${${index + 1}:${parm.name}}`).join(`:`)})`;
							item.detail = prettyKeywords(procedure.keyword);
							item.documentation = procedure.description;
							items.push(item);
						}

						for (const subroutine of localCache.subroutines) {
							const item = CompletionItem.create(subroutine.name);
							item.kind = CompletionItemKind.Function;
							item.insertText = subroutine.name;
							item.documentation = subroutine.description;
							items.push(item);
						}

						for (const variable of localCache.variables) {
							const item = CompletionItem.create(variable.name);
							item.kind = CompletionItemKind.Variable;
							item.insertText = variable.name;
							item.detail = prettyKeywords(variable.keyword);
							item.documentation = variable.description;
							items.push(item);
						}

						localCache.files.forEach(file => {
							const item = CompletionItem.create(file.name);
							item.kind = CompletionItemKind.File;
							item.insertText = file.name;
							item.detail = prettyKeywords(file.keyword);
							item.documentation = file.description;
							items.push(item);

							for (const struct of file.subItems) {
								const item = CompletionItem.create(struct.name);
								item.kind = CompletionItemKind.Struct;
								item.insertText = struct.name;
								item.detail = prettyKeywords(struct.keyword);
								item.documentation = struct.description;
								items.push(item);

								if (!struct.keyword[`QUALIFIED`]) {
									struct.subItems.forEach((subItem: Declaration) => {
										const item = CompletionItem.create(subItem.name);
										item.kind = CompletionItemKind.Property;
										item.insertText = `${subItem.name}`;
										item.detail = prettyKeywords(subItem.keyword);
										item.documentation = subItem.description + ` (${struct.name})`;
										items.push(item);
									});
								}
							}
						});

						for (const struct of localCache.structs) {
							const item = CompletionItem.create(struct.name);
							item.kind = CompletionItemKind.Struct;
							item.insertText = struct.name;
							item.detail = prettyKeywords(struct.keyword);
							item.documentation = struct.description;
							items.push(item);

							if (!struct.keyword[`QUALIFIED`]) {
								struct.subItems.forEach((subItem: Declaration) => {
									const item = CompletionItem.create(subItem.name);
									item.kind = CompletionItemKind.Property;
									item.insertText = `${subItem.name}`;
									item.detail = prettyKeywords(subItem.keyword);
									item.documentation = subItem.description + ` (${struct.name})`;
									items.push(item);
								});
							}
						}

						for (const constant of localCache.constants) {
							const item = CompletionItem.create(constant.name);
							item.kind = CompletionItemKind.Constant;
							item.insertText = constant.name;
							item.detail = prettyKeywords(constant.keyword);
							item.documentation = constant.description;
							items.push(item);

							if (!constant.keyword[`QUALIFIED`]) {
								constant.subItems.forEach((subItem: Declaration) => {
									const item = CompletionItem.create(subItem.name);
									item.kind = CompletionItemKind.Property;
									item.insertText = `${subItem.name}`;
									item.detail = prettyKeywords(subItem.keyword);
									item.documentation = subItem.description + ` (${constant.name})`;
									items.push(item);
								});
							}
						}
					};

					expandScope(doc);

					if (currentProcedure) {
						// If we have the entire scope, perfect
						if (currentProcedure.scope) {
							expandScope(currentProcedure.scope);
						}

						// subItems get moved to parameters when the procedure is ended correctly.
						// So if the user is in the middle of a statement, then they still exist in the subItems
						else if (currentProcedure.subItems.length > 0) {
							for (const subItem of currentProcedure.subItems) {
								const item = CompletionItem.create(subItem.name);
								item.kind = CompletionItemKind.TypeParameter;
								item.insertText = subItem.name;
								item.detail = [`parameter`, prettyKeywords(subItem.keyword)].join(` `);
								item.documentation = subItem.description;
								items.push(item);
							}
						}
					}

					if (isFree) {
						const isInclude = currentPath.toLowerCase().endsWith(`.rpgleinc`);
						const insertAt = doc.getDefinitionBlockEnd(document.uri) + 1;
						const insertRange = Range.create(insertAt, 0, insertAt, 0);

						[...ileExports.bodies, ...getInterfaces()].filter(
							// Check the prototype doesn't exist
							api => !doc.procedures.some(proc => {
								const apiNameUpper = api.name.toUpperCase();
								if (proc.name.toUpperCase() === apiNameUpper) return true;

								let possibleExternalName = proc.keyword[`EXTPROC`] || proc.keyword[`EXTPGM`];

								if (typeof possibleExternalName === `string`) {
									if (possibleExternalName.startsWith(`'`)) possibleExternalName = possibleExternalName.substring(1);
									if (possibleExternalName.endsWith(`'`)) possibleExternalName = possibleExternalName.substring(0, possibleExternalName.length - 1);
									if (possibleExternalName.toUpperCase() === apiNameUpper) return true;
								}
							}) &&

								// And also the struct hasn't been defined with the same name
								!doc.structs.some(struct => struct.name.toUpperCase() === api.name.toUpperCase())
						).forEach(currentExport => {

							const item = CompletionItem.create(currentExport.name);
							item.kind = completionKind[currentExport.type];
							item.detail = `${currentExport.detail} (auto-import)`;

							item.documentation = {
								kind: `markdown`,
								value: [
									currentExport.description,
									(currentExport.example ?
										[`---`, '', '```rpgle', currentExport.example.join(eol), '```'].join(eol)
										: undefined)
								].filter(v => v).join(eol + eol)
							};

							item.insertTextFormat = InsertTextFormat.Snippet;

							// If it's an include, we really only want the prototype
							if (isInclude) {
								item.insertText = currentExport.prototype.join(eol);

							} else {
								item.insertText = currentExport.insertText;

								item.additionalTextEdits = [{
									range: insertRange,
									newText: eol + currentExport.prototype.join(eol) + eol
								}];
							}

							items.push(item);
						})
					}
				}
			}
		}
	}

	return items;
}