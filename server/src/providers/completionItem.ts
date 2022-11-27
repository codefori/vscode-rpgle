import path = require('path');
import { CompletionItem, CompletionItemKind, CompletionParams, InsertTextFormat, Position, Range } from 'vscode-languageserver';
import { documents, getWordRangeAtPosition, parser } from '.';
import Cache from '../language/models/cache';
import Declaration from '../language/models/declaration';
import * as ileExports from './apis';
import * as Project from "./project";

const completionKind = {
  function: CompletionItemKind.Function,
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
			const currentProcedure = doc.procedures.find(proc => lineNumber >= proc.range.start && lineNumber <= proc.range.end);

			const currentLine = document.getText(Range.create(
				handler.position.line,
				0,
				handler.position.line,
				200
			));

			// This means we're just looking for subfields in the struct
			if (trigger === `.`) {
				let currentPosition = Position.create(handler.position.line, handler.position.character - 2);
				let preWord = getWordRangeAtPosition(document, currentPosition)?.toUpperCase();

				// Uh oh! Maybe we found dim struct?
				if (!preWord) {
					const startBracket = currentLine.lastIndexOf(`(`, currentPosition.character);

					if (startBracket > -1) {
						currentPosition = Position.create(handler.position.line, startBracket - 1);
						preWord = getWordRangeAtPosition(document, currentPosition);
					}
				}

				if (preWord) {
					let possibleStruct: Declaration | undefined;

					if (currentProcedure && currentProcedure.scope) {
						const procScop = currentProcedure.scope;

						possibleStruct = currentProcedure.subItems.find(subitem => subitem.name.toUpperCase() === preWord && subitem.subItems.length > 0);

						if (!possibleStruct) {
							possibleStruct = procScop.structs.find(struct => struct.name.toUpperCase() === preWord);
						}
					}

					if (!possibleStruct) {
						possibleStruct = doc.structs.find(struct => struct.name.toUpperCase() === preWord);
					}

					if (possibleStruct && possibleStruct.keyword[`QUALIFIED`]) {
						items.push(...possibleStruct.subItems.map(subItem => {
							const item = CompletionItem.create(subItem.name);
							item.kind = CompletionItemKind.Property;
							item.insertText = subItem.name;
							item.detail = subItem.keywords.join(` `);
							item.documentation = subItem.description + `${possibleStruct ? ` (${possibleStruct.name})` : ``}`;
							return item;
						}));
					}
				}
			} else {
				// Normal defines and all that.....
				// TODO: handle /COPY and /INCLUDE
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

				} else {
					const expandScope = (localCache: Cache) => {
						for (const procedure of localCache.procedures) {
							const item = CompletionItem.create(`${procedure.name}`);
							item.kind = CompletionItemKind.Function;
							item.insertTextFormat = InsertTextFormat.Snippet;
							item.insertText = `${procedure.name}(${procedure.subItems.map((parm, index) => `\${${index + 1}:${parm.name}}`).join(`:`)})`;
							item.detail = procedure.keywords.join(` `);
							item.documentation = procedure.description;
							items.push(item);
						}

						for (const subroutine of localCache.subroutines) {
							const item = CompletionItem.create(`${subroutine.name}`);
							item.kind = CompletionItemKind.Function;
							item.insertText = `${subroutine.name}`;
							item.documentation = subroutine.description;
							items.push(item);
						}

						for (const variable of localCache.variables) {
							const item = CompletionItem.create(`${variable.name}`);
							item.kind = CompletionItemKind.Variable;
							item.insertText = `${variable.name}`;
							item.detail = variable.keywords.join(` `);
							item.documentation = variable.description;
							items.push(item);
						}

						localCache.files.forEach(file => {
							const item = CompletionItem.create(`${file.name}`);
							item.kind = CompletionItemKind.File;
							item.insertText = `${file.name}`;
							item.detail = file.keywords.join(` `);
							item.documentation = file.description;
							items.push(item);

							for (const struct of file.subItems) {
								const item = CompletionItem.create(`${struct.name}`);
								item.kind = CompletionItemKind.Struct;
								item.insertText = `${struct.name}`;
								item.detail = struct.keywords.join(` `);
								item.documentation = struct.description;
								items.push(item);

								if (!struct.keyword[`QUALIFIED`]) {
									struct.subItems.forEach((subItem: Declaration) => {
										const item = CompletionItem.create(`${subItem.name}`);
										item.kind = CompletionItemKind.Property;
										item.insertText = `${subItem.name}`;
										item.detail = subItem.keywords.join(` `);
										item.documentation = subItem.description + ` (${struct.name})`;
										items.push(item);
									});
								}
							}
						});

						for (const struct of localCache.structs) {
							const item = CompletionItem.create(`${struct.name}`);
							item.kind = CompletionItemKind.Struct;
							item.insertText = `${struct.name}`;
							item.detail = struct.keywords.join(` `);
							item.documentation = struct.description;
							items.push(item);

							if (!struct.keyword[`QUALIFIED`]) {
								struct.subItems.forEach((subItem: Declaration) => {
									const item = CompletionItem.create(`${subItem.name}`);
									item.kind = CompletionItemKind.Property;
									item.insertText = `${subItem.name}`;
									item.detail = subItem.keywords.join(` `);
									item.documentation = subItem.description + ` (${struct.name})`;
									items.push(item);
								});
							}
						}

						for (const constant of localCache.constants) {
							const item = CompletionItem.create(`${constant.name}`);
							item.kind = CompletionItemKind.Constant;
							item.insertText = `${constant.name}`;
							item.detail = constant.keywords.join(` `);
							item.documentation = constant.description;
							items.push(item);
						}
					};

					expandScope(doc);

					if (currentProcedure) {
						for (const subItem of currentProcedure.subItems) {
							const item = CompletionItem.create(`${subItem.name}`);
							item.kind = CompletionItemKind.Variable;
							item.insertText = subItem.name;
							item.detail = [`parameter`, ...subItem.keywords].join(` `);
							item.documentation = subItem.description;
							items.push(item);
						}

						if (currentProcedure.scope) {
							expandScope(currentProcedure.scope);
						}
					}

					// Next, we're going to make some import suggestions for system APIs
					// TODO: support not free
					if (isFree) {
						const insertAt = doc.getDefinitionBlockEnd(document.uri) + 1;
						const insertRange = Range.create(insertAt, 0, insertAt, 0);

						ileExports.names.filter(
							// Check the prototype doesn't exist
							apiName => !doc.procedures.some(proc => {
								const apiNameUpper = apiName.toUpperCase();
								if (proc.name.toUpperCase() === apiNameUpper) return true;

								let possibleExternalName = proc.keyword[`EXTPROC`] || proc.keyword[`EXTPGM`];

								if (typeof possibleExternalName === `string`) {
									if (possibleExternalName.startsWith(`'`)) possibleExternalName = possibleExternalName.substring(1);
									if (possibleExternalName.endsWith(`'`)) possibleExternalName = possibleExternalName.substring(0, possibleExternalName.length - 1);
									if (possibleExternalName.toUpperCase() === apiNameUpper) return true;
								}
							}) &&

								// And also the struct hasn't been defined with the same name
								!doc.structs.some(struct => struct.name.toUpperCase() === apiName.toUpperCase())
						).forEach(apiName => {
							const currentExport = ileExports.bodies[apiName];

							const item = CompletionItem.create(apiName);
							item.kind = completionKind[currentExport.type];
							item.insertTextFormat = InsertTextFormat.Snippet;
							item.insertText = currentExport.insertText;
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

							item.additionalTextEdits = [{
								range: insertRange,
								newText: eol + currentExport.prototype.join(eol) + eol
							}];

							items.push(item);
						})
					}
				}
			}
		}
	}

	return items;
}