import { CompletionItem, CompletionItemKind, CompletionParams, Position, Range } from 'vscode-languageserver';
import { documents, getWordRangeAtPosition, parser } from '.';
import Cache from '../language/models/cache';
import Declaration from '../language/models/declaration';

export default async function completionItemProvider(handler: CompletionParams): Promise<CompletionItem[]> {
	const items: CompletionItem[] =[];
	const lineNumber = handler.position.line;

	const trigger = handler.context?.triggerCharacter;
	const currentPath = handler.textDocument.uri;
	const document = documents.get(currentPath);

	if (document) {
		const word = getWordRangeAtPosition(document, handler.position);
		const doc = await parser.getDocs(currentPath, document.getText());
		if (doc) {

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
				let currentPosition = Position.create(handler.position.line, handler.position.character - 1);
				let preWord = getWordRangeAtPosition(document, currentPosition);

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

						possibleStruct = currentProcedure.subItems.find(subitem => subitem.name.toUpperCase() === word && subitem.subItems.length > 0);

						if (!possibleStruct) {
							possibleStruct = procScop.structs.find(struct => struct.name.toUpperCase() === word);
						}
					}

					if (!possibleStruct) {
						possibleStruct = doc.structs.find(struct => struct.name.toUpperCase() === word);
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

				const expandScope = (localCache: Cache) => {
					for (const procedure of localCache.procedures) {
						const item = CompletionItem.create(`${procedure.name}`);
						item.kind = CompletionItemKind.Function;
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
			}
		}
	}

	return items;
}