import { DocumentSymbol, DocumentSymbolParams, Range, SymbolKind } from 'vscode-languageserver';
import { documents, parser, prettyKeywords } from '.';
import Cache from '../../../../language/models/cache';
import { Position } from '../../../../language/models/DataPoints';

export default async function documentSymbolProvider(handler: DocumentSymbolParams): Promise<DocumentSymbol[]> {
	const currentPath = handler.textDocument.uri;
	const symbols: DocumentSymbol[] = [];
	const document = documents.get(currentPath);

	if (document) {
		const doc = await parser.getDocs(currentPath, document.getText());

		/**
		 * @param {Cache} scope
		 * @returns {vscode.DocumentSymbol[]}
		 */
		const getScopeVars = (scope: Cache) => {
			const currentScopeDefs: DocumentSymbol[] = [];

			scope.procedures
				.filter(proc => proc.position && proc.position.path === currentPath && proc.range.start && proc.range.end)
				.forEach(proc => {
					const procDef = DocumentSymbol.create(
						proc.name,
						prettyKeywords(proc.keyword),
						SymbolKind.Function,
						Range.create(proc.range.start!, 0, proc.range.end!, 0),
						Range.create(proc.range.start!, 0, proc.range.start!, 0),
					);

					if (proc.scope) {
						procDef.children = proc.subItems
						.filter(subitem => subitem.position && subitem.position.path === currentPath)
						.map(subitem => DocumentSymbol.create(
							subitem.name,
							prettyKeywords(subitem.keyword),
							SymbolKind.Property,
							Range.create(subitem.position.range.line, 0, subitem.position.range.line, 0),
							Range.create(subitem.position.range.line, 0, subitem.position.range.line, 0)
						));
						
						procDef.children.push(...getScopeVars(proc.scope));
					}

					currentScopeDefs.push(procDef);
				});

			currentScopeDefs.push(
				...scope.subroutines.filter(sub => sub.position && sub.position.path === currentPath && sub.range.start && sub.range.end)
					.filter(def => def.range.start)
					.map(def => DocumentSymbol.create(
						def.name,
						prettyKeywords(def.keyword),
						SymbolKind.Function,
						Range.create(def.range.start!, 0, def.range.end!, 0),
						Range.create(def.range.start!, 0, def.range.start!, 0),
					)),

				...scope.variables
					.filter(variable => variable.position && variable.position.path === currentPath)
					.map(def => DocumentSymbol.create(
						def.name,
						prettyKeywords(def.keyword),
						SymbolKind.Variable,
						Range.create(def.position.range.line, 0, def.position.range.line, 0),
						Range.create(def.position.range.line, 0, def.position.range.line, 0)
					))
			);

			scope.constants
				.filter(constant => constant.position && constant.position.path === currentPath)
				.forEach(def => {
					const constantDef = DocumentSymbol.create(
						def.name,
						prettyKeywords(def.keyword),
						SymbolKind.Constant,
						Range.create(def.position.range.line, 0, def.position.range.line, 0),
						Range.create(def.position.range.line, 0, def.position.range.line, 0)
					);

					if (def.subItems.length > 0) {
						constantDef.children = def.subItems
							.filter(subitem => subitem.position && subitem.position.path === currentPath)
							.map(subitem => DocumentSymbol.create(
								subitem.name,
								prettyKeywords(subitem.keyword),
								SymbolKind.Property,
								Range.create(subitem.position.range.line, 0, subitem.position.range.line, 0),
								Range.create(subitem.position.range.line, 0, subitem.position.range.line, 0)
							));
					}

					currentScopeDefs.push(constantDef);
				})

			scope.files
				.filter(struct => struct.position && struct.position.path === currentPath)
				.forEach(file => {
					const fileDef = DocumentSymbol.create(
						file.name,
						prettyKeywords(file.keyword),
						SymbolKind.File,
						Range.create(file.position.range.line, 0, file.position.range.line, 0),
						Range.create(file.position.range.line, 0, file.position.range.line, 0)
					);

					fileDef.children = [];

					file.subItems
						.filter(recordFormat => recordFormat.position && recordFormat.position.path === currentPath)
						.forEach(recordFormat => {
							const recordFormatDef = DocumentSymbol.create(
								recordFormat.name,
								prettyKeywords(recordFormat.keyword),
								SymbolKind.Struct,
								Range.create(recordFormat.position.range.line, 0, recordFormat.position.range.line, 0),
								Range.create(recordFormat.position.range.line, 0, recordFormat.position.range.line, 0)
							);

							recordFormatDef.children = recordFormat.subItems
								.filter(subitem => subitem.position && subitem.position.path === currentPath)
								.map(subitem => DocumentSymbol.create(
									subitem.name,
									prettyKeywords(subitem.keyword),
									SymbolKind.Property,
									Range.create(subitem.position.range.line, 0, subitem.position.range.line, 0),
									Range.create(subitem.position.range.line, 0, subitem.position.range.line, 0)
								));

							if (fileDef.children) {
								fileDef.children.push(recordFormatDef);
							}
						});

					currentScopeDefs.push(fileDef);
				});

			scope.structs
				.filter(struct => struct.position && struct.position.path === currentPath && struct.range.start && struct.range.end)
				.forEach(struct => {
					const structDef = DocumentSymbol.create(
						struct.name,
						prettyKeywords(struct.keyword),
						SymbolKind.Struct,
						Range.create(struct.range.start!, 0, struct.range.end!, 0),
						Range.create(struct.range.start!, 0, struct.range.start!, 0),
					);

					structDef.children = struct.subItems
						.filter(subitem => subitem.position && subitem.position.path === currentPath)
						.map(subitem => DocumentSymbol.create(
							subitem.name,
							prettyKeywords(subitem.keyword),
							SymbolKind.Property,
							Range.create(subitem.position.range.line, 0, subitem.position.range.line, 0),
							Range.create(subitem.position.range.line, 0, subitem.position.range.line, 0)
						));

					currentScopeDefs.push(structDef);
				});

			return currentScopeDefs;
		};

		if (doc) {
			symbols.push(
				...getScopeVars(doc),
			);
		}
	}

	return symbols;
}