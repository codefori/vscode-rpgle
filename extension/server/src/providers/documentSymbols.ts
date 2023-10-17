import { DocumentSymbol, DocumentSymbolParams, Range, SymbolKind } from 'vscode-languageserver';
import { documents, parser } from '.';
import Cache from '../../../../language/models/cache';

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
				.filter(proc => proc.position && proc.position.path === currentPath)
				.forEach(proc => {
					const procDef = DocumentSymbol.create(
						proc.name,
						proc.keywords.join(` `).trim(),
						SymbolKind.Function,
						Range.create(proc.range.start, 0, proc.range.end, 0),
						Range.create(proc.range.start, 0, proc.range.start, 0),
					);

					if (proc.scope) {
						procDef.children = proc.subItems
						.filter(subitem => subitem.position && subitem.position.path === currentPath)
						.map(subitem => DocumentSymbol.create(
							subitem.name,
							subitem.keywords.join(` `).trim(),
							SymbolKind.Property,
							Range.create(subitem.position.line, 0, subitem.position.line, 0),
							Range.create(subitem.position.line, 0, subitem.position.line, 0)
						));
						
						procDef.children.push(...getScopeVars(proc.scope));
					}

					currentScopeDefs.push(procDef);
				});

			currentScopeDefs.push(
				...scope.subroutines.filter(sub => sub.position && sub.position.path === currentPath)
					.filter(def => def.range.start)
					.map(def => DocumentSymbol.create(
						def.name,
						def.keywords.join(` `).trim(),
						SymbolKind.Function,
						Range.create(def.range.start, 0, def.range.end, 0),
						Range.create(def.range.start, 0, def.range.start, 0),
					)),

				...scope.variables
					.filter(variable => variable.position && variable.position.path === currentPath)
					.map(def => DocumentSymbol.create(
						def.name,
						def.keywords.join(` `).trim(),
						SymbolKind.Variable,
						Range.create(def.position.line, 0, def.position.line, 0),
						Range.create(def.position.line, 0, def.position.line, 0)
					)),

				...scope.constants
					.filter(constant => constant.position && constant.position.path === currentPath)
					.map(def => DocumentSymbol.create(
						def.name,
						def.keywords.join(` `).trim(),
						SymbolKind.Constant,
						Range.create(def.position.line, 0, def.position.line, 0),
						Range.create(def.position.line, 0, def.position.line, 0)
					)),

          ...scope.cursors
					.filter(cursor => cursor.position && cursor.position.path === currentPath)
					.map(def => DocumentSymbol.create(
						def.name,
						def.keywords.join(` `).trim(),
						SymbolKind.Interface,
						Range.create(def.position.line, 0, def.position.line, 0),
						Range.create(def.position.line, 0, def.position.line, 0)
					))          
			);

			scope.files
				.filter(struct => struct.position && struct.position.path === currentPath)
				.forEach(file => {
					const fileDef = DocumentSymbol.create(
						file.name,
						file.keywords.join(` `).trim(),
						SymbolKind.File,
						Range.create(file.position.line, 0, file.position.line, 0),
						Range.create(file.position.line, 0, file.position.line, 0)
					);

					fileDef.children = [];

					file.subItems
						.filter(recordFormat => recordFormat.position && recordFormat.position.path === currentPath)
						.forEach(recordFormat => {
							const recordFormatDef = DocumentSymbol.create(
								recordFormat.name,
								recordFormat.keywords.join(` `).trim(),
								SymbolKind.Struct,
								Range.create(recordFormat.position.line, 0, recordFormat.position.line, 0),
								Range.create(recordFormat.position.line, 0, recordFormat.position.line, 0)
							);

							recordFormatDef.children = recordFormat.subItems
								.filter(subitem => subitem.position && subitem.position.path === currentPath)
								.map(subitem => DocumentSymbol.create(
									subitem.name,
									subitem.keywords.join(` `).trim(),
									SymbolKind.Property,
									Range.create(subitem.position.line, 0, subitem.position.line, 0),
									Range.create(subitem.position.line, 0, subitem.position.line, 0)
								));

							if (fileDef.children) {
								fileDef.children.push(recordFormatDef);
							}
						});

					currentScopeDefs.push(fileDef);
				});

			scope.structs
				.filter(struct => struct.position && struct.position.path === currentPath)
				.forEach(struct => {
					const structDef = DocumentSymbol.create(
						struct.name,
						struct.keywords.join(` `).trim(),
						SymbolKind.Struct,
						Range.create(struct.range.start, 0, struct.range.end, 0),
						Range.create(struct.range.start, 0, struct.range.start, 0),
					);

					structDef.children = struct.subItems
						.filter(subitem => subitem.position && subitem.position.path === currentPath)
						.map(subitem => DocumentSymbol.create(
							subitem.name,
							subitem.keywords.join(` `).trim(),
							SymbolKind.Property,
							Range.create(subitem.position.line, 0, subitem.position.line, 0),
							Range.create(subitem.position.line, 0, subitem.position.line, 0)
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