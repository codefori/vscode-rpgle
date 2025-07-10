import { DocumentSymbol, DocumentSymbolParams, Range, SymbolKind } from 'vscode-languageserver';
import { documents, parser, prettyKeywords } from '.';
import Cache from '../../../../language/models/cache';
import Declaration from '../../../../language/models/declaration';

export default async function documentSymbolProvider(handler: DocumentSymbolParams): Promise<DocumentSymbol[]> {
	const currentPath = handler.textDocument.uri;
	const symbols: DocumentSymbol[] = [];
	const document = documents.get(currentPath);

	const validRange = (def: Declaration) => {
		return def.range.start !== null && def.range.start >= 0 && def.range.end !== null;
	}

	const expandStruct = (def: Declaration): DocumentSymbol => {
		let start = def.range.start || def.position.range.line;
		let end = def.range.end || def.position.range.line;
		let hasChildren = def.subItems && def.subItems.length > 0;

		const parent = DocumentSymbol.create(
			def.name,
			prettyKeywords(def.keyword),
			hasChildren ? SymbolKind.Struct : SymbolKind.Property,
			Range.create(start, 0, end, 0),
			Range.create(start, 0, start, 0),
		);

		if (hasChildren) {
			parent.children = def.subItems
				.filter(subitem => subitem.position && subitem.position.path === currentPath)
				.map(subitem => expandStruct(subitem));
		}
			
		return parent;
	}

	if (document) {
		const doc = await parser.getDocs(currentPath, document.getText());

		/**
		 * @param {Cache} scope
		 * @returns {vscode.DocumentSymbol[]}
		 */
		const getScopeVars = (scope: Cache) => {
			const currentScopeDefs: DocumentSymbol[] = [];

			scope.procedures
				.filter(proc => proc.position && proc.position.path === currentPath && validRange(proc))
				.forEach(proc => {
					const procDef = DocumentSymbol.create(
						proc.name,
						prettyKeywords(proc.keyword),
						proc.prototype ? SymbolKind.Interface : SymbolKind.Function,
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
				...scope.subroutines
					.filter(sub => sub.position && sub.position.path === currentPath && validRange(sub))
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
					const start = file.range.start || file.position.range.line;
					const end = file.range.end || file.position.range.line;
					const fileDef = DocumentSymbol.create(
						file.name,
						prettyKeywords(file.keyword),
						SymbolKind.File,
						Range.create(start, 0, end, 0),
						Range.create(start, 0, end, 0)
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
				.filter(struct => struct.position && struct.position.path === currentPath && validRange(struct))
				.forEach(struct => {
					currentScopeDefs.push(expandStruct(struct));
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