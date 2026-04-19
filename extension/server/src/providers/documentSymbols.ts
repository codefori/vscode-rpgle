import { DocumentSymbol, DocumentSymbolParams, Range, SymbolKind } from 'vscode-languageserver';
import { documents, parser, prettyKeywords } from '.';
import Cache from '../../../../language/models/cache';
import Declaration from '../../../../language/models/declaration';
import Document from '../../../../language/document';
import { isInSqlBlock } from '../utils/sqlDetection';

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

	// Helper functions for detecting code blocks
	const isCommentLine = (line: string): boolean => {
		const trimmed = line.trim();
		return trimmed.startsWith('//') || trimmed.startsWith('*');
	};

	const isInString = (line: string, position: number): boolean => {
		let inString = false;
		for (let i = 0; i < position; i++) {
			if (line[i] === "'") {
				inString = !inString;
			}
		}
		return inString;
	};

	const getCodeBlockSymbols = (text: string, startLine: number, endLine: number): DocumentSymbol[] => {
		const blockSymbols: DocumentSymbol[] = [];
		const blockPatterns = [
			{ pattern: /\b(if|ifeq|ifne|ifgt|iflt|ifge|ifle)\s+(.+?)(?:;|$)/gi, kind: SymbolKind.Null },
			{ pattern: /\/(if)\b/gi, kind: SymbolKind.Null },
			{ pattern: /\b(dow|doweq|downe|dowgt|dowlt|dowge|dowle)\s+(.+?)(?:;|$)/gi, kind: SymbolKind.Null },
			{ pattern: /\b(dou|doueq|doune|dougt|doult|douge|doule)\s+(.+?)(?:;|$)/gi, kind: SymbolKind.Null },
			{ pattern: /\b(do)\s+(.+?)(?:;|$)/gi, kind: SymbolKind.Null },
			{ pattern: /\b(for|for-each)\s+(.+?)(?:;|$)/gi, kind: SymbolKind.Null },
			{ pattern: /\b(select)\b/gi, kind: SymbolKind.Null },
			{ pattern: /\b(monitor)\b/gi, kind: SymbolKind.Null },
			{ pattern: /\b(dcl-ds)\s+(\w+)/gi, kind: SymbolKind.Struct },
			{ pattern: /\b(dcl-pi)\s+(\w+)/gi, kind: SymbolKind.Interface },
			{ pattern: /\b(dcl-enum)\s+(\w+)/gi, kind: SymbolKind.Enum },
			{ pattern: /\b(casxx|caseq|casne|casgt|caslt|casge|casle)\s+(.+?)(?:;|$)/gi, kind: SymbolKind.Null },
		];

		const lines = text.split('\n');
		
		for (let lineNum = startLine; lineNum <= endLine && lineNum < lines.length; lineNum++) {
			const line = lines[lineNum];
			
			if (isCommentLine(line)) continue;

			for (const { pattern, kind } of blockPatterns) {
				pattern.lastIndex = 0;
				const match = pattern.exec(line);
				
				if (match) {
					if (isInString(line, match.index)) continue;
					
					const keyword = match[1].toLowerCase();
					
					// Skip SELECT if it's inside an SQL block
					if (keyword === 'select' && document && isInSqlBlock(text, document.offsetAt({ line: lineNum, character: match.index }))) {
						continue;
					}
					
					const name = match[2] || '';
					
					// Build display name
					// Check if this is a directive (starts with /)
					const isDirective = match[0].startsWith('/');
					let displayName = isDirective ? `/${keyword.toUpperCase()}` : keyword.toUpperCase();
					
					if (name) {
						displayName += ` ${name}`;
					} else if (isDirective) {
						// For directives, extract condition from the full line
						const matchEnd = match.index + match[0].length;
						const restOfLine = line.substring(matchEnd).trim();
						if (restOfLine) {
							displayName += ` ${restOfLine}`;
						}
					} else if (match[0].length > keyword.length) {
						const condition = match[0].substring(keyword.length).trim();
						if (condition && condition !== ';') {
							displayName += ` ${condition.replace(/;$/, '')}`;
						}
					}

					blockSymbols.push(DocumentSymbol.create(
						displayName,
						undefined,
						kind,
						Range.create(lineNum, 0, lineNum, line.length),
						Range.create(lineNum, 0, lineNum, line.length)
					));
					
					break;
				}
			}
		}

		return blockSymbols;
	};

	if (document) {
		const doc = await parser.getDocs(currentPath, document.getText());
		const text = document.getText();

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
							Range.create(subitem.range.start!, 0, subitem.range.end!, 0),
							Range.create(subitem.range.start!, 0, subitem.range.end!, 0)
						));
						
						procDef.children.push(...getScopeVars(proc.scope));
						
						// Add code block symbols (IF, DOW, FOR, etc.) as children
						const blockSymbols = getCodeBlockSymbols(text, proc.range.start!, proc.range.end!);
						procDef.children.push(...blockSymbols);
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
						Range.create(def.range.start!, 0, def.range.end!, 0),
					)),

				...scope.variables
					.filter(variable => variable.position && variable.position.path === currentPath)
					.map(def => DocumentSymbol.create(
						def.name,
						prettyKeywords(def.keyword),
						SymbolKind.Variable,
						Range.create(def.range.start!, 0, def.range.end!, 0),
						Range.create(def.range.start!, 0, def.range.end!, 0)
					))
			);

			scope.constants
				.filter(constant => constant.position && constant.position.path === currentPath)
				.forEach(def => {
					const constantDef = DocumentSymbol.create(
						def.name,
						prettyKeywords(def.keyword),
						SymbolKind.Constant,
						Range.create(def.range.start!, 0, def.range.end!, 0),
						Range.create(def.range.start!, 0, def.range.end!, 0)
					);

					if (def.subItems.length > 0) {
						constantDef.children = def.subItems
							.filter(subitem => subitem.position && subitem.position.path === currentPath)
							.map(subitem => DocumentSymbol.create(
								subitem.name,
								prettyKeywords(subitem.keyword),
								SymbolKind.Property,
								Range.create(subitem.range.start!, 0, subitem.range.start!, 0),
								Range.create(subitem.range.end!, 0, subitem.range.end!, 0)
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
