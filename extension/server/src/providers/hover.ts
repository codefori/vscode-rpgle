import { Hover, HoverParams, MarkupKind, Range } from 'vscode-languageserver';
import { documents, parser, getReturnValue, getWordRangeAtPosition, prettyKeywords } from '.';
import Parser from '../../../../language/ile/parser';
import { URI } from 'vscode-uri';
import { Keywords } from '../../../../language/ile/parserTypes';
import Cache from '../../../../language/models/cache';
import Declaration from '../../../../language/models/declaration';
import { ParserFactory } from '../../../../language/parserFactory';

export default async function hoverProvider(params: HoverParams): Promise<Hover | undefined> {
	const currentPath = params.textDocument.uri;

	if (ParserFactory.isOpmFile(currentPath)) return;

	const currentLine = params.position.line;
	const document = documents.get(currentPath);

	if (document) {
		// Use only the warm cache; avoids triggering expensive include fetches on every hover
		const doc = parser.getParsedCache(currentPath);
		if (doc) {
			const word = getWordRangeAtPosition(document, params.position);

			// Walk left through the line text once to build the full dot-chain.
			// qualifiedName is used for display; parts drives Tier 2 symbol resolution.
			let qualifiedName: string | undefined;
			let parts: string[] = [];
			if (word) {
				const lineText = (document.getText().split(`\n`)[currentLine]) || ``;
				const character = Math.min(lineText.length - 1, Math.max(0, params.position.character));
				const wordMatch = /[\w\#\$@]/;
				let wordStart = character;
				while (wordStart > 0 && wordMatch.test(lineText.charAt(wordStart - 1))) wordStart--;

				if (wordStart > 0 && lineText.charAt(wordStart - 1) === `.`) {
					parts = [word];
					let scanPos = wordStart - 1;
					while (scanPos >= 0 && lineText.charAt(scanPos) === `.`) {
						let segEnd = scanPos;
						let segStart = segEnd;
						while (segStart > 0 && wordMatch.test(lineText.charAt(segStart - 1))) segStart--;
						if (segStart === segEnd) break;
						parts.unshift(lineText.substring(segStart, segEnd));
						scanPos = segStart - 1;
					}
					qualifiedName = parts.join(`.`);
				}
			}

			// Tier 1: offset-based lookup via stored references (fastest path)
			let symbol = Cache.referenceByOffset(currentPath, doc, document.offsetAt(params.position));

			// Tier 2: qualified name lookup using the chain built above.
			// Reference-tracking-independent; handles name collisions correctly.
			if (!symbol && parts.length >= 2) {
				let current: Declaration | undefined = doc.findDefinition(currentLine, parts[0]);
				for (let idx = 1; idx < parts.length && current; idx++) {
					current = current.subItems.find(sub => sub.name.toUpperCase() === parts[idx].toUpperCase());
				}
				if (current) symbol = current;
			}

			// Tier 3: plain word lookup
			if (!symbol) {
				if (!word) return;
				symbol = doc.findDefinition(currentLine, word);
			}

			if (symbol) {
				if (symbol.type === `procedure`) {

					// If a symbol is found, but there are no docs,
					// maybe the docs exist on a matching prototype?
					if (symbol.tags.length === 0) {
						const withDocs = doc.findAll(word || ``).find(p => p.type === `procedure` && p.tags.length > 0);
						if (withDocs) {
							symbol = withDocs;
						}
					}

					let markdown = ``;
					const returnValue = getReturnValue(symbol);

					const returnTag = symbol.tags.find(tag => tag.tag === `return`);
					const deprecatedTag = symbol.tags.find(tag => tag.tag === `deprecated`);

					// Deprecated notice
					if (deprecatedTag) {
						markdown += `**Deprecated:** ${deprecatedTag.content}\n\n`;
					}

					// Formatted code
					markdown += `\`\`\`vb\n${symbol.name}(`;

					if (symbol.subItems.length > 0) {
						markdown += `\n  ${symbol.subItems.map(parm => `${parm.name}: ${prettyKeywords(parm.keyword)}`).join(`,\n  `)}\n`;
					}

					markdown += `): ${returnValue}\n\`\`\` \n`;

					const titleTag = symbol.tags.find(tag => tag.tag === `title`);
					const descriptionTag = symbol.tags.find(tag => tag.tag === `description`);

					const header = [titleTag ? titleTag.content : undefined, descriptionTag ? descriptionTag.content : undefined].filter(x => x).join(` — `);

					// Header
					markdown += `${header}\n\n`;

					// Params
					markdown += symbol.subItems.map((parm) => `*@param* \`${parm.name.replace(new RegExp(`\\*`, `g`), `\\*`)}\` ${parm.tags.find(t => t.tag === `description`)?.content || ``}`).join(`\n\n`);

					// Return value
					if (returnTag) {
						markdown += `\n\n*@returns* ${returnTag.content}`;
					}

					if (symbol.position && currentPath !== symbol.position.path) {
						markdown += `\n\n*@file* \`${symbol.position.path}:${symbol.position.range.line + 1}\``;
					}

					return {
						contents: {
							kind: MarkupKind.Markdown,
							value: markdown
						}
					};
				} else {
					// Variable definition found
					const refs = symbol.references.length;
					const displayName = (symbol.type === `subitem` && qualifiedName) ? qualifiedName : symbol.name;

					let markdown = `\`${displayName} ${prettyKeywords(symbol.keyword)}\` (${refs} reference${refs === 1 ? `` : `s`})`;

					// Add description if available
					const descriptionTag = symbol.tags.find(tag => tag.tag === `description`);
					if (descriptionTag) {
						markdown += `\n\n${descriptionTag.content}`;
					}

					if (symbol.position && currentPath !== symbol.position.path) {
						markdown += `\n\n*@file* \`${symbol.position.path}:${symbol.position.range.line + 1}\``;
					}

					return {
						contents: {
							kind: MarkupKind.Markdown,
							value: markdown
						}
					};
				}

			} else {
				const lineContent = document.getText(Range.create(currentLine, 0, currentLine, 200));

				const includeDirective = Parser.getIncludeFromDirective(lineContent);

				if (includeDirective && parser.includeFileFetch) {
					const include = await parser.includeFileFetch(currentPath, includeDirective);
					let displayName = includeDirective;

					if (include.found && include.uri) {
						const foundUri = URI.parse(include.uri);

						if (foundUri.scheme === `member`) {
							const lastIndex = foundUri.fsPath.lastIndexOf(`.`);
							if (lastIndex >= 0) {
								displayName = foundUri.fsPath.substring(0, lastIndex);
							} else {
								displayName = foundUri.fsPath;
							}

						} else {
							displayName = foundUri.fsPath;
						}
					}

					return {
						contents: {
							kind: MarkupKind.Markdown,
							value: (include.found ? `\`${displayName}\`` : includeDirective) + ` (${include.found ? `found` : `not found`})`
						}
					};
				}
			}
		}
	}

	return;
}