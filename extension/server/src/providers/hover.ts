import { Hover, HoverParams, MarkupKind, Range } from 'vscode-languageserver';
import { documents, getReturnValue, getWordRangeAtPosition, parser, prettyKeywords } from '.';
import Parser from "../../../../language/parser";
import { URI } from 'vscode-uri';
import { Keywords } from '../../../../language/parserTypes';

export default async function hoverProvider(params: HoverParams): Promise<Hover | undefined> {
	const currentPath = params.textDocument.uri;
	const currentLine = params.position.line;
	const document = documents.get(currentPath);

	if (document) {
		const doc = await parser.getDocs(currentPath, document.getText());
		if (doc) {
			const word = getWordRangeAtPosition(document, params.position);
			if (!word) return;

			let symbol = doc.findDefinition(currentLine, word);

			if (symbol) {
				if (symbol.type === `procedure`) {

					// If a symbol is found, but there are no docs,
					// maybe the docs exist on a matching prototype?
					if (symbol.tags.length === 0) {
						const withDocs = doc.findAll(word).find(p => p.type === `procedure` && p.tags.length > 0);
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

					let markdown = `\`${symbol.name} ${prettyKeywords(symbol.keyword)}\` (${refs} reference${refs === 1 ? `` : `s`})`;

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
							const lastIndex = foundUri.path.lastIndexOf(`.`);
							if (lastIndex >= 0) {
								displayName = foundUri.path.substring(0, lastIndex);
							} else {
								displayName = foundUri.path;
							}

							if (displayName.startsWith(`/`)) displayName = displayName.substring(1);

						} else {
							displayName = foundUri.path;
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