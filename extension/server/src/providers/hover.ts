import { Hover, HoverParams, MarkupKind, Range } from 'vscode-languageserver';
import { documents, getWordRangeAtPosition, parser, prettyKeywords } from '.';
import Parser from "../../../../language/parser";
import { URI } from 'vscode-uri';
import { Keywords } from '../../../../language/parserTypes';

export default async function hoverProvider(params: HoverParams): Promise<Hover|undefined> {
	const currentPath = params.textDocument.uri;
	const currentLine = params.position.line;
	const document = documents.get(currentPath);

	if (document) {
		const doc = await parser.getDocs(currentPath, document.getText());
		if (doc) {
			const word = getWordRangeAtPosition(document, params.position);
			if (!word) return;

			const procedure = doc.procedures.find(proc => proc.name.toUpperCase() === word.toUpperCase());

			if (procedure) {
				let markdown = ``;
				let returnValue = `void`

				let returnKeywords: Keywords = {
					...procedure.keyword,
				};
				delete returnKeywords[`EXTPROC`];

				if (Object.keys(returnKeywords).length > 0) returnValue = prettyKeywords(returnKeywords);

				const returnTag = procedure.tags.find(tag => tag.tag === `return`);
				const deprecatedTag = procedure.tags.find(tag => tag.tag === `deprecated`);

				// Deprecated notice
				if (deprecatedTag) {
					markdown += `**Deprecated:** ${deprecatedTag.content}\n\n`;
				}

				// Formatted code
				markdown += `\`\`\`vb\n${procedure.name}(`;

				if (procedure.subItems.length > 0) {
					markdown += `\n  ${procedure.subItems.map(parm => `${parm.name}: ${prettyKeywords(parm.keyword)}`).join(`,\n  `)}\n`;
				}

				markdown += `): ${returnValue}\n\`\`\` \n`;

				// Description
				if (procedure.description)
					markdown += `${procedure.description}\n\n`;

				// Params
				const paramTags = procedure.tags.filter(tag => tag.tag === `param`);
				markdown += procedure.subItems.map((parm, i) => `*@param* \`${parm.name.replace(new RegExp(`\\*`, `g`), `\\*`)}\` ${paramTags[i] ? paramTags[i].content : parm.description}`).join(`\n\n`);

				// Return value
				if (returnTag) {
					markdown += `\n\n*@returns* ${returnTag.content}`;
				}

				if (procedure.position && currentPath !== procedure.position.path) {
					markdown += `\n\n*@file* \`${procedure.position.path}:${procedure.position.range.line+1}\``;
				}
				
				return {
					contents: {
						kind: MarkupKind.Markdown,
						value: markdown
					}
				};
			} else {
				// If they're inside of a procedure, let's get the stuff from there too
				const currentProcedure = doc.procedures.find(proc => proc.range.start && proc.range.end && currentLine >= proc.range.start && currentLine <= proc.range.end);
				let theVariable;

				if (currentProcedure && currentProcedure.scope) {
					theVariable = currentProcedure.scope.find(word);
				}

				if (!theVariable) {
					theVariable = doc.find(word);
				}

				if (theVariable) {
					// Variable definition found
					const refs = theVariable.references.length;
										
					let markdown = `\`${theVariable.name} ${prettyKeywords(theVariable.keyword)}\` (${refs} reference${refs === 1 ? `` : `s`})`;

					if (theVariable.position && currentPath !== theVariable.position.path) {
						markdown += `\n\n*@file* \`${theVariable.position.path}:${theVariable.position.range.line+1}\``;
					}

					return {
						contents: {
							kind: MarkupKind.Markdown,
							value: markdown
						}
					};

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
	}

	return;
}