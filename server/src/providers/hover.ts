import path = require('path');
import { Hover, HoverParams, MarkupKind, Range } from 'vscode-languageserver';
import { documents, getWordRangeAtPosition, parser } from '.';
import Parser from "../language/parser";

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
				let retrunValue = procedure.keywords.filter(keyword => keyword !== `EXTPROC`);
				if (retrunValue.length === 0) retrunValue = [`void`];

				const returnTag = procedure.tags.find(tag => tag.tag === `return`);
				const deprecatedTag = procedure.tags.find(tag => tag.tag === `deprecated`);

				// Deprecated notice
				if (deprecatedTag) {
					markdown += `**Deprecated:** ${deprecatedTag.content}\n\n`;
				}

				// Formatted code
				markdown += `\`\`\`vb\n${procedure.name}(`;

				if (procedure.subItems.length > 0) {
					markdown += `\n  ${procedure.subItems.map(parm => `${parm.name}: ${parm.keywords.join(` `).trim()}`).join(`,\n  `)}\n`;
				}

				markdown += `): ${retrunValue.join(` `)}\n\`\`\` \n`;

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

				if (procedure.position) {
					markdown += `\n\n*@file* \`${procedure.position.path}:${procedure.position.line+1}\``;
				}
				
				return {
					contents: {
						kind: MarkupKind.Markdown,
						value: markdown
					}
				};
			} else {
				// If they're inside of a procedure, let's get the stuff from there too
				const currentProcedure = doc.procedures.find(proc => currentLine >= proc.range.start && currentLine <= proc.range.end);
				let theVariable;

				if (currentProcedure && currentProcedure.scope) {
					theVariable = currentProcedure.scope.find(word);
				}

				if (!theVariable) {
					theVariable = doc.find(word);
				}

				if (theVariable) {
					// Variable definition found
					return {
						contents: {
							kind: MarkupKind.Markdown,
							value: `\`${theVariable.name}\`: \`${theVariable.keywords.join(` `).trim()}\``
						}
					};

				} else {
					const lineContent = document.getText(Range.create(currentLine, 0, currentLine, 200));

					const includeDirective = Parser.getIncludeFromDirective(lineContent);
					
					if (includeDirective) {
						const include = await parser.includeFileFetch(currentPath, includeDirective);
						const displayName = include.uri ? path.basename(include.uri) : includeDirective;

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