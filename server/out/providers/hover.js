"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const vscode_languageserver_1 = require("vscode-languageserver");
const _1 = require(".");
const parser_1 = require("../language/parser");
async function hoverProvider(params) {
    const currentPath = params.textDocument.uri;
    const currentLine = params.position.line;
    const document = _1.documents.get(currentPath);
    if (document) {
        const doc = await _1.parser.getDocs(currentPath, document.getText());
        if (doc) {
            const word = (0, _1.getWordRangeAtPosition)(document, params.position);
            if (!word)
                return;
            const procedure = doc.procedures.find(proc => proc.name.toUpperCase() === word.toUpperCase());
            if (procedure) {
                let markdown = ``;
                let retrunValue = procedure.keywords.filter(keyword => keyword !== `EXTPROC`);
                if (retrunValue.length === 0)
                    retrunValue = [`void`];
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
                markdown += procedure.subItems.map(parm => `*@param* \`${parm.name.replace(new RegExp(`\\*`, `g`), `\\*`)}\` ${parm.description}`).join(`\n\n`);
                // Return value
                if (returnTag) {
                    markdown += `\n\n*@returns* ${returnTag.content}`;
                }
                if (procedure.position) {
                    markdown += `\n\n*@file* \`${procedure.position.path}:${procedure.position.line + 1}\``;
                }
                return {
                    contents: {
                        kind: vscode_languageserver_1.MarkupKind.Markdown,
                        value: markdown
                    }
                };
            }
            else {
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
                            kind: vscode_languageserver_1.MarkupKind.Markdown,
                            value: `\`${theVariable.name}\`: \`${theVariable.keywords.join(` `).trim()}\``
                        }
                    };
                }
                else {
                    const lineContent = document.getText(vscode_languageserver_1.Range.create(currentLine, 0, currentLine, 200));
                    const includeDirective = parser_1.default.getIncludeFromDirective(lineContent);
                    if (includeDirective) {
                        const include = await _1.parser.includeFileFetch(currentPath, includeDirective);
                        return {
                            contents: {
                                kind: vscode_languageserver_1.MarkupKind.Markdown,
                                value: (include.found ? `\`${include.uri}\`` : includeDirective) + ` (${include.found ? `` : `not found`})`
                            }
                        };
                    }
                }
            }
        }
    }
    return;
}
exports.default = hoverProvider;
//# sourceMappingURL=hover.js.map