"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.getWordRangeAtPosition = exports.parser = exports.findFile = exports.documents = void 0;
const node_1 = require("vscode-languageserver/node");
const vscode_languageserver_textdocument_1 = require("vscode-languageserver-textdocument");
const parser_1 = require("../language/parser");
// Create a simple text document manager.
exports.documents = new node_1.TextDocuments(vscode_languageserver_textdocument_1.TextDocument);
function findFile(fileString, scheme = ``) {
    return exports.documents.keys().find(fileUri => fileUri.includes(fileString) && fileUri.startsWith(`${scheme}:`));
}
exports.findFile = findFile;
exports.parser = new parser_1.default();
function getWordRangeAtPosition(document, position) {
    const lines = document.getText().split(`\n`); // Safe to assume \n because \r is then at end of lines
    const line = Math.min(lines.length - 1, Math.max(0, position.line));
    const lineText = lines[line];
    const character = Math.min(lineText.length - 1, Math.max(0, position.character));
    let startChar = character;
    while (startChar > 0 && !/[\s\W]/.test(lineText.charAt(startChar - 1))) {
        startChar -= 1;
    }
    let endChar = character;
    while (endChar < lineText.length && (!/[\s\W]/.test(lineText.charAt(endChar + 1)))) {
        endChar += 1;
    }
    if (startChar === endChar)
        return undefined;
    else
        return document.getText(node_1.Range.create(line, startChar, line, endChar + 1));
}
exports.getWordRangeAtPosition = getWordRangeAtPosition;
//# sourceMappingURL=index.js.map