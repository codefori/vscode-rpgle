"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const vscode_languageserver_1 = require("vscode-languageserver");
const _1 = require(".");
async function definitionProvider(handler) {
    const currentPath = handler.textDocument.uri;
    const currentLine = handler.position.line;
    const document = _1.documents.get(currentPath);
    if (document) {
        const doc = await _1.parser.getDocs(currentPath, document.getText());
        if (doc) {
            const word = (0, _1.getWordRangeAtPosition)(document, handler.position);
            const def = doc.findDefinition(currentLine, word);
            if (def) {
                return vscode_languageserver_1.Location.create(def.position.path, vscode_languageserver_1.Range.create(def.position.line, 0, def.position.line, 0));
            }
        }
    }
    return null;
}
exports.default = definitionProvider;
//# sourceMappingURL=definition.js.map