"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.refreshDiagnostics = void 0;
const vscode_languageserver_1 = require("vscode-languageserver");
const connection_1 = require("../connection");
const linter_1 = require("../language/linter");
const calculateOffset = (document, error) => {
    const offset = error.offset;
    if (offset && offset.position !== undefined && offset.end !== undefined) {
        const docOffsetStart = document.offsetAt(error.range.start) + offset.position;
        const docOffsetEnd = document.offsetAt(error.range.start) + offset.end;
        return vscode_languageserver_1.Range.create(document.positionAt(docOffsetStart), document.positionAt(docOffsetEnd));
    }
    else {
        return vscode_languageserver_1.Range.create(error.range.start, error.range.end);
    }
};
function refreshDiagnostics(document, docs) {
    const isFree = (document.getText(vscode_languageserver_1.Range.create(0, 0, 0, 6)).toUpperCase() === `**FREE`);
    if (isFree) {
        const text = document.getText();
        const indentDiags = [];
        const generalDiags = [];
        //const options = await this.getLinterOptions(document.uri);
        const options = {
            indent: 2,
            IncorrectVariableCase: true,
            NoUnreferenced: true
        };
        let detail;
        try {
            detail = linter_1.default.getErrors({
                uri: document.uri,
                content: text,
            }, options, docs);
        }
        catch (e) {
            console.log(`Error linting ${document.uri}: ${e.message}`);
            console.log(e.stack);
            return;
        }
        const indentErrors = detail.indentErrors;
        const errors = detail.errors;
        if (indentErrors.length > 0) {
            indentErrors.forEach(error => {
                const range = vscode_languageserver_1.Range.create(error.line, 0, error.line, error.currentIndent);
                indentDiags.push(vscode_languageserver_1.Diagnostic.create(range, `Incorrect indentation. Expected ${error.expectedIndent}, got ${error.currentIndent}`, vscode_languageserver_1.DiagnosticSeverity.Warning));
            });
        }
        if (errors.length > 0) {
            errors.forEach(error => {
                const range = calculateOffset(document, error);
                const diagnostic = vscode_languageserver_1.Diagnostic.create(range, linter_1.default.getErrorText(error.type), vscode_languageserver_1.DiagnosticSeverity.Warning);
                generalDiags.push(diagnostic);
            });
        }
        connection_1.connection.sendDiagnostics({ uri: document.uri, diagnostics: [...indentDiags, ...generalDiags] });
    }
}
exports.refreshDiagnostics = refreshDiagnostics;
//# sourceMappingURL=lintProvider.js.map