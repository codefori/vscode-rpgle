"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const vscode_languageserver_1 = require("vscode-languageserver");
const _1 = require(".");
async function documentSymbolProvider(handler) {
    const currentPath = handler.textDocument.uri;
    const symbols = [];
    const document = _1.documents.get(currentPath);
    if (document) {
        const doc = await _1.parser.getDocs(currentPath, document.getText());
        /**
         * @param {Cache} scope
         * @returns {vscode.DocumentSymbol[]}
         */
        const getScopeVars = (scope) => {
            const currentScopeDefs = [];
            currentScopeDefs.push(...scope.subroutines.filter(sub => sub.position && sub.position.path === currentPath)
                .filter(def => def.range.start)
                .map(def => vscode_languageserver_1.DocumentSymbol.create(def.name, def.keywords.join(` `).trim(), vscode_languageserver_1.SymbolKind.Function, vscode_languageserver_1.Range.create(def.range.start, 0, def.range.end, 0), vscode_languageserver_1.Range.create(def.range.start, 0, def.range.start, 0))), ...scope.variables
                .filter(variable => variable.position && variable.position.path === currentPath)
                .map(def => vscode_languageserver_1.DocumentSymbol.create(def.name, def.keywords.join(` `).trim(), vscode_languageserver_1.SymbolKind.Variable, vscode_languageserver_1.Range.create(def.position.line, 0, def.position.line, 0), vscode_languageserver_1.Range.create(def.position.line, 0, def.position.line, 0))), ...scope.constants
                .filter(constant => constant.position && constant.position.path === currentPath)
                .map(def => vscode_languageserver_1.DocumentSymbol.create(def.name, def.keywords.join(` `).trim(), vscode_languageserver_1.SymbolKind.Constant, vscode_languageserver_1.Range.create(def.position.line, 0, def.position.line, 0), vscode_languageserver_1.Range.create(def.position.line, 0, def.position.line, 0))));
            scope.files
                .filter(struct => struct.position && struct.position.path === currentPath)
                .forEach(file => {
                const fileDef = vscode_languageserver_1.DocumentSymbol.create(file.name, file.keywords.join(` `).trim(), vscode_languageserver_1.SymbolKind.File, vscode_languageserver_1.Range.create(file.position.line, 0, file.position.line, 0), vscode_languageserver_1.Range.create(file.position.line, 0, file.position.line, 0));
                fileDef.children = [];
                file.subItems
                    .filter(recordFormat => recordFormat.position && recordFormat.position.path === currentPath)
                    .forEach(recordFormat => {
                    const recordFormatDef = vscode_languageserver_1.DocumentSymbol.create(recordFormat.name, recordFormat.keywords.join(` `).trim(), vscode_languageserver_1.SymbolKind.Struct, vscode_languageserver_1.Range.create(recordFormat.position.line, 0, recordFormat.position.line, 0), vscode_languageserver_1.Range.create(recordFormat.position.line, 0, recordFormat.position.line, 0));
                    recordFormatDef.children = recordFormat.subItems
                        .filter(subitem => subitem.position && subitem.position.path === currentPath)
                        .map(subitem => vscode_languageserver_1.DocumentSymbol.create(subitem.name, subitem.keywords.join(` `).trim(), vscode_languageserver_1.SymbolKind.Property, vscode_languageserver_1.Range.create(subitem.position.line, 0, subitem.position.line, 0), vscode_languageserver_1.Range.create(subitem.position.line, 0, subitem.position.line, 0)));
                    if (fileDef.children) {
                        fileDef.children.push(recordFormatDef);
                    }
                });
                currentScopeDefs.push(fileDef);
            });
            scope.structs
                .filter(struct => struct.position && struct.position.path === currentPath)
                .forEach(struct => {
                const structDef = vscode_languageserver_1.DocumentSymbol.create(struct.name, struct.keywords.join(` `).trim(), vscode_languageserver_1.SymbolKind.Struct, vscode_languageserver_1.Range.create(struct.range.start, 0, struct.range.end, 0), vscode_languageserver_1.Range.create(struct.range.start, 0, struct.range.start, 0));
                structDef.children = struct.subItems
                    .filter(subitem => subitem.position && subitem.position.path === currentPath)
                    .map(subitem => vscode_languageserver_1.DocumentSymbol.create(subitem.name, subitem.keywords.join(` `).trim(), vscode_languageserver_1.SymbolKind.Property, vscode_languageserver_1.Range.create(subitem.position.line, 0, subitem.position.line, 0), vscode_languageserver_1.Range.create(subitem.position.line, 0, subitem.position.line, 0)));
                currentScopeDefs.push(structDef);
            });
            return currentScopeDefs;
        };
        if (doc) {
            symbols.push(...getScopeVars(doc));
            doc.procedures
                .filter(proc => proc.position && proc.position.path === currentPath)
                .forEach(proc => {
                const procDef = vscode_languageserver_1.DocumentSymbol.create(proc.name, proc.keywords.join(` `).trim(), vscode_languageserver_1.SymbolKind.Function, vscode_languageserver_1.Range.create(proc.range.start, 0, proc.range.end, 0), vscode_languageserver_1.Range.create(proc.range.start, 0, proc.range.start, 0));
                procDef.children = proc.subItems
                    .filter(subitem => subitem.position && subitem.position.path === currentPath)
                    .map(subitem => vscode_languageserver_1.DocumentSymbol.create(subitem.name, subitem.keywords.join(` `).trim(), vscode_languageserver_1.SymbolKind.Property, vscode_languageserver_1.Range.create(subitem.position.line, 0, subitem.position.line, 0), vscode_languageserver_1.Range.create(subitem.position.line, 0, subitem.position.line, 0)));
                if (proc.scope && procDef.children) {
                    procDef.children = getScopeVars(proc.scope);
                }
                symbols.push(procDef);
            });
        }
    }
    return symbols;
}
exports.default = documentSymbolProvider;
//# sourceMappingURL=documentSymbols.js.map