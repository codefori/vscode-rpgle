"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
/* eslint-disable no-case-declarations */
/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
const node_1 = require("vscode-languageserver/node");
const documentSymbols_1 = require("./providers/documentSymbols");
const providers_1 = require("./providers");
const definition_1 = require("./providers/definition");
const vscode_uri_1 = require("vscode-uri");
const completionItem_1 = require("./providers/completionItem");
const hover_1 = require("./providers/hover");
const connection_1 = require("./connection");
const lintProvider_1 = require("./providers/lintProvider");
let hasConfigurationCapability = false;
let hasWorkspaceFolderCapability = false;
let hasDiagnosticRelatedInformationCapability = false;
connection_1.connection.onInitialize((params) => {
    const capabilities = params.capabilities;
    // Does the client support the `workspace/configuration` request?
    // If not, we fall back using global settings.
    hasConfigurationCapability = !!(capabilities.workspace && !!capabilities.workspace.configuration);
    hasWorkspaceFolderCapability = !!(capabilities.workspace && !!capabilities.workspace.workspaceFolders);
    hasDiagnosticRelatedInformationCapability = !!(capabilities.textDocument &&
        capabilities.textDocument.publishDiagnostics &&
        capabilities.textDocument.publishDiagnostics.relatedInformation);
    const result = {
        capabilities: {
            textDocumentSync: node_1.TextDocumentSyncKind.Incremental,
            // Tell the client that this server supports code completion.
            documentSymbolProvider: true,
            definitionProvider: true,
            completionProvider: {
                triggerCharacters: [` `, `.`, `:`]
            },
            hoverProvider: true,
        }
    };
    if (hasWorkspaceFolderCapability) {
        result.capabilities.workspace = {
            workspaceFolders: {
                supported: true
            },
        };
    }
    return result;
});
providers_1.parser.setIncludeFileFetch(async (stringUri, includeString) => {
    const currentUri = vscode_uri_1.URI.parse(stringUri);
    const [_, baseLibrary, baseSourceFile, basename] = currentUri.path.split(`/`);
    let cleanString;
    switch (currentUri.scheme) {
        case `member`:
            if (includeString.startsWith(`'`) && includeString.endsWith(`'`)) {
                // IFS fetch
                cleanString = includeString.substring(1, includeString.length - 1);
                // TODO:....
            }
            else {
                // Member fetch
                // Split by /,
                const parts = includeString.split(`/`).map(s => s.split(`,`)).flat();
                cleanString = [
                    ``,
                    parts[parts.length - 3] ? parts[parts.length - 3] : baseLibrary,
                    parts[parts.length - 2] ? parts[parts.length - 2] : `QRPGLEREF`,
                    parts[parts.length - 1] + `.rpgleinc`
                ].join(`/`);
                cleanString = vscode_uri_1.URI.from({
                    scheme: `member`,
                    path: cleanString
                }).toString();
            }
            break;
        case `file`:
            cleanString = includeString;
            if (cleanString.startsWith(`'`) && cleanString.endsWith(`'`)) {
                cleanString = cleanString.substring(1, cleanString.length - 1);
            }
            break;
    }
    if (cleanString) {
        const validUri = await (0, connection_1.validateUri)(cleanString, currentUri.scheme);
        if (validUri) {
            const validSource = await (0, connection_1.getFileRequest)(validUri);
            if (validSource) {
                return {
                    found: true,
                    uri: validUri,
                    lines: validSource.split(`\n`)
                };
            }
        }
    }
    return {
        found: false
    };
});
connection_1.connection.onDocumentSymbol(documentSymbols_1.default);
connection_1.connection.onDefinition(definition_1.default);
connection_1.connection.onCompletion(completionItem_1.default);
connection_1.connection.onHover(hover_1.default);
providers_1.documents.onDidChangeContent(handler => {
    providers_1.parser.getDocs(handler.document.uri, handler.document.getText(), {
        withIncludes: true,
        ignoreCache: true
    }).then(cache => {
        if (cache)
            (0, lintProvider_1.refreshDiagnostics)(handler.document, cache);
    });
});
// Make the text document manager listen on the connection
// for open, change and close text document events
providers_1.documents.listen(connection_1.connection);
// Listen on the connection
connection_1.connection.listen();
//# sourceMappingURL=server.js.map