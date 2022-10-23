"use strict";
/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", { value: true });
exports.deactivate = exports.activate = void 0;
const path = require("path");
const vscode_1 = require("vscode");
const node_1 = require("vscode-languageclient/node");
let client;
function activate(context) {
    // The server is implemented in node
    const serverModule = context.asAbsolutePath(path.join('server', 'out', 'server.js'));
    // The debug options for the server
    // --inspect=6009: runs the server in Node's Inspector mode so VS Code can attach to the server for debugging
    const debugOptions = { execArgv: ['--nolazy', '--inspect=6009'] };
    // If the extension is launched in debug mode then the debug server options are used
    // Otherwise the run options are used
    const serverOptions = {
        run: { module: serverModule, transport: node_1.TransportKind.ipc },
        debug: {
            module: serverModule,
            transport: node_1.TransportKind.ipc,
            options: debugOptions
        }
    };
    // Options to control the language client
    const clientOptions = {
        // Register the server for plain text documents
        documentSelector: [
            { language: 'rpgle' },
        ],
        synchronize: {
        // Notify the server about file changes to '.clientrc files contained in the workspace
        //fileEvents: workspace.createFileSystemWatcher('**/.clientrc'),
        }
    };
    // Create the language client and start the client.
    client = new node_1.LanguageClient('languageServerExample', 'Language Server Example', serverOptions, clientOptions);
    client.onReady().then(() => {
        client.onRequest("getUri", async (stringUri) => {
            const uri = vscode_1.Uri.parse(stringUri);
            let doc;
            try {
                doc = await vscode_1.workspace.openTextDocument(uri);
            }
            catch (e) {
                doc = undefined;
            }
            if (doc) {
                return doc.uri.toString();
            }
            else if (uri.scheme === `file`) {
                const basename = path.basename(uri.path);
                const [possibleFile] = await vscode_1.workspace.findFiles(`**/${basename}`, undefined, 1);
                if (possibleFile) {
                    return possibleFile.toString();
                }
            }
        });
        client.onRequest("getFile", async (stringUri) => {
            // Always assumes URI is valid. Use getUri first
            const uri = vscode_1.Uri.parse(stringUri);
            const doc = await vscode_1.workspace.openTextDocument(uri);
            if (doc) {
                return doc.getText();
            }
        });
    });
    // Start the client. This will also launch the server
    client.start();
    console.log(`started`);
}
exports.activate = activate;
function deactivate() {
    if (!client) {
        return undefined;
    }
    return client.stop();
}
exports.deactivate = deactivate;
//# sourceMappingURL=extension.js.map