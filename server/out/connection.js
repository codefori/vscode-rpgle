"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.getFileRequest = exports.validateUri = exports.connection = void 0;
const node_1 = require("vscode-languageserver/node");
const providers_1 = require("./providers");
// Create a connection for the server, using Node's IPC as a transport.
// Also include all preview / proposed LSP features.
exports.connection = (0, node_1.createConnection)(node_1.ProposedFeatures.all);
async function validateUri(stringUri, scheme = ``) {
    // First, check local cache
    const possibleCachedFile = (0, providers_1.findFile)(stringUri, scheme);
    if (possibleCachedFile)
        return possibleCachedFile;
    // Then reach out to the extension to find it
    const uri = await exports.connection.sendRequest("getUri", stringUri);
    if (uri)
        return uri;
}
exports.validateUri = validateUri;
async function getFileRequest(uri) {
    // First, check if it's local
    const localCacheDoc = providers_1.documents.get(uri);
    if (localCacheDoc)
        return localCacheDoc.getText();
    // If not, then grab it from remote
    const body = await exports.connection.sendRequest("getFile", uri);
    if (body) {
        // Gets cached automatically due to extension called `openTextDocument`
        // which then stores into `documents` automatically due to the `listen`
        return body;
    }
}
exports.getFileRequest = getFileRequest;
//# sourceMappingURL=connection.js.map