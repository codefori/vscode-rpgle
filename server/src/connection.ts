import {
	createConnection,
	ProposedFeatures,
	_Connection
} from 'vscode-languageserver/node';


import { documents, findFile } from './providers';

// Create a connection for the server, using Node's IPC as a transport.
// Also include all preview / proposed LSP features.
export const connection: _Connection = createConnection(ProposedFeatures.all);

export async function validateUri(stringUri: string, scheme = ``) {
	// First, check local cache
	const possibleCachedFile = findFile(stringUri, scheme);
	if (possibleCachedFile) return possibleCachedFile;

	// Then reach out to the extension to find it
	const uri: string|undefined = await connection.sendRequest("getUri", stringUri);
	if (uri) return uri; 
}

export async function getFileRequest(uri: string) {
	// First, check if it's local
	const localCacheDoc = documents.get(uri);
	if (localCacheDoc) return localCacheDoc.getText();

	// If not, then grab it from remote
	const body: string|undefined = await connection.sendRequest("getFile", uri);
	if (body) { 
		// Gets cached automatically due to extension called `openTextDocument`
		// which then stores into `documents` automatically due to the `listen`
		return body; 
	}
}