import { connect } from 'http2';
import {
	createConnection,
	DidChangeWatchedFilesParams,
	ProposedFeatures,
	_Connection
} from 'vscode-languageserver/node';


import { documents, findFile } from './providers';

// Create a connection for the server, using Node's IPC as a transport.
// Also include all preview / proposed LSP features.
export const connection: _Connection = createConnection(ProposedFeatures.all);

export let watchedFilesChangeEvent: ((params: DidChangeWatchedFilesParams) => void)[] = [];
connection.onDidChangeWatchedFiles((params: DidChangeWatchedFilesParams) => {
	watchedFilesChangeEvent.forEach(editEvent => editEvent(params));
})

export async function validateUri(stringUri: string, scheme = ``) {
	// First, check local cache
	const possibleCachedFile = findFile(stringUri, scheme);
	if (possibleCachedFile) return possibleCachedFile;

	// Then reach out to the extension to find it
	const uri: string|undefined = await connection.sendRequest("getUri", stringUri);
	if (uri) return uri; 

	return;
}

export async function getFileRequest(uri: string) {
	// First, check if it's local
	const localCacheDoc = documents.get(uri);
	if (localCacheDoc) return localCacheDoc.getText();

	// If not, then grab it from remote
	const body: string|undefined = await connection.sendRequest("getFile", uri);
	if (body) { 
		// TODO.. cache it?
		return body; 
	}

	return;
}

export function getWorkingDirectory(): Promise<string|undefined> {
	return connection.sendRequest("getWorkingDirectory");
}

export function getObject(objectPath: string): Promise<object[]> {
	return connection.sendRequest("getObject", objectPath);
}

export function getProjectFiles(): Promise<string[]|undefined> {
	return connection.sendRequest("getProjectFiles");
}

export interface PossibleInclude {
	uri: string;
	relative: string
};

export function getIncludesUris(uri: string): Promise<PossibleInclude[]> {
	return connection.sendRequest(`getIncludesUris`, uri);
}

export interface BindingDirectory {
	lib?: string;
	name: string;
}

export function symbolLookup(symbol: string, binders: BindingDirectory[]): Promise<string[]|undefined> {
	return connection.sendRequest(`symbolLookup`, {
		symbol,
		binders
	});
}