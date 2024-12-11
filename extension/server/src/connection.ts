import { IBMiMember } from '@halcyontech/vscode-ibmi-types';

import {
	createConnection,
	DidChangeWatchedFilesParams,
	ProposedFeatures,
	_Connection,
	WorkspaceFolder
} from 'vscode-languageserver/node';

import PQueue from 'p-queue';

import { documents, findFile } from './providers';
import { includePath } from './providers/project';

// Create a connection for the server, using Node's IPC as a transport.
// Also include all preview / proposed LSP features.
export const connection: _Connection = createConnection(ProposedFeatures.all);

const queue = new PQueue();

export let watchedFilesChangeEvent: ((params: DidChangeWatchedFilesParams) => void)[] = [];
connection.onDidChangeWatchedFiles((params: DidChangeWatchedFilesParams) => {
	watchedFilesChangeEvent.forEach(editEvent => editEvent(params));
})

export async function validateUri(stringUri: string, scheme = ``) {
	// First, check local cache
	const possibleCachedFile = findFile(stringUri, scheme);
	if (possibleCachedFile) return possibleCachedFile;

	console.log(`Validating file from server: ${stringUri}`);

	// Then reach out to the extension to find it
	const uri: string|undefined = await connection.sendRequest("getUri", stringUri);
	if (uri) return uri; 

	return;
}

export async function getFileRequest(uri: string) {
	// First, check if it's local
	const localCacheDoc = documents.get(uri);
	if (localCacheDoc) return localCacheDoc.getText();

	console.log(`Fetching file from server: ${uri}`);

	// If not, then grab it from remote
	const body: string|undefined = await connection.sendRequest("getFile", uri);
	if (body) {
		// TODO.. cache it?
		return body; 
	}

	return;
}

export let resolvedMembers: {[baseUri: string]: {[fileKey: string]: IBMiMember}} = {};
export let resolvedStreamfiles: {[baseUri: string]: {[fileKey: string]: string}} = {};

export async function memberResolve(baseUri: string, member: string, file: string): Promise<IBMiMember|undefined> {
	const fileKey = file+member;

	if (resolvedMembers[baseUri] && resolvedMembers[baseUri][fileKey]) return resolvedMembers[baseUri][fileKey];

	try {
		const resolvedMember = await queue.add(() => {return connection.sendRequest("memberResolve", [member, file])}) as IBMiMember|undefined;
		// const resolvedMember = await connection.sendRequest("memberResolve", [member, file]) as IBMiMember|undefined;

		if (resolvedMember) {
			if (!resolvedMembers[baseUri]) resolvedMembers[baseUri] = {};
			resolvedMembers[baseUri][fileKey] = resolvedMember;
		}

		return resolvedMember;
	} catch (e) {
		console.log(`Member resolve failed.`);
		console.log(JSON.stringify({baseUri, member, file}));
		console.log(e);
	}

	return undefined;
}

export async function streamfileResolve(baseUri: string, base: string[]): Promise<string|undefined> {
	const baseString = base.join(`-`);
	if (resolvedStreamfiles[baseUri] && resolvedStreamfiles[baseUri][baseString]) return resolvedStreamfiles[baseUri][baseString];

	const workspace = await getWorkspaceFolder(baseUri);

	const paths = (workspace ? includePath[workspace.uri] : []) || [];

	try {
		const resolvedPath = await queue.add(() => {return connection.sendRequest("streamfileResolve", [base, paths])}) as string|undefined;
		//  const resolvedPath = await connection.sendRequest("streamfileResolve", [base, paths]) as string|undefined;

		if (resolvedPath) {
			if (!resolvedStreamfiles[baseUri]) resolvedStreamfiles[baseUri] = {};
			resolvedStreamfiles[baseUri][baseString] = resolvedPath;
		}

		return resolvedPath;
	} catch (e) {
		console.log(`Streamfile resolve failed.`);
		console.log(JSON.stringify({baseUri, base, paths}));
		console.log(e);
	}

	return undefined;
}

export function getWorkingDirectory(): Promise<string|undefined> {
	return connection.sendRequest("getWorkingDirectory");
}

export function getObject(objectPath: string): Promise<object[]> {
	return connection.sendRequest("getObject", objectPath);
}

export interface PossibleInclude {
	uri: string;
	relative: string
};

export async function getWorkspaceFolder(baseUri: string) {
	let workspaceFolder: WorkspaceFolder | undefined;

	const workspaceFolders = await connection.workspace.getWorkspaceFolders();

	if (workspaceFolders) {
		workspaceFolder = workspaceFolders.find(folderUri => baseUri.startsWith(folderUri.uri))
	}

	return workspaceFolder
}

export interface BindingDirectory {
	lib?: string;
	name: string;
}