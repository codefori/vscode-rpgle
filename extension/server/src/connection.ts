import { IBMiMember } from '@halcyontech/vscode-ibmi-types';

import {
	createConnection,
	DidChangeWatchedFilesParams,
	ProposedFeatures,
	_Connection,
	WorkspaceFolder
} from 'vscode-languageserver/node';

import PQueue from 'p-queue';

import { documents, findFile, parser } from './providers';
import { includePath } from './providers/project';

// Create a connection for the server, using Node's IPC as a transport.
// Also include all preview / proposed LSP features.
export const connection: _Connection = createConnection(ProposedFeatures.all);

const queue = new PQueue();

export let watchedFilesChangeEvent: ((params: DidChangeWatchedFilesParams) => void)[] = [];
connection.onDidChangeWatchedFiles((params: DidChangeWatchedFilesParams) => {
	watchedFilesChangeEvent.forEach(editEvent => editEvent(params));
})

function logWithTimestamp(message: string) {
	const now = new Date();
	const timestamp = now.toTimeString().split(' ')[0] + '.' + now.getMilliseconds().toString().padStart(3, '0');
	console.log(`[${timestamp}] ${message}`);
}

export async function validateUri(stringUri: string, scheme = ``) {
	const startTime = Date.now();

	// First, check local cache
	const possibleCachedFile = findFile(stringUri, scheme);
	if (possibleCachedFile) {
		const duration = Date.now() - startTime;
		logWithTimestamp(`URI validation: ${stringUri} (${duration}ms, local cache)`);
		return possibleCachedFile;
	}

	logWithTimestamp(`URI validation: ${stringUri} (requesting from server...)`);

	// Then reach out to the extension to find it
	const uri: string|undefined = await connection.sendRequest("getUri", stringUri);
	const duration = Date.now() - startTime;

	if (uri) {
		logWithTimestamp(`URI validation: ${stringUri} -> ${uri} (${duration}ms, server)`);
		return uri;
	}

	logWithTimestamp(`URI validation: ${stringUri} (${duration}ms, NOT FOUND)`);
	return;
}

// Track include files being fetched (to skip debounce on open)
export const filesBeingFetchedForIncludes = new Set<string>();

export async function getFileRequest(uri: string, skipDebounce: boolean = false) {
	const startTime = Date.now();
	const fileName = uri.split('/').pop() || uri;

	// First, check if it's local
	const localCacheDoc = documents.get(uri);
	if (localCacheDoc) {
		const duration = Date.now() - startTime;
		logWithTimestamp(`File fetch: ${fileName} (${duration}ms, local documents cache)`);
		return localCacheDoc.getText();
	}

	logWithTimestamp(`File fetch: ${fileName} (requesting from server...)`);

	// If this is an include file fetch, track it to skip debounce
	if (skipDebounce) {
		filesBeingFetchedForIncludes.add(uri);
	}

	try {
		// If not, then grab it from remote
		const body: string|undefined = await connection.sendRequest("getFile", uri);
		const duration = Date.now() - startTime;

		if (body) {
			logWithTimestamp(`File fetch: ${fileName} (${duration}ms, ${body.length} bytes from server)`);
			// TODO.. cache it?
			return body;
		}

		logWithTimestamp(`File fetch: ${fileName} (${duration}ms, NOT FOUND)`);
		return;
	} finally {
		// Always clean up tracking
		if (skipDebounce) {
			filesBeingFetchedForIncludes.delete(uri);
		}
	}
}

export let resolvedMembers: {[baseUri: string]: {[fileKey: string]: IBMiMember}} = {};
export let resolvedStreamfiles: {[baseUri: string]: {[fileKey: string]: string}} = {};

export async function memberResolve(baseUri: string, member: string, file: string): Promise<IBMiMember|undefined> {
	// Normalize baseUri by removing query parameters to ensure consistent cache keys
	const normalizedBaseUri = baseUri.split('?')[0];
	const fileKey = file+member;
	const startTime = Date.now();

	// Check cache
	if (resolvedMembers[normalizedBaseUri] && resolvedMembers[normalizedBaseUri][fileKey]) {
		const cached = resolvedMembers[normalizedBaseUri][fileKey];
		const duration = Date.now() - startTime;
		logWithTimestamp(`Member resolve CACHE HIT: ${file}/${member} -> ${cached.library}/${cached.file}/${cached.name} (${duration}ms)`);
		return cached;
	}

	logWithTimestamp(`Member resolve CACHE MISS: ${file}/${member} (baseUri=${normalizedBaseUri})`);

	try {
		const resolvedMember = await queue.add(() => {return connection.sendRequest("memberResolve", [member, file])}) as IBMiMember|undefined;
		const duration = Date.now() - startTime;

		if (resolvedMember) {
			logWithTimestamp(`Member resolve SUCCESS: ${file}/${member} -> ${resolvedMember.library}/${resolvedMember.file}/${resolvedMember.name} (${duration}ms)`);

			if (!resolvedMembers[normalizedBaseUri]) resolvedMembers[normalizedBaseUri] = {};
			resolvedMembers[normalizedBaseUri][fileKey] = resolvedMember;
		} else {
			logWithTimestamp(`Member resolve NOT FOUND: ${file}/${member} (${duration}ms)`);
		}

		return resolvedMember;
	} catch (e) {
		const duration = Date.now() - startTime;
		logWithTimestamp(`Member resolve ERROR: ${file}/${member} (${duration}ms)`);
		console.log(JSON.stringify({baseUri, member, file}));
		console.log(e);
	}

	return undefined;
}

export async function streamfileResolve(baseUri: string, base: string[]): Promise<string|undefined> {
	// Normalize baseUri by removing query parameters to ensure consistent cache keys
	const normalizedBaseUri = baseUri.split('?')[0];
	const baseString = base.join(`-`);
	const startTime = Date.now();

	// Check cache
	if (resolvedStreamfiles[normalizedBaseUri] && resolvedStreamfiles[normalizedBaseUri][baseString]) {
		const cached = resolvedStreamfiles[normalizedBaseUri][baseString];
		const duration = Date.now() - startTime;
		const requestingFile = normalizedBaseUri.split('/').pop() || normalizedBaseUri;
		logWithTimestamp(`Streamfile resolve CACHE HIT: ${base[0]} (requesting: ${requestingFile}) -> ${cached} (${duration}ms)`);
		return cached;
	}

	const requestingFile = normalizedBaseUri.split('/').pop() || normalizedBaseUri;
	logWithTimestamp(`Streamfile resolve CACHE MISS: ${base[0]} (requesting: ${requestingFile})`);

	const workspace = await getWorkspaceFolder(baseUri);

	const paths = (workspace ? includePath[workspace.uri] : []) || [];

	try {
		const resolvedPath = await queue.add(() => {return connection.sendRequest("streamfileResolve", [base, paths])}) as string|undefined;
		const duration = Date.now() - startTime;

		if (resolvedPath) {
			logWithTimestamp(`Streamfile resolve SUCCESS: ${base[0]} (requesting: ${requestingFile}) -> ${resolvedPath} (${duration}ms)`);

			if (!resolvedStreamfiles[normalizedBaseUri]) resolvedStreamfiles[normalizedBaseUri] = {};
			resolvedStreamfiles[normalizedBaseUri][baseString] = resolvedPath;
		} else {
			logWithTimestamp(`Streamfile resolve NOT FOUND: ${base[0]} (requesting: ${requestingFile}) (${duration}ms)`);
		}

		return resolvedPath;
	} catch (e) {
		const duration = Date.now() - startTime;
		logWithTimestamp(`Streamfile resolve ERROR: ${base[0]} (requesting: ${requestingFile}) (${duration}ms)`);
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

export function handleClientRequests() {
	connection.onRequest(`clearTableCache`, () => {
		parser.clearTableCache();
	});

	connection.onRequest(`getCache`, (uri: string) => {
		const doc = parser.getParsedCache(uri);
		if (!doc) return undefined;
		return {
			keyword: doc.keyword,
			parameters: doc.parameters,
			subroutines: doc.subroutines,
			procedures: doc.procedures.map(p => ({
				...p,
				prototype: p.prototype
			})),
			files: doc.files,
			variables: doc.variables,
			structs: doc.structs,
			constants: doc.constants,
			sqlReferences: doc.sqlReferences,
			indicators: doc.indicators,
			tags: doc.tags,
			includes: doc.includes,
		}
	});
}

export interface BindingDirectory {
	lib?: string;
	name: string;
}