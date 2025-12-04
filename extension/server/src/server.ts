/* eslint-disable no-case-declarations */
/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
import {
	InitializeParams,
	TextDocumentSyncKind,
	InitializeResult,
	WorkspaceFolder,
} from 'vscode-languageserver/node';

import documentSymbolProvider from './providers/documentSymbols';
import { documents, parser } from './providers';
import definitionProvider from './providers/definition';
import { URI } from 'vscode-uri';
import completionItemProvider from './providers/completionItem';
import hoverProvider from './providers/hover';

import { connection, getFileRequest, getObject as getObjectData, handleClientRequests, memberResolve, streamfileResolve, validateUri } from "./connection";
import * as Linter from './providers/linter';
import { referenceProvider } from './providers/reference';
import Declaration from '../../../language/models/declaration';
import { getPrettyType } from '../../../language/models/fixed';

import * as Project from './providers/project';
import workspaceSymbolProvider from './providers/project/workspaceSymbol';
import implementationProvider from './providers/implementation';
import { dspffdToRecordFormats, isInMerlin, parseMemberUri } from './data';
import path = require('path');
import { existsSync } from 'fs';
import { renamePrepareProvider, renameRequestProvider } from './providers/rename';
import genericCodeActionsProvider from './providers/codeActions';
import { isLinterEnabled } from './providers/linter';
import { signatureHelpProvider } from './providers/signatureHelp';

let hasConfigurationCapability = false;
let hasWorkspaceFolderCapability = false;
let hasDiagnosticRelatedInformationCapability = false;

const outsideMerlin = !isInMerlin();

const languageToolsEnabled = outsideMerlin;
const formatterEnabled = outsideMerlin;

let projectEnabled = false;

connection.onInitialize((params: InitializeParams) => {
	const capabilities = params.capabilities;

	console.log(capabilities.textDocument?.completion);

	// Does the client support the `workspace/configuration` request?
	// If not, we fall back using global settings.
	hasConfigurationCapability = !!(
		capabilities.workspace && !!capabilities.workspace.configuration
	);
	hasWorkspaceFolderCapability = !!(
		capabilities.workspace && !!capabilities.workspace.workspaceFolders
	);
	hasDiagnosticRelatedInformationCapability = !!(
		capabilities.textDocument &&
		capabilities.textDocument.publishDiagnostics &&
		capabilities.textDocument.publishDiagnostics.relatedInformation
	);

	const result: InitializeResult = {
		capabilities: {
			textDocumentSync: TextDocumentSyncKind.Incremental
		}
	};

	if (languageToolsEnabled) {
		result.capabilities.documentSymbolProvider = true;
		result.capabilities.definitionProvider = true;
		result.capabilities.completionProvider = {
			triggerCharacters: [`.`, `:`],
		};
		result.capabilities.hoverProvider = true;
		result.capabilities.referencesProvider = true;
		result.capabilities.implementationProvider = true;
		result.capabilities.renameProvider = {prepareProvider: true};
		result.capabilities.signatureHelpProvider = {
			triggerCharacters: [`(`, `:`]
		}
	}

	if (isLinterEnabled()) {
		result.capabilities.codeActionProvider = true;
		if (formatterEnabled) {
			result.capabilities.documentFormattingProvider = {
				workDoneProgress: true
			};
		}
	}

	if (hasWorkspaceFolderCapability) {
		result.capabilities.workspace = {
			workspaceFolders: {
				supported: true
			},
		};
	}

	if (languageToolsEnabled && hasWorkspaceFolderCapability) {
		const workspaceFolders = params.workspaceFolders;

		if (workspaceFolders && workspaceFolders.length > 0) {
			projectEnabled = true;
			result.capabilities.workspaceSymbolProvider = true;
		}
	}

	console.log(`Project Mode enabled: ${projectEnabled}`);

	return result;
});

connection.onInitialized(() => {
	if (projectEnabled) {
		Project.initialise();
	}

	handleClientRequests();
});

parser.setTableFetch(async (table: string, aliases = false): Promise<Declaration[]> => {
	if (!languageToolsEnabled) return [];

	console.log(`Server is resolving ${table}`);

	const data = await getObjectData(table);

	console.log(`Resolved ${table} and got ${data.length} rows.`);

	return dspffdToRecordFormats(data, aliases);
});

let fetchingInProgress: { [fetchKey: string]: boolean } = {};

parser.setIncludeFileFetch(async (stringUri: string, includeString: string) => {
	const currentUri = URI.parse(stringUri);
	const uriPath = currentUri.path;
	// Extract clean filename without query parameters
	const parentFileName = (uriPath.split('/').pop() || stringUri).split('?')[0];
	const fetchStartTime = Date.now();

	let cleanString: string | undefined;
	let validUri: string | undefined;

	if (!fetchingInProgress[includeString]) {
		fetchingInProgress[includeString] = true;
		logWithTimestamp(`Include fetch started: ${includeString} (from ${parentFileName})`);

		// Right now we are resolving based on the base file schema.
		// This is likely bad since you can include across file systems.

		const hasQuotes = (includeString.startsWith(`'`) && includeString.endsWith(`'`)) || (includeString.startsWith(`"`) && includeString.endsWith(`"`))
		const isUnixPath = hasQuotes || (includeString.includes(`/`) && !includeString.includes(`,`));

		cleanString = includeString;

		if (hasQuotes) {
			cleanString = cleanString.substring(1, cleanString.length - 1);
		}

		if (isUnixPath) {
			if (![`streamfile`, `member`].includes(currentUri.scheme)) {
				// Local file system search (scheme is usually file)
				const workspaceFolders = await connection.workspace.getWorkspaceFolders();
				let workspaceFolder: WorkspaceFolder | undefined;
				if (workspaceFolders) {
					workspaceFolder = workspaceFolders.find(folderUri => uriPath.startsWith(URI.parse(folderUri.uri).path))
				}

				if (Project.isEnabled) {
					// Project mode is enable. Let's do a search for the path.
					validUri = await validateUri(cleanString, currentUri.scheme);

				} else {
					// Because project mode is disabled, likely due to the large workspace, we don't search
					if (workspaceFolder) {
						cleanString = path.posix.join(URI.parse(workspaceFolder.uri).path, cleanString)
					}

					validUri = existsSync(cleanString) ?
						URI.from({
							scheme: currentUri.scheme,
							path: cleanString
						}).toString()
						: undefined;
				}

				if (!validUri) {
					// Ok, no local file was found. Let's see if we can do a server lookup?
					const foundStreamfile = await streamfileResolve(stringUri, [cleanString]);

					if (foundStreamfile) {
						validUri = URI.from({
							scheme: `streamfile`,
							path: foundStreamfile
						}).toString();
					}
				}

			} else {
				// Resolving IFS path from member or streamfile

				// IFS fetch

				if (cleanString.startsWith(`/`)) {
					// Path from root
					validUri = URI.from({
						scheme: `streamfile`,
						path: cleanString
					}).toString();

				} else {
					// TODO: Instead of searching for `.*`, search for:
					//   - `${cleanString}`
					//   - `${cleanString}.rpgleinc`
					//   - `${cleanString}.rpgle`
					const possibleFiles = [cleanString, `${cleanString}.rpgleinc`, `${cleanString}.rpgle`];

					// Path from home directory?
					const foundStreamfile = await streamfileResolve(stringUri, possibleFiles);

					if (foundStreamfile) {
						validUri = URI.from({
							scheme: `streamfile`,
							path: foundStreamfile
						}).toString();
					}
				}
			}

		} else {
			// Member fetch
			// Split by /,
			const parts = parseMemberUri(includeString);

			// If there is no file provided, assume QRPGLESRC
			let baseFile = parts.file || `QRPGLESRC`;
			let baseMember = parts.name;

			if (parts.library && parts.library.startsWith(`*`)) {
				parts.library = undefined;
			}

			if (parts.library) {
				cleanString = [
					``,
					...(parts.asp ? [parts.asp] : []),
					parts.library,
					baseFile,
					baseMember + `.rpgleinc`
				].join(`/`);

				cleanString = URI.from({
					scheme: `member`,
					path: cleanString
				}).toString();

				validUri = await validateUri(cleanString, currentUri.scheme);

			} else {
				// No base library provided, let's do a resolve

				const foundMember = await memberResolve(stringUri, baseMember, baseFile);

				if (foundMember) {
					cleanString = [
						``,
						...(parts.asp ? [parts.asp] : []),
						foundMember.library,
						foundMember.file,
						foundMember.name + `.rpgleinc`
					].join(`/`);

					validUri = URI.from({
						scheme: `member`,
						path: cleanString
					}).toString();
				}
			}
		}

		if (validUri) {
			const validSource = await getFileRequest(validUri);
			if (validSource) {
				const duration = Date.now() - fetchStartTime;
				const fileName = validUri.split('/').pop() || validUri;
				logWithTimestamp(`Include fetch completed: ${includeString} -> ${fileName} (${duration}ms, found)`);
				fetchingInProgress[includeString] = false;
				return {
					found: true,
					uri: validUri,
					content: validSource
				};
			}
		}

		const duration = Date.now() - fetchStartTime;
		logWithTimestamp(`Include fetch completed: ${includeString} (${duration}ms, NOT FOUND)`);
		fetchingInProgress[includeString] = false;
		return {
			found: false,
			uri: validUri
		};
	} else {
		logWithTimestamp(`Include fetch skipped: ${includeString} (already fetching)`);
		return {
			found: false,
			uri: validUri
		};
	}
});

if (languageToolsEnabled) {
	// regular language stuff
	connection.onDocumentSymbol(documentSymbolProvider);
	connection.onDefinition(definitionProvider);
	connection.onCompletion(completionItemProvider);
	connection.onHover(hoverProvider);
	connection.onReferences(referenceProvider);
	connection.onPrepareRename(renamePrepareProvider);
	connection.onRenameRequest(renameRequestProvider);
	connection.onCodeAction(genericCodeActionsProvider);
	connection.onSignatureHelp(signatureHelpProvider);

	// project specific
	connection.onWorkspaceSymbol(workspaceSymbolProvider);
	connection.onImplementation(implementationProvider);
}

if (isLinterEnabled()) Linter.initialise(connection);

// Helper function for timestamped logging
function logWithTimestamp(message: string) {
	const now = new Date();
	const timestamp = now.toTimeString().split(' ')[0] + '.' + now.getMilliseconds().toString().padStart(3, '0');
	console.log(`[${timestamp}] ${message}`);
}

// Track parsing state for each document
const documentParseState: {
	[uri: string]: {
		timer?: NodeJS.Timeout,
		parseId: number,
		parseStartTime?: number
	}
} = {};

// Always get latest stuff
documents.onDidChangeContent(handler => {
	const uri = handler.document.uri;
	// Extract clean filename without query parameters
	const fileName = (uri.split('/').pop() || uri).split('?')[0];

	// Initialize state if needed
	if (!documentParseState[uri]) {
		documentParseState[uri] = { parseId: 0 };
	}

	const state = documentParseState[uri];

	// Clear any existing timer
	if (state.timer) {
		clearTimeout(state.timer);
		logWithTimestamp(`Debounce: Timer reset for ${fileName} (parseId: ${state.parseId + 1})`);
	} else {
		logWithTimestamp(`Debounce: Timer started for ${fileName} (300ms)`);
	}

	// Increment parse ID to invalidate any in-flight parses
	state.parseId++;
	const currentParseId = state.parseId;

	// Set a new timer to parse after a short delay
	state.timer = setTimeout(() => {
		delete state.timer;
		logWithTimestamp(`Debounce: Timer expired for ${fileName}, starting parse (parseId: ${currentParseId})`);

		// Always start a new parse - don't wait for old ones to finish
		// Old parses will be invalidated by the parseId check
		// This allows max 2 concurrent parses per document

		const parseStartTime = Date.now();
		state.parseStartTime = parseStartTime;
		logWithTimestamp(`Parse started: ${fileName} (parseId: ${currentParseId})`);

		parser.getDocs(
			uri,
			handler.document.getText(),
			{
				withIncludes: true,
				ignoreCache: true,
				collectReferences: true
			}
		).then(cache => {
			const duration = Date.now() - parseStartTime;
			const isLatest = currentParseId === state.parseId;

			// Only update diagnostics if this is still the latest parse
			if (cache && isLatest) {
				Linter.refreshLinterDiagnostics(handler.document, cache);
				logWithTimestamp(`Parse completed: ${fileName} (parseId: ${currentParseId}, ${duration}ms, diagnostics updated)`);
			} else if (cache) {
				logWithTimestamp(`Parse completed: ${fileName} (parseId: ${currentParseId}, ${duration}ms, STALE - ignored)`);
			} else {
				logWithTimestamp(`Parse completed: ${fileName} (parseId: ${currentParseId}, ${duration}ms, no cache)`);
			}
		}).catch(err => {
			const duration = Date.now() - parseStartTime;
			logWithTimestamp(`Parse error: ${fileName} (parseId: ${currentParseId}, ${duration}ms)`);
			console.error(`Error parsing ${uri}:`, err);
		});
	}, 300); // Reduced to 300ms for better responsiveness
});

// Make the text document manager listen on the connection
// for open, change and close text document events
documents.listen(connection);

// Listen on the connection
connection.listen();