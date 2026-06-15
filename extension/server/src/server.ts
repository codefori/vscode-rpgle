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
import { documents, getParser, opmParser, parser } from './providers';
import definitionProvider from './providers/definition';
import { URI } from 'vscode-uri';
import completionItemProvider from './providers/completionItem';
import hoverProvider from './providers/hover';
import foldingRangeProvider from './providers/foldingRange';

import { connection, filesBeingFetchedForIncludes, getDisplayName, getFileRequest, getObject as getObjectData, handleClientRequests, initializeLogLevel, LogLevel, memberResolve, streamfileResolve, validateUri, logWithTimestamp, applyRemoteCacheSettings } from "./connection";
import * as Linter from './providers/linter';
import { referenceProvider } from './providers/reference';
import Declaration from '../../../language/models/declaration';

import * as Project from './providers/project';
import workspaceSymbolProvider from './providers/project/workspaceSymbol';
import implementationProvider from './providers/implementation';
import { dspffdToRecordFormats, isInMerlin, parseMemberUri } from './data';
import { resolveWorkspaceIncludePath } from './includeResolver';
import path = require('path');
import { existsSync } from 'fs';
import { renamePrepareProvider, renameRequestProvider } from './providers/rename';
import genericCodeActionsProvider from './providers/codeActions';
import { isLinterEnabled } from './providers/linter';
import { signatureHelpProvider } from './providers/signatureHelp';

import { CacheMetrics } from '../../../language/ile/parser';
import { log } from 'console';

let hasConfigurationCapability = false;
let hasWorkspaceFolderCapability = false;
let hasDiagnosticRelatedInformationCapability = false;

const outsideMerlin = !isInMerlin();

const languageToolsEnabled = outsideMerlin;
const formatterEnabled = outsideMerlin;

let projectEnabled = false;

connection.onInitialize((params: InitializeParams) => {
	const capabilities = params.capabilities;

	// Apply cache settings passed from the VS Code client as initializationOptions
	const opts = params.initializationOptions as { fileTTLSeconds?: number, fileMaxEntries?: number} | undefined;
	if (opts) {
		const ttlMs = (opts.fileTTLSeconds ?? 300) * 1000;
		const maxEntries = opts.fileMaxEntries ?? 200;
		applyRemoteCacheSettings(ttlMs, maxEntries);
		includeCacheTTL = ttlMs;
		includeCacheMaxSize = maxEntries * 2;
		console.log(`Cache settings applied: TTL=${ttlMs}ms, max=${maxEntries} `);
	}

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
		};
		result.capabilities.foldingRangeProvider = true;
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
	initializeLogLevel();

	if (projectEnabled) {
		Project.initialise();
	}

	handleClientRequests();
});

const tableFetch = async (table: string, aliases = false): Promise<Declaration[]> => {
	if (!languageToolsEnabled) return [];



	const data = await getObjectData(table);



	return dspffdToRecordFormats(data, aliases);
};

parser.setTableFetch(tableFetch);
opmParser.setTableFetch(tableFetch);

/** In-flight include resolution promises — concurrent fetches for the same key share one Promise */
let fetchingInProgress: { [fetchKey: string]: Promise<{found: boolean, uri?: string, content?: string}> } = {};

/** Short-lived cache of resolved include results keyed by "baseUri::includeString".
 *
 * Tuning knobs — controlled via VS Code settings (vscode-rpgle.cache.*).
 * Values are set at server startup from initializationOptions.
 */
let includeCacheTTL = 5 * 60 * 1000;    // 5 minutes default
let includeCacheMaxSize = 500;
let resolvedIncludeCache: Map<string, {result: {found: boolean, uri?: string, content?: string}, fetched: number}> = new Map();

function clearAllCaches() {
	parser.clearTableCache();

	for (const uri of Object.keys(parser.parsedCache)) {
		parser.clearParsedCache(uri);
	}

	resolvedIncludeCache.clear();
	fetchingInProgress = {};
	CacheMetrics.reset();
}

connection.onRequest(`clearAllCache`, () => {
	clearAllCaches();
});

const includeFileFetch = async (stringUri: string, includeString: string) => {
	const fetchKey = `${stringUri}::${includeString}`;
	const now = Date.now();
	const currentUri = URI.parse(stringUri);
	const uriPath = currentUri.fsPath;
	// Extract clean filename without query parameters
	const parentFileName = getDisplayName(stringUri);
	const fetchStartTime = Date.now();

	// Check short-lived resolved cache first
	const cached = resolvedIncludeCache.get(fetchKey);
	if (cached && now <= cached.fetched + includeCacheTTL) {
		CacheMetrics.includeHits++;
		logWithTimestamp(`[cache] include-hit for ${fetchKey}`, LogLevel.DEBUG);
		return cached.result;
	}
	CacheMetrics.includeMisses++;
	logWithTimestamp(`[cache] include-miss for ${fetchKey}`, LogLevel.DEBUG);

	// Deduplicate concurrent fetches for the same key
	if (fetchingInProgress[fetchKey] !== undefined) {
		logWithTimestamp(`Include fetch already in progress for: ${includeString} (from ${parentFileName}), awaiting existing fetch`, LogLevel.DEBUG);
		return fetchingInProgress[fetchKey];
	}

	const resolveInclude = async (): Promise<{found: boolean, uri?: string, content?: string}> => {
		const currentUri = URI.parse(stringUri);
		const uriPath = currentUri.fsPath;

		let cleanString: string | undefined;
		let validUri: string | undefined;

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
					workspaceFolder = workspaceFolders.find(folderUri => uriPath.startsWith(URI.parse(folderUri.uri).fsPath))
				}

				if (Project.isEnabled) {
					// Project mode is enable. Let's do a search for the path.
					validUri = await validateUri(cleanString, currentUri.scheme);

				} else {
					// Because project mode is disabled, likely due to the large workspace, we don't search
					if (workspaceFolder) {
						const resolved = resolveWorkspaceIncludePath(workspaceFolder.uri, cleanString);
						cleanString = resolved.absolutePath;
						validUri = existsSync(cleanString) ? resolved.fileUri : undefined;
					} else {
						validUri = existsSync(cleanString) ? URI.file(cleanString).toString() : undefined;
					}
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
					// Search for the include with common extensions
					const possibleFiles = [cleanString, `${cleanString}.rpgleinc`, `${cleanString}.rpgle`, `${cleanString}.sqlrplge`];

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

		let result: {found: boolean, uri?: string, content?: string};

		if (validUri) {
			const validSource = await getFileRequest(validUri, true); // true = skip debounce for include files
			if (validSource) {
				result = { found: true, uri: validUri, content: validSource };
			} else {
				result = { found: false, uri: validUri };
			}
		} else {
			result = { found: false, uri: validUri };
		}

		// Store in short-lived cache; evict oldest entries when over max size
		resolvedIncludeCache.set(fetchKey, { result, fetched: Date.now() });
		if (resolvedIncludeCache.size > includeCacheMaxSize) {
			const oldestKey = resolvedIncludeCache.keys().next().value;
			if (oldestKey) {
				resolvedIncludeCache.delete(oldestKey);
			}
		}

		return result;
	};

	fetchingInProgress[fetchKey] = resolveInclude().finally(() => {
		delete fetchingInProgress[fetchKey];
		logWithTimestamp(`Include fetch completed: ${includeString} (from ${parentFileName}), fetch promise cleared`, LogLevel.DEBUG);
	});

	return fetchingInProgress[fetchKey];
};

parser.setIncludeFileFetch(includeFileFetch);
opmParser.setIncludeFileFetch(includeFileFetch);

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
	connection.onFoldingRanges(foldingRangeProvider);

	// project specific
	connection.onWorkspaceSymbol(workspaceSymbolProvider);
	connection.onImplementation(implementationProvider);
}

if (isLinterEnabled()) Linter.initialise(connection);

// Track parsing state for each document
const documentParseState: {
	[uri: string]: {
		timer?: NodeJS.Timeout,
		parseId: number,
		parseStartTime?: number,
		isParsing: boolean,
		needsReparse: boolean
	}
} = {};

// Execute a parse for a document
function executeParse(uri: string, parseId: number, document: any) {
	const fileName = getDisplayName(uri);
	const state = documentParseState[uri];

	if (!state) return;

	// Mark parse as active
	state.isParsing = true;
	state.needsReparse = false;
	const parseStartTime = Date.now();
	state.parseStartTime = parseStartTime;
	logWithTimestamp(`Parse started: ${fileName} (parseId: ${parseId})`, LogLevel.DEBUG);


	const activeParser = getParser(uri);


	activeParser.getDocs(
		uri,
		document.getText(),
		{
			withIncludes: true,
			ignoreCache: true,
			collectReferences: true
		}
	).then(cache => {
		const duration = Date.now() - parseStartTime;
		const isLatest = parseId === state.parseId;

		// Mark parse as complete
		state.isParsing = false;

		// Only update diagnostics if this is still the latest parse
		if (cache && isLatest) {
			Linter.refreshLinterDiagnostics(document, cache);

			// When includes are changed, use the reverse dependency index for targeted invalidation
			// instead of scanning all cached files (O(dependents) vs O(all cached))
			for (const depPath of parser.getDependents(uri)) {
				parser.clearParsedCache(depPath);
			}

			// Evict stale resolved-include cache entries for the changed file
			for (const key of resolvedIncludeCache.keys()) {
				const entry = resolvedIncludeCache.get(key);
				if (entry?.result?.uri === uri) {
					resolvedIncludeCache.delete(key);
				}
			}

			logWithTimestamp(`Parse completed: ${fileName} (parseId: ${parseId}, ${duration}ms, diagnostics updated)`, LogLevel.INFO);
		} else if (cache) {
			logWithTimestamp(`Parse completed: ${fileName} (parseId: ${parseId}, ${duration}ms, STALE - ignored)`, LogLevel.DEBUG);
		} else {
			logWithTimestamp(`Parse completed: ${fileName} (parseId: ${parseId}, ${duration}ms, no cache)`, LogLevel.DEBUG);
		}

		// If a re-parse was queued while this parse was running, trigger it now
		if (state.needsReparse) {
			state.needsReparse = false;
			const latestParseId = state.parseId;
			logWithTimestamp(`Triggering queued re-parse for ${fileName} (parseId: ${latestParseId})`, LogLevel.DEBUG);
			setTimeout(() => executeParse(uri, latestParseId, document), 0);
		}
	}).catch(err => {
		const duration = Date.now() - parseStartTime;
		state.isParsing = false;
		logWithTimestamp(`Parse error: ${fileName} (parseId: ${parseId}, ${duration}ms)`, LogLevel.ERROR);
		console.error(`Error parsing ${uri}:`, err);

		// If a re-parse was queued, trigger it even after an error
		if (state.needsReparse) {
			state.needsReparse = false;
			const latestParseId = state.parseId;
			logWithTimestamp(`Triggering queued re-parse after error for ${fileName} (parseId: ${latestParseId})`, LogLevel.DEBUG);
			setTimeout(() => executeParse(uri, latestParseId, document), 0);
		}
	});
}

// Always get latest stuff
documents.onDidChangeContent(handler => {
	const uri = handler.document.uri;
	// Extract clean filename without query parameters
	const fileName = getDisplayName(uri);

	// Initialize state if needed
	if (!documentParseState[uri]) {
		documentParseState[uri] = { parseId: 0, isParsing: false, needsReparse: false };
	}

	const state = documentParseState[uri];
	const isFirstOpen = state.parseId === 0;
	const isIncludeFile = filesBeingFetchedForIncludes.has(uri);

	// Increment parse ID to invalidate any in-flight parses
	state.parseId++;
	const currentParseId = state.parseId;

	// Parse immediately without debounce for:
	// - Include files (opened during getFileRequest with skipDebounce)
	// - Main files on first open
	// Use debounce timer for main files being edited
	const debounceDelay = (isIncludeFile || isFirstOpen) ? 0 : 300;

	// Clear any existing timer
	if (state.timer) {
		clearTimeout(state.timer);
		logWithTimestamp(`Debounce: Timer reset for ${fileName} (parseId: ${currentParseId})`, LogLevel.DEBUG);
	} else if (!isFirstOpen && !isIncludeFile) {
		logWithTimestamp(`Debounce: Timer started for ${fileName} (${debounceDelay}ms)`, LogLevel.DEBUG);
	}

	// Set a new timer to parse after delay (0ms for includes/first open, 300ms for edits)
	state.timer = setTimeout(() => {
		delete state.timer;

		if (!isFirstOpen && !isIncludeFile) {
			logWithTimestamp(`Debounce: Timer expired for ${fileName}, starting parse (parseId: ${currentParseId})`, LogLevel.DEBUG);
		}

		// Check if a parse is already running for this document
		if (state.isParsing) {
			// A parse is already active - queue a re-parse to run after it completes
			state.needsReparse = true;
			logWithTimestamp(`Parse queued: ${fileName} (parseId: ${currentParseId}, waiting for active parse to complete)`, LogLevel.DEBUG);
			return;
		}

		// Execute the parse
		executeParse(uri, currentParseId, handler.document);
	}, debounceDelay); // 0ms for first open, 300ms for edits
});

// Make the text document manager listen on the connection
// for open, change and close text document events
documents.listen(connection);

// Listen on the connection
connection.listen();