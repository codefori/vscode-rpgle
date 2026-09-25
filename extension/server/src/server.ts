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

import { connection, filesBeingFetchedForIncludes, getDisplayName, getFileRequest, getObject as getObjectData, handleClientRequests, initializeLogLevel, LogLevel, memberResolve, streamfileResolve, validateUri, logWithTimestamp, watchedFilesChangeEvent } from "./connection";
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

let hasConfigurationCapability = false;
let hasWorkspaceFolderCapability = false;
let hasDiagnosticRelatedInformationCapability = false;

const outsideMerlin = !isInMerlin();

const languageToolsEnabled = outsideMerlin;
const formatterEnabled = outsideMerlin;

let projectEnabled = false;
const CROSS_REFERENCE_READY_NOTIFICATION = `vscode-rpgle/crossReferenceReady`;
const CROSS_REFERENCE_STATE_NOTIFICATION = `vscode-rpgle/crossReferenceState`;
const crossReferenceNotified = new Set<string>();
const DEFAULT_LARGE_FILE_THRESHOLD = 6000;
let crossReferenceReadyLineThreshold = DEFAULT_LARGE_FILE_THRESHOLD;

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
		result.capabilities.renameProvider = { prepareProvider: true };
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

	void connection.workspace.getConfiguration('vscode-rpgle').then((config) => {
		const configuredThreshold = Number(config?.bracketHighlightingMaxLines);
		if (Number.isFinite(configuredThreshold) && configuredThreshold >= 0) {
			crossReferenceReadyLineThreshold = configuredThreshold;
		} else {
			crossReferenceReadyLineThreshold = DEFAULT_LARGE_FILE_THRESHOLD;
		}
	});

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

let fetchingInProgress: { [fetchKey: string]: boolean } = {};
const INCLUDE_CACHE_LIMIT = 200;
const includeUriCache = new Map<string, string>();
const includeContentCache = new Map<string, string>();

const normalizeUriForCache = (uri: string): string => {
	if (!uri) return ``;
	const trimmed = uri.trim();
	return trimmed.split(`?`)[0].split(`#`)[0];
};

const pruneIncludeCache = () => {
	while (includeUriCache.size > INCLUDE_CACHE_LIMIT) {
		const oldestKey = includeUriCache.keys().next().value;
		if (oldestKey === undefined) break;

		const oldestUri = includeUriCache.get(oldestKey);
		includeUriCache.delete(oldestKey);

		if (oldestUri) {
			const stillReferenced = Array.from(includeUriCache.values()).includes(oldestUri);
			if (!stillReferenced) {
				includeContentCache.delete(oldestUri);
			}
		}
	}
};

const getIncludeCacheKey = (baseUri: string, includeLiteral: string): string => {
	const cleanBase = normalizeUriForCache(baseUri);
	const slashIndex = Math.max(cleanBase.lastIndexOf(`/`), cleanBase.lastIndexOf(`\\`));
	const baseDir = slashIndex >= 0 ? cleanBase.substring(0, slashIndex) : cleanBase;
	const includePath = includeLiteral.trim().replace(/^['"]|['"]$/g, ``);
	return `${baseDir}::${includePath}`;
};

const invalidateIncludeCacheForUri = (uri: string) => {
	const cacheUri = normalizeUriForCache(uri);
	includeContentCache.delete(cacheUri);

	for (const [cacheKey, resolvedUri] of includeUriCache.entries()) {
		if (resolvedUri === cacheUri) {
			includeUriCache.delete(cacheKey);
		}
	}
};

const includeFileFetch = async (stringUri: string, includeString: string) => {
	const currentUri = URI.parse(stringUri);
	const uriPath = currentUri.fsPath;
	const parentFileName = getDisplayName(stringUri);
	const fetchStartTime = Date.now();
	const includeCacheKey = getIncludeCacheKey(stringUri, includeString);

	let cleanString: string | undefined;
	let validUri: string | undefined;

	if (fetchingInProgress[includeCacheKey]) {
		logWithTimestamp(`Include fetch skipped: ${includeString} (already fetching)`, LogLevel.DEBUG);
		return {
			found: false,
			uri: validUri
		};
	}

	fetchingInProgress[includeCacheKey] = true;
	try {
		logWithTimestamp(`Include fetch started: ${includeString} (from ${parentFileName})`, LogLevel.DEBUG);

		const cachedUri = includeUriCache.get(includeCacheKey);
		if (cachedUri) {
			const cachedContent = includeContentCache.get(cachedUri);
			if (cachedContent) {
				const duration = Date.now() - fetchStartTime;
				const fileName = getDisplayName(cachedUri);
				logWithTimestamp(`Include fetch cache hit: ${includeString} -> ${fileName} (${duration}ms, memory cache)`, LogLevel.DEBUG);
				return {
					found: true,
					uri: cachedUri,
					content: cachedContent
				};
			}

			validUri = cachedUri;
		}

		if (!validUri) {
			const hasQuotes = (includeString.startsWith(`'`) && includeString.endsWith(`'`)) || (includeString.startsWith(`"`) && includeString.endsWith(`"`));
			const isUnixPath = hasQuotes || (includeString.includes(`/`) && !includeString.includes(`,`));

			cleanString = includeString;

			if (hasQuotes) {
				cleanString = cleanString.substring(1, cleanString.length - 1);
			}

			if (isUnixPath) {
				if (![`streamfile`, `member`].includes(currentUri.scheme)) {
					const workspaceFolders = await connection.workspace.getWorkspaceFolders();
					let workspaceFolder: WorkspaceFolder | undefined;
					if (workspaceFolders) {
						workspaceFolder = workspaceFolders.find(folderUri => uriPath.startsWith(URI.parse(folderUri.uri).fsPath));
					}

					if (Project.isEnabled) {
						validUri = await validateUri(cleanString, currentUri.scheme);
					} else {
						if (workspaceFolder) {
							const resolved = resolveWorkspaceIncludePath(workspaceFolder.uri, cleanString);
							cleanString = resolved.absolutePath;
							validUri = existsSync(cleanString) ? resolved.fileUri : undefined;
						} else {
							validUri = existsSync(cleanString) ? URI.file(cleanString).toString() : undefined;
						}
					}

					if (!validUri) {
						const foundStreamfile = await streamfileResolve(stringUri, [cleanString]);

						if (foundStreamfile) {
							validUri = URI.from({
								scheme: `streamfile`,
								path: foundStreamfile
							}).toString();
						}
					}
				} else {
					if (cleanString.startsWith(`/`)) {
						validUri = URI.from({
							scheme: `streamfile`,
							path: cleanString
						}).toString();
					} else {
						const possibleFiles = [cleanString, `${cleanString}.rpgleinc`, `${cleanString}.rpgle`];
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
				const parts = parseMemberUri(includeString);
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
		}

		if (validUri) {
			const normalizedUri = normalizeUriForCache(validUri);
			includeUriCache.set(includeCacheKey, normalizedUri);
			pruneIncludeCache();

			const cachedContent = includeContentCache.get(normalizedUri);
			if (cachedContent) {
				const duration = Date.now() - fetchStartTime;
				const fileName = getDisplayName(normalizedUri);
				logWithTimestamp(`Include fetch cache hit: ${includeString} -> ${fileName} (${duration}ms, memory cache)`, LogLevel.DEBUG);
				return {
					found: true,
					uri: normalizedUri,
					content: cachedContent
				};
			}

			const validSource = await getFileRequest(validUri, true);
			if (validSource) {
				includeContentCache.set(normalizedUri, validSource);
				const duration = Date.now() - fetchStartTime;
				const fileName = getDisplayName(normalizedUri);
				logWithTimestamp(`Include fetch completed: ${includeString} -> ${fileName} (${duration}ms, found)`, LogLevel.INFO);
				return {
					found: true,
					uri: normalizedUri,
					content: validSource
				};
			}

			includeUriCache.delete(includeCacheKey);
		}

		const duration = Date.now() - fetchStartTime;
		logWithTimestamp(`Include fetch completed: ${includeString} (${duration}ms, NOT FOUND)`, LogLevel.WARN);
		return {
			found: false,
			uri: validUri
		};
	} finally {
		fetchingInProgress[includeCacheKey] = false;
	}
};

parser.setIncludeFileFetch(includeFileFetch);
opmParser.setIncludeFileFetch(includeFileFetch);

watchedFilesChangeEvent.push((params) => {
	for (const fileEvent of params.changes) {
		invalidateIncludeCacheForUri(fileEvent.uri);
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
	logWithTimestamp(`Parse started: ${fileName} (parseId: ${parseId})`, LogLevel.INFO);
	const lineCount = document.lineCount || 0;
	if (crossReferenceReadyLineThreshold > 0 && lineCount >= crossReferenceReadyLineThreshold) {
		connection.sendNotification(CROSS_REFERENCE_STATE_NOTIFICATION, {
			phase: `started`,
			uri,
			fileName,
			lineCount,
			parseId,
			startedAt: parseStartTime,
		});
	}


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

			// When includes are changed, clear cache for any files that reference it
			for (const [thePath, cache] of Object.entries(parser.parsedCache)) {
				if (cache) {
					const includePaths = cache.includes.map(include => include.toPath);
					if (includePaths.includes(document.uri)) {
						parser.clearParsedCache(thePath);
					}
				}
			}

			logWithTimestamp(`Parse completed: ${fileName} (parseId: ${parseId}, ${duration}ms, diagnostics updated)`, LogLevel.INFO);

			const lineCount = document.lineCount || 0;
			if (crossReferenceReadyLineThreshold > 0 && lineCount >= crossReferenceReadyLineThreshold) {
				connection.sendNotification(CROSS_REFERENCE_STATE_NOTIFICATION, {
					phase: `completed`,
					uri,
					fileName,
					lineCount,
					parseId,
					durationMs: duration,
				});
			}
			if (crossReferenceReadyLineThreshold > 0 && lineCount >= crossReferenceReadyLineThreshold && !crossReferenceNotified.has(uri)) {
				crossReferenceNotified.add(uri);
				connection.sendNotification(CROSS_REFERENCE_READY_NOTIFICATION, {
					uri,
					fileName,
					lineCount,
					durationMs: duration,
				});
			}
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
	invalidateIncludeCacheForUri(uri);
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

documents.onDidOpen(handler => {
	const uri = handler.document.uri;
	const fileName = getDisplayName(uri);
	crossReferenceNotified.delete(uri);
	if (!documentParseState[uri]) {
		documentParseState[uri] = { parseId: 0, isParsing: false, needsReparse: false };
	}

	const state = documentParseState[uri];
	state.parseId++;
	const currentParseId = state.parseId;
	logWithTimestamp(`Document opened: ${fileName} (parseId: ${currentParseId})`, LogLevel.DEBUG);

	if (
		handler.document.languageId === `rpgle`
		|| handler.document.languageId === `rpg`
		|| handler.document.languageId === `sqlrpgle`
		|| handler.document.languageId === `sqlrpg`
		|| handler.document.languageId === `rpgleinc`
		|| handler.document.languageId === `rpginc`
	) {
		executeParse(uri, currentParseId, handler.document);
	}
});

documents.onDidClose(handler => {
	const uri = handler.document.uri;
	crossReferenceNotified.delete(uri);
	delete documentParseState[uri];
});

// Make the text document manager listen on the connection
// for open, change and close text document events
documents.listen(connection);

// Listen on the connection
connection.listen();