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

import { connection, getFileRequest, getObject as getObjectData, memberResolve, streamfileResolve, validateUri } from "./connection";
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

let hasConfigurationCapability = false;
let hasWorkspaceFolderCapability = false;
let hasDiagnosticRelatedInformationCapability = false;

const outsideMerlin = !isInMerlin();

const languageToolsEnabled = outsideMerlin;
const linterEnabled = true;
const formatterEnabled = outsideMerlin;

let projectEnabled = false;

connection.onInitialize((params: InitializeParams) => {
	const capabilities = params.capabilities;

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
			triggerCharacters: [`.`, `:`]
		};
		result.capabilities.hoverProvider = true;
		result.capabilities.referencesProvider = true;
		result.capabilities.implementationProvider = true;
		result.capabilities.renameProvider = {prepareProvider: true};
	}

	if (linterEnabled) {
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
});

parser.setTableFetch(async (table: string, aliases = false): Promise<Declaration[]> => {
	if (!languageToolsEnabled) return [];

	const data = await getObjectData(table);

	return dspffdToRecordFormats(data, aliases);
});

let fetchingInProgress: { [fetchKey: string]: boolean } = {};

parser.setIncludeFileFetch(async (stringUri: string, includeString: string) => {
	const currentUri = URI.parse(stringUri);
	const uriPath = currentUri.path;

	let cleanString: string | undefined;
	let validUri: string | undefined;

	if (!fetchingInProgress[includeString]) {
		fetchingInProgress[includeString] = true;

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

		fetchingInProgress[includeString] = false;

		if (validUri) {
			const validSource = await getFileRequest(validUri);
			if (validSource) {
				return {
					found: true,
					uri: validUri,
					lines: validSource.split(`\n`)
				};
			}
		}

	}
	
	fetchingInProgress[includeString] = false;

	return {
		found: false,
		uri: validUri
	};
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

	// project specific
	connection.onWorkspaceSymbol(workspaceSymbolProvider);
	connection.onImplementation(implementationProvider)
}

if (linterEnabled) Linter.initialise(connection);

// Always get latest stuff
documents.onDidChangeContent(handler => {
	parser.getDocs(
		handler.document.uri,
		handler.document.getText(),
		{
			withIncludes: true,
			ignoreCache: true
		}
	).then(cache => {
		if (cache) {
			Linter.refreshLinterDiagnostics(handler.document, cache);
		}
	});
});

// Make the text document manager listen on the connection
// for open, change and close text document events
documents.listen(connection);

// Listen on the connection
connection.listen();