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

import { connection, getFileRequest, getObject as getObjectData, validateUri } from "./connection";
import * as Linter from './providers/linter';
import { referenceProvider } from './providers/reference';
import Declaration from '../../../language/models/declaration';
import { getPrettyType } from '../../../language/models/fixed';

import * as Project from './providers/project';
import workspaceSymbolProvider from './providers/project/workspaceSymbol';
import implementationProvider from './providers/implementation';
import { dspffdToRecordFormats } from './data';
import path = require('path');
import { existsSync } from 'fs';

let hasConfigurationCapability = false;
let hasWorkspaceFolderCapability = false;
let hasDiagnosticRelatedInformationCapability = false;

const languageToolsEnabled = process.env.LANGUAGE_TOOLS_ENABLED;
const linterEnabled = process.env.LINTER_ENABLED;
const formatterEnabled = process.env.FORMATTER_ENABLED;

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
			triggerCharacters: [` `, `.`, `:`]
		};
		result.capabilities.hoverProvider = true;
		result.capabilities.referencesProvider = true;
		result.capabilities.implementationProvider = true;
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
})

parser.setIncludeFileFetch(async (stringUri: string, includeString: string) => {
	const currentUri = URI.parse(stringUri);
	const uriPath = currentUri.path;

	let cleanString: string | undefined;
	let validUri: string | undefined;

	switch (currentUri.scheme) {
		case `member`:
			let possibleAsp = undefined;
			let baseLibrary = `QSYSINC`;
			const memberPath = uriPath.startsWith(`/`) ? uriPath.substring(1).split(`/`) : uriPath.split(`/`);

			// if (path.length > 0) result.basename = path[path.length - 1];
			// if (path.length > 1) result.file = path[path.length - 2];
			if (memberPath.length > 2) baseLibrary = memberPath[memberPath.length - 3];
			if (memberPath.length > 3) possibleAsp = memberPath[memberPath.length - 4];

			if (includeString.startsWith(`'`) && includeString.endsWith(`'`)) {
				// IFS fetch
				cleanString = includeString.substring(1, includeString.length - 1);
				// TODO:....

			} else {

				// Member fetch
				// Split by /,
				const parts = includeString.split(`/`).map(s => s.split(`,`)).flat();
				cleanString = [
					``,
					...(possibleAsp ? [possibleAsp] : []),
					parts[parts.length - 3] ? parts[parts.length - 3] : baseLibrary,
					parts[parts.length - 2] ? parts[parts.length - 2] : `QRPGLEREF`,
					parts[parts.length - 1] + `.rpgleinc`
				].join(`/`);

				cleanString = URI.from({
					scheme: `member`,
					path: cleanString
				}).toString();
			}
			validUri = await validateUri(cleanString, currentUri.scheme);
			break;

		case `file`:
			const workspaceFolders = await connection.workspace.getWorkspaceFolders();
			let workspaceFolder: WorkspaceFolder | undefined;
			if (workspaceFolders) {
				workspaceFolder = workspaceFolders.find(folderUri => uriPath.startsWith(URI.parse(folderUri.uri).path))
			}

			cleanString = includeString;

			if (cleanString.startsWith(`'`) && cleanString.endsWith(`'`)) {
				cleanString = cleanString.substring(1, cleanString.length - 1);
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
			break;
	}

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
			Linter.refreshDiagnostics(handler.document, cache);
		}
	});
});

// Make the text document manager listen on the connection
// for open, change and close text document events
documents.listen(connection);

// Listen on the connection
connection.listen();