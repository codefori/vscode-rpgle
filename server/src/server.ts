/* eslint-disable no-case-declarations */
/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
import {
	InitializeParams,
	TextDocumentSyncKind,
	InitializeResult,
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
import Declaration from './language/models/declaration';
import { getPrettyType } from './language/models/fixed';

import * as Project from './providers/project';
import workspaceSymbolProvider from './providers/project/workspaceSymbol';
import implementationProvider from './providers/project/implementation';
import { dspffdToRecordFormats } from './data';

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
			result.capabilities.implementationProvider = true;
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
	const [_, baseLibrary, baseSourceFile, basename] = currentUri.path.split(`/`);
	let cleanString: string|undefined;

	switch (currentUri.scheme) {
		case `member`:
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
					parts[parts.length - 3] ? parts[parts.length - 3] : baseLibrary,
					parts[parts.length - 2] ? parts[parts.length - 2] : `QRPGLEREF`,
					parts[parts.length - 1] + `.rpgleinc`
				].join(`/`);

				cleanString = URI.from({
					scheme: `member`,
					path: cleanString
				}).toString();
			}
			break;

		case `file`:
			cleanString = includeString;
			
			if (cleanString.startsWith(`'`) && cleanString.endsWith(`'`)) {
				cleanString = cleanString.substring(1, cleanString.length - 1);
			}
			break;
	}

	if (cleanString) {
		const validUri = await validateUri(cleanString, currentUri.scheme);

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

	return {
		found: false
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