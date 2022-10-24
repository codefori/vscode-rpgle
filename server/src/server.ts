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

import { connection, getFileRequest, validateUri } from "./connection";
import { refreshDiagnostics } from './providers/lintProvider';

let hasConfigurationCapability = false;
let hasWorkspaceFolderCapability = false;
let hasDiagnosticRelatedInformationCapability = false;

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
			textDocumentSync: TextDocumentSyncKind.Incremental,
			// Tell the client that this server supports code completion.
			documentSymbolProvider: true,
			definitionProvider: true,
			completionProvider: {
				triggerCharacters: [` `, `.`, `:`]
			},
			hoverProvider: true,
		}
	};
	if (hasWorkspaceFolderCapability) {
		result.capabilities.workspace = {
			workspaceFolders: {
				supported: true
			},
		};
	}
	return result;
});

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

connection.onDocumentSymbol(documentSymbolProvider);
connection.onDefinition(definitionProvider);
connection.onCompletion(completionItemProvider);
connection.onHover(hoverProvider);


documents.onDidChangeContent(handler => {
	parser.getDocs(
		handler.document.uri,
		handler.document.getText(),
		{
			withIncludes: true,
			ignoreCache: true
		}
	).then(cache => {
		if (cache)
			refreshDiagnostics(handler.document, cache);
	});
});

// Make the text document manager listen on the connection
// for open, change and close text document events
documents.listen(connection);

// Listen on the connection
connection.listen();