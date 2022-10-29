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

let hasConfigurationCapability = false;
let hasWorkspaceFolderCapability = false;
let hasDiagnosticRelatedInformationCapability = false;

const languageToolsEnabled = process.env.LANGUAGE_TOOLS_ENABLED;
const linterEnabled = process.env.LINTER_ENABLED;
const formatterEnabled = process.env.FORMATTER_ENABLED;

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
			Project.initialise();
		}
	}

	return result;
});

parser.setTableFetch(async (table: string, aliases = false) => {
	if (!languageToolsEnabled) return [];
	
	let recordFormats: {[name: string]: Declaration} = {};

	const data = await getObjectData(table);

	data.forEach((row: any) => {
		const {
			WHNAME: formatName,
			WHFLDT: type,
			WHFLDB: strLength, 
			WHFLDD: digits,
			WHFLDP: decimals,
			WHFTXT: text,
		} = row;

		const name = aliases ? row.WHALIS || row.WHFLDE : row.WHFLDE;

		if (name.trim() === ``) return;
		if (name.startsWith(`*`)) return;

		let recordFormat;
		if (recordFormats[formatName]) {
			recordFormat = recordFormats[formatName];
		} else {
			recordFormat = new Declaration(`struct`);
			recordFormat.name = formatName;
			recordFormats[formatName] = recordFormat;
		}

		const currentSubfield = new Declaration(`subitem`);
		currentSubfield.name = name;
		const keywords = [];

		if (row.WHVARL === `Y`) keywords.push(`VARYING`);

		currentSubfield.keywords = [getPrettyType({
			type,
			len: digits === 0 ? strLength : digits,
			decimals: decimals,
			keywords: [],
		})];
		currentSubfield.description = text.trim();

		recordFormat.subItems.push(currentSubfield);
	});

	return Object.values(recordFormats);
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