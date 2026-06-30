/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

import * as path from 'path';
import { workspace, ExtensionContext } from 'vscode';

import * as Linter from "./linter";
import * as columnAssist from "./language/columnAssist";
import { registerBracketMatcher, registerJumpToMatchingBlock } from "./language/bracketMatcher";
import { registerCommentStatementCommand, registerUncommentStatementCommand, registerToggleCommentCommand } from './commentStmt';


import {
	LanguageClient,
	LanguageClientOptions,
	ServerOptions,
	TransportKind
} from 'vscode-languageclient/node';

import { projectFilesGlob } from './configuration';
import { clearTableCache, buildRequestHandlers } from './requests';
import { getServerImplementationProvider, getServerSymbolProvider } from './language/serverReferences';
import { checkAndWait, loadBase, onCodeForIBMiConfigurationChange } from './base';
import { registerCommands } from './commands';
import { setLanguageSettings } from './language/config';

let client: LanguageClient;

// GLOBAL ERROR HANDLERS - catch unhandled promise rejections and exceptions
if (process) {
	process.on('uncaughtException', (error) => {
		console.error('[vscode-rpgle] UNCAUGHT EXCEPTION:', error);
	});
	process.on('unhandledRejection', (reason, promise) => {
		console.error('[vscode-rpgle] UNHANDLED REJECTION:', reason, 'promise:', promise);
	});
}

export function activate(context: ExtensionContext) {
	// The server is implemented in node
	const serverModule = context.asAbsolutePath(
		path.join('out', 'server.js')
	);
	// The debug options for the server
	// --inspect=6009: runs the server in Node's Inspector mode so VS Code can attach to the server for debugging
	const debugOptions = { execArgv: ['--nolazy', '--inspect=8789'] };

	// If the extension is launched in debug mode then the debug server options are used
	// Otherwise the run options are used
	const serverOptions: ServerOptions = {
		run: { module: serverModule, transport: TransportKind.ipc },
		debug: {
			module: serverModule,
			transport: TransportKind.ipc,
			options: debugOptions
		}
	};

	loadBase();

	// Options to control the language client
	const clientOptions: LanguageClientOptions = {
		// Register the server for both ILE and OPM RPG documents.
		documentSelector: [
			{ language: 'rpgle' },
			{ language: 'rpg' },
		],
		synchronize: {
			fileEvents: [
				workspace.createFileSystemWatcher('**/iproj.json'),
				workspace.createFileSystemWatcher('**/rpglint.json'),
				workspace.createFileSystemWatcher(projectFilesGlob),
			]
		}
	};

	// Create the language client and start the client.
	client = new LanguageClient(
		'lsp-rpgle-client',
		'RPGLE language client',
		serverOptions,
		clientOptions
	);

	client.onReady().then(async () => {
		try {
			buildRequestHandlers(client);

			const instance = await checkAndWait();

			// We need to clear table caches when the connection changes
			if (instance) {
				// When the connection is established
				instance.subscribe(context, "connected", "vscode-rpgle", () => {
					clearTableCache(client);
				});

				// When the library list changes
				context.subscriptions.push(
					onCodeForIBMiConfigurationChange("connectionSettings", async () => {
						clearTableCache(client);
					}),
				);
			}
		} catch (err) {
			console.error('[vscode-rpgle] Unhandled error in client.onReady:', err);
		}
	}).catch((err) => {
		console.error('[vscode-rpgle] Unhandled error in client.onReady promise:', err);
	});

	// Start the client. This will also launch the server
	client.start();

	try {
		Linter.initialise(context);
	} catch (err) {
		console.error('[vscode-rpgle] Error in Linter.initialise:', err);
	}

	try {
		columnAssist.registerColumnAssist(context);
	} catch (err) {
		console.error('[vscode-rpgle] Error in columnAssist.registerColumnAssist:', err);
	}

	try {
		registerBracketMatcher(context);
	} catch (err) {
		console.error('[vscode-rpgle] Error in registerBracketMatcher:', err);
	}

	try {
		registerJumpToMatchingBlock(context);
	} catch (err) {
		console.error('[vscode-rpgle] Error in registerJumpToMatchingBlock:', err);
	}

	try {
		registerCommentStatementCommand(context);
	} catch (err) {
		console.error('[vscode-rpgle] Error in registerCommentStatementCommand:', err);
	}

	try {
		registerUncommentStatementCommand(context);
	} catch (err) {
		console.error('[vscode-rpgle] Error in registerUncommentStatementCommand:', err);
	}

	try {
		registerToggleCommentCommand(context);
	} catch (err) {
		console.error('[vscode-rpgle] Error in registerToggleCommentCommand:', err);
	}

	try {
		registerCommands(context, client);
	} catch (err) {
		console.error('[vscode-rpgle] Error in registerCommands:', err);
	}

	try {
		context.subscriptions.push(getServerSymbolProvider());
		context.subscriptions.push(getServerImplementationProvider());
		context.subscriptions.push(setLanguageSettings());
	} catch (err) {
		console.error('[vscode-rpgle] Error registering providers:', err);
	}
}

export function deactivate(): Thenable<void> | undefined {
	if (!client) {
		return undefined;
	}
	return client.stop();
}
