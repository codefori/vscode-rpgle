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

export function activate(context: ExtensionContext) {
	console.log('[vscode-rpgle] extension.activate: STARTING');
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
			console.log('[vscode-rpgle] extension.ts: client.onReady callback starting');
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
			console.log('[vscode-rpgle] extension.ts: client.onReady callback completed successfully');
		} catch (err) {
			console.error('[vscode-rpgle] extension.ts: client.onReady callback threw error:', err);
		}
	});

	// Start the client. This will also launch the server
	client.start();

	try {
		console.log('[vscode-rpgle] extension.ts: Linter.initialise starting');
		Linter.initialise(context);
		console.log('[vscode-rpgle] extension.ts: Linter.initialise completed');
	} catch (err) {
		console.error('[vscode-rpgle] extension.ts: Linter.initialise threw error:', err);
	}

	try {
		console.log('[vscode-rpgle] extension.ts: columnAssist.registerColumnAssist starting');
		columnAssist.registerColumnAssist(context);
		console.log('[vscode-rpgle] extension.ts: columnAssist.registerColumnAssist completed');
	} catch (err) {
		console.error('[vscode-rpgle] extension.ts: columnAssist.registerColumnAssist threw error:', err);
	}

	try {
		console.log('[vscode-rpgle] extension.ts: About to call registerBracketMatcher');
		registerBracketMatcher(context);
		console.log('[vscode-rpgle] extension.ts: registerBracketMatcher completed successfully');
	} catch (err) {
		console.error('[vscode-rpgle] extension.ts: registerBracketMatcher threw error:', err);
	}

	try {
		console.log('[vscode-rpgle] extension.ts: registerJumpToMatchingBlock starting');
		registerJumpToMatchingBlock(context);
		console.log('[vscode-rpgle] extension.ts: registerJumpToMatchingBlock completed');
	} catch (err) {
		console.error('[vscode-rpgle] extension.ts: registerJumpToMatchingBlock threw error:', err);
	}

	try {
		console.log('[vscode-rpgle] extension.ts: registerCommentStatementCommand starting');
		registerCommentStatementCommand(context);
		console.log('[vscode-rpgle] extension.ts: registerCommentStatementCommand completed');
	} catch (err) {
		console.error('[vscode-rpgle] extension.ts: registerCommentStatementCommand threw error:', err);
	}

	try {
		console.log('[vscode-rpgle] extension.ts: registerUncommentStatementCommand starting');
		registerUncommentStatementCommand(context);
		console.log('[vscode-rpgle] extension.ts: registerUncommentStatementCommand completed');
	} catch (err) {
		console.error('[vscode-rpgle] extension.ts: registerUncommentStatementCommand threw error:', err);
	}

	try {
		console.log('[vscode-rpgle] extension.ts: registerToggleCommentCommand starting');
		registerToggleCommentCommand(context);
		console.log('[vscode-rpgle] extension.ts: registerToggleCommentCommand completed');
	} catch (err) {
		console.error('[vscode-rpgle] extension.ts: registerToggleCommentCommand threw error:', err);
	}

	try {
		console.log('[vscode-rpgle] extension.ts: registerCommands starting');
		registerCommands(context, client);
		console.log('[vscode-rpgle] extension.ts: registerCommands completed');
	} catch (err) {
		console.error('[vscode-rpgle] extension.ts: registerCommands threw error:', err);
	}

	try {
		console.log('[vscode-rpgle] extension.ts: registerProviders starting');
		context.subscriptions.push(getServerSymbolProvider());
		context.subscriptions.push(getServerImplementationProvider());
		context.subscriptions.push(setLanguageSettings());
		console.log('[vscode-rpgle] extension.ts: registerProviders completed');
	} catch (err) {
		console.error('[vscode-rpgle] extension.ts: registerProviders threw error:', err);
	}

	console.log('[vscode-rpgle] extension.ts: activate() COMPLETED SUCCESSFULLY');
}

export function deactivate(): Thenable<void> | undefined {
	if (!client) {
		return undefined;
	}
	return client.stop();
}
