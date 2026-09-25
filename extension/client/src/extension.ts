/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

import * as path from 'path';
import { workspace, ExtensionContext, window, ProgressLocation } from 'vscode';

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
import { checkAndWait, loadBase } from './base';
import { registerCommands } from './commands';
import { setLanguageSettings } from './language/config';
import { fieldListChecker } from '../../../language/components/field_list';
import { initRpgleOutput, logError, logInfo } from '../../../language/components/logger';

let client: LanguageClient;

export function activate(context: ExtensionContext) {
	initRpgleOutput(context);

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

	// Options to control the language client
	const clientOptions: LanguageClientOptions = {
		// Register the server for both ILE and OPM RPG documents.
		documentSelector: [
			{ language: 'rpgle' },
			{ language: 'rpg' },
			{ language: 'sqlrpgle' },
			{ language: 'sqlrpg' },
			{ language: 'rpgleinc' },
			{ language: 'rpginc' },
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
		buildRequestHandlers(client);

		const formatSeconds = (durationMs: number) => `${(durationMs / 1000).toFixed(1)}s`;
		const showCrossReferenceToast = async (message: string, durationMs = 5000) => {
			await window.withProgress(
				{ location: ProgressLocation.Notification, title: message, cancellable: false },
				async () => {
					await new Promise(resolve => setTimeout(resolve, durationMs));
				}
			);
		};

		client.onNotification(`vscode-rpgle/crossReferenceState`, (payload: {
			phase?: string,
			uri?: string,
			fileName?: string,
			lineCount?: number,
			parseId?: number,
			durationMs?: number,
		}) => {
			const fileName = payload.fileName || `document`;
			const lineCount = payload.lineCount || 0;
			const parseId = payload.parseId || 0;

			if (payload.phase === `started`) {
				logInfo(`[vscode-rpgle] Cross-reference build started: ${fileName} (${lineCount.toLocaleString()} lines, parseId=${parseId}).`);
				return;
			}

			if (payload.phase === `completed`) {
				const durationMs = payload.durationMs || 0;
				logInfo(`[vscode-rpgle] Cross-reference build completed: ${fileName} (${lineCount.toLocaleString()} lines, parseId=${parseId}, duration=${durationMs}ms).`);
			}
		});

		client.onNotification(`vscode-rpgle/crossReferenceReady`, (payload: {
			uri?: string,
			fileName?: string,
			lineCount?: number,
			durationMs?: number,
		}) => {
			const fileName = payload.fileName || `document`;
			const lineCount = payload.lineCount || 0;
			const durationMs = payload.durationMs || 0;
			const message = `RPGLE: Cross references ready for ${fileName} (${lineCount.toLocaleString()} lines, ${formatSeconds(durationMs)}).`;
			logInfo(`[vscode-rpgle] ${message}`);
			void showCrossReferenceToast(message, 5000);
		});

		const instance = await checkAndWait();
		const base = loadBase();
		let startupRefreshStarted = false;
		let startupRefreshCompleted = false;

		const ensureFieldList = async () => {
			try {
				const connection = instance?.getConnection?.();
				if (!connection) {
					logInfo(`[vscode-rpgle] FIELD_LIST ensure skipped: no active connection`);
					return;
				}

				logInfo(`[vscode-rpgle] FIELD_LIST ensure: checking remote state`);
				const state = await fieldListChecker.getRemoteState(connection);
				logInfo(`[vscode-rpgle] FIELD_LIST ensure: remote state=${state}`);

				if (state === 'Installed') {
					return;
				}

				logInfo(`[vscode-rpgle] FIELD_LIST ensure: running update`);
				const updateState = await fieldListChecker.update(connection);
				logInfo(`[vscode-rpgle] FIELD_LIST ensure: update result=${updateState}`);
			} catch (e) {
				logError(`[vscode-rpgle] FIELD_LIST ensure failed`, e);
			}
		};

		const rebuildFieldMetadataCache = async (reason: string) => {
			if (startupRefreshCompleted) {
				logInfo(`[vscode-rpgle] Startup field metadata refresh skipped (${reason}): already completed.`);
				return;
			}

			if (startupRefreshStarted) {
				logInfo(`[vscode-rpgle] Startup field metadata refresh skipped (${reason}): refresh already in progress.`);
				return;
			}

			startupRefreshStarted = true;
			try {
				logInfo(`[vscode-rpgle] Startup field metadata refresh started (${reason}).`);
				await clearTableCache(client, 'startup', true);
				await client.sendRequest(`refreshTableCache`, true);
				startupRefreshCompleted = true;
				logInfo(`[vscode-rpgle] Startup field metadata refresh completed (${reason}).`);
			} catch (e) {
				startupRefreshStarted = false;
				logError(`[vscode-rpgle] Startup field metadata refresh failed (${reason}).`, e);
			}
		};

		if (base?.componentRegistry) {
			logInfo(`[vscode-rpgle] Registering FIELD_LIST component checker`);
			base.componentRegistry.registerComponent(context, fieldListChecker as any);
		} else {
			logInfo(`[vscode-rpgle] FIELD_LIST componentRegistry unavailable`);
		}

		// We need to clear table caches when the connection changes
		if (instance && base) {
			// When the connection is established
			instance.subscribe(context, "connected", "vscode-rpgle", async () => {
				await ensureFieldList();
				await rebuildFieldMetadataCache(`connection event`);
			});

			// If we're already connected by the time onReady runs, ensure immediately.
			await ensureFieldList();
			await rebuildFieldMetadataCache(`already connected on activation`);

			// When the library list changes
			context.subscriptions.push(
				base.onCodeForIBMiConfigurationChange("connectionSettings", async () => {
					await clearTableCache(client, 'manual');
				}),
			);
		}
	});

	// Start the client. This will also launch the server
	client.start();

	Linter.initialise(context);
	columnAssist.registerColumnAssist(context);
	registerBracketMatcher(context);
	registerJumpToMatchingBlock(context);
	registerCommentStatementCommand(context);
	registerUncommentStatementCommand(context);
	registerToggleCommentCommand(context);

	registerCommands(context, client);

	context.subscriptions.push(getServerSymbolProvider());
	context.subscriptions.push(getServerImplementationProvider());
	context.subscriptions.push(setLanguageSettings());
	// context.subscriptions.push(...initBuilder(client));
}

export function deactivate(): Thenable<void> | undefined {
	if (!client) {
		return undefined;
	}
	return client.stop();
}
