/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

import * as path from 'path';
import { workspace, ExtensionContext, Uri, commands, RelativePattern } from 'vscode';

import * as Linter from "./linter";
import * as columnAssist from "./columnAssist";


import {
	LanguageClient,
	LanguageClientOptions,
	ServerOptions,
	TransportKind
} from 'vscode-languageclient/node';

import { projectFilesGlob } from './configuration';
import buildRequestHandlers from './requests';

let client: LanguageClient;

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

	// Options to control the language client
	const clientOptions: LanguageClientOptions = {
		// Register the server for plain text documents
		documentSelector: [
			{ language: 'rpgle' },
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

	client.onReady().then(() => {
		buildRequestHandlers(client);
	});

	// Start the client. This will also launch the server
	client.start();

	Linter.initialise(context);
	columnAssist.registerColumnAssist(context);

	// context.subscriptions.push(...initBuilder(client));

	console.log(`started`);
}

export function deactivate(): Thenable<void> | undefined {
	if (!client) {
		return undefined;
	}
	return client.stop();
}
