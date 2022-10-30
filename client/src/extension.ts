/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

import * as path from 'path';
import { workspace, ExtensionContext, Uri, commands } from 'vscode';

import * as Linter from "./linter";
import * as columnAssist from "./columnAssist";


import {
	LanguageClient,
	LanguageClientOptions,
	ServerOptions,
	TransportKind
} from 'vscode-languageclient/node';

import getBase from './base';
import { initBuilder } from './builder';

let client: LanguageClient;

const projectFilesGlob = `**/*.{rpgle,RPGLE,sqlrpgle,SQLRPGLE,rpgleinc,RPGLEINC}`;

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
			// Notify the server about file changes to '.clientrc files contained in the workspace
			fileEvents: [
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
		client.onRequest("getUri", async (stringUri: string): Promise<string|undefined> => {
			const uri = Uri.parse(stringUri);
			let doc;
			try {
				doc = await workspace.openTextDocument(uri);
			} catch (e: any) {
				doc = undefined;
			}

			if (doc) {
				return doc.uri.toString();
			} else
			if (uri.scheme === `file`) {
				const basename = path.basename(uri.path);
				const [possibleFile] = await workspace.findFiles(`**/${basename}`, undefined, 1);
				if (possibleFile) {
					return possibleFile.toString();
				}
			}

			return;
		});

		client.onRequest("getFile", async (stringUri: string) : Promise<string|undefined> => { 
			// Always assumes URI is valid. Use getUri first
			const uri = Uri.parse(stringUri);
			const doc = await workspace.openTextDocument(uri);

			if (doc) {
				return doc.getText();
			}

			return;
		});

		client.onRequest(`getProjectFiles`, async (): Promise<string[]|undefined> => {
			if (workspace.workspaceFolders) {
				const uris = await workspace.findFiles(projectFilesGlob);
				return uris.map(uri => uri.toString());
			}

			return undefined;
		});

		client.onRequest(`getObject`, async (table: string) => {
			const instance = getBase();

			if (instance) {
				const connection = instance.getConnection();
				if (connection) {
					const content = instance.getContent();
					const config = instance.getConfig();
		
					const dateStr = Date.now().toString().substr(-6);
					const randomFile = `R${table.substring(0, 3)}${dateStr}`.substring(0, 10);
					const fullPath = `${config.tempLibrary}/${randomFile}`;

					console.log(`Temp OUTFILE: ${fullPath}`);

					const parts = {
						schema: `*LIBL`,
						table: ``,
					};
		
					if (table.includes(`/`)) {
						const splitName = table.split(`/`);
						if (splitName.length >= 2) parts.schema = splitName[splitName.length - 2];
						if (splitName.length >= 1) parts.table = splitName[splitName.length - 1];
					} else {
						parts.table = table;
					}

					const outfileRes: any = await commands.executeCommand(`code-for-ibmi.runCommand`, {
						environment: `ile`,
						command: `DSPFFD FILE(${parts.schema}/${parts.table}) OUTPUT(*OUTFILE) OUTFILE(${fullPath})`
					});

					console.log(outfileRes);
					const resultCode = outfileRes.code || 0;

					if (resultCode === 0) {
						const data: object[] = await content.getTable(config.tempLibrary, randomFile);

						console.log(`Temp OUTFILE read. ${data.length} rows.`);

						return data;
					}
				}
			}

			return [];
		});
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
