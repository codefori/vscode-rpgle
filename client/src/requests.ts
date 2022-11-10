import path = require('path');
import { Uri, workspace, RelativePattern, commands } from 'vscode';
import { LanguageClient } from 'vscode-languageclient/node';
import getBase from './base';
import {projectFilesGlob} from "./configuration";

export default function buildRequestHandlers(client: LanguageClient) {
	client.onRequest("getUri", async (stringUri: string): Promise<string | undefined> => {
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
				const [possibleFile] = await workspace.findFiles(`**/${basename}`, `**/.git`, 1);
				if (possibleFile) {
					return possibleFile.toString();
				}
			}

		return;
	});

	client.onRequest("getWorkingDirectory", async (): Promise<string|undefined> => {
		const instance = getBase();
		if (instance && instance.getConnection()) {
			const config = instance.getConfig();
			return config.homeDirectory;
		}
	})

	client.onRequest("getFile", async (stringUri: string): Promise<string | undefined> => {
		// Always assumes URI is valid. Use getUri first
		const uri = Uri.parse(stringUri);
		const doc = await workspace.openTextDocument(uri);

		if (doc) {
			return doc.getText();
		}

		return;
	});

	client.onRequest(`getProjectFiles`, async (): Promise<string[] | undefined> => {
		if (workspace.workspaceFolders) {
			const uris = await workspace.findFiles(projectFilesGlob, `**/.git`);
			return uris.map(uri => uri.toString());
		}

		return undefined;
	});

	client.onRequest(`getIncludesUris`, async (stringUri: string): Promise<{uri: string, relative: string}[]> => {
		if (workspace.workspaceFolders) {
			const uri = Uri.parse(stringUri);
			const workspaceFolder = workspace.getWorkspaceFolder(uri);

			if (workspaceFolder) {
				const relativeWorkspace = new RelativePattern(workspaceFolder, `**/*.{rpgleinc,RPGLEINC}`);
				const localFiles = await workspace.findFiles(relativeWorkspace, `**/.git`);

				return localFiles.map(localFile => {
					return {
						uri: localFile.toString(),
						relative: path.relative(workspaceFolder.uri.path, localFile.path)
					};
				});
			}
		}

		return [];
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
}