import path = require('path');
import { Uri, workspace, RelativePattern, commands } from 'vscode';
import { LanguageClient } from 'vscode-languageclient/node';
import { getInstance } from './base';
import { projectFilesGlob } from "./configuration";
import { IBMiMember } from '@halcyontech/vscode-ibmi-types';

let streamFileSupportChecked = false;
let streamFileSupported = false;

export default function buildRequestHandlers(client: LanguageClient) {
	/**
	 * Validates a URI.
	 * 1. Attemps to open a valid full path
	 * 2. If running in a workspace, will search for the file by basename
	 */
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

	/**
	 * Returns the working directory from Code for IBM i.
	 */
	client.onRequest("getWorkingDirectory", async (): Promise<string | undefined> => {
		const instance = getInstance();
		if (instance && instance.getConnection()) {
			const config = instance.getConfig();
			if (config) {
				return config.homeDirectory;
			}
		}
	})

	/**
	 * Gets the text content for a provided Uri
	 */
	client.onRequest("getFile", async (stringUri: string): Promise<string | undefined> => {
		// Always assumes URI is valid. Use getUri first
		const uri = Uri.parse(stringUri);
		try {
			const doc = await workspace.openTextDocument(uri);

			if (doc) {
				return doc.getText();
			}
		} catch (e) { }

		return;
	});

	/**
	 * Resolves member paths
	 */
	client.onRequest("memberResolve", async (parms: string[]): Promise<IBMiMember | undefined> => {
		let memberName = parms[0], sourceFile = parms[1];

		const instance = getInstance();

		if (instance) {
			const config = instance?.getConfig();
			const content = instance?.getContent();

			if (config && content) {
				const files = [config?.currentLibrary, ...config?.libraryList!]
					.filter(l => l !== undefined)
					.map(l => ({ name: sourceFile, library: l! }));

				try {
					const member = await content?.memberResolve(memberName, files);

					return member;
				} catch (e) {
					console.log(e);
					return undefined;
				}
			}
		}
	});

	client.onRequest("streamfileResolve", async (parms: any[]): Promise<string | undefined> => {
		const bases: string[] = parms[0];
		const includePaths: string[] = parms[1];

		const instance = getInstance();

		const content = instance?.getContent();
		const config = instance?.getConfig()!;

		if (instance && content && config) {
			if (includePaths.length === 0) {
				includePaths.push(config.homeDirectory);
			}

			const resolvedPath = await content?.streamfileResolve(bases, includePaths);

			return resolvedPath;
		}
	});

	/**
	 * Gets the column information for a provided file
	 */
	client.onRequest(`getObject`, async (table: string) => {
		const instance = getInstance();

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

				const outfileRes: any = await connection.runCommand({
					environment: `ile`,
					command: `DSPFFD FILE(${parts.schema}/${parts.table}) OUTPUT(*OUTFILE) OUTFILE(${fullPath})`
				});

				console.log(outfileRes);
				const resultCode = outfileRes.code || 0;

				if (resultCode === 0) {
					const data: object[] = await content.getTable(config.tempLibrary, randomFile, randomFile, true);

					console.log(`Temp OUTFILE read. ${data.length} rows.`);

					return data;
				}
			}
		}

		return [];
	});
}