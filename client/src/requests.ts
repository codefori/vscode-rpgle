import path = require('path');
import { Uri, workspace, RelativePattern, commands } from 'vscode';
import { LanguageClient } from 'vscode-languageclient/node';
import getBase from './base';
import { projectFilesGlob } from "./configuration";

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
		const instance = getBase();
		if (instance && instance.getConnection()) {
			const config = instance.getConfig();
			return config.homeDirectory;
		}
	})

	/**
	 * Gets the text content for a provided Uri
	 */
	client.onRequest("getFile", async (stringUri: string): Promise<string | undefined> => {
		// Always assumes URI is valid. Use getUri first
		const uri = Uri.parse(stringUri);
		const doc = await workspace.openTextDocument(uri);

		if (doc) {
			return doc.getText();
		}

		return;
	});

	/**
	 * Returns all valid RPGLE files in the local workspace.
	 */
	client.onRequest(`getProjectFiles`, async (): Promise<string[] | undefined> => {
		if (workspace.workspaceFolders) {
			const uris = await workspace.findFiles(projectFilesGlob, `**/.git`);
			return uris.map(uri => uri.toString());
		}

		return undefined;
	});

	/**
	 * Returns all valid rpgleinc files in the local workspace.
	 */
	client.onRequest(`getIncludesUris`, async (stringUri: string): Promise<{ uri: string, relative: string }[]> => {
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

	/**
	 * Gets the column information for a provided file
	 */
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

	client.onRequest(`symbolLookup`, async (data: { symbol: string, binders: { lib?: string, name: string }[] }): Promise<string[] | undefined> => {
		const { symbol, binders } = data;
		const instance = getBase();

		if (instance) {
			const connection = instance.getConnection();
			if (connection) {
				const config = instance.getConfig();
				const currentLibrary = config.currentLibrary;

				const binderCondition = binders.map(binder => `(c.BINDING_DIRECTORY_LIBRARY = '${(binder.lib || currentLibrary).toUpperCase()}' and c.BINDING_DIRECTORY = '${binder.name.toUpperCase()}')`)
				const statement = [
					`select`,
					`	b.SYMBOL_NAME,`,
					`	c.ENTRY_LIBRARY as PGM_LIB,`,
					`	c.ENTRY as PGM_NAME,`,
					`	a.BOUND_MODULE_LIBRARY as MOD_LIB, `,
					`	a.BOUND_MODULE as MOD_NAME, `,
					`	a.SOURCE_FILE_LIBRARY as LIB, `,
					`	a.SOURCE_FILE as SPF, `,
					`	a.SOURCE_FILE_MEMBER as MBR,`,
					` a.MODULE_ATTRIBUTE as ATTR`,
					`from QSYS2.BOUND_MODULE_INFO as a`,
					`right join QSYS2.PROGRAM_EXPORT_IMPORT_INFO as b`,
					`	on a.PROGRAM_LIBRARY = b.PROGRAM_LIBRARY and a.PROGRAM_NAME = b.PROGRAM_NAME`,
					`right join qsys2.BINDING_DIRECTORY_INFO as c`,
					`	on c.ENTRY_LIBRARY = b.PROGRAM_LIBRARY and c.ENTRY = b.PROGRAM_NAME`,
					`where UPPER(b.SYMBOL_NAME) = '${symbol.toUpperCase()}'`,
					`  and (${binderCondition.join(` or `)})`,
					`  and a.SOURCE_FILE_MEMBER is not null`
				].join(` `);

				try {
					const rows: any[] = await commands.executeCommand(`code-for-ibmi.runQuery`, statement);

					return rows.map(row => {
						return Uri.from({
							scheme: `member`,
							path: [``, row.LIB, row.SPF, `${row.MBR}.${row.ATTR}`].join(`/`)
						}).toString();
					})
				} catch (e) {
					console.log(e);
				}
			}
		}
		return;
	})
}