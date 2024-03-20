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

	client.onRequest(`symbolLookup`, async (data: { symbol?: string, binders: { lib?: string, name: string }[] }): Promise<{ [symbol: string]: string[] } | undefined> => {
		const { symbol, binders } = data;
		const instance = getInstance();

		if (instance) {
			const connection = instance.getConnection();
			if (connection) {

				const content = instance.getContent();

				/**
				 * We need to support 7.3 and above. SOURCE_STREAM_FILE_PATH does not exist in BOUND_MODULE_INFO
				 * in 7.3, but it does exist from 7.4. We use this simple SQL statement to determine if the view
				 * contains the column for streamfile support. If it does, we support it, if not, we don't.
				 */

				if (!streamFileSupportChecked) {
					streamFileSupportChecked = true;
					const statement = [
						`select 'a' from qsys2.syscolumns`,
						`where TABLE_SCHEMA = 'QSYS2' and TABLE_NAME = 'BOUND_MODULE_INFO' and COLUMN_NAME = 'SOURCE_STREAM_FILE_PATH'`,
						`limit 1`
					].join(` `);

					const rows: any[] = await content.runSQL(statement);
					streamFileSupported = (rows.length >= 1);
				}

				const config = instance.getConfig();
				const libraryList = [config.currentLibrary, ...config.libraryList];
				const symbolClause = symbol ? `UPPER(b.SYMBOL_NAME) = '${symbol.toUpperCase()}' and` : ``;

				const libraryInList = libraryList.map(lib => `'${lib.toUpperCase()}'`).join(`, `);

				const binderCondition = binders.map(binder => {
					return `(c.BINDING_DIRECTORY_LIBRARY ${binder.lib ? `= '${binder.lib}'` : `in (${libraryInList})`} and c.BINDING_DIRECTORY = '${binder.name.toUpperCase()}')`;
				})
				const statement = [
					`select`,
					`	b.SYMBOL_NAME,`,
					`	b.PROGRAM_LIBRARY as PGM_LIB,`,
					`	c.ENTRY as PGM_NAME,`,
					`	a.BOUND_MODULE_LIBRARY as MOD_LIB, `,
					`	a.BOUND_MODULE as MOD_NAME, `,
					...(streamFileSupported ? [`a.SOURCE_STREAM_FILE_PATH as PATH,`] : []),
					`	a.SOURCE_FILE_LIBRARY as LIB, `,
					`	a.SOURCE_FILE as SPF, `,
					`	a.SOURCE_FILE_MEMBER as MBR,`,
					` a.MODULE_ATTRIBUTE as ATTR`,
					`from QSYS2.BOUND_MODULE_INFO as a`,
					`right join QSYS2.PROGRAM_EXPORT_IMPORT_INFO as b`,
					`	on a.PROGRAM_LIBRARY = b.PROGRAM_LIBRARY and a.PROGRAM_NAME = b.PROGRAM_NAME`,
					`right join qsys2.BINDING_DIRECTORY_INFO as c`,
					`	on c.ENTRY = b.PROGRAM_NAME`,
					`where ${symbolClause}`,
					`  (${binderCondition.join(` or `)}) and`,
					`  ((c.ENTRY_LIBRARY = b.PROGRAM_LIBRARY) or (c.ENTRY_LIBRARY = '*LIBL' and b.PROGRAM_LIBRARY in (${libraryInList}))) and`,
					`  (${streamFileSupported ? `a.SOURCE_STREAM_FILE_PATH is not null or` : ``} a.SOURCE_FILE_MEMBER is not null)`
				].join(` `);

				try {
					const rows: any[] = await content.runSQL(statement);

					let symbolFiles: { [symbol: string]: string[] } = {};

					rows.forEach(row => {
						let uri;
						// row.PATH is never null, but if row.MBR is null that means we likely have a streamfile
						if (streamFileSupported && row.MBR === null) {
							uri = Uri.from({
								scheme: `streamfile`,
								path: row.PATH
							}).toString();
						} else {
							uri = Uri.from({
								scheme: `member`,
								path: [``, row.LIB, row.SPF, `${row.MBR}.${row.ATTR}`].join(`/`)
							}).toString();
						}

						if (symbolFiles[row.SYMBOL_NAME]) {
							symbolFiles[row.SYMBOL_NAME].push(uri);
						} else {
							symbolFiles[row.SYMBOL_NAME] = [uri];
						}
					})

					return symbolFiles;
				} catch (e) {
					console.log(e);
				}
			}
		}
		return;
	})
}