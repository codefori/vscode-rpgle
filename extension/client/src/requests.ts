import path = require('path');
import { Uri, workspace } from 'vscode';
import { LanguageClient } from 'vscode-languageclient/node';
import { getInstance } from './base';
import { IBMiMember } from '@halcyontech/vscode-ibmi-types';

export function buildRequestHandlers(client: LanguageClient) {
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
		if (instance) {
			const connection = instance.getConnection();
			const config = connection?.getConfig();
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
		const connection = instance?.getConnection();

		if (connection) {
			const config = connection.getConfig();
			const content = connection.getContent();

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
		const connection = instance?.getConnection();

		if (connection) {
			const content = connection.getContent();
			const config = connection.getConfig()!;

			if (instance && content && config) {
				if (includePaths.length === 0) {
					includePaths.push(config.homeDirectory);
				}

				const resolvedPath = await content?.streamfileResolve(bases, includePaths);

				return resolvedPath;
			}
		}
	});

	/**
	 * Gets the column information for a provided file
	 */
	client.onRequest(`getObject`, async (table: string) => {
		const instance = getInstance();

		if (instance) {
			console.log(`Fetching table: ${table}`);

			const connection = instance.getConnection();
			if (connection) {
				const content = connection.getContent();
				const config = connection.getConfig();

				// A counter rather than the time: two lookups can start in the same millisecond
				const fileNumber = (outfileCounter++ % 1000000).toString().padStart(6, `0`);
				const randomFile = `R${table.substring(0, 3)}${fileNumber}`.substring(0, 10);
				const fullPath = `QTEMP/${randomFile}`;

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

				// TODO: handle .env file here?

				if (connection.enableSQL) {
					// Run everything in the SQL job: going through runCommand would queue
					// these requests with the user's saves and actions.
					const { currentLibrary, libraryList } = getEffectiveLibraries(config);

					// One statement, so nothing else runs in the SQL job between CHGLIBL and DSPFFD.
					// The job's library list is put back afterwards: an Action may be running
					// in the same job with its own library list.
					const dspffd = [
						`begin`,
						`  declare oldLibl varchar(2750) default '*NONE';`,
						`  declare oldCurlib varchar(10) default '*CRTDFT';`,
						`  declare exit handler for sqlexception begin`,
						`    call QSYS2.QCMDEXC('QSYS/CHGLIBL LIBL(' concat oldLibl concat ') CURLIB(' concat oldCurlib concat ')');`,
						`    resignal;`,
						`  end;`,
						`  select coalesce(listagg(SYSTEM_SCHEMA_NAME, ' ') within group (order by ORDINAL_POSITION), '*NONE') into oldLibl from QSYS2.LIBRARY_LIST_INFO where TYPE = 'USER';`,
						`  select coalesce(max(SYSTEM_SCHEMA_NAME), '*CRTDFT') into oldCurlib from QSYS2.LIBRARY_LIST_INFO where TYPE = 'CURRENT';`,
						`  call QSYS2.QCMDEXC('QSYS/CHGLIBL LIBL(${sqlString(libraryList.join(` `) || `*NONE`)}) CURLIB(${sqlString(currentLibrary || `*CRTDFT`)})');`,
						`  call QSYS2.QCMDEXC('QSYS/DSPFFD FILE(${sqlString(parts.schema)}/${sqlString(parts.table)}) OUTPUT(*OUTFILE) OUTFILE(${fullPath})');`,
						`  call QSYS2.QCMDEXC('QSYS/CHGLIBL LIBL(' concat oldLibl concat ') CURLIB(' concat oldCurlib concat ')');`,
						`end`
					].join(`\n`);

					let data: any[];
					try {
						// runStatements rather than getQTempTable, which Code for IBM i has removed
						data = await content.runStatements(
							dspffd,
							`select * from ${fullPath.replace(`/`, `.`)}`
						);
					} catch (e) {
						// Same as without SQL: a file that is not found has no fields
						console.log(e);
						return [];
					}

					// Only when DSPFFD worked: if it failed there is no outfile, and a failing
					// DLTOBJ is one more command in the SQL job
					content.runStatements(`@QSYS/DLTOBJ OBJ(${fullPath}) OBJTYPE(*FILE)`).catch(() => {});

					// Same values as the CSV export used without SQL
					return data.map(row => {
						for (const key of Object.keys(row)) {
							if (typeof row[key] === `string`) row[key] = row[key].trimEnd();
						}
						return row;
					});
				}

				const outfileRes: any = await connection.runCommand({
					environment: `ile`,
					command: `QSYS/DSPFFD FILE(${parts.schema}/${parts.table}) OUTPUT(*OUTFILE) OUTFILE(${fullPath})`
				});

				console.log(outfileRes);
				const resultCode = outfileRes.code || 0;

				if (resultCode === 0) {
					const data: any[] = await content.getTable('QTEMP', randomFile, randomFile, true);

					console.log(`Temp OUTFILE read. ${data.length} rows.`);

					connection.runCommand({
						environment: `ile`,
						command: `QSYS/DLTOBJ OBJ(${fullPath}) OBJTYPE(*FILE)`
					});

					return data;
				}
			}
		}

		return [];
	});
}

let outfileCounter = 0;

/**
 * Doubles the apostrophes of a value put inside an SQL string literal: names can come
 * from the source (EXTFILE/EXTNAME) or the settings, and must not end the literal.
 */
function sqlString(value: string) {
	return value.replace(/'/g, `''`);
}

/**
 * The current library and library list used by Code for IBM i's ILE commands:
 * the CURLIB and LIBL custom variables override the connection settings.
 */
export function getEffectiveLibraries(config: { currentLibrary: string, libraryList: string[], customVariables?: { name: string, value: string }[] }) {
	const variable = (name: string) => config.customVariables?.find(v => v.name.toUpperCase() === name)?.value;

	const currentLibrary = variable(`CURLIB`) || config.currentLibrary || ``;
	const libraryList = variable(`LIBL`)?.split(` `).filter(library => library) || config.libraryList;

	return {
		currentLibrary: /\*CURLIB/i.test(currentLibrary) ? `` : currentLibrary,
		libraryList: libraryList.filter((library, index, list) => list.indexOf(library) === index)
	};
}

export function clearTableCache(client: LanguageClient) {
	client.sendRequest(`clearTableCache`);
}

export function getCache(client: LanguageClient, uri: Uri): Promise<any> {
	return client.sendRequest(`getCache`, uri.toString());
}