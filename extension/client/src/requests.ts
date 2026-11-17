import path = require('path');
import { Uri, workspace } from 'vscode';
import { LanguageClient } from 'vscode-languageclient/node';
import { getInstance } from './base';
import { IBMiMember } from '@halcyontech/vscode-ibmi-types';
import { queryFieldList, convertFieldListToDspffdFormat } from '../../../language/components/fieldListQuery';
import { logError, logInfo, logWarn } from '../../../language/components/logger';
import { shouldUseCachedTableMetadata, TableCacheEntry } from '../../../language/utils/tableCache';
import { pruneOldest } from '../../../language/utils/pruneOldest';

const TABLE_CACHE_TTL = 30 * 60 * 1000;
const TABLE_CACHE_LIMIT = 250;

const tableObjectCache = new Map<string, TableCacheEntry>();
const tableObjectInFlight = new Map<string, Promise<any[]>>();
let tableCacheGeneration = 0;

function parseTableName(table: string): { schema: string; file: string } {
	const parts = table.trim().toUpperCase().split(`/`).filter(Boolean);
	return {
		schema: parts.length >= 2 ? parts[parts.length - 2] : `*LIBL`,
		file: parts[parts.length - 1] || ``,
	};
}

function getTableCacheKey(table: string): string {
	const { schema, file } = parseTableName(table);
	return `${schema}/${file}`;
}

function pruneTableCache() {
	pruneOldest(tableObjectCache, TABLE_CACHE_LIMIT);
}

function clearLocalTableObjectCache() {
	tableCacheGeneration++;
	tableObjectCache.clear();
	tableObjectInFlight.clear();
}

function logTableMetadataLifecycle(
	cacheKey: string,
	phase: 'start' | 'complete' | 'join' | 'error',
	options: {
		table?: string,
		provider?: string,
		startedAt?: number,
		completedAt?: number,
		rows?: number,
		source?: 'UDTF' | 'DSPFFD',
		error?: unknown,
	} = {}
) {
	const startedAt = options.startedAt ?? Date.now();
	const completedAt = options.completedAt ?? startedAt;
	const durationMs = Math.max(0, completedAt - startedAt);
	const common = [
		`cacheKey=${cacheKey}`,
		options.table ? `table=${options.table}` : undefined,
		options.provider ? `provider=${options.provider}` : undefined,
	].filter(Boolean).join(' ');

	if (phase === 'join') {
		logInfo(`[vscode-rpgle] table metadata fetch queued: ${common}`);
		return;
	}

	if (phase === 'start') {
		logInfo(`[vscode-rpgle] table metadata fetch started: ${common}`);
		return;
	}

	if (phase === 'error') {
		const errorText = options.error === undefined ? '' : ` error=${String(options.error)}`;
		logError(`[vscode-rpgle] table metadata fetch failed: ${common} durationMs=${durationMs}${errorText}`.trim(), options.error);
		return;
	}

	logInfo(`[vscode-rpgle] table metadata fetch completed: ${common} durationMs=${durationMs} rows=${options.rows ?? 0} source=${options.source ?? 'UDTF'}`);
}

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
		const cacheKey = getTableCacheKey(table);
		const now = Date.now();

		const cached = tableObjectCache.get(cacheKey);
		if (cached && shouldUseCachedTableMetadata(cached, now, TABLE_CACHE_TTL)) {
			// Refresh LRU order.
			tableObjectCache.delete(cacheKey);
			tableObjectCache.set(cacheKey, cached);
			logInfo(`[vscode-rpgle] Using cached table metadata for ${cacheKey}. source=${cached.source} rows=${cached.data.length}.`);
			return cached.data;
		}

		const activeFetch = tableObjectInFlight.get(cacheKey);
		if (activeFetch) {
			logTableMetadataLifecycle(cacheKey, 'join', { table, provider: workspace.getConfiguration('vscode-rpgle').get<string>('fieldMetadataProvider', 'CL') });
			return activeFetch;
		}

		const cacheGeneration = tableCacheGeneration;

		const fetchPromise = (async () => {
			const startedAt = Date.now();
			const instance = getInstance();

			if (instance) {
				const connection = instance.getConnection();
				if (connection) {
					const content = connection.getContent();
					const parts = parseTableName(table);

					const rpgleConfig = workspace.getConfiguration('vscode-rpgle');
					const fieldDefMethod = rpgleConfig.get<string>('fieldMetadataProvider', 'CL');
					const fieldListLibrary = connection.getConfig().tempLibrary?.trim().toUpperCase();
					if (!fieldListLibrary) {
						throw new Error('FIELD_LIST requires Code for IBM i to configure a temporary library.');
					}
					logTableMetadataLifecycle(cacheKey, 'start', { table, provider: fieldDefMethod, startedAt });

					const fetchViaDspffd = async () => {
						const dateStr = Date.now().toString().substr(-6);
						const randomFile = `R${parts.file.substring(0, 3)}${dateStr}`.substring(0, 10);
						const fullPath = `QTEMP/${randomFile}`;

						const outfileRes: any = await connection.runCommand({
							environment: `ile`,
							command: `QSYS/DSPFFD FILE(${parts.schema}/${parts.table}) OUTPUT(*OUTFILE) OUTFILE(${fullPath})`
						});
						const resultCode = outfileRes.code || 0;

						if (resultCode !== 0) {
							return undefined;
						}

						return content.getTable('QTEMP', randomFile, randomFile, true);
					};

					try {
						let data: any[] | undefined;
						let source: 'UDTF' | 'DSPFFD' = 'DSPFFD';

						if (fieldDefMethod === 'UDTF') {
							source = 'UDTF';
							const fieldListRows = await queryFieldList(
								connection,
								parts.schema,
								parts.table,
								'*ALL',
								fieldListLibrary
							);
							data = convertFieldListToDspffdFormat(fieldListRows);
							if (data.length === 0) {
								logInfo(`[vscode-rpgle] FIELD_LIST returned 0 rows for ${cacheKey};`);
							}
						} else {
							data = await fetchViaDspffd();
						}

						if (data) {
							if (cacheGeneration === tableCacheGeneration) {
								tableObjectCache.set(cacheKey, {
									fetchedAt: Date.now(),
									source,
									data
								});
								pruneTableCache();
							}
							logTableMetadataLifecycle(cacheKey, 'complete', { table, provider: source, startedAt, completedAt: Date.now(), rows: data.length, source });
							return data;
						}
					} catch (error) {
						logTableMetadataLifecycle(cacheKey, 'error', { table, provider: fieldDefMethod, startedAt, completedAt: Date.now(), error });
						if (fieldDefMethod === 'UDTF') {
							logWarn(`Failed to get field definitions using UDTF FIELD_LIST for ${cacheKey}.`);
						}
					}
				}
			}

			return [];
		})();

		tableObjectInFlight.set(cacheKey, fetchPromise);
		try {
			return await fetchPromise;
		} finally {
			if (tableObjectInFlight.get(cacheKey) === fetchPromise) {
				tableObjectInFlight.delete(cacheKey);
			}
		}
	});
}

export async function clearTableCache(client: LanguageClient): Promise<void> {
	// Parse/reparse cycles do not trigger this. File metadata refresh is intentionally
	// on startup or explicit user action only, not on every document edit.
	clearLocalTableObjectCache();
	await client.sendRequest(`clearTableCache`);
}

export function getCache(client: LanguageClient, uri: Uri): Promise<any> {
	return client.sendRequest(`getCache`, uri.toString());
}