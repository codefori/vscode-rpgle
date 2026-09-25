import path = require('path');
import { Uri, workspace } from 'vscode';
import { LanguageClient } from 'vscode-languageclient/node';
import { getInstance } from './base';
import { IBMiMember } from '@halcyontech/vscode-ibmi-types';
import { queryFieldList, convertFieldListToDspffdFormat, FieldListRow } from '../../../language/components/fieldListQuery';
import { logError, logInfo, logWarn } from '../../../language/components/logger';
import { shouldUseCachedTableMetadata, TableCacheEntry } from '../../../language/utils/tableCache';

const TABLE_CACHE_TTL = 30 * 60 * 1000;
const TABLE_CACHE_LIMIT = 250;

type CachedTableEntry = TableCacheEntry;

const tableObjectCache = new Map<string, CachedTableEntry>();
const tableObjectInFlight = new Map<string, Promise<any[]>>();
let forceTableRefresh = false;

function resolveFieldListLibrary(connection: any): string {
	const configured = workspace.getConfiguration('vscode-rpgle')
		.get<string>('fieldListLibrary', '*TEMPLIB')
		?.trim()
		.toUpperCase();

	const config = connection?.getConfig?.() ?? connection?.config;
	const tempLib = (config?.tempLibrary as string | undefined)?.trim().toUpperCase();
	const currentLib = (config?.currentLibrary as string | undefined)?.trim().toUpperCase();

	if (!configured || configured === '*TEMPLIB') {
		return tempLib || currentLib || 'ILEDITOR';
	}

	return configured;
}

function getTableCacheKey(table: string): string {
	const upper = table.trim().toUpperCase();
	if (upper.includes(`/`)) {
		const splitName = upper.split(`/`).filter(part => part.length > 0);
		const schema = splitName.length >= 2 ? splitName[splitName.length - 2] : `*LIBL`;
		const file = splitName[splitName.length - 1] || ``;
		return `${schema}/${file}`;
	}

	return `*LIBL/${upper}`;
}

function pruneTableCache() {
	while (tableObjectCache.size > TABLE_CACHE_LIMIT) {
		const oldestKey = tableObjectCache.keys().next().value;
		if (oldestKey === undefined) break;
		tableObjectCache.delete(oldestKey);
	}
}

function clearLocalTableObjectCache() {
	tableObjectCache.clear();
	tableObjectInFlight.clear();
}

function formatTimestamp(date = new Date()) {
	return `${date.toTimeString().split(' ')[0]}.${date.getMilliseconds().toString().padStart(3, '0')}`;
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
		`startedAt=${formatTimestamp(new Date(startedAt))}`,
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
		logError(`[vscode-rpgle] table metadata fetch failed: ${common} completedAt=${formatTimestamp(new Date(completedAt))} durationMs=${durationMs}${errorText}`.trim(), options.error);
		return;
	}

	logInfo(`[vscode-rpgle] table metadata fetch completed: ${common} completedAt=${formatTimestamp(new Date(completedAt))} durationMs=${durationMs} rows=${options.rows ?? 0} source=${options.source ?? 'UDTF'}`);
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
	client.onRequest(`getObject`, async (table: string, forceRefresh = false) => {
		const cacheKey = getTableCacheKey(table);
		const now = Date.now();
		const cached = tableObjectCache.get(cacheKey);

		if (forceTableRefresh) {
			logInfo(`[vscode-rpgle] Forced table metadata refresh: clearing cached ${cacheKey}.`);
			clearLocalTableObjectCache();
			forceTableRefresh = false;
		}

		if (cached && shouldUseCachedTableMetadata(cached, now, forceRefresh || forceTableRefresh)) {
			// Refresh LRU order.
			tableObjectCache.delete(cacheKey);
			tableObjectCache.set(cacheKey, cached);
			logInfo(`[vscode-rpgle] Using cached table metadata for ${cacheKey}. source=${cached.source} rows=${cached.data.length}.`);
			return cached.data;
		}

		const shouldBypassInflightFetch = forceRefresh || forceTableRefresh;
		const activeFetch = tableObjectInFlight.get(cacheKey);
		if (activeFetch && !shouldBypassInflightFetch) {
			logTableMetadataLifecycle(cacheKey, 'join', { table, provider: workspace.getConfiguration('vscode-rpgle').get<string>('fieldMetadataProvider', 'CL') });
			return activeFetch;
		}

		if (activeFetch && shouldBypassInflightFetch) {
			logInfo(`[vscode-rpgle] Forcing fresh fetch for ${cacheKey}; bypassing in-flight request.`);
			tableObjectInFlight.delete(cacheKey);
		}

		const fetchPromise = (async () => {
			const startedAt = Date.now();
			const instance = getInstance();

			if (instance) {
				const connection = instance.getConnection();
				if (connection) {
					const content = connection.getContent();
					const config = connection.getConfig();

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

					const rpgleConfig = workspace.getConfiguration('vscode-rpgle');
					const fieldDefMethod = rpgleConfig.get<string>('fieldMetadataProvider', 'CL');
					const fieldListLibrary = resolveFieldListLibrary(connection);
					logTableMetadataLifecycle(cacheKey, 'start', { table, provider: fieldDefMethod, startedAt });

					const fetchViaDspffd = async (source: 'DSPFFD') => {
						const dateStr = Date.now().toString().substr(-6);
						const randomFile = `R${table.substring(0, 3)}${dateStr}`.substring(0, 10);
						const fullPath = `QTEMP/${randomFile}`;

						const outfileRes: any = await connection.runCommand({
							environment: `ile`,
							command: `QSYS/DSPFFD FILE(${parts.schema}/${parts.table}) OUTPUT(*OUTFILE) OUTFILE(${fullPath})`
						});
						const resultCode = outfileRes.code || 0;

						if (resultCode !== 0) {
							return undefined;
						}

						const data: any[] = await content.getTable('QTEMP', randomFile, randomFile, true);

						tableObjectCache.set(cacheKey, {
							fetchedAt: Date.now(),
							source,
							data
						});
						pruneTableCache();
						logTableMetadataLifecycle(cacheKey, 'complete', { table, provider: source, startedAt, completedAt: Date.now(), rows: data.length, source });
						return data;
					};

					try {
						if (fieldDefMethod === 'UDTF') {
							const fieldListRows = await queryFieldList(
								connection,
								parts.schema,
								parts.table,
								'*ALL',
								fieldListLibrary
							);
							const data = convertFieldListToDspffdFormat(fieldListRows);
							if (data.length === 0) {
								logInfo(`[vscode-rpgle] FIELD_LIST returned 0 rows for ${cacheKey};`);
							}

							tableObjectCache.set(cacheKey, {
								fetchedAt: Date.now(),
								source: 'UDTF',
								data
							});
							pruneTableCache();

							logTableMetadataLifecycle(cacheKey, 'complete', { table, provider: 'UDTF', startedAt, completedAt: Date.now(), rows: data.length, source: 'UDTF' });
							return data;
						} else {
							const dspffdData = await fetchViaDspffd('DSPFFD');
							if (dspffdData) {
								return dspffdData;
							}
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
			tableObjectInFlight.delete(cacheKey);
		}
	});
}

export type TableCacheRefreshSource = 'startup' | 'manual';

export async function clearTableCache(client: LanguageClient, source: TableCacheRefreshSource = 'manual', forceRemoteRefresh = false): Promise<void> {
	// Parse/reparse cycles do not trigger this. File metadata refresh is intentionally
	// on startup or explicit user action only, not on every document edit.
	if (source !== 'startup' && source !== 'manual') {
		return;
	}

	clearLocalTableObjectCache();
	if (forceRemoteRefresh) {
		forceTableRefresh = true;
	}
	await client.sendRequest(`clearTableCache`);
}

export function getCache(client: LanguageClient, uri: Uri): Promise<any> {
	return client.sendRequest(`getCache`, uri.toString());
}