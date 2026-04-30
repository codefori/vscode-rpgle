import { ConfigurationTarget, workspace } from 'vscode';

export function get<T>(prop: string) {
	const globalData = workspace.getConfiguration(`vscode-rpgle`);
	return globalData.get<T>(prop);
}

export const RULER_ENABLED_BY_DEFAULT = `rulerEnabledByDefault`;
export const projectFilesGlob = `**/*.{rpgle,RPGLE,sqlrpgle,SQLRPGLE,rpgleinc,RPGLEINC}`;

export const CACHE_FILE_TTL_SECONDS = `cache.fileTTLSeconds`;
export const CACHE_FILE_MAX_ENTRIES = `cache.fileMaxEntries`;

export interface CacheSettings {
	fileTTLSeconds: number;
	fileMaxEntries: number;
}

export function getCacheSettings(): CacheSettings {
	const fileTTLSeconds = get<number>(CACHE_FILE_TTL_SECONDS) ?? 300;
	const fileMaxEntries = get<number>(CACHE_FILE_MAX_ENTRIES) ?? 500;
	return { fileTTLSeconds, fileMaxEntries };
}