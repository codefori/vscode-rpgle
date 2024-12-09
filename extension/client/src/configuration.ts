import { ConfigurationTarget, workspace } from 'vscode';

export function get<T>(prop: string) {
	const globalData = workspace.getConfiguration(`vscode-rpgle`);
	return globalData.get<T>(prop);
}

export const RULER_ENABLED_BY_DEFAULT = `rulerEnabledByDefault`;
export const projectFilesGlob = `**/*.{rpgle,RPGLE,sqlrpgle,SQLRPGLE,rpgleinc,RPGLEINC}`;