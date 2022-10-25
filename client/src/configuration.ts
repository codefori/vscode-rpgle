import { workspace } from 'vscode';

export function get(prop: string) {
	const globalData = workspace.getConfiguration(`vscode-rpgle`);
	return globalData.get(prop);
}