import { getFileRequest, getProjectFiles, watchedFilesChangeEvent } from '../../connection';
import { parser } from '..';
import Linter from '../../language/linter';
import Cache from '../../language/models/cache';
import { DidChangeWatchedFilesParams, FileChangeType } from 'vscode-languageserver';

export let isEnabled = false;
/**
 * Assumes client has workspace
 */
export async function initialise() {
	isEnabled = true;

	loadWorkspace();

	watchedFilesChangeEvent.push((params: DidChangeWatchedFilesParams) => {
		params.changes.forEach(fileEvent => {
			switch (fileEvent.type) {
				case FileChangeType.Created:
				case FileChangeType.Changed:
					loadFile(fileEvent.uri);
					break;
					
				default:
					parser.clearParsedCache(fileEvent.uri);
					break;
			}
		})
	});
}

async function loadWorkspace() {
	const uris = await getProjectFiles();

	if (uris) {
		const documents = await Promise.allSettled(uris?.map(uri => loadFile(uri)));
	}
}

async function loadFile(uri: string) {
	const content = await getFileRequest(uri);

	if (content) {
		parser.getDocs(uri, content).then(cache => {
			// Linter / reference collector only works on free-format.
			if (cache) {
				if (content.length >= 6 && content.substring(0, 6).toUpperCase() === `**FREE`) {
					Linter.getErrors({
						uri,
						content,
					}, {
						CollectReferences: true
					}, cache);
				}
			}
		});
	}
}