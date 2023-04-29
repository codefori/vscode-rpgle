import * as fs from "fs/promises";

import { connection, getIncludesUris, PossibleInclude, watchedFilesChangeEvent } from '../../connection';
import { parser } from '..';
import Linter from '../../language/linter';
import { DidChangeWatchedFilesParams, FileChangeType } from 'vscode-languageserver';
import { URI } from 'vscode-uri';

import { glob } from "glob";
const projectFilesGlob = `**/*.{rpgle,sqlrpgle,rpgleinc}`;

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
					loadLocalFile(fileEvent.uri);

					if (fileEvent.uri.toLowerCase().endsWith(`.rpgleinc`)) {
						currentIncludes = undefined;
					}
					break;

				default:
					parser.clearParsedCache(fileEvent.uri);
					break;
			}
		})
	});

	connection.onRequest(`getCache`, (uri: string) => {
		return parser.getParsedCache(uri);
	});
}

async function loadWorkspace() {
	const workspaces = await connection.workspace.getWorkspaceFolders();

	console.log(workspaces);

	if (workspaces) {
		let uris: string[] = [];

		workspaces.forEach((workspaceUri => {
			const folderPath = URI.parse(workspaceUri.uri).fsPath;

			console.log(`Starting search of: ${folderPath}`);
			const files = glob.sync(projectFilesGlob, {
				cwd: folderPath,
				absolute: true,
				nocase: true,
			});

			console.log(`Found RPGLE files: ${files.length}`);

			uris.push(...files.map(file => URI.from({
				scheme: `file`,
				path: file
			}).toString()))
		}));

		if (uris.length < 1000) {
			for (const uri of uris) {
				await loadLocalFile(uri);
			}
		} else {
			console.log(`Disabling project mode for large project.`);
			isEnabled = false;
		}
	}
}

async function loadLocalFile(uri: string) {
	const validPath = URI.parse(uri).fsPath;
	const content = await fs.readFile(validPath, { encoding: `utf-8` });

	const cache = await parser.getDocs(uri, content, {withIncludes: false});
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
}

let currentIncludes: PossibleInclude[] | undefined = [];

export async function getIncludes(basePath: string) {
	if (!currentIncludes || currentIncludes && currentIncludes.length === 0) currentIncludes = await getIncludesUris(basePath);

	return currentIncludes;
}
