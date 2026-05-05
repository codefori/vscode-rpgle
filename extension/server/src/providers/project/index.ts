import * as fs from "fs/promises";

import { connection, getWorkspaceFolder, PossibleInclude, watchedFilesChangeEvent } from '../../connection';
import { documents, parser } from '..';
import Linter from '../../../../../language/linter';
import { DidChangeWatchedFilesParams, FileChangeType } from 'vscode-languageserver';
import { URI } from 'vscode-uri';

import { glob } from "glob";
import * as path from "path";
import { TextDocument } from 'vscode-languageserver-textdocument';
const projectFilesGlob = `**/*.{rpgle,sqlrpgle,rpgleinc}`;
const defaultProjectFileLimit = 1000;
const projectFileLimitSetting = `projectFileLimit`;

export let includePath: {[workspaceUri: string]: string[]} = {};

export let isEnabled = false;
/**
 * Assumes client has workspace
 */
export async function initialise() {
	isEnabled = true;

	loadWorkspace();

	watchedFilesChangeEvent.push((params: DidChangeWatchedFilesParams) => {
		params.changes.forEach(fileEvent => {
			const pathData = path.parse(fileEvent.uri);
			const ext = pathData.ext.toLowerCase();

			switch (fileEvent.type) {
				case FileChangeType.Created:
				case FileChangeType.Changed:
					switch (ext) {
						case `.rpgleinc`:
						case `.rpgleh`:
							loadLocalFile(fileEvent.uri);

							currentIncludes = [];
							break;
						case `.json`:
							if (pathData.base === `iproj.json`) {
								updateIProj(fileEvent.uri);
							}
							break;
					}
					break;

				default:
					parser.clearParsedCache(fileEvent.uri);
					break;
			}
		})
	});
}

async function loadWorkspace() {
	const workspaces = await connection.workspace.getWorkspaceFolders();
	const projectFileLimit = await getProjectFileLimit();

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
			}).toString()));

			const iprojFiles = glob.sync(`**/iproj.json`, {
				cwd: folderPath,
				absolute: true,
				nocase: true,
			});

			if (iprojFiles.length > 0) {
				const base = iprojFiles[0];
				const iprojUri = URI.from({
					scheme: `file`,
					path: base
				}).toString();

				updateIProj(iprojUri);
			}
		}));

		if (uris.length < projectFileLimit) {
			await Promise.all(uris.map(uri => loadLocalFile(uri)));
		} else {
			console.log(`Disabling project mode for large project. Found ${uris.length} files, limit is ${projectFileLimit}.`);
			isEnabled = false;
		}
	}
}

async function getProjectFileLimit() {
	try {
		const settings = await connection.workspace.getConfiguration(`vscode-rpgle`);
		const configuredLimit = settings[projectFileLimitSetting];

		if (Number.isInteger(configuredLimit) && configuredLimit > 0) {
			return configuredLimit;
		}
	} catch (e) {}

	return defaultProjectFileLimit;
}

async function updateIProj(uri: string) {
	const workspace = await getWorkspaceFolder(uri);
	if (workspace) {
		const document = await getTextDoc(uri);
		const content = document?.getText();

		if (content) {
			try {
				const asJson = JSON.parse(content);
				if (asJson.includePath && Array.isArray(asJson.includePath)) {
					const includeArray: any[] = asJson.includePath;

					const invalid = includeArray.some(v => typeof v !== `string`);

					if (!invalid) {
						includePath[workspace.uri] = asJson.includePath;
					} else {
						console.log(`${uri} -> 'includePath' is not a valid string array.`);
					}
				}

			} catch (e) {
				console.log(`Unable to parse JSON in ${uri}.`);
			}
		}
	}
}

async function loadLocalFile(uri: string) {
	const document = await getTextDoc(uri);

	if (document) {
		const content = document?.getText();
		await parser.getDocs(uri, content);
	}
}

export async function getTextDoc(uri: string): Promise<TextDocument | undefined> {
	let document = documents.get(uri);

	if (document) {
		return document;
	}

	try {
		const content = await fs.readFile(URI.parse(uri).fsPath, { encoding: `utf-8` });
		return TextDocument.create(uri, `rpgle`, 1, content);
	} catch (e) {}

	return;
}

let currentIncludes: PossibleInclude[] = [];

export async function getIncludes(baseUri: string) {
	const workspace = await getWorkspaceFolder(baseUri);
	if (workspace) {
		const workspacePath = URI.parse(workspace?.uri).path;

		if (!currentIncludes || currentIncludes && currentIncludes.length === 0) {
			currentIncludes = glob.sync(`**/*.{rpgleinc,rpgleh}`, {
				cwd: workspacePath,
				nocase: true,
				absolute: true
			}).map(truePath => ({
				uri: URI.file(truePath).toString(),
				relative: path.relative(workspacePath, truePath)
			}))
		}
	}

	return currentIncludes;
}
