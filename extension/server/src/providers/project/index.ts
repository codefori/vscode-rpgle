import * as fs from "fs/promises";

import { connection, getWorkspaceFolder, PossibleInclude, watchedFilesChangeEvent } from '../../connection';
import { documents, parser } from '..';
import Linter from '../../../../../language/linter';
import { DidChangeWatchedFilesParams, FileChangeType } from 'vscode-languageserver';
import { URI } from 'vscode-uri';

import { glob } from "glob";
import * as path from "path";
import { TextDocument } from 'vscode-languageserver-textdocument';
const projectFilesGlob = `**/*.{rpgle,sqlrpgle,rpgleinc,rpgleh}`;

interface iProject {
	big?: boolean;
	includePath?: string[]
}

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

	connection.onRequest(`getCache`, (uri: string) => {
		return parser.getParsedCache(uri);
	});
}

async function loadWorkspace() {
	const progress = await connection.window.createWorkDoneProgress();

	const workspaces = await connection.workspace.getWorkspaceFolders();
	let handleBigProjects = false;
	progress.begin(`RPGLE`, undefined, `Loading workspaces`);

	if (workspaces) {
		let uris: string[] = [];

		for (const workspaceUri of workspaces) {

			const folderPath = URI.parse(workspaceUri.uri).fsPath;

			progress.report(`Starting search of ${workspaceUri.name}`);
			console.log(`Starting search of: ${folderPath}`);
			const files = glob.sync(projectFilesGlob, {
				cwd: folderPath,
				absolute: true,
				nocase: true,
			});

			progress.report(`Found RPGLE files: ${files.length}`);
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

				const iproj = await updateIProj(iprojUri);

				if (iproj.big) {
					handleBigProjects = true;
				}
			}
		};

		if (handleBigProjects) {
			progress.report(`Big mode detected!`);
			console.log(`Big mode detected!`);
		}

		if (uris.length < 1000 || handleBigProjects) {

			await Promise.allSettled(uris.map((uri, i) => {
				progress.report(`Loading ${i}/${uris.length}`);
				return loadLocalFile(uri);
			}));

		} else {
			progress.report(`Disabling project mode for large project.`);
			console.log(`Disabling project mode for large project.`);
			isEnabled = false;
		}
	}

	progress.done();
}

async function updateIProj(uri: string): Promise<iProject> {
	const workspace = await getWorkspaceFolder(uri);
	if (workspace) {
		const document = await getTextDoc(uri);
		const content = document?.getText();

		if (content) {
			try {
				const asJson = JSON.parse(content) as iProject;
				if (asJson.includePath && Array.isArray(asJson.includePath)) {
					const includeArray: any[] = asJson.includePath;

					const invalid = includeArray.some(v => typeof v !== `string`);

					if (!invalid) {
						includePath[workspace.uri] = asJson.includePath;
					} else {
						console.log(`${uri} -> 'includePath' is not a valid string array.`);
					}
				}

				return asJson;

			} catch (e) {
				console.log(`Unable to parse JSON in ${uri}.`);
			}
		}
	}

	return {};
}

async function loadLocalFile(uri: string) {
	const document = await getTextDoc(uri);

	if (document) {
		const content = document?.getText();
		const cache = await parser.getDocs(uri, content, {withIncludes: true, butIgnoreMembers: true});
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
