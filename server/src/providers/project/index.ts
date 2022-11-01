import { connection, getFileRequest, getIncludesUris, getProjectFiles, PossibleInclude, watchedFilesChangeEvent } from '../../connection';
import { documents, parser } from '..';
import Linter from '../../language/linter';
import Cache from '../../language/models/cache';
import { DidChangeWatchedFilesParams, FileChangeType, Location, Range } from 'vscode-languageserver';
import Declaration from '../../language/models/declaration';
import { calculateOffset } from '../linter';

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
	const uris = await getProjectFiles();

	if (uris) {
		await Promise.allSettled(uris?.map(uri => loadFile(uri)));
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

let currentIncludes: PossibleInclude[]|undefined = [];

export async function getIncludes(basePath: string) {
	if (!currentIncludes || currentIncludes && currentIncludes.length === 0) currentIncludes = await getIncludesUris(basePath);

	return currentIncludes;
}

export function findAllReferences(def: Declaration): Location[] {
	let locations: Location[] = [];

	const parsedFiles = Object.keys(parser.parsedCache);

	const document = documents.get(def.position.path);

	if (document) {
		locations.push(
			...def.references.map(ref => Location.create(
				def.position.path,
				calculateOffset(document, ref)
			))
		);
	}

	if (isEnabled) {
		if (def.keyword[`EXPORT`]) {
			// If we are looking for references to an export function
			// scan entire project for `EXTPROC` definitions that point to this
			const upperName = def.name.toUpperCase();

			for (const keyPath of parsedFiles) {
				const document = documents.get(keyPath);

				if (document) {
					const cache = parser.getParsedCache(keyPath);

					cache.procedures.forEach(proc => {
						let addReference = false;
						const keyword = proc.keyword[`EXTPROC`];
						if (keyword) {
							if (keyword === true) {
								if (proc.name.toUpperCase() === upperName) {

									addReference = true;
								}
							} else
								if (trimQuotes(keyword).toUpperCase() === upperName) {
									addReference = true;
								}
						} else

							// Also turns out, any `DCL-PR` without any keywords is `EXTPROC` by default.
							if (!proc.keyword[`EXPORT`] && proc.name.toUpperCase() === upperName) {
								addReference = true;
							}

						if (addReference) {
							// Don't add duplicates
							if (!locations.some(loc => loc.uri === keyPath)) {
								locations.push(
									// First, we push the copybook where it is brought in.
									// We do this because we don't have references for non-**free
									Location.create(proc.position.path, Range.create(proc.position.line, 0, proc.position.line, 0)),

									// Then we push the references. Empty for non-**free
									...proc.references.map(ref => Location.create(
										keyPath,
										calculateOffset(document, ref)
									))
								);
							}
						}

					})
				}
			}

		} else {
			// Otherwise, we are looking for references to the current definition
			const baseUri = def.position.path;

			parsedFiles.forEach(uri => {
				const document = documents.get(uri);
				if (document) {
					const cache = parser.getParsedCache(uri);

					// It's only a valid reference if it is imported somewhere else
					const foundInclude = cache.includes.find(include => include.toPath === baseUri)
					if (foundInclude) {
						const possibleDef = cache.find(def.name);

						// Okay, we found something with a similar name in another file...
						if (possibleDef) {
							if (possibleDef.position.path === def.position.path) {
								locations.push(
									// First, we push the copybook where it is brought in.
									// We do this because we don't have references for non-**free
									Location.create(uri, Range.create(foundInclude.line, 0, foundInclude.line, 0)),

									// Then we push the references. Empty for non-**free
									...possibleDef.references.map(ref => Location.create(
										uri,
										calculateOffset(document, ref)
									))
								);
							}
						}
					}
				}
			})
		}
	}

	return locations;
}

function trimQuotes(input: string) {
	if (input[0] === `'`) input = input.substring(1);
	if (input[input.length - 1] === `'`) input = input.substring(0, input.length - 1);
	return input;
}