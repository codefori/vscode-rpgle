import { getFileRequest, getProjectFiles, watchedFilesChangeEvent } from '../../connection';
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

export function findAllReferences(def: Declaration): Location[] {
	let locations: Location[] = [];

	if (isEnabled) {
		const baseUri = def.position.path;

		const parsedCache = Object.keys(parser.parsedCache);

		parsedCache.forEach(uri => {
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

	return locations;
}