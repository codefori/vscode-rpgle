import { Definition, ImplementationParams, Location, Range } from 'vscode-languageserver';
import { documents, parser, getWordRangeAtPosition } from '.';
import { getSymbolFiles, parseBnddir } from '../bindingDirectory';
import { BindingDirectory, symbolLookup, validateUri } from '../connection';

import * as Project from './project';

export default async function implementationProvider(params: ImplementationParams): Promise<Definition | undefined> {
	const currentPath = params.textDocument.uri;
	const document = documents.get(currentPath);

	if (document) {
		const word = getWordRangeAtPosition(document, params.position);
		if (word) {
			const upperName = word.toUpperCase();

			if (Project.isEnabled) {
				// If project mode is enabled, then we start by looking through the local cache
				const parsedFiles = Object.keys(parser.parsedCache);

				for (const uri of parsedFiles) {
					const cache = parser.getParsedCache(uri);
					for (const proc of cache.procedures) {
						const keyword = proc.keyword[`EXPORT`];
						if (keyword) {
							if (proc.name.toUpperCase() === upperName) {
								return Location.create(
									proc.position.path,
									Range.create(
										proc.position.line,
										0,
										proc.position.line,
										0
									)
								);
							}
						}
					}
				};
			}

			// Then, we fall back to the server to see if we can find a reference or something?
			const cache = parser.getParsedCache(currentPath);
			if (cache) {
				const bnddir: string | undefined = cache.keyword[`BNDDIR`];
				if (bnddir) {
					const uris = await getSymbolFiles(bnddir, word);
					if (uris) {
						const validUris = await Promise.allSettled(uris.map(uri => validateUri(uri)));
						// By this time, if they were valid, they are part of the cache.

						for (const possibleUri of validUris) {
							if (possibleUri.status === `fulfilled` && possibleUri.value) {
								const cache = parser.getParsedCache(possibleUri.value);
								if (cache) {
									const proc = cache.find(word);
									return Location.create(
										proc.position.path,
										Range.create(
											proc.position.line,
											0,
											proc.position.line,
											0
										)
									);
								}
							}
						}
					}
				}
			}
		}
	}

	return;
}