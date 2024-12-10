import { Definition, ImplementationParams, Location, Range } from 'vscode-languageserver';
import { documents, parser, getWordRangeAtPosition } from '.';
import { BindingDirectory, symbolLookup, validateUri } from '../connection';

import * as Project from './project';

export default async function implementationProvider(params: ImplementationParams): Promise<Definition | undefined> {
	const currentPath = params.textDocument.uri;
	const document = documents.get(currentPath);

	// We only handle local implementations here.
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
		}
	}

	return;
}

function trimQuotes(input: string) {
	if (input[0] === `'`) input = input.substring(1);
	if (input[input.length - 1] === `'`) input = input.substring(0, input.length - 1);
	return input;
}