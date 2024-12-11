
import { Location, Range } from 'vscode-languageserver';
import Declaration from '../../../../../language/models/declaration';
import { calculateOffset } from '../linter';
import { documents, parser } from '..';
import { getTextDoc, isEnabled } from '.';

export async function findAllProjectReferences(def: Declaration): Promise<Location[]> {
	let locations: Location[] = [];

	if (isEnabled) {
		const parsedFiles = Object.keys(parser.parsedCache);
		
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

		}
	}

	return locations;
}

function trimQuotes(input: string) {
	if (input[0] === `'`) input = input.substring(1);
	if (input[input.length - 1] === `'`) input = input.substring(0, input.length - 1);
	return input;
}