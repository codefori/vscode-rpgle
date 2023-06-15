
import { Location, Range } from 'vscode-languageserver';
import Declaration from '../../../../../language/models/declaration';
import { calculateOffset } from '../linter';
import { documents, parser } from '..';
import { getTextDoc, isEnabled } from '.';

export async function findAllLocalReferences(def: Declaration): Promise<Location[]> {
	let locations: Location[] = [];

	if (isEnabled) {
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

			for (const uri of parsedFiles) {
				const document = await getTextDoc(uri);
				if (document) {
					const cache = parser.getParsedCache(uri);

					// It's only a valid reference if it is imported somewhere else
					const foundInclude = cache.includes.find(include => include.toPath === baseUri)
					if (foundInclude) {
						const possibleDef = cache.find(def.name);

						// Okay, we found something with a similar name in another file...
						if (possibleDef) {
							if (possibleDef.position.path === def.position.path) {
								if (document.getText(Range.create(0, 0, 0, 6)).toUpperCase() !== `**FREE` || possibleDef.references.length === 0) {
									locations.push(
										// First, we push the copybook where it is brought in.
										// We do this because we don't have references for non-**free
										Location.create(uri, Range.create(foundInclude.line, 0, foundInclude.line, 0)),
									);
								} else {
									// But since it's **free, and we probably have referneces...
									locations.push(
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