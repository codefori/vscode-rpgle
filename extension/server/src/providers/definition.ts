import { DefinitionParams, Location, Definition, Range } from 'vscode-languageserver';
import { documents, getWordRangeAtPosition, parser } from '.';
import Parser from '../../../../language/parser';
import Cache from '../../../../language/models/cache';
import Declaration from '../../../../language/models/declaration';

export default async function definitionProvider(handler: DefinitionParams): Promise<Definition|null> {
	const currentUri = handler.textDocument.uri;
	const lineNumber = handler.position.line;
	const document = documents.get(currentUri);

	if (document) {
		const doc = await parser.getDocs(currentUri, document.getText());
		if (doc) {
			const editingLine = document.getText(Range.create(lineNumber, 0, lineNumber, 200));
			const possibleInclude = Parser.getIncludeFromDirective(editingLine);

			if (possibleInclude && parser.includeFileFetch) {
				const include = await parser.includeFileFetch(currentUri, possibleInclude);
				if (include.found && include.uri) {
					return Location.create(include.uri, Range.create(0, 0, 0, 0));
				}

			} else {
				let def: Declaration|undefined;

				// First, we try and get the reference by offset
				def = Cache.referenceByOffset(currentUri, doc, document.offsetAt(handler.position));

				if (def) {
					return Location.create(
						def.position.path,
						Range.create(
							def.position.range.line,
							0,
							def.position.range.line,
							0
						)
					);
				}

				// If we can't find the def by offset, we do a basic word lookup

				const word = getWordRangeAtPosition(document, handler.position);
				if (word) {
					def = doc.findDefinition(lineNumber, word);

					if (def) {
						return Location.create(
							def.position.path,
							Range.create(
								def.position.range.line,
								0,
								def.position.range.line,
								0
							)
						);
					}
				}
			}
		}
	}

	return null;
}