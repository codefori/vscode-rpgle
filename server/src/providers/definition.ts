import { DefinitionParams, Location, Definition, Range } from 'vscode-languageserver';
import { documents, getWordRangeAtPosition, parser } from '.';
import Parser from '../language/parser';

export default async function definitionProvider(handler: DefinitionParams): Promise<Definition|null> {
	const currentPath = handler.textDocument.uri;
	const lineNumber = handler.position.line;
	const document = documents.get(currentPath);

	if (document) {
		const doc = await parser.getDocs(currentPath, document.getText());
		if (doc) {
			const editingLine = document.getText(Range.create(lineNumber, 0, lineNumber, 200));
			const possibleInclude = Parser.getIncludeFromDirective(editingLine);

			if (possibleInclude) {
				const include = await parser.includeFileFetch(currentPath, possibleInclude);
				if (include.found && include.uri) {
					return Location.create(include.uri, Range.create(0, 0, 0, 0));
				}

			} else {
				const word = getWordRangeAtPosition(document, handler.position);
				if (word) {
					const def = doc.findDefinition(lineNumber, word);

					if (def) {
						return Location.create(
							def.position.path,
							Range.create(
								def.position.line,
								0,
								def.position.line,
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