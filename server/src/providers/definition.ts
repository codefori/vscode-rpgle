import { DefinitionParams, Location, Definition, Range } from 'vscode-languageserver';
import { documents, getWordRangeAtPosition, parser } from '.';

export default async function definitionProvider(handler: DefinitionParams): Promise<Definition|null> {
	const currentPath = handler.textDocument.uri;
	const currentLine = handler.position.line;
	const document = documents.get(currentPath);

	if (document) {
		const doc = await parser.getDocs(currentPath, document.getText());
		if (doc) {
			const word = getWordRangeAtPosition(document, handler.position);
			const def = doc.findDefinition(currentLine, word);

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

	return null;
}