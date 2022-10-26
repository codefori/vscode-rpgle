import { Location, Range, ReferenceParams } from 'vscode-languageserver';
import { documents, getWordRangeAtPosition, parser } from '.';
import Linter from '../language/linter';
import { calculateOffset } from './linter';

export async function referenceProvider(params: ReferenceParams): Promise<Location[]|undefined> {
	const uri = params.textDocument.uri;
	const position = params.position;
	const document = documents.get(uri);

	if (document) {
		const isFree = (document.getText(Range.create(0, 0, 0, 6)).toUpperCase() === `**FREE`);

		if (isFree) {
			let locations: Location[] = [];
			const word = getWordRangeAtPosition(document, position);

			if (word) {
				const doc = await parser.getDocs(uri, document.getText());

				if (doc) {
					Linter.getErrors(
						{
							uri,
							content: document.getText()
						},
						{
							CollectReferences: true
						},
						doc
					);

					const def = doc.findDefinition(position.line, word);

					if (def) {
						return def.references.map(ref => Location.create(
							def.position.path,
							calculateOffset(document, ref)
						));
					}
				}
			}
		}
	}

	return;
}