import { CodeAction, CodeActionParams, Range } from 'vscode-languageserver';
import { getActions, refreshDiagnostics } from '.';
import { documents, parser } from '..';

export default async function codeActionsProvider(params: CodeActionParams): Promise<CodeAction[]|undefined> {
	const uri = params.textDocument.uri;
	const range = params.range;
	const document = documents.get(uri);

	if (document) {
		const isFree = (document.getText(Range.create(0, 0, 0, 6)).toUpperCase() === `**FREE`);
		if (isFree) {
			const docs = await parser.getDocs(document.uri);

			if (docs) {
				const detail = await refreshDiagnostics(document, docs, false);
				if (detail) {
					const fixErrors = detail.errors.filter(error => range.start.line === error.range.start.line );

					if (fixErrors.length > 0) {
						return getActions(document, fixErrors);
					}
				}
			}
		}
	}

	return;
}