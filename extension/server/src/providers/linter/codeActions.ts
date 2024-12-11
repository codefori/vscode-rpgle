import { CodeAction, CodeActionKind, CodeActionParams, Range } from 'vscode-languageserver';
import { getActions, getExtractProcedureAction, getSubroutineActions, refreshLinterDiagnostics } from '.';
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
				const subroutineOption = getSubroutineActions(document, docs, range);
				if (subroutineOption) {
					return [subroutineOption];
				}

				const extractOption = getExtractProcedureAction(document, docs, range);
				if (extractOption) {
					return [extractOption];
				}

				const detail = await refreshLinterDiagnostics(document, docs, false);
				if (detail) {
					const fixErrors = detail.errors.filter(error => 
						range.start.line >= document.positionAt(error.offset.start!).line &&
						range.end.line <= document.positionAt(error.offset.end!).line
					);

					if (fixErrors.length > 0) {
						return getActions(document, fixErrors);
					}
				}
			}
		}
	}

	return;
}