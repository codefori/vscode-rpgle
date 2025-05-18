import { Range } from 'vscode-languageserver';
import { getActions, refreshLinterDiagnostics } from '.';
import { TextDocument } from 'vscode-languageserver-textdocument';
import Cache from '../../../../../language/models/cache';

/**
 * Get the CodeActions for a given document and range.
 */
export async function getLinterCodeActions(docs: Cache, document: TextDocument, range: Range) {
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