import { CodeAction, CodeActionKind, Range } from 'vscode-languageserver';
import { getActions, refreshLinterDiagnostics } from '.';
import { TextDocument } from 'vscode-languageserver-textdocument';
import Cache from '../../../../../language/models/cache';
import { getIndentationDiagnostic, getIndentationEdits } from './indentation';
import { getFixableIndentationErrors } from '../../../../../language/ile/indentation';

/**
 * Get the CodeActions for a given document and range.
 */
export async function getLinterCodeActions(docs: Cache, document: TextDocument, range: Range) {
	const detail = await refreshLinterDiagnostics(document, docs, false);
	if (detail) {
		// A selection ending at column zero does not include that final line.
		const lastLine = range.end.line > range.start.line && range.end.character === 0
			? range.end.line - 1
			: range.end.line;
		const overlapsSelection = (first: number, last = first) => first <= lastLine && last >= range.start.line;
		// Keep individual lint actions focused on the diagnostic containing the
		// selection; a block selection should not list one action for every error.
		const fixErrors = detail.errors.filter(error =>
			range.start.line >= document.positionAt(error.offset.start).line
      && lastLine <= document.positionAt(Math.max(error.offset.start, error.offset.end - 1)).line
		);
		const actions = fixErrors.length > 0 ? getActions(document, fixErrors) : [];

		const indentErrors = getFixableIndentationErrors(detail).filter(error => overlapsSelection(error.line));
		const indentFixes = getIndentationEdits(document, indentErrors);

		if (indentFixes.length > 0) {
			const action = CodeAction.create(`Fix indentation`, CodeActionKind.QuickFix);
			action.isPreferred = true;
			action.diagnostics = indentErrors.map(error => getIndentationDiagnostic(document, error));
			action.edit = { changes: { [document.uri]: indentFixes } };
			actions.push(action);
		}

		if (actions.length > 0) {
			return actions;
		}
	}
}
