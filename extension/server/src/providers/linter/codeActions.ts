import { CodeAction, CodeActionKind, Range, TextEdit } from 'vscode-languageserver';
import { getActions, refreshLinterDiagnostics } from '.';
import { TextDocument } from 'vscode-languageserver-textdocument';
import Cache from '../../../../../language/models/cache';
import { getIndentationDiagnostic, getIndentationRange } from './indentation';

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
    const fixErrors = detail.errors.filter(error => overlapsSelection(
      document.positionAt(error.offset.start).line,
      document.positionAt(Math.max(error.offset.start, error.offset.end - 1)).line
    ));
    const actions = fixErrors.length > 0 ? getActions(document, fixErrors) : [];

    // Once a closing statement takes the block depth below zero, later targets
    // can look valid but still be too shallow. Only fix lines before that point.
    const firstInvalidLine = detail.indentErrors.find(error => error.expectedIndent < 0)?.line;
    const indentErrors = detail.indentErrors.filter(error =>
      error.expectedIndent >= 0 && overlapsSelection(error.line)
      && (firstInvalidLine === undefined || error.line < firstInvalidLine)
    );
    const indentFixes = indentErrors
      .map(error => TextEdit.replace(
        getIndentationRange(document, error.line),
        ` `.repeat(error.expectedIndent)
      ));

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
