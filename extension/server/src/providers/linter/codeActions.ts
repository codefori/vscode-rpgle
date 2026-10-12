import { CodeAction, CodeActionKind, Range, TextEdit } from 'vscode-languageserver';
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
    const actions = fixErrors.length > 0 ? getActions(document, fixErrors) : [];

    // A selection ending at column zero does not include that final line.
    const lastLine = range.end.line > range.start.line && range.end.character === 0
      ? range.end.line - 1
      : range.end.line;
    const indentFixes = detail.indentErrors
      // Unmatched closing statements can produce negative indentation, which cannot be fixed with whitespace.
      .filter(error => error.expectedIndent >= 0 && error.line >= range.start.line && error.line <= lastLine)
      .map(error => TextEdit.replace(
        Range.create(error.line, 0, error.line, error.currentIndent),
        ` `.repeat(error.expectedIndent)
      ));

    if (indentFixes.length > 0) {
      const action = CodeAction.create(`Fix indentation`, CodeActionKind.QuickFix);
      action.edit = { changes: { [document.uri]: indentFixes } };
      actions.push(action);
    }

    if (actions.length > 0) {
      return actions;
    }
  }
}
