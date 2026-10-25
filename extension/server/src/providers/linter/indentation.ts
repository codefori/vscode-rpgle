import { Diagnostic, DiagnosticSeverity, Range } from 'vscode-languageserver';
import { TextDocument } from 'vscode-languageserver-textdocument';
import type { IndentError } from '../../../../../language/ile/linter';

export function getIndentationRange(document: TextDocument, line: number): Range {
  const text = document.getText(Range.create(line, 0, line + 1, 0));
  const whitespace = text.match(/^[ \t]*/)![0];
  return Range.create(line, 0, line, whitespace.length);
}

export function getIndentationDiagnostic(document: TextDocument, error: IndentError): Diagnostic {
  return Diagnostic.create(
    getIndentationRange(document, error.line),
    `Incorrect indentation. Expected ${error.expectedIndent}, got ${error.currentIndent}`,
    DiagnosticSeverity.Warning
  );
}
