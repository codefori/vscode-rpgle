import { Diagnostic, DiagnosticSeverity, Range, TextEdit } from 'vscode-languageserver';
import { TextDocument } from 'vscode-languageserver-textdocument';
import type { IndentError } from '../../../../../language/ile/linter';
import { lineAt, determineIndent } from '../lines';
import { getIndentationMessage } from '../../../../../language/ile/indentation';

export function getIndentationRange(document: TextDocument, line: number): Range {
	return Range.create(line, 0, line, determineIndent(lineAt(document, line)));
}

export function getIndentationDiagnostic(document: TextDocument, error: IndentError): Diagnostic {
	return Diagnostic.create(
		getIndentationRange(document, error.line),
		getIndentationMessage(error),
		DiagnosticSeverity.Warning
	);
}

export function getIndentationEdits(document: TextDocument, errors: IndentError[]): TextEdit[] {
	return errors.map(error => TextEdit.replace(getIndentationRange(document, error.line), ` `.repeat(error.expectedIndent)));
}
