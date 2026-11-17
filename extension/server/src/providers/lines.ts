import { Range } from 'vscode-languageserver';
import type { TextDocument } from 'vscode-languageserver-textdocument';
import { getLeadingWhitespace } from '../../../../language/ile/indentation';

export function lineAt(document: TextDocument, line: number): string {
	return document.getText(Range.create(line, 0, line + 1, 0)).replace(/\r?\n$/, ``);
}

export function determineIndent(line: string): number {
	return getLeadingWhitespace(line).length;
}
