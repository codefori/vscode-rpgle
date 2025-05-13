
import {
	TextDocuments,
	Position,
	Range,
	_Connection
} from 'vscode-languageserver/node';

import {
	TextDocument
} from 'vscode-languageserver-textdocument';
import Parser from '../../../../language/parser';

type Keywords = { [key: string]: string | boolean };

// Create a simple text document manager.
export const documents: TextDocuments<TextDocument> = new TextDocuments(TextDocument);

export function findFile(fileString: string, scheme = ``) {
	return documents.keys().find(fileUri => fileUri.includes(fileString) && fileUri.startsWith(`${scheme}:`));
}

export const parser = new Parser();

const wordMatch = /[\w\#\$@]/;

export function getWordRangeAtPosition(document: TextDocument, position: Position): string | undefined {
	const lines = document.getText().split(`\n`); // Safe to assume \n because \r is then at end of lines
	const line = Math.min(lines.length - 1, Math.max(0, position.line));
	const lineText = lines[line];
	const character = Math.min(lineText.length - 1, Math.max(0, position.character));

	let startChar = character;
	while (startChar > 0 && wordMatch.test(lineText.charAt(startChar - 1))) {
		startChar -= 1;
	}

	let endChar = character;
	while (endChar < lineText.length && wordMatch.test(lineText.charAt(endChar + 1))) {
		endChar += 1;
	}

	if (startChar === endChar)
		return undefined;
	else
		return document.getText(Range.create(line, Math.max(0, startChar), line, endChar + 1)).replace(/(\r\n|\n|\r)/gm, "");
}

const filteredKeywords = ['CONST', 'QUALIFIED', 'EXPORT'];

export function prettyKeywords(keywords: Keywords, filter: boolean = false): string {
	return Object.keys(keywords).map(key => {
		if ((!filter || !filteredKeywords.includes(key)) && keywords[key]) {
			if (typeof keywords[key] === `boolean`) {
				return key.toLowerCase();
			}

			return `${key.toLowerCase()}(${keywords[key]})`;
		} else {
			return undefined;
		}
	}).filter(k => k).join(` `);
}