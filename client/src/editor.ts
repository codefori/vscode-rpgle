import { ExtensionContext, Range, SnippetString, TextDocument, commands, window } from "vscode";

export function initialise(context: ExtensionContext) {
	context.subscriptions.push(
		commands.registerCommand("vscode-rpgle.surroundWithMonitor", surroundWithMonitor)
	);
}

function surroundWithMonitor() {
	const editor = window.activeTextEditor;
	if (editor) {
		const document = editor.document;
		let line = editor.selection.start.line;
		if (isFreeRPGLE(document, line)) {
			// First let's find out if this line starts elsewhere
			while ((line - 1) >= 0 && !document.lineAt(line).text.trim().endsWith(`;`) && line-- > 0);
			const startLine = line;

			// Now get all the relevant lines
			const lines: string[] = [];
			let initialIdentation = document.lineAt(line).firstNonWhitespaceCharacterIndex;
			while (line < document.lineCount) {
				const currentLine = document.lineAt(line++);
				const text = currentLine.text;
				//Keep identation
				const indent = currentLine.firstNonWhitespaceCharacterIndex - initialIdentation;
				lines.push(`${"".padEnd(2 + (indent > 0 ? indent : 0))}${text.trim()}`);
				const maybeComment = text.lastIndexOf("//");
				if (line > editor.selection.end.line && (maybeComment > -1 ? text.substring(maybeComment + 1) : text).endsWith(';')) {
					break;
				}
			}

			editor.insertSnippet(new SnippetString([
				"",
				"monitor;",
				...lines,
				"on-excp '$1';",
				"on-error$2;",
				"endmon;",
				""
			].join("\n")), new Range(startLine, document.lineAt(startLine).firstNonWhitespaceCharacterIndex, line-1, 9999));
		}
		else{
			window.showWarningMessage("Selection is not valid Free RPGLE code");
		}
	}
}

function isFreeRPGLE(document: TextDocument, currentLine: number) {
	if (document.lineAt(0).text.substring(0, 6).toLowerCase() === `**free`) {
		return true;
	}
	else {
		for (let i = currentLine; i < document.lineCount; i++) {
			const line = document.lineAt(i).text;
			if (line.length > 10 && line.substring(6, 11).toLowerCase() === '/free') {
				return false;
			}
			else if (line.length > 14 && line.substring(6, 15).toLowerCase() === '/end-free') {
				return true;
			}
		}
	}
}
