import { ExtensionContext, Range, SnippetString, TextDocument, commands, window } from "vscode";

export function initialise(context: ExtensionContext) {
	context.subscriptions.push(
		commands.registerCommand("vscode-rpgle.surroundWithMonitor", () => surroundWithSnippet(lines => [
			"",
			"monitor;",
			...lines,
			"on-excp '$1';",
			"on-error$2;",
			"endmon;",
			""
		]))
	);
}

function surroundWithSnippet(snippetProvider: (lines: string[]) => string[]) {
	const editor = window.activeTextEditor;
	if (editor) {
		const document = editor.document;
		const isFullyFree = isFullyFreeRPGLE(document);
		let line = editor.selection.start.line;
		if (isFree(document.lineAt(line).text, isFullyFree)) {
			// First let's find out if this line starts elsewhere
			if (!isFreeLineEnd(document.lineAt(line).text, isFullyFree)) {
				while ((line - 1) >= 0 && !isFreeLineEnd(document.lineAt(line - 1).text, isFullyFree) && line-- > 0);
			}
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
				if (line > editor.selection.end.line && ((maybeComment > -1 ? text.substring(maybeComment + 1) : text).endsWith(';') || isFreeLineEnd(text, isFullyFree))) {
					break;
				}
			}

			editor.insertSnippet(new SnippetString(snippetProvider(lines).join("\n")),
				new Range(startLine, document.lineAt(startLine).firstNonWhitespaceCharacterIndex, line - 1, 9999));
		} else {
			window.showWarningMessage("Selection is not valid Free RPGLE code");
		}
	}
}

function isFullyFreeRPGLE(document: TextDocument) {
	if (document.lineAt(0).text.substring(0, 6).toLowerCase() === `**free`) {
		return true;
	}
	else {
		return false;
	}
}

function isFreeLineEnd(line: string, free: boolean) {
	const endPos = line.lastIndexOf(';');
	const commentPos = line.lastIndexOf('//');
	return !isFree(line, free) || line.trim().endsWith(`;`) || (endPos > -1 && commentPos > endPos);
}

function isFree(line: string, free: boolean) {
	return free || !(['H', 'F', 'D', 'I', 'C', 'O', 'P'].includes(line.charAt(5).toUpperCase()) || ['/', '*'].includes(line.charAt(6)));
}