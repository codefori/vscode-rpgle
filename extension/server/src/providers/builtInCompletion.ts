import { CompletionItem, CompletionItemKind, InsertTextFormat, Position, Range, TextEdit } from 'vscode-languageserver';
import { getBuiltIns } from './apis/bif';

export const builtInFunctionCompletionItems: CompletionItem[] = getBuiltIns().map(builtIn => {
	const item = CompletionItem.create(builtIn.name);
	item.filterText = builtIn.name.substring(1);
	item.kind = CompletionItemKind.Function;
	item.detail = builtIn.returnType || `void`;
	item.documentation = `Built-in function`;
	item.insertText = builtIn.name + `(\${1})`;
	item.insertTextFormat = InsertTextFormat.Snippet;
	item.command = {command: `editor.action.triggerParameterHints`, title: `Trigger Parameter Hints`};
	item.sortText = item.filterText;
	return item;
});

/**
 * True when `index` in `line` is inside a string literal (e.g. `like 'A%te`)
 * or after a `//` comment.
 */
export function isInsideStringOrComment(line: string, index: number): boolean {
	let inString = false;
	for (let i = 0; i < index; i++) {
		if (line[i] === `'`) {
			inString = !inString;
		} else if (!inString && line.startsWith(`//`, i)) {
			return true;
		}
	}
	return inString;
}

/**
 * Built-in function completion items for the cursor position.
 *
 * `%` is not part of the word pattern, so when it has already been typed
 * the items must replace it explicitly or it ends up doubled (`%%trim`).
 */
export function getBuiltInCompletionItems(line: string, position: Position): CompletionItem[] {
	const beforeCursor = line.substring(0, position.character);
	const typedBuiltIn = beforeCursor.match(/%\w*$/);

	if (!typedBuiltIn) {
		return builtInFunctionCompletionItems;
	}

	// A `%` inside a string or comment is not the start of a built-in function
	if (isInsideStringOrComment(beforeCursor, typedBuiltIn.index!)) {
		return [];
	}

	const replaceRange = Range.create(position.line, typedBuiltIn.index!, position.line, position.character);
	return builtInFunctionCompletionItems.map(item => ({
		...item,
		filterText: item.label,
		textEdit: TextEdit.replace(replaceRange, item.insertText!)
	}));
}
