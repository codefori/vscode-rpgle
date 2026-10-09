import * as vscode from 'vscode';
import * as rpgle from './rpgtools-comment-helpers';

export function registerAddDeveloperTagCommand(context: vscode.ExtensionContext) {
  const disposable = vscode.commands.registerCommand('vscode-rpgle.addDeveloperTag', async () => {
    const editor = vscode.window.activeTextEditor;
    if (!editor) {
      vscode.window.showErrorMessage('No active editor window found.');
      return;
    }

    const rawTag = await vscode.window.showInputBox({
      prompt: 'Enter Developer Tag (e.g., PW01)',
      placeHolder: 'Developer Tag'
    });

    if (!rawTag) return; // Cancelled
    const tag = rawTag.trim();
    if (!tag) return; // Empty tag

    const doc = editor.document;
    const edits: vscode.TextEdit[] = [];
    const editedLines = new Set<number>();

    // Determine the middle of the tag for the '|' character
    const middleIndex = Math.floor(tag.length / 2);
    const middleStr = ' '.repeat(middleIndex) + '|' + ' '.repeat(Math.max(0, tag.length - middleIndex - 1));

    try {
      for (const sel of editor.selections) {
        const start = Math.min(sel.start.line, sel.end.line);
        let end = Math.max(sel.start.line, sel.end.line);
        
        // Full-line selections end at col 0 of the next line, mistakenly including an unselected line.
        if (start < end && sel.end.character === 0) {
            end--;
        }

        // Find the longest line in the selection to align the comments
        let maxLength = 0;
        for (let i = start; i <= end; i++) {
          if (i < doc.lineCount) {
            const lineText = doc.lineAt(i).text;
            
            // Skip compile-time data
            if (lineText.startsWith('**CTDATA') || lineText.startsWith('** ')) {
                continue;
            }
            
            const lineLength = lineText.replace(/\s+$/, '').length;
            if (lineLength > maxLength) {
              maxLength = lineLength;
            }
          }
        }

        const targetColumn = maxLength + 2;

        for (let i = start; i <= end; i++) {
          if (i >= doc.lineCount) continue;
          if (editedLines.has(i)) continue;

          const originalLine = doc.lineAt(i).text;
          
          // Skip compile-time data
          if (originalLine.startsWith('**CTDATA') || originalLine.startsWith('** ')) {
              continue;
          }
          
          // Skip fixed-format spec lines
          const specType = rpgle.getSpecType(originalLine);
          if (specType && specType.trim() !== '') {
              continue;
          }
          
          editedLines.add(i);

          const trimmedRight = originalLine.replace(/\s+$/, '');
          const paddingSpaces = targetColumn - trimmedRight.length;
          // Math.max is removed since targetColumn = maxLength + 2, and trimmedRight is <= maxLength
          const padding = ' '.repeat(paddingSpaces); 

          let appendedTag = '';
          if (i === start || i === end) {
            appendedTag = `// ${tag}`;
          } else {
            appendedTag = `// ${middleStr}`;
          }

          const newLine = trimmedRight + padding + appendedTag;
          const range = doc.lineAt(i).range;
          edits.push(vscode.TextEdit.replace(range, newLine));
        }
      }

      if (edits.length > 0) {
        const edit = new vscode.WorkspaceEdit();
        edit.set(doc.uri, edits);
        const success = await vscode.workspace.applyEdit(edit);
        if (!success) {
            vscode.window.showErrorMessage('Failed to apply developer tags.');
        }
      }
    } catch (e) {
      rpgle.log('Error adding developer tag: ' + (e as Error).message);
      vscode.window.showErrorMessage('An error occurred: ' + (e as Error).message);
    }
  });

  context.subscriptions.push(disposable);
}
