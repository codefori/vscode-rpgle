import * as vscode from 'vscode';
import * as rpgle from './rpgtools-comment-helpers';

export function registerAddDeveloperTagCommand(context: vscode.ExtensionContext) {
  const disposable = vscode.commands.registerCommand('vscode-rpgle.addDeveloperTag', async () => {
    const editor = vscode.window.activeTextEditor;
    if (!editor) {
      vscode.window.showErrorMessage('No active editor window found.');
      return;
    }

    const tag = await vscode.window.showInputBox({
      prompt: 'Enter Developer Tag (e.g., PW01)',
      placeHolder: 'Developer Tag'
    });

    if (!tag) {
      return; // Cancelled
    }

    const doc = editor.document;
    const edits: vscode.TextEdit[] = [];
    const allLines = doc.getText().split(rpgle.getEOL());

    // Determine the middle of the tag for the '|' character
    const middleIndex = Math.floor(tag.length / 2);
    let middleStr = '';
    for (let i = 0; i < tag.length; i++) {
      if (i === middleIndex) {
        middleStr += '|';
      } else {
        middleStr += ' ';
      }
    }

    for (const sel of editor.selections) {
      const start = Math.min(sel.start.line, sel.end.line);
      const end = Math.max(sel.start.line, sel.end.line);

      // Find the longest line in the selection to align the comments
      let maxLength = 0;
      for (let i = start; i <= end; i++) {
        if (i < allLines.length) {
          const lineLength = allLines[i].replace(/\s+$/, '').length;
          if (lineLength > maxLength) {
            maxLength = lineLength;
          }
        }
      }

      // Add a couple of spaces of padding after the longest line
      const targetColumn = maxLength + 2;

      for (let i = start; i <= end; i++) {
        if (i >= allLines.length) continue;

        const originalLine = allLines[i];
        const trimmedRight = originalLine.replace(/\s+$/, '');
        
        // Skip completely empty lines if desired, or just tag them. We'll tag them.
        
        const paddingSpaces = targetColumn - trimmedRight.length;
        const padding = ' '.repeat(Math.max(1, paddingSpaces)); // at least 1 space

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
      await vscode.workspace.applyEdit(edit);
    }
  });

  context.subscriptions.push(disposable);
}
