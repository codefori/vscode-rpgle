import * as vscode from 'vscode';
import * as rpgle from './rpgtools-comment-helpers';

export function registerAddDeveloperTagCommand(context: vscode.ExtensionContext) {
  const disposable = vscode.commands.registerCommand('vscode-rpgle.addDeveloperTag', async () => {
    const config = vscode.workspace.getConfiguration('vscode-rpgle');
    if (!config.get<boolean>('developerTagEnabled', true)) {
      vscode.window.showInformationMessage('Developer Tag command is disabled. Enable it in Settings (vscode-rpgle.developerTagEnabled).');
      return;
    }

    const editor = vscode.window.activeTextEditor;
    if (!editor) {
      vscode.window.showErrorMessage('No active editor window found.');
      return;
    }

    const rawTag = await vscode.window.showInputBox({
      prompt: 'Enter Developer Tag (e.g., PW01)',
      placeHolder: 'Developer Tag',
      validateInput: (value) => {
        if (!value || !value.trim()) {
          return 'Tag cannot be empty.';
        }
        return null;
      }
    });

    if (!rawTag) return; // Cancelled
    const tag = rawTag.trim();

    const doc = editor.document;

    try {
      await editor.edit((editBuilder) => {
        for (const sel of editor.selections) {
          const start = Math.min(sel.start.line, sel.end.line);
          let end = Math.max(sel.start.line, sel.end.line);

          // Full-line selections end at col 0 of the next line; exclude that extra line.
          if (start < end && sel.end.character === 0) {
            end--;
          }

          const startLinePos = new vscode.Position(start, 0);
          const endLine = doc.lineAt(end);

          // Insert the closing tag after the last selected line
          editBuilder.insert(
            new vscode.Position(end, endLine.text.length),
            doc.eol === vscode.EndOfLine.CRLF ? `\r\n// ${tag} - End` : `\n// ${tag} - End`
          );

          // Insert the opening tag before the first selected line
          editBuilder.insert(
            startLinePos,
            `// ${tag} - Start` + (doc.eol === vscode.EndOfLine.CRLF ? '\r\n' : '\n')
          );
        }
      });
    } catch (e) {
      rpgle.log('Error adding developer tag: ' + (e as Error).message);
      vscode.window.showErrorMessage('An error occurred: ' + (e as Error).message);
    }
  });

  context.subscriptions.push(disposable);
}
