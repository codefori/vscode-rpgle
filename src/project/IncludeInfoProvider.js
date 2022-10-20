
const vscode = require(`vscode`);
const { findIncludeReferences } = require(`./internals`);

const unsupportedSchemes = [`member`, `streamfile`]

/**
 * @param {vscode.ExtensionContext} context 
 */
exports.initialiseIncludeInfoFinder = (context) => {
  context.subscriptions.push(
    vscode.commands.registerCommand(`vscode-rpgle.project.showIncludeReferences`, () => {
      const activeEditor = vscode.window.activeTextEditor;
      if (activeEditor) {
        const document = activeEditor.document;
        const uri = document.uri;

        if (document.languageId === `rpgle` && !unsupportedSchemes.includes(uri.path)) {
          const locations = findIncludeReferences(uri);

          vscode.commands.executeCommand(
            `editor.action.showReferences`, 
            document.uri, 
            new vscode.Position(0, 0),
            locations 
          );
        }
      }
    })
  )
}