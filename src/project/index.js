
const vscode = require(`vscode`);
const { initialise } = require(`./internals`);
const ReferencesLensProvider = require(`./ReferencesLensProvider`);

/**
 * @param {vscode.ExtensionContext} context
 */
exports.startup = (context) => {
  const hasWorkspace = vscode.workspace.workspaceFolders.length > 0;

  if (hasWorkspace) {
    initialise();

    context.subscriptions.push(
      vscode.languages.registerCodeLensProvider(
        {
          language: `rpgle`,
        },
        new ReferencesLensProvider()
      )
    )
  }
}