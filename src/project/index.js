
const vscode = require(`vscode`);
const {Parser} = require(`../parser`);
const { initialise, parseUris } = require(`./internals`);
const ReferencesLensProvider = require(`./ReferencesLensProvider`);

/**
 * @param {vscode.ExtensionContext} context
 */
exports.startup = (context) => {
  const hasWorkspace = vscode.workspace.workspaceFolders.length > 0;

  if (hasWorkspace) {
    initialise();

    const fsWatcher = vscode.workspace.createFileSystemWatcher(`**`);

    const reparse = (uri) => {
      const activeEditor = vscode.window.activeTextEditor;
      if (activeEditor && activeEditor.document.uri.path === uri.path) return;
      parseUris([uri]);
    }

    fsWatcher.onDidChange(reparse);
    fsWatcher.onDidCreate(reparse);
    fsWatcher.onDidDelete(uri => {
      Parser.clearParsedCache(uri.path);
    });

    context.subscriptions.push(
      fsWatcher,
      vscode.languages.registerCodeLensProvider(
        {
          language: `rpgle`,
        },
        new ReferencesLensProvider()
      )
    );
  }
}