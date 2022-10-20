
const vscode = require(`vscode`);
const {Parser} = require(`../parser`);

const Output = require(`../output`);
const Configuration = require(`../configuration`);

const { initialiseIncludeInfoFinder } = require(`./IncludeInfoProvider`);
const { scanWorkspace: initialise, parseUris } = require(`./internals`);
const ReferencesLensProvider = require(`./ReferencesLensProvider`);

const projectFileName = `.ibmiproject`;

exports.started = false;

exports.canEnable = async () => {
  const hasWorkspace = vscode.workspace.workspaceFolders.length > 0;
  const [projectFile] = await vscode.workspace.findFiles(`**/.vscode/${projectFileName}`, null, 1);
  Output.write(`Project mode: profile file: ${projectFile ? projectFile.path : `not found. (.vscode/${projectFileName})`}`);

  return hasWorkspace && (Configuration.get(`rpgleProjectMode`) === true || projectFile !== undefined);
}

/**
 * @param {vscode.ExtensionContext} context
 */
exports.startup = async (context) => {
  const canStart = await this.canEnable();
  Output.write(`Project mode: can start: ${canStart}`);

  if (canStart) {
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
    
    initialiseIncludeInfoFinder(context);
    exports.started = true;
  } else {
    // We do this in case the ${projectFileName} file is created and isn't enabled already.
    context.subscriptions.push(
      vscode.workspace.onDidCreateFiles(async event => {
        if (!exports.started) {
          const newProjectFile = event.files.some(uri => uri.path.endsWith(projectFileName));
          if (newProjectFile) {
            Output.write(`Project mode: ${projectFileName} file created. Trying to start up.`);
          
            const canStart = await this.canEnable();
            if (canStart) {
              this.startup(context);
            }
          }
        }
      })
    )
  }
}