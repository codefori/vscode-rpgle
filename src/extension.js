// The module 'vscode' contains the VS Code extensibility API
// Import the module and reference it with the alias vscode in your code below
const vscode = require(`vscode`);
const path = require(`path`);

const Worker = require(`./worker`);
const defaultConfig = require(`./models/default`);

const getInstance = require(`./base`);
const Output = require(`./output`);

// this method is called when your extension is activated
// your extension is activated the very first time the command is executed

/**
 * @param {vscode.ExtensionContext} context
 */
function activate(context) {

  // Use the console to output diagnostic information (console.log) and errors (console.error)
  // This line of code will only be executed once when your extension is activated
  console.log(`Congratulations, your extension "vscode-rpgle" is now active!`);

  /** @type {Worker} */
  let worker;

  worker = new Worker(context);

  Output.init();

  context.subscriptions.push(
    vscode.commands.registerCommand(`vscode-rpgle.openLintConfig`, async (filter) => {
      const instance = getInstance();
      const editor = vscode.window.activeTextEditor;

      if (editor && editor.document.uri.scheme === `file`) {
        const workspaces = vscode.workspace.workspaceFolders;
        if (workspaces && workspaces.length > 0) {
          const linter = await vscode.workspace.findFiles(`**/.vscode/rpglint.json`, null, 1);
          let uri;
          if (linter && linter.length > 0) {
            uri = linter[0];

            Output.write(`Uri path: ${JSON.stringify(uri)}`);

          } else {
            Output.write(`String path: ${path.join(workspaces[0].uri.path, `.vscode`, `rpglint.json`)}`);

            uri = vscode.Uri.parse(path.join(workspaces[0].uri.path, `.vscode`, `rpglint.json`));

            Output.write(`Creating Uri path: ${JSON.stringify(uri)}`);

            await vscode.workspace.fs.writeFile(
              uri, 
              Buffer.from(JSON.stringify(defaultConfig, null, 2), `utf8`)
            );
          }

          vscode.workspace.openTextDocument(uri).then(doc => {
            vscode.window.showTextDocument(doc, {
              viewColumn: vscode.ViewColumn.One
            });
          });
        }

      } else if (instance && instance.getConnection()) {
        /** @type {"member"|"streamfile"} */
        let type = `member`;
        let path;

        if (filter && filter.description) {
          // Bad way to get the library for the filter ..
          const library = filter.description.split(`/`)[0];
          path = `${library}/VSCODE/RPGLINT.JSON`;

        } else if (editor) {
          //@ts-ignore
          type = editor.document.uri.scheme;
          
          Output.write(`Uri remote path: ${JSON.stringify(editor.document.uri)}`);
          const lintInfo = worker.getLintConfigPath(editor.document.uri);

          if (lintInfo) {
            path = lintInfo.path;
          } else {
            vscode.window.showErrorMessage(`No lint config path for this file. File must either be a member or a streamfile on the host IBM i.`);
          }
        } else {
          vscode.window.showErrorMessage(`No active editor found.`);
        }

        if (path) {
          Output.write(`Current path: ${path}`);

          const exists = await vscode.commands.executeCommand(`code-for-ibmi.openEditable`, path);

          if (!exists) {
            const content = instance.getContent();

            vscode.window.showErrorMessage(`RPGLE linter config doesn't exist for this file. Would you like to create a default at ${path}?`, `Yes`, `No`).then
            (async (value) => {
              if (value === `Yes`) {
                const jsonString = JSON.stringify(defaultConfig, null, 2);

                switch (type) {
                case `member`:
                  const memberPath = path.split(`/`);
                  try {
                    await vscode.commands.executeCommand(
                      `code-for-ibmi.runCommand`,
                      {
                        'command': `CRTSRCPF FILE(${memberPath[0]}/VSCODE) RCDLEN(112)`
                      }
                    )
                  } catch (e) {
                    Output.write(e);
                  }

                  try {
                    await vscode.commands.executeCommand(
                      `code-for-ibmi.runCommand`,
                      {
                        command: `ADDPFM FILE(${memberPath[0]}/VSCODE) MBR(RPGLINT) SRCTYPE(JSON)`
                      }
                    );
                  } catch (e) {
                    Output.write(e);
                  }

                  try {
                    Output.write(`Member path: ${[memberPath[0], `VSCODE`, `RPGLINT`].join(`/`)}`);

                    await content.uploadMemberContent(null, memberPath[0], `VSCODE`, `RPGLINT`, jsonString);
                    await vscode.commands.executeCommand(`code-for-ibmi.openEditable`, path);
                  } catch (e) {
                    Output.write(e);
                  }
                  break;

                case `streamfile`:
                  Output.write(`IFS path: ${path}`);

                  await content.writeStreamfile(path, jsonString);
                  await vscode.commands.executeCommand(`code-for-ibmi.openEditable`, path);
                  break;
                }
              }
            });
          }
        }
      } else {
        vscode.window.showErrorMessage(`Not connected to a system.`);
      }
    })
  );
}

// this method is called when your extension is deactivated
function deactivate() {}

module.exports = {
  activate,
  deactivate
}
