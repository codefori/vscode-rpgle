// The module 'vscode' contains the VS Code extensibility API
// Import the module and reference it with the alias vscode in your code below
const vscode = require(`vscode`);
const util = require(`util`);

const Worker = require(`./worker`);
const defaultConfig = require(`./models/default`);


const { instance } = vscode.extensions.getExtension(`halcyontechltd.code-for-ibmi`).exports;

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

  instance.on(`connected`, () => {
    worker = new Worker(context);
  });

  context.subscriptions.push(
    vscode.commands.registerCommand(`vscode-rpgle.openLintConfig`, async () => {
      if (worker) {
        const editor = vscode.window.activeTextEditor;

        if (editor) {
          const lintInfo = worker.getLintConfigPath(editor.document.uri);

          if (lintInfo) {
            const exists = await vscode.commands.executeCommand(`code-for-ibmi.openEditable`, lintInfo.path);

            if (!exists) {
              const content = instance.getContent();

              vscode.window.showErrorMessage(`RPGLE linter config doesn't exist for this file. Would you like to create a default at ${lintInfo.path}?`, `Yes`, `No`).then
              (async (value) => {
                if (value === `Yes`) {
                  const jsonString = JSON.stringify(defaultConfig, null, 2);

                  switch (lintInfo.type) {
                  case `member`:
                    const memberPath = lintInfo.path.split(`/`);
                    try {
                      await vscode.commands.executeCommand(
                        `code-for-ibmi.runCommand`,
                        {
                          'command': `CRTSRCPF FILE(${memberPath[0]}/VSCODE) RCDLEN(112)`
                        }
                      )
                    } catch (e) {
                      console.log(e);
                    }

                    try {
                      await vscode.commands.executeCommand(
                        `code-for-ibmi.runCommand`,
                        {
                          command: `ADDPFM FILE(${memberPath[0]}/VSCODE) MBR(RPGLINT) SRCTYPE(JSON)`
                        }
                      );
                    } catch (e) {
                      console.log(e);
                    }

                    try {
                      await content.uploadMemberContent(null, memberPath[0], memberPath[1], `RPGLINT`, jsonString);
                      await vscode.commands.executeCommand(`code-for-ibmi.openEditable`, lintInfo.path);
                    } catch (e) {
                      console.log(e);
                    }
                    break;
                  case `streamfile`:
                    await content.writeStreamfile(lintInfo.path, jsonString);
                    await vscode.commands.executeCommand(`code-for-ibmi.openEditable`, lintInfo.path);
                    break;
                  }
                }
              });
            }
          } else {
            vscode.window.showErrorMessage(`No lint config path for this file. File must either be a member or a streamfile on the host IBM i.`);
          }
        } else {
          vscode.window.showErrorMessage(`No active editor found.`);
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
