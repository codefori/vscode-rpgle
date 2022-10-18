
const vscode = require(`vscode`);
const Cache = require(`./language/models/cache`);

/** @type {vscode.OutputChannel} */
let channel = undefined;

exports.init = () => {
  channel = vscode.window.createOutputChannel(`RPGLE language tools`);
}

exports.write = (string) => {
  channel.appendLine(string);
}

exports.append = (string) => {
  channel.append(string);
}

/**
 * 
 * @param {Cache} cache 
 * @param {string} content 
 */
exports.debugInfo = (cache, content) => {
  vscode.window.showErrorMessage(`Looks like a lint error has happened. Do you want to display the debug info?`, `Open .json`).then(async result => {
    if (result === `Open .json`) {
      cache.indicators = cache.indicators.filter(ind => !ind.name.startsWith(`IN`));
      const jsonContent = JSON.stringify(
        {
          type: `lint`,
          cache,
          content
        }, null, 2);
      const textDoc = await vscode.workspace.openTextDocument({language: `json`, content: jsonContent });
      await vscode.window.showTextDocument(textDoc);

      vscode.window.showInformationMessage(
        `To help us resolve this, we recommend sharing this JSON with us on a new GitHub issue or privately. Note that this JSON does contain the active source code.`,
        `Copy to clipboard`, `Copy to clipboard and open new Issue`
      ).then(async result => {
        if ([`Copy to clipboard`, `Copy to clipboard and open new Issue`].includes(result)) {
          await vscode.env.clipboard.writeText(jsonContent);
        }

        if (result === `Copy to clipboard and open new Issue`) {
          vscode.commands.executeCommand(`vscode.open`, vscode.Uri.parse(
            `https://github.com/halcyon-tech/vscode-rpgle/issues/new?body=${encodeURIComponent(`Paste the JSON here. If the JSON is too large, consider uploading it as a file or via gist or pastebin.`)}`
          ));
        }
      })
    }
  });
}