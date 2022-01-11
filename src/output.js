
const vscode = require(`vscode`);

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
