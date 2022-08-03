// The module 'vscode' contains the VS Code extensibility API
// Import the module and reference it with the alias vscode in your code below
const vscode = require(`vscode`);

const LinterWorker = require(`./vscode/LinterWorker`);
const LanguageWorker = require(`./vscode/LanguageWorker`);

const { registerColumnAssist } = require(`./columnAssist`);

const Configuration = require(`./configuration`);
const Output = require(`./output`);

// this method is called when your extension is activated
// your extension is activated the very first time the command is executed

/**
 * @param {vscode.ExtensionContext} context
 */
function activate(context) {
  Output.init();

  const env = process.env.TARGET;

  console.log(`Congratulations, your extension "vscode-rpgle" for ${env} is now active!`);
  Output.write(`Congratulations, your extension "vscode-rpgle" for ${env} is now active!`);

  /** @type {LanguageWorker} */
  let languageWorker;
  const languageEnabled = (Configuration.get(`rpgleLanguageToolsEnabled`) || env === `web`);

  /** @type {LinterWorker} */
  let linterWorker;
  const linterEnabled = (Configuration.get(`rpgleLinterSupportEnabled`) || env === `web`)

  if (languageEnabled) {
    languageWorker = new LanguageWorker(context);
  }

  if (linterEnabled) {
    linterWorker = new LinterWorker(context);
  }

  registerColumnAssist(context);
}

// this method is called when your extension is deactivated
function deactivate() {}

module.exports = {
  activate,
  deactivate
}
