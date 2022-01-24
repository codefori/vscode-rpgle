// The module 'vscode' contains the VS Code extensibility API
// Import the module and reference it with the alias vscode in your code below
const vscode = require(`vscode`);
const path = require(`path`);

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

  // Use the console to output diagnostic information (console.log) and errors (console.error)
  // This line of code will only be executed once when your extension is activated
  console.log(`Congratulations, your extension "vscode-rpgle" is now active!`);
  
  Output.init();

  /** @type {LinterWorker} */
  let linterWorker;

  /** @type {LanguageWorker} */
  let languageWorker;

  if (Configuration.get(`rpgleLanguageToolsEnabled`)) {
    linterWorker = new LinterWorker(context);
  }

  if (Configuration.get(`rpgleLinterSupportEnabled`)) {
    languageWorker = new LanguageWorker(context);
  }

  registerColumnAssist(context);
}

// this method is called when your extension is deactivated
function deactivate() {}

module.exports = {
  activate,
  deactivate
}
