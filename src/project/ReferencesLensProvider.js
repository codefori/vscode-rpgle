
const vscode = require(`vscode`);
const { findOtherPrototypes } = require(`./internals`);
const { Parser } = require(`../parser`);

const unsupportedSchemes = [`member`, `streamfile`]

/**
 * @type {vscode.CodeLensProvider}
 */
module.exports = class ReferencesLensProvider {
  constructor() {
    this.emitter = new vscode.EventEmitter();
    this.onDidChangeCodeLenses = this.emitter.event;
  }

  refresh() {
    this.emitter.fire();
  }

  /**
   * @param {vscode.TextDocument} document
   * @returns {Promise<vscode.CodeLens[]>} 
   */
  async provideCodeLenses(document)  {
    let codeLens = [];
    
    if (!unsupportedSchemes.includes(document.uri.scheme)) {
      const cache = await Parser.getDocs(document.uri, document.getText());

      cache.procedures.filter(proc => proc.keyword[`EXPORT`]).forEach(proc => {
        const references = findOtherPrototypes(`function`, proc.name);
        const locations = references.map(ref => new vscode.Location(
          vscode.Uri.from({
            scheme: document.uri.scheme,
            path: ref.path
          }), new vscode.Range(ref.line, 0, ref.line, 0)
        ));

        codeLens.push(new vscode.CodeLens(
          new vscode.Range(
            proc.position.line, 0, proc.position.line, 0
          ),
          {
            title: `${references.length} references`,
            command: `editor.action.showReferences`,
            arguments: [
              document.uri, 
              new vscode.Range(proc.position.line, 0, proc.position.line, 0).start, 
              locations
            ]
          }
        ));
      });
    }

    return codeLens;
  }
}