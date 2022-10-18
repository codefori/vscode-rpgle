
const vscode = require(`vscode`);
const { findOtherPrototypes, trimQuotes, findExportDefinition, findProgramFile } = require(`./internals`);
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

      // EXTPGM prototype code lens
      cache.procedures.filter(proc => proc.keyword[`EXTPGM`]).forEach(proc => {
        let actualName;
        const keyword = proc.keyword[`EXTPGM`];

        if (keyword) {
          if (keyword === true) {
            actualName = proc.name;
          } else
          if (keyword !== proc.name) {
            actualName = trimQuotes(keyword);
          }
        }

        if (actualName) {
          const possibleFile = findProgramFile(actualName)
          if (possibleFile) {
            const currentLineRange = new vscode.Range(
              proc.position.line, 0, proc.position.line, 0
            );
            codeLens.push(
              new vscode.CodeLens(
                currentLineRange,
                {
                  title: `Open implementation (program)`,
                  command: `vscode.open`,
                  arguments: [
                    vscode.Uri.from({
                      scheme: document.uri.scheme,
                      path: possibleFile
                    }), 
                    {
                      preview: true
                    }
                  ]
                }
              ),
            )
          }
        }
      });

      // EXTPROC prototype code lens
      // TODO: write implementation provider
      cache.procedures.filter(proc => proc.keyword[`EXTPROC`]).forEach(proc => {
        let actualName;
        const keyword = proc.keyword[`EXTPROC`];

        if (keyword) {
          if (keyword === true) {
            actualName = proc.name;
          } else
          if (keyword !== proc.name) {
            actualName = trimQuotes(keyword);
          }
        }

        if (actualName) {
          const implementation = findExportDefinition(actualName);
          if (implementation) {
            if (implementation.path !== document.uri.path) {
              const currentLineRange = new vscode.Range(
                proc.position.line, 0, proc.position.line, 0
              );
              codeLens.push(
                new vscode.CodeLens(
                  currentLineRange,
                  {
                    title: `Open implementation (function)`,
                    command: `vscode.open`,
                    arguments: [
                      vscode.Uri.from({
                        scheme: document.uri.scheme,
                        path: implementation.path
                      }), 
                      {
                        preview: true,
                        selection: new vscode.Range(implementation.line, 0, implementation.line, 0)
                      }
                    ]
                  }
                ),
              )
            }
          }
        }
      });

      // Export function code lens
      cache.procedures.filter(proc => proc.keyword[`EXPORT`]).forEach(proc => {
        const references = findOtherPrototypes(`function`, proc.name);
        const locations = references.map(ref => new vscode.Location(
          vscode.Uri.from({
            scheme: document.uri.scheme,
            path: ref.path
          }), new vscode.Range(ref.line, 0, ref.line, 0)
        ));

        const currentLineRange = new vscode.Range(
          proc.position.line, 0, proc.position.line, 0
        );

        codeLens.push(
          new vscode.CodeLens(
            currentLineRange,
            {
              title: `${references.length} reference${references.length === 1 ? `` : `s`}`,
              command: `editor.action.showReferences`,
              arguments: [
                document.uri, 
                new vscode.Range(proc.position.line, 0, proc.position.line, 0).start, 
                locations
              ]
            }
          ),
          new vscode.CodeLens(
            currentLineRange,
            {
              title: `Copy prototype`,
              command: `vscode-rpgle.rpgleGetPrototype`,
              arguments: [proc.name]
            }
          ),
        );
      });
    }

    return codeLens;
  }
}