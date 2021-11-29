
const path = require(`path`);
const vscode = require(`vscode`);

const Configuration = require(`./configuration`);

const { registerColumnAssist } = require(`./columnAssist`);

const Cache = require(`./models/cache`);
const possibleTags = require(`./models/tags`);

const Linter = require(`./linter`);
const Parser = require(`./parser`);
const Generic = require(`./generic`);

const lintFile = {
  member: `vscode,rpglint`,
  streamfile: `.vscode/rpglint.json`
};

module.exports = class Worker {
  /**
   * @param {vscode.ExtensionContext} context
   */
  constructor(context) {
    this.linterDiagnostics = vscode.languages.createDiagnosticCollection(`Lint`);

    this.parser = new Parser();

    /** @type {{[spfPath: string]: object}} */
    this.linterRules = {};

    registerColumnAssist(context);

    context.subscriptions.push(
      this.linterDiagnostics,

      vscode.commands.registerCommand(`vscode-rpgle.rpgleOpenInclude`, () => {
        const editor = vscode.window.activeTextEditor;
          
        if (editor) {
          const document = editor.document;
          const position = editor.selection.active;
          if (document.languageId === `rpgle`) {
            const linePieces = document.lineAt(position.line).text.trim().split(` `);
            if ([`/COPY`, `/INCLUDE`].includes(linePieces[0].toUpperCase())) {
              const {finishedPath, type} = Generic.getPathInfo(document.uri, linePieces[1]);

              switch (type) {
              case `member`:
                vscode.commands.executeCommand(`code-for-ibmi.openEditable`, `${finishedPath.substr(1)}.rpgle`);
                break;

              case `streamfile`:
                vscode.commands.executeCommand(`code-for-ibmi.openEditable`, finishedPath);
                break;
              }
            }
          }
        }
      }),

      vscode.commands.registerCommand(`vscode-rpgle.rpgleGetPrototype`, () => {
        const editor = vscode.window.activeTextEditor;
          
        if (editor) {
          const document = editor.document;
          const position = editor.selection.active.line;
          if (document.languageId === `rpgle`) {
            if (document.getText(new vscode.Range(0, 0, 0, 6)).toUpperCase() === `**FREE`) {
              const text = document.getText();
              this.parser.getDocs(document.uri).then(docs => {
                const currentProcedure = docs.procedures.find(proc => position >= proc.range.start && position <= proc.range.end);

                if (currentProcedure) {
                  let prototype = [
                    `Dcl-Pr ${currentProcedure.name} ${currentProcedure.keywords.join(` `)};`,
                    ...currentProcedure.subItems.map(subItem => 
                      `  ${subItem.name} ${subItem.keywords.join(` `)};`
                    ),
                    `End-Pr;`
                  ].join(`\n`);

                  vscode.env.clipboard.writeText(prototype);
                  vscode.window.showInformationMessage(`Prototype copied to clipboard.`);

                } else {
                  vscode.window.showErrorMessage(`No procedure block at line ${position}.`);
                }
              });
            } else {
              vscode.window.showErrorMessage(`You can only get the prototype of a **FREE source.`);
            }
          }
        }
      }),

      vscode.workspace.onDidChangeTextDocument(async editor => {
        if (editor) {
          const document = editor.document;
          if (document.languageId === `rpgle`) {
            if (document.getText(new vscode.Range(0, 0, 0, 6)).toUpperCase() === `**FREE`) {
              const text = document.getText();
              this.parser.clearParsedCache(document.uri.path);
              this.parser.getDocs(document.uri, text).then(docs => {
                if (Configuration.get(`rpgleLinterSupportEnabled`)) {
                  this.refreshDiagnostics(document, docs);
                }
              });
            } 
          }
        }
      }),

      vscode.languages.registerCodeActionsProvider(`rpgle`, {
        provideCodeActions: async (document, range) => {
          if (Configuration.get(`rpgleLinterSupportEnabled`)) {

            /** @type {vscode.CodeAction[]} */
            let actions = [];

            /** @type {vscode.CodeAction} */
            let action;

            const isFree = (document.getText(new vscode.Range(0, 0, 0, 6)).toUpperCase() === `**FREE`);
            const text = document.getText();
            if (isFree) {
              const options = this.getLinterOptions(document.uri);
              const docs = await this.parser.getDocs(document.uri);

              const detail = Linter.getErrors(text, {
                indent: Number(vscode.window.activeTextEditor.options.tabSize),
                ...options
              }, docs);

              const fixErrors = detail.errors.filter(error => error.range.intersection(range) );

              if (fixErrors.length > 0) {
                let errorRange;
                fixErrors.forEach(error => {
                  errorRange = this.calculateOffset(document, error);

                  switch (error.type) {
                  case `UppercaseConstants`:
                    action = new vscode.CodeAction(`Convert constant name to uppercase`, vscode.CodeActionKind.QuickFix);
                    action.edit = new vscode.WorkspaceEdit();
                    action.edit.replace(document.uri, errorRange, error.newValue);
                    actions.push(action);
                    break;
  
                  case `ForceOptionalParens`:
                    action = new vscode.CodeAction(`Add brackets around expression`, vscode.CodeActionKind.QuickFix);
                    action.edit = new vscode.WorkspaceEdit();
                    action.edit.insert(document.uri, errorRange.end, `)`);
                    action.edit.insert(document.uri, errorRange.start, `(`);
                    actions.push(action);
                    break;
  
                  case `UselessOperationCheck`:
                    action = new vscode.CodeAction(`Remove operation code`, vscode.CodeActionKind.QuickFix);
                    action.edit = new vscode.WorkspaceEdit();
                    action.edit.delete(document.uri, errorRange);
                    actions.push(action);
                    break;
  
                  case `SpecificCasing`:
                  case `IncorrectVariableCase`:
                  case `UppercaseDirectives`:
                    action = new vscode.CodeAction(`Correct casing to '${error.newValue}'`, vscode.CodeActionKind.QuickFix);
                    action.edit = new vscode.WorkspaceEdit();
                    action.edit.replace(document.uri, errorRange, error.newValue);
                    actions.push(action);
                    break;

                  case `RequiresProcedureDescription`:
                    action = new vscode.CodeAction(`Add title and description`, vscode.CodeActionKind.QuickFix);
                    action.edit = new vscode.WorkspaceEdit();
                    action.edit.insert(document.uri, errorRange.start, `///\n// Title\n// Description\n///\n`);
                    actions.push(action);
                    break;

                  case `RequireBlankSpecial`:
                    action = new vscode.CodeAction(`Convert constant name to uppercase`, vscode.CodeActionKind.QuickFix);
                    action.edit = new vscode.WorkspaceEdit();
                    action.edit.replace(document.uri, errorRange, error.newValue);
                    actions.push(action);
                    break;

                  case `CopybookDirective`:
                    action = new vscode.CodeAction(`Switch to '${error.newValue}'`, vscode.CodeActionKind.QuickFix);
                    action.edit = new vscode.WorkspaceEdit();
                    action.edit.replace(document.uri, errorRange, error.newValue);
                    actions.push(action);
                    break;
                  }
                });
              }

              console.log(actions);
            }
          
            return actions;
          }
        }
      }),

      vscode.languages.registerHoverProvider({language: `rpgle`}, {
        provideHover: async (document, position, token) => {
          const text = document.getText();
          const doc = await this.parser.getDocs(document.uri, text);
          const range = document.getWordRangeAtPosition(position);
          const word = document.getText(range).toUpperCase();

          const procedure = doc.procedures.find(proc => proc.name.toUpperCase() === word);

          if (procedure) {
            let markdown = ``;
            let retrunValue = procedure.keywords.filter(keyword => keyword !== `EXTPROC`);
            if (retrunValue.length === 0) retrunValue = [`void`];

            const returnTag = procedure.tags.find(tag => tag.tag === `return`);
            const deprecatedTag = procedure.tags.find(tag => tag.tag === `deprecated`);

            // Deprecated notice
            if (deprecatedTag) {
              markdown += `**Deprecated:** ${deprecatedTag.content}\n\n`;
            }

            // Formatted code
            markdown += `\`\`\`vb\n${procedure.name}(`;

            if (procedure.subItems.length > 0) {
              markdown += `\n  ${procedure.subItems.map(parm => `${parm.name}: ${parm.keywords.join(` `)}`).join(`,\n  `)}\n`;
            }

            markdown += `): ${retrunValue.join(` `)}\n\`\`\` \n`;

            // Description
            if (procedure.description)
              markdown += `${procedure.description}\n\n`;

            // Params
            markdown += procedure.subItems.map(parm => `*@param* \`${parm.name.replace(new RegExp(`\\*`, `g`), `\\*`)}\` ${parm.description}`).join(`\n\n`);

            // Return value
            if (returnTag) {
              markdown += `\n\n*@returns* ${returnTag.content}`;
            }

            if (procedure.position) {
              markdown += `\n\n*@file* \`${procedure.position.path}:${procedure.position.line+1}\``;
            }

            return new vscode.Hover(
              new vscode.MarkdownString(
                markdown
              )
            );
          }

          const linePieces = document.lineAt(position.line).text.trim().split(` `);
          if ([`/COPY`, `/INCLUDE`].includes(linePieces[0].toUpperCase())) {
            const {type, memberPath, finishedPath} = Generic.getPathInfo(document.uri, linePieces[1]);

            return new vscode.Hover(
              new vscode.MarkdownString(
                `\`'${finishedPath}'\` (${type})`
              )
            )
          }

          return null;
        }
      }),

      vscode.languages.registerDocumentSymbolProvider({ language: `rpgle` }, 
        {
          provideDocumentSymbols: async (document, token) => {
            const isFree = (document.getText(new vscode.Range(0, 0, 0, 6)).toUpperCase() === `**FREE`);
              
            const text = document.getText();
            if (isFree) {
              const doc = await this.parser.getDocs(document.uri, text);

              const currentPath = document.uri.path;

              /** @type vscode.SymbolInformation[] */
              let currentDefs = [];

              currentDefs.push(
                ...[
                  ...doc.procedures.filter(proc => proc.position && proc.position.path === currentPath),
                  ...doc.subroutines.filter(sub => sub.position && sub.position.path === currentPath),
                ].map(def => new vscode.SymbolInformation(
                  def.name,
                  vscode.SymbolKind.Function,
                  new vscode.Range(def.position.line, 0, def.position.line, 0),
                  document.uri
                ))
              );

              currentDefs.push(
                ...doc.variables
                  .filter(variable => variable.position && variable.position.path === currentPath)
                  .map(def => new vscode.SymbolInformation(
                    def.name,
                    vscode.SymbolKind.Variable,
                    new vscode.Range(def.position.line, 0, def.position.line, 0),
                    document.uri
                  ))
              );

              currentDefs.push(
                ...doc.structs
                  .filter(struct => struct.position && struct.position.path === currentPath)
                  .map(def => new vscode.SymbolInformation(
                    def.name,
                    vscode.SymbolKind.Struct,
                    new vscode.Range(def.position.line, 0, def.position.line, 0),
                    document.uri
                  ))
              );

              currentDefs.push(
                ...doc.constants
                  .filter(constant => constant.position && constant.position.path === currentPath)
                  .map(def => new vscode.SymbolInformation(
                    def.name,
                    vscode.SymbolKind.Constant,
                    new vscode.Range(def.position.line, 0, def.position.line, 0),
                    document.uri
                  ))
              );

              return currentDefs;
            }

            return [];
          }
        }),

      vscode.languages.registerDefinitionProvider({ language: `rpgle` }, {
        provideDefinition: async (document, position, token) => {
          const isFree = (document.getText(new vscode.Range(0, 0, 0, 6)).toUpperCase() === `**FREE`);
          const doc = await this.parser.getDocs(document.uri);
          const range = document.getWordRangeAtPosition(position);
          const word = document.getText(range).toUpperCase();

          if (doc) {
            const types = Object.keys(doc);
            const type = types.find(type => doc[type].find(def => def.name.toUpperCase() === word));
            if (doc[type]) {
              const def = doc[type].find(def => def.name.toUpperCase() === word);
              if (def) {
                let {finishedPath, type} = Generic.getPathInfo(document.uri, def.position.path);
                if (type === `member`) {
                  finishedPath = `${finishedPath}.rpgle`;
                }

                return new vscode.Location(
                  vscode.Uri.parse(finishedPath).with({scheme: type, path: finishedPath}),
                  new vscode.Range(def.position.line, 0, def.position.line, 0)
                );
              }
            }
          }
        }}),

      vscode.languages.registerCompletionItemProvider({language: `rpgle`, }, {
        provideCompletionItems: async (document, position) => {
          const isFree = (document.getText(new vscode.Range(0, 0, 0, 6)).toUpperCase() === `**FREE`);
          const text = document.getText();
          if (isFree) {
            const currentLine = document.getText(new vscode.Range(position.line, 0, position.line, position.character));
            const doc = await this.parser.getDocs(document.uri, text);

            /** @type vscode.CompletionItem[] */
            let items = [];
            let item;

            if (currentLine.startsWith(`//`)) {
              for (const tag in possibleTags) {
                item = new vscode.CompletionItem(`@${tag}`, vscode.CompletionItemKind.Property);
                item.insertText = new vscode.SnippetString(`@${tag} $0`);
                item.detail = possibleTags[tag];
                items.push(item);
              }

            } else {
              for (const procedure of doc.procedures) {
                item = new vscode.CompletionItem(`${procedure.name}`, vscode.CompletionItemKind.Function);
                item.insertText = new vscode.SnippetString(`${procedure.name}(${procedure.subItems.map((parm, index) => `\${${index+1}:${parm.name}}`).join(`:`)})\$0`)
                item.detail = procedure.keywords.join(` `);
                item.documentation = procedure.description;
                items.push(item);
              }

              for (const subroutine of doc.subroutines) {
                item = new vscode.CompletionItem(`${subroutine.name}`, vscode.CompletionItemKind.Function);
                item.insertText = new vscode.SnippetString(`${subroutine.name}\$0`);
                item.documentation = subroutine.description;
                items.push(item);
              }

              for (const variable of doc.variables) {
                item = new vscode.CompletionItem(`${variable.name}`, vscode.CompletionItemKind.Variable);
                item.insertText = new vscode.SnippetString(`${variable.name}\$0`);
                item.detail = variable.keywords.join(` `);
                item.documentation = variable.description;
                items.push(item);
              }

              for (const struct of doc.structs) {
                item = new vscode.CompletionItem(`${struct.name}`, vscode.CompletionItemKind.Struct);
                item.insertText = new vscode.SnippetString(`${struct.name}\$0`);
                item.detail = struct.keywords.join(` `);
                item.documentation = struct.description;
                items.push(item);
              }

              for (const constant of doc.constants) {
                item = new vscode.CompletionItem(`${constant.name}`, vscode.CompletionItemKind.Constant);
                item.insertText = new vscode.SnippetString(`${constant.name}\$0`);
                item.detail = constant.keywords.join(` `);
                item.documentation = constant.description;
                items.push(item);
              }
            }

            return items;
          }
        }
      }),

      vscode.window.onDidChangeActiveTextEditor(async (e) => {
        if (e && e.document) {
          if (e.document.languageId === `rpgle`) {
            const document = e.document;

            const text = document.getText();
            const isFree = (document.getText(new vscode.Range(0, 0, 0, 6)).toUpperCase() === `**FREE`);
            if (isFree) {
              this.parser.updateCopybookCache(document.uri, text);

              this.parser.getDocs(document.uri, text).then(doc => {
                this.refreshDiagnostics(document, doc);
              });
            }
          }
        }
      }),

      vscode.workspace.onDidSaveTextDocument((document) => {
        const workingUri = document.uri;
        const {finishedPath} = Generic.getPathInfo(workingUri, path.basename(workingUri.path));
        const text = document.getText();
        const isFree = (document.getText(new vscode.Range(0, 0, 0, 6)).toUpperCase() === `**FREE`);

        if (this.parser.getCopybook(finishedPath)) {
          //Update stored copy book
          const lines = text.replace(new RegExp(`\\\r`, `g`), ``).split(`\n`);
          this.parser.setCopybook(finishedPath, lines);

          // The user usually switches tabs very quickly, so we trigger this event too.
          if (vscode.window.activeTextEditor) {
            if (workingUri.path !== vscode.window.activeTextEditor.document.uri.path) {
              this.parser.getDocs(workingUri).then(docs => {
                this.refreshDiagnostics(vscode.window.activeTextEditor.document, docs);
              });
            }
          }
        }
        else if (document.languageId === `rpgle`) {
          //Else fetch new info from source being edited
          if (isFree) {
            this.parser.updateCopybookCache(workingUri, text)
          }
        }
      }),

      vscode.workspace.onDidOpenTextDocument((document) => {
        let text;
        switch (document.languageId) {
        case `rpgle`:
          const isFree = (document.getText(new vscode.Range(0, 0, 0, 6)).toUpperCase() === `**FREE`);
          text = document.getText();
          if (isFree) {
            this.parser.updateCopybookCache(document.uri, text);
  
            if (Configuration.get(`rpgleLinterSupportEnabled`)) {
              this.getLinterFile(document).then(file => {
                this.parser.getDocs(document.uri, text).then(docs => {
                  this.refreshDiagnostics(document, docs);
                });
              });
            }
          }

          break;
        
        // We need to update our copy of the linter configuration
        case `json`:
          text = document.getText();
          if (Configuration.get(`rpgleLinterSupportEnabled`)) {
            let upperPath;
            switch (document.uri.scheme) {
            case `member`:
              upperPath = document.uri.path.toUpperCase().substring(0, document.uri.path.length - 5); //without the extension
              break;
            case `streamfile`:
              upperPath = document.uri.path.toUpperCase();
              break;
            }

            if (upperPath.includes(`RPGLINT`)) {
              if (!this.parser.getCopybook(upperPath))
                this.parser.setCopybook(upperPath, text);
            }
          }
          break;
        }
      })
    )
    
  }

  /**
   * Returns relative linter configuration path
   * @param {vscode.Uri} uri 
   */
  getLintConfigPath(uri) {
    const lintPath = lintFile[uri.scheme];

    if (lintPath) {
      let {finishedPath, type} = Generic.getPathInfo(uri, lintPath);
      switch (type) {
      case `member`:
        return {path: `${finishedPath.substr(1)}.JSON`, type: `member`};
      case `streamfile`:
        return {path: finishedPath.toLowerCase(), type: `streamfile`};
      }
    }

    return null;
  }

  /**
   * @param {vscode.TextDocument} document 
   */
  getLinterFile(document) {
    // Used to fetch the linter settings
    // Will only download once.
    const lintPath = lintFile[document.uri.scheme];
    if (lintPath) {
      return this.parser.getContent(document.uri, lintPath);
    }
  }

  getLinterOptions(workingUri) {
    let options = {};

    const localLintPath = lintFile[workingUri.scheme];
    if (localLintPath) {
      let {finishedPath} = Generic.getPathInfo(workingUri, localLintPath);

      const possibleJson = this.parser.getCopybook(finishedPath);
      if (possibleJson) {
        const jsonString = possibleJson.join(``).trim();
        if (jsonString) {
          try {
            options = JSON.parse(jsonString);
            return options;
          } catch (e) {
            //vscode.window.showErrorMessage(`Failed to parse rpglint.json file at ${lintPath}.`);
          }
        }
      }
    }

    return options;
  }

  /** 
   * @param {vscode.TextDocument} document 
   * @param {Cache} [docs]
   * */
  async refreshDiagnostics(document, docs) {
    const isFree = (document.getText(new vscode.Range(0, 0, 0, 6)).toUpperCase() === `**FREE`);
    if (isFree) {
      const text = document.getText();

      /** @type {vscode.Diagnostic[]} */
      let indentDiags = [];

      /** @type {vscode.Diagnostic[]} */
      let generalDiags = [];

      const options = this.getLinterOptions(document.uri);

      const detail = Linter.getErrors(text, {
        indent: Number(vscode.window.activeTextEditor.options.tabSize),
        ...options
      }, docs);

      const indentErrors = detail.indentErrors;
      const errors = detail.errors;

      if (indentErrors.length > 0) {
        indentErrors.forEach(error => {
          const range = new vscode.Range(error.line, 0, error.line, error.currentIndent);

          indentDiags.push(new vscode.Diagnostic(
            range, 
            `Incorrect indentation. Expected ${error.expectedIndent}, got ${error.currentIndent}`, 
            vscode.DiagnosticSeverity.Warning
          ));
        });
      }

      if (errors.length > 0) {
        errors.forEach(error => {
          const range = this.calculateOffset(document, error);

          const diagnostic = new vscode.Diagnostic(
            range, 
            Linter.getErrorText(error.type), 
            vscode.DiagnosticSeverity.Warning
          );

          generalDiags.push(diagnostic);
        });
      }

      this.linterDiagnostics.set(document.uri, [...indentDiags, ...generalDiags]);
    }
  }

  /**
   * @param {vscode.TextDocument} document
   * @param {{range: vscode.Range, offset?: {position: number, length: number}}} error 
   */
  calculateOffset(document, error) {
    const offset = error.offset;
    let range;

    if (offset) {
      const docOffsetStart = document.offsetAt(error.range.start) + offset.position;
      const docOffsetEnd = document.offsetAt(error.range.start) + offset.length;
      range = new vscode.Range(
        document.positionAt(docOffsetStart),
        document.positionAt(docOffsetEnd)
      );
    } else {
      range = error.range;
    }
    
    return range;
  }
}