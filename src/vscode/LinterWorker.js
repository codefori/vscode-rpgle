
const path = require(`path`);
const vscode = require(`vscode`);

const Output = require(`../output`);
const getInstance = require(`../base`);
const defaultConfig = require(`../schemas/default`);

const Cache = require(`../language/models/cache`);

const Linter = require(`../language/linter`);
const Generic = require(`../language/generic`);

const { Parser } = require(`../parser`);

const lintFile = {
  member: `vscode,rpglint`,
  streamfile: `.vscode/rpglint.json`,
  file: `.vscode/rpglint.json`
};

module.exports = class LinterWorker {
  /**
   * @param {vscode.ExtensionContext} context
   * @param {number} [waitTime]
   */
  constructor(context, waitTime = 2000) {
    this.linterDiagnostics = vscode.languages.createDiagnosticCollection(`Lint`);

    /** @type {{[spfPath: string]: object}} */
    this.linterRules = {};

    this.editTimeout = null;

    context.subscriptions.push(
      this.linterDiagnostics,

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
              Output.write(`String path: ${path.join(workspaces[0].uri.fsPath, `.vscode`, `rpglint.json`)}`);
  
              uri = vscode.Uri.from({
                scheme: `file`,
                path: path.join(workspaces[0].uri.fsPath, `.vscode`, `rpglint.json`)
              });
  
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
            const lintInfo = LinterWorker.getLintConfigPath(editor.document.uri);
  
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
  
            const exists = await vscode.commands.executeCommand(`code-for-ibmi.openEditable`, path, 1);
  
            if (!exists) {
              const content = instance.getContent();
  
              vscode.window.showErrorMessage(`RPGLE linter config doesn't exist for this file. Would you like to create a default at ${path}?`, `Yes`, `No`).then
              (async (value) => {
                if (value === `Yes`) {
                  const jsonString = JSON.stringify(defaultConfig, null, 2);
  
                  switch (type) {
                  case `member`:
                    const memberPath = path.split(`/`);

                    // Will not crash, even if it fails
                    await vscode.commands.executeCommand(
                      `code-for-ibmi.runCommand`,
                      {
                        'command': `CRTSRCPF FILE(${memberPath[0]}/VSCODE) RCDLEN(112)`
                      }
                    );
  
                    // Will not crash, even if it fails
                    await vscode.commands.executeCommand(
                      `code-for-ibmi.runCommand`,
                      {
                        command: `ADDPFM FILE(${memberPath[0]}/VSCODE) MBR(RPGLINT) SRCTYPE(JSON)`
                      }
                    );
  
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
      }),

      vscode.commands.registerCommand(`vscode-rpgle.fixAllErrors`, async () => {
        const editor = vscode.window.activeTextEditor;
          
        if (editor) {
          const document = editor.document;
          if (document.languageId === `rpgle`) {
            if (document.getText(new vscode.Range(0, 0, 0, 6)).toUpperCase() === `**FREE`) {
              vscode.window.withProgress({
                location: vscode.ProgressLocation.Notification,
                title: `Fixing issues in document..`,
              }, async (progress) => {
                progress.report({
                  message: `Fetching lint configuration`
                });
                
                const options = await this.getLinterOptions(document.uri);
                const docs = await Parser.getDocs(document.uri, document.getText());

                // Define the rules 
                const rules = {
                  indent: Number(vscode.window.activeTextEditor.options.tabSize),
                  literalMinimum: 1,
                  ...options
                };

                progress.report({
                  message: `Fixing indentation`,
                  increment: 1,
                });

                // First we do all the indentation fixes.
                const { indentErrors } = Linter.getErrors({
                  uri: document.uri,
                  content: document.getText()
                }, rules, docs);

                if (indentErrors.length > 0) {
                  const fixes = indentErrors.map(error => {
                    const range = Generic.calculateOffset(document, {
                      range: new vscode.Range(error.line, 0, error.line, error.currentIndent), 
                      offset: undefined,
                      type: undefined,
                      newValue: undefined,
                    });
                    return new vscode.TextEdit(range, ``.padEnd(error.expectedIndent, ` `));
                  });

                  editor.edit(editBuilder => {
                    fixes.forEach(fix => editBuilder.replace(fix.range, fix.newText));
                  });
                }
              
                while (true) {
                  progress.report({
                    message: `Fixing other issues..`,
                    increment: 2,
                  });

                  // Next up, let's fix all the other things!
                  const {errors} = Linter.getErrors({
                    uri: document.uri,
                    content: document.getText()
                  }, rules, docs);

                  const actions = LinterWorker.getActions(document, errors);
                  let edits = [];

                  if (actions.length > 0) {
                    const linesChanged = [];
                    progress.report({
                      message: `Fixing other issues (remaining: ${actions.length})`,
                      increment: (actions.length > 100 ? 2 : 100 - actions.length),
                    });

                    // We only ever do the first one over and over.
                    /** @type {vscode.TextEdit[]} */
                    const edits = [];

                    actions.filter(action => action.edit).map(action => {
                      const entries = action.edit.entries();
                      for (const [uri, actionEdits] of entries) {
                        if (actionEdits.length > 0) {
                          const changedLine = actionEdits[0].range.start.line;
                          if (!linesChanged.includes(changedLine)) {
                            edits.push(...actionEdits);
                            linesChanged.push(changedLine);
                          }
                        }
                      }
                    });

                    if (edits.length) {
                      const workEdits = new vscode.WorkspaceEdit();
                      workEdits.set(document.uri, edits); // give the edits
                      await vscode.workspace.applyEdit(workEdits);

                    } else {
                      break;
                    }
                  } else {
                    break;
                  }
                }
              });
            }
          }
        }
      }),

      /** 
       * Provides the quick fixes on errors.
       */
      vscode.languages.registerCodeActionsProvider(`rpgle`, {
        provideCodeActions: async (document, range) => {
          /** @type {vscode.CodeAction[]} */
          let actions = [];

          const isFree = (document.getText(new vscode.Range(0, 0, 0, 6)).toUpperCase() === `**FREE`);
          if (isFree) {
            const docs = await Parser.getDocs(document.uri);

            if (docs) {
              const options = await this.getLinterOptions(document.uri);
              const text = document.getText();
              const detail = Linter.getErrors({
                uri: document.uri,
                content: text
              }, {
                indent: Number(vscode.window.activeTextEditor.options.tabSize),
                ...options
              }, docs);

              const fixErrors = detail.errors.filter(error => error.range.intersection(range) );

              if (fixErrors.length > 0) {
                actions = LinterWorker.getActions(document, fixErrors);
              }
            }
          }
          
          return actions;
        }
      }),

      /**
       * When the document changes, we want to fetch the updated errors.
       */
      vscode.workspace.onDidChangeTextDocument(async editor => {
        if (editor) {
          const document = editor.document;
          if (document.languageId === `rpgle`) {
            clearTimeout(this.editTimeout);

            this.editTimeout = setTimeout(async () => {
              if (document.getText(new vscode.Range(0, 0, 0, 6)).toUpperCase() === `**FREE`) {
                const text = document.getText();
                Parser.clearParsedCache(document.uri.path);
                Parser.getDocs(document.uri, text).then(docs => {
                  this.refreshDiagnostics(document, docs);
                });
              }
            }, waitTime);
          }
        }
      }),

      /**
       * When the active document changes, we want to fetch the updated errors.
       */
      vscode.window.onDidChangeActiveTextEditor(async (e) => {
        if (e && e.document) {
          const document = e.document;
          if (document.languageId === `rpgle`) {
            clearTimeout(this.editTimeout);

            this.editTimeout = setTimeout(async () => {
              const isFree = (document.getText(new vscode.Range(0, 0, 0, 6)).toUpperCase() === `**FREE`);
              if (isFree) {
                const text = document.getText();
                Parser.getDocs(document.uri, text).then(doc => {
                  this.refreshDiagnostics(document, doc);
                });
              }
            }, waitTime);
          }

          // If it's a local file
          if (document.uri.scheme === `file`) {
            // Clear local URI from path if it is null as it now means the file exists
            
            Parser.clearUris(document.uri.path);
          }
        }
      }),
    )
    
  }

  /**
   * Returns relative linter configuration path
   * @param {vscode.Uri} uri 
   */
  static getLintConfigPath(uri) {
    const lintPath = lintFile[uri.scheme];

    if (lintPath) {
      let {finishedPath, type} = Generic.getPathInfo(uri, lintPath);
      switch (type) {
      case `member`:
        return {path: `${finishedPath.substring(1)}.JSON`, type: `member`};
      case `streamfile`:
        return {path: finishedPath.toLowerCase(), type: `streamfile`};
      }
    }

    return null;
  }

  /**
   * @param {vscode.Uri} uri 
   */
  getLinterFile(uri) {
    // Used to fetch the linter settings
    // Will only download once.
    const lintPath = lintFile[uri.scheme];
    if (lintPath) {
      return Parser.getContent(uri, lintPath);
    }
  }

  async getLinterOptions(workingUri) {
    let options = {};

    const possibleJson = await this.getLinterFile(workingUri);
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

      const options = await this.getLinterOptions(document.uri);

      let detail;

      try {
        detail = Linter.getErrors({
          uri: document.uri,
          content: text,
        }, {
          indent: Number(vscode.window.activeTextEditor.options.tabSize),
          ...options
        }, docs);
      } catch (e) {
        Output.write(`Error linting ${document.uri.path}: ${e.message}`);
        Output.write(e.stack);
        return;
      }

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
          const range = Generic.calculateOffset(document, error);

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
   * @param {IssueRange[]} errors 
   */
  static getActions(document, errors) {
    /** @type {vscode.CodeAction[]} */
    let actions = [];

    // We need to move subroutine to the end and reverse the contents
    const NoGlobalSubroutines = errors.filter(e => e.type === `NoGlobalSubroutines`);

    // Then remove them from the error list
    errors = errors.filter(e => e.type !== `NoGlobalSubroutines`);

    // Before reversing an adding them back
    NoGlobalSubroutines.reverse();
    errors.push(...NoGlobalSubroutines);

    errors.forEach(error => {
      let action;
      let errorRange = Generic.calculateOffset(document, error);

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
      case `StringLiteralDupe`:
      case `NoGlobalSubroutines`:
        if (error.newValue) {
          action = new vscode.CodeAction(`Switch to '${error.newValue}'`, vscode.CodeActionKind.QuickFix);
          action.edit = new vscode.WorkspaceEdit();
          action.edit.replace(document.uri, errorRange, error.newValue);
          actions.push(action);
        }
        break;
      
      case `PrettyComments`:
        action = new vscode.CodeAction(`Fix comment formatting`, vscode.CodeActionKind.QuickFix);
        action.edit = new vscode.WorkspaceEdit();
        action.edit.replace(document.uri, errorRange, error.newValue);
        actions.push(action);
        break;
      }
    });

    return actions;
  }
}