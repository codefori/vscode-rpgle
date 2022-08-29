
const vscode = require(`vscode`);

const possibleTags = require(`../language/models/tags`);

const Linter = require(`../language/linter`);
const Generic = require(`../language/generic`);
const Cache = require(`../language/models/cache`);

const Output = require(`../output`);
const { Parser } = require(`../parser`);
const Declaration = require(`../language/models/declaration`);

module.exports = class LanguageWorker {
  /**
   * @param {vscode.ExtensionContext} context
   */
  constructor(context) {
    /** @type {number|undefined} */
    this.lineEditedBefore = undefined;
    /** @type {NodeJS.Timeout|undefined} */
    this.editTimeout = undefined;

    context.subscriptions.push(
      vscode.commands.registerCommand(`vscode-rpgle.rpgleOpenInclude`, async () => {
        const editor = vscode.window.activeTextEditor;
          
        if (editor) {
          const document = editor.document;
          const position = editor.selection.active;
          if (document.languageId === `rpgle`) {
            const linePieces = document.lineAt(position.line).text.trim().split(` `);
            if ([`/COPY`, `/INCLUDE`].includes(linePieces[0].toUpperCase())) {
              const {uri} = await Parser.getContent(document.uri, linePieces[1]);
  
              if (uri) {
                vscode.workspace.openTextDocument(uri).then(doc => {
                  vscode.window.showTextDocument(doc);
                });
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
            Parser.getDocs(document.uri).then(docs => {
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
            vscode.window.showErrorMessage(`You can only get the prototype of RPGLE source.`);
          }
        }
      }), 

      /**
       * When the editor is opened or switch to, we clear the parsed cache
       * so it will scan again incase any of the underlying copybooks have changed
       */
      vscode.window.onDidChangeActiveTextEditor(async (e) => {
        if (e && e.document) {
          if (e.document.languageId === `rpgle`) {
            const document = e.document;
            Parser.clearParsedCache(document.uri.path);
          }
        }
      }),

      /**
       * When the editor is saved, we clear the parsed cache incase
       * any new defintions were added.
       */
      vscode.workspace.onDidSaveTextDocument((document) => {
        if (document.languageId === `rpgle`) {
          //Else fetch new info from source being edited
          Parser.clearParsedCache(document.uri.path);
        }
      }),

      /**
       * When we change the document, we want to refresh the outline view
       */
      vscode.workspace.onDidChangeTextDocument(event => {
        const document = event.document;

        if (document.isDirty) {
          const currentEditingLine = 
            event.contentChanges.length === 1 && 
            event.contentChanges[0].range.isSingleLine && 
            !event.contentChanges[0].text.includes(`\n`)
              ? event.contentChanges[0].range.start.line : undefined;
          
          // Refresh the document when we change a different line
          if (this.lineEditedBefore === undefined || currentEditingLine !== this.lineEditedBefore) {
            clearTimeout(this.editTimeout);
            this.editTimeout = setTimeout(() => {
              console.log(`refresh`);
              Parser.clearParsedCache(document.uri.path);
            }, 500);
          }

          this.lineEditedBefore = currentEditingLine;
        }
      }),

      vscode.languages.registerHoverProvider({language: `rpgle`}, {
        provideHover: async (document, position, token) => {
          const text = document.getText();
          const doc = await Parser.getDocs(document.uri, text);
          const range = document.getWordRangeAtPosition(position);
          const word = document.getText(range).toUpperCase();

          const linePieces = document.lineAt(position.line).text.trim().split(` `);
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
              markdown += `\n  ${procedure.subItems.map(parm => `${parm.name}: ${parm.keywords.join(` `).trim()}`).join(`,\n  `)}\n`;
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
          } else {
            // If they're typing inside of a procedure, let's get the stuff from there too
            const currentProcedure = doc.procedures.find(proc => range.start.line >= proc.range.start && range.start.line <= proc.range.end);
            let theVariable;

            if (currentProcedure) {
              theVariable = currentProcedure.scope.find(word);
            }

            if (!theVariable) {
              theVariable = doc.find(word);
            }

            if (theVariable) {
              // Variable definition found
              return new vscode.Hover(
                new vscode.MarkdownString(
                  `\`${theVariable.name}\`: \`${theVariable.keywords.join(` `).trim()}\``
                )
              )

            } else {
              if ([`/COPY`, `/INCLUDE`].includes(linePieces[0].toUpperCase())) {
                const include = await Parser.getContent(document.uri, linePieces[1]);
    
                return new vscode.Hover(
                  new vscode.MarkdownString(
                    (include.path ? `\`${include.path}\`` : linePieces[1]) + `(${include.type}${include.found ? `` : `, not found`})`
                  )
                )
              }
            }
          }

          return null;
        }
      }),

      /**
       * Used for the Outline view
       */
      vscode.languages.registerDocumentSymbolProvider({ language: `rpgle` }, 
        {
          provideDocumentSymbols: async (document, token) => {
            const text = document.getText();
            
            /** @type {Cache} */
            let doc;

            try {
              doc = await Parser.getDocs(document.uri, text);

              if (!doc) {
                Output.write(`Error parsing ${document.uri.path}: Usually indicates scoping issue.`);
                return [];
              }
            } catch (e) {
              Output.write(`Error parsing ${document.uri.path}: ${e.message}`);
              Output.write(e.stack);
              console.log(e);
              return [];
            }

            const currentPath = document.uri.path;
            /** @type vscode.DocumentSymbol[] */
            const betterDefs = [];

            /**
             * @param {Cache} scope
             * @returns {vscode.DocumentSymbol[]}
             */
            const getScopeVars = (scope) => {
              /** @type vscode.DocumentSymbol[] */
              const currentScopeDefs = [];

              currentScopeDefs.push(
                ...scope.subroutines.filter(sub => sub.position && sub.position.path === currentPath)
                  .map(def => new vscode.DocumentSymbol(
                    def.name,
                    def.keywords.join(` `).trim(), 
                    vscode.SymbolKind.Function,
                    new vscode.Range(def.range.start, 0, def.range.end, 0),
                    new vscode.Range(def.range.start, 0, def.range.start, 0),
                  )),

                ...scope.variables
                  .filter(variable => variable.position && variable.position.path === currentPath)
                  .map(def => new vscode.DocumentSymbol(
                    def.name, 
                    def.keywords.join(` `).trim(), 
                    vscode.SymbolKind.Variable,
                    new vscode.Range(def.position.line, 0, def.position.line, 0),
                    new vscode.Range(def.position.line, 0, def.position.line, 0)
                  )),

                ...scope.constants
                  .filter(constant => constant.position && constant.position.path === currentPath)
                  .map(def => new vscode.DocumentSymbol(
                    def.name, 
                    def.keywords.join(` `).trim(), 
                    vscode.SymbolKind.Constant,
                    new vscode.Range(def.position.line, 0, def.position.line, 0),
                    new vscode.Range(def.position.line, 0, def.position.line, 0)
                  ))
              );

              scope.files
                .filter(struct => struct.position && struct.position.path === currentPath)
                .forEach(file => {
                  const fileDef = new vscode.DocumentSymbol(
                    file.name,
                    file.keywords.join(` `).trim(),
                    vscode.SymbolKind.File,
                    new vscode.Range(file.position.line, 0, file.position.line, 0),
                    new vscode.Range(file.position.line, 0, file.position.line, 0)
                  );

                  file.subItems
                    .filter(recordFormat => recordFormat.position && recordFormat.position.path === currentPath)
                    .forEach(recordFormat => {
                      const recordFormatDef = new vscode.DocumentSymbol(
                        recordFormat.name,
                        recordFormat.keywords.join(` `).trim(),
                        vscode.SymbolKind.Struct,
                        new vscode.Range(recordFormat.position.line, 0, recordFormat.position.line, 0),
                        new vscode.Range(recordFormat.position.line, 0, recordFormat.position.line, 0)
                      );
  
                      recordFormatDef.children.push(
                        ...recordFormat.subItems
                          .filter(subitem => subitem.position && subitem.position.path === currentPath)
                          .map(subitem => new vscode.DocumentSymbol(
                            subitem.name,
                            subitem.keywords.join(` `).trim(),
                            vscode.SymbolKind.Property,
                            new vscode.Range(subitem.position.line, 0, subitem.position.line, 0),
                            new vscode.Range(subitem.position.line, 0, subitem.position.line, 0)
                          ))
                      );

                      fileDef.children.push(recordFormatDef);
                    });

                  currentScopeDefs.push(fileDef);
                });

              scope.structs
                .filter(struct => struct.position && struct.position.path === currentPath)
                .forEach(struct => {
                  const structDef = new vscode.DocumentSymbol(
                    struct.name,
                    struct.keywords.join(` `).trim(),
                    vscode.SymbolKind.Struct,
                    new vscode.Range(struct.range.start, 0, struct.range.end, 0),
                    new vscode.Range(struct.range.start, 0, struct.range.start, 0),
                  );

                  structDef.children.push(
                    ...struct.subItems
                      .filter(subitem => subitem.position && subitem.position.path === currentPath)
                      .map(subitem => new vscode.DocumentSymbol(
                        subitem.name,
                        subitem.keywords.join(` `).trim(),
                        vscode.SymbolKind.Property,
                        new vscode.Range(subitem.position.line, 0, subitem.position.line, 0),
                        new vscode.Range(subitem.position.line, 0, subitem.position.line, 0)
                      ))
                  );

                  currentScopeDefs.push(structDef);
                });

              return currentScopeDefs;
            }

            betterDefs.push(
              ...getScopeVars(doc),
            );

            doc.procedures
              .filter(proc => proc.position && proc.position.path === currentPath)
              .forEach(proc => {
                const procDef = new vscode.DocumentSymbol(
                  proc.name,
                  proc.keywords.join(` `).trim(),
                  vscode.SymbolKind.Function,
                  new vscode.Range(proc.range.start, 0, proc.range.end, 0),
                  new vscode.Range(proc.range.start, 0, proc.range.start, 0),
                );

                procDef.children.push(
                  ...proc.subItems
                    .filter(subitem => subitem.position && subitem.position.path === currentPath)
                    .map(subitem => new vscode.DocumentSymbol(
                      subitem.name,
                      subitem.keywords.join(` `).trim(),
                      vscode.SymbolKind.Property,
                      new vscode.Range(subitem.position.line, 0, subitem.position.line, 0),
                      new vscode.Range(subitem.position.line, 0, subitem.position.line, 0)
                    ))
                );

                if (proc.scope) {
                  procDef.children.push(
                    ...getScopeVars(proc.scope)
                  );
                }

                betterDefs.push(procDef);
              });

            return betterDefs;
          }
        }),

      /**
       * This implements 'Go to definition' and 'Peek definition' for RPGLE
       */
      vscode.languages.registerDefinitionProvider({ language: `rpgle` }, {
        provideDefinition: async (document, position, token) => {
          const doc = await Parser.getDocs(document.uri);
          const range = document.getWordRangeAtPosition(position);
          const line = position.line;
          const word = document.getText(range).toUpperCase();

          if (doc) {
            // If they're typing inside of a procedure, let's get the stuff from there too
            const currentProcedure = doc.procedures.find(proc => line >= proc.range.start && line <= proc.range.end);
            let def;
            
            if (currentProcedure) {
              // First look at all definitions
              def = currentProcedure.scope.find(word);

              if (!def) {
                // Maybe it's a parameter?
                def = currentProcedure.subItems.find(subitem => subitem.name.toUpperCase() === word);
              }
            }
            
            if (!def) {
              def = doc.find(word);
            }
            
            if (def) {
              let {path, type, uri} = await Parser.getContent(document.uri, def.position.path);
              if (document.uri.path.startsWith(path)) {
                return new vscode.Location(
                  document.uri,
                  new vscode.Range(def.position.line, 0, def.position.line, 0)
                );

              } else {
                if (uri) {
                  return new vscode.Location(
                    uri,
                    new vscode.Range(def.position.line, 0, def.position.line, 0)
                  );
                }
              }
            }
          }
        }}
      ),

      vscode.languages.registerRenameProvider({ language: `rpgle`}, {
        prepareRename: async (document, position, cancelToken) => {
          const range = document.getWordRangeAtPosition(position);
          const lineNumber = position.line;
          const word = document.getText(range).toUpperCase();

          const isFree = (document.getText(new vscode.Range(0, 0, 0, 6)).toUpperCase() === `**FREE`);
          if (isFree) {
            const docs = await Parser.getDocs(document.uri);
            const text = document.getText();

            // Updates docs
            Linter.getErrors({
              uri: document.uri,
              content: text
            }, {
              CollectReferences: true,
            }, docs);
            
            const currentPath = document.uri.path;
            const definition = await LanguageWorker.findDefintion(document, position, word);

            if (definition) {
              if (definition.position && definition.position.path === currentPath) {
                // We don't store the defintion range, just line number
                const defIndex = document.lineAt(definition.position.line).text.indexOf(definition.name);
                const currentReference = definition.references.find(ref => ref.range.start.line === lineNumber);

                if (currentReference) {
                  return {
                    range: Generic.calculateOffset(document, currentReference),
                    placeholder: definition.name
                  };
                } else {
                  return {
                    range: new vscode.Range(definition.position.line, defIndex, definition.position.line, defIndex+definition.name.length),
                    placeholder: definition.name
                  };
                }
              }
            }
          }

          throw new Error(`Unable to find variable definition.`);
        },
        provideRenameEdits: async (document, position, newName, cancelToken) => {
          // We assume it is **free at this point.
          const range = document.getWordRangeAtPosition(position);
          const word = document.getText(range).toUpperCase();

          const docs = await Parser.getDocs(document.uri);
          const text = document.getText();

          // Updates docs
          Linter.getErrors({
            uri: document.uri,
            content: text
          }, {
            CollectReferences: true,
          }, docs);
          
          const currentPath = document.uri.path;
          const definition = await LanguageWorker.findDefintion(document, position, word);

          if (definition) {
            if (definition.position && definition.position.path === currentPath) {
              const edits = new vscode.WorkspaceEdit();
              // We don't store the defintion range, just line number
              const defIndex = document.lineAt(definition.position.line).text.indexOf(definition.name);

              edits.replace(document.uri, new vscode.Range(definition.position.line, defIndex, definition.position.line, defIndex+definition.name.length), newName);
              definition.references.forEach(ref => {
                console.log(ref.range);
                edits.replace(document.uri, Generic.calculateOffset(document, ref), newName);
              })

              return edits;
            }
          }
        },
      }),

      /**
       * This implements 'Find references' and 'Peek references' for RPGLE
       */
      vscode.languages.registerReferenceProvider({ language: `rpgle` }, {
        provideReferences: async (document, position, context, token) => {
          /** @type {vscode.Location[]} */
          let refs = [];

          const range = document.getWordRangeAtPosition(position);
          const word = document.getText(range).toUpperCase();
            
          const isFree = (document.getText(new vscode.Range(0, 0, 0, 6)).toUpperCase() === `**FREE`);
          if (isFree) {
            try {
              const declaration = await LanguageWorker.findDefintion(document, position, word);

              if (declaration) {
                declaration.references.forEach(ref => {
                  refs.push(new vscode.Location(
                    document.uri, 
                    Generic.calculateOffset(document, ref)
                  ))
                });
              }
            } catch (e) {
              console.log(e);
            }
          } else {
            vscode.window.showInformationMessage(`You can only use 'Find references' with RPGLE free-format (**FREE)`);
          }
          
          return refs;
        }
      }),
      
      /**
       * Provides content assist when writing code
       */
      vscode.languages.registerCompletionItemProvider({language: `rpgle` }, {
        provideCompletionItems: async (document, position, token, context) => {
          const text = document.getText();
          const lineNumber = position.line;
          const currentLine = document.getText(new vscode.Range(lineNumber, 0, lineNumber, position.character));
          const doc = await Parser.getDocs(document.uri, text);

          /** @type vscode.CompletionItem[] */
          let items = [];
          let item;

          // If they're typing inside of a procedure, let's get the stuff from there too
          const currentProcedure = doc.procedures.find(proc => lineNumber >= proc.range.start && lineNumber <= proc.range.end);

          // This means we're just looking for subfields in the struct
          if (context && context.triggerCharacter === `.`) {
            let currentPosition = new vscode.Position(position.line, position.character - 1);
            let range = document.getWordRangeAtPosition(currentPosition);

            // Uh oh! Maybe we found dim struct?
            if (!range) {
              const startBracket = currentLine.lastIndexOf(`(`, currentPosition.character);

              if (startBracket > -1) {
                currentPosition = new vscode.Position(position.line, startBracket-1);
                range = document.getWordRangeAtPosition(currentPosition);
              }
            }

            if (range) {
              const word = document.getText(range).toUpperCase();

              if (word) {
                let possibleStruct;

                if (currentProcedure && currentProcedure.scope) {
                  const procScop = currentProcedure.scope;

                  possibleStruct = currentProcedure.subItems.find(subitem => subitem.name.toUpperCase() === word && subitem.subItems.length > 0);

                  if (!possibleStruct) {
                    possibleStruct = procScop.structs.find(struct => struct.name.toUpperCase() === word);
                  }
                }

                if (!possibleStruct) {
                  possibleStruct = doc.structs.find(struct => struct.name.toUpperCase() === word);
                }

                if (possibleStruct) {
                  if (possibleStruct.keyword[`QUALIFIED`]) {
                    possibleStruct.subItems.forEach(subItem => {
                      item = new vscode.CompletionItem(`${subItem.name}`, vscode.CompletionItemKind.Property);
                      item.insertText = new vscode.SnippetString(`${subItem.name}\$0`);
                      item.detail = subItem.keywords.join(` `);
                      item.documentation = subItem.description + ` (${possibleStruct.name})`;
                      items.push(item);
                    });
                  }
                }
              }
            }

          } else {

            if (currentLine.startsWith(`//`)) {
              for (const tag in possibleTags) {
                item = new vscode.CompletionItem(`@${tag}`, vscode.CompletionItemKind.Property);
                item.insertText = new vscode.SnippetString(`@${tag} $0`);
                item.detail = possibleTags[tag];
                items.push(item);
              }

            } else {
              /**
               * @param {Cache} localCache 
               */
              const expandScope = (localCache) => {
                for (const procedure of localCache.procedures) {
                  item = new vscode.CompletionItem(`${procedure.name}`, vscode.CompletionItemKind.Function);
                  item.insertText = new vscode.SnippetString(`${procedure.name}(${procedure.subItems.map((parm, index) => `\${${index+1}:${parm.name}}`).join(`:`)})\$0`)
                  item.detail = procedure.keywords.join(` `);
                  item.documentation = procedure.description;
                  items.push(item);
                }
  
                for (const subroutine of localCache.subroutines) {
                  item = new vscode.CompletionItem(`${subroutine.name}`, vscode.CompletionItemKind.Function);
                  item.insertText = new vscode.SnippetString(`${subroutine.name}\$0`);
                  item.documentation = subroutine.description;
                  items.push(item);
                }
  
                for (const variable of localCache.variables) {
                  item = new vscode.CompletionItem(`${variable.name}`, vscode.CompletionItemKind.Variable);
                  item.insertText = new vscode.SnippetString(`${variable.name}\$0`);
                  item.detail = variable.keywords.join(` `);
                  item.documentation = variable.description;
                  items.push(item);
                }
  
                localCache.files.forEach(file => {
                  item = new vscode.CompletionItem(`${file.name}`, vscode.CompletionItemKind.File);
                  item.insertText = new vscode.SnippetString(`${file.name}\$0`);
                  item.detail = file.keywords.join(` `);
                  item.documentation = file.description;
                  items.push(item);

                  for (const struct of file.subItems) {
                    item = new vscode.CompletionItem(`${struct.name}`, vscode.CompletionItemKind.Struct);
                    item.insertText = new vscode.SnippetString(`${struct.name}\$0`);
                    item.detail = struct.keywords.join(` `);
                    item.documentation = struct.description;
                    items.push(item);
    
                    if (!struct.keyword[`QUALIFIED`]) {
                      struct.subItems.forEach(subItem => {
                        item = new vscode.CompletionItem(`${subItem.name}`, vscode.CompletionItemKind.Property);
                        item.insertText = new vscode.SnippetString(`${subItem.name}\$0`);
                        item.detail = subItem.keywords.join(` `);
                        item.documentation = subItem.description + ` (${struct.name})`;
                        items.push(item);
                      });
                    }
                  }
                });

                for (const struct of localCache.structs) {
                  item = new vscode.CompletionItem(`${struct.name}`, vscode.CompletionItemKind.Struct);
                  item.insertText = new vscode.SnippetString(`${struct.name}\$0`);
                  item.detail = struct.keywords.join(` `);
                  item.documentation = struct.description;
                  items.push(item);
  
                  if (!struct.keyword[`QUALIFIED`]) {
                    struct.subItems.forEach(subItem => {
                      item = new vscode.CompletionItem(`${subItem.name}`, vscode.CompletionItemKind.Property);
                      item.insertText = new vscode.SnippetString(`${subItem.name}\$0`);
                      item.detail = subItem.keywords.join(` `);
                      item.documentation = subItem.description + ` (${struct.name})`;
                      items.push(item);
                    });
                  }
                }
  
                for (const constant of localCache.constants) {
                  item = new vscode.CompletionItem(`${constant.name}`, vscode.CompletionItemKind.Constant);
                  item.insertText = new vscode.SnippetString(`${constant.name}\$0`);
                  item.detail = constant.keywords.join(` `);
                  item.documentation = constant.description;
                  items.push(item);
                }
              }

              expandScope(doc);


              if (currentProcedure) {
                for (const subItem of currentProcedure.subItems) {
                  item = new vscode.CompletionItem(`${subItem.name}`, vscode.CompletionItemKind.Variable);
                  item.insertText = new vscode.SnippetString(`${subItem.name}\$0`);
                  item.detail = [`parameter`, ...subItem.keywords].join(` `);
                  item.documentation = subItem.description;
                  items.push(item);
                }

                if (currentProcedure.scope) {
                  expandScope(currentProcedure.scope);
                }
              }
            }
          }

          return items;
        }
      }, `.`),
    )
  }

  /**
   * 
   * @param {vscode.TextDocument} document 
   * @param {vscode.Position} currentPosition 
   * @param {string} word 
   * @return {Promise<Declaration>}
   */
  static async findDefintion(document, currentPosition, word) {
    const lineNumber = currentPosition.line;
    const docs = await Parser.getDocs(document.uri);
    const text = document.getText();

    // Updates docs
    Linter.getErrors({
      uri: document.uri,
      content: text
    }, {
      CollectReferences: true,
    }, docs);

    // If they're typing inside of a procedure, let's get the stuff from there too
    const currentProcedure = docs.procedures.find(proc => lineNumber >= proc.range.start && lineNumber <= proc.range.end);

    if (currentProcedure) {
      const localDef = currentProcedure.scope.find(word);

      if (localDef) {
        return localDef;
      }
    }

    const globalDef = docs.find(word);

    if (globalDef) {
      return globalDef;
    }
  }
}