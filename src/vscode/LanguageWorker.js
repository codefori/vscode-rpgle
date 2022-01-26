
const vscode = require(`vscode`);

const possibleTags = require(`../language/models/tags`);

const Linter = require(`../language/linter`);
const Generic = require(`../language/generic`);

const { Parser } = require(`../parser`);

module.exports = class LanguageWorker {
  /**
   * @param {vscode.ExtensionContext} context
   */
  constructor(context) {
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

    context.subscriptions.push(
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
                const {type, memberPath, finishedPath} = Generic.getPathInfo(document.uri, linePieces[1]);
    
                return new vscode.Hover(
                  new vscode.MarkdownString(
                    `\`'${finishedPath}'\` (${type})`
                  )
                )
              }
            }
          }

          return null;
        }
      }),

      vscode.languages.registerDocumentSymbolProvider({ language: `rpgle` }, 
        {
          provideDocumentSymbols: async (document, token) => {
            const text = document.getText();
            const doc = await Parser.getDocs(document.uri, text);

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

            doc.structs
              .filter(struct => struct.position && struct.position.path === currentPath)
              .forEach(struct => {
                currentDefs.push(
                  new vscode.SymbolInformation(
                    struct.name,
                    vscode.SymbolKind.Struct,
                    new vscode.Range(struct.position.line, 0, struct.position.line, 0),
                    document.uri
                  )
                );

                if (!struct.keywords.includes(`QUALIFIED`)) {
                  currentDefs.push(
                    ...struct.subItems
                      .filter(cStruct => cStruct.position && cStruct.position.path === currentPath)
                      .map(def => new vscode.SymbolInformation(
                        def.name,
                        vscode.SymbolKind.Property,
                        new vscode.Range(def.position.line, 0, def.position.line, 0),
                        document.uri
                      )))
                }
              });

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
        }),

      vscode.languages.registerDefinitionProvider({ language: `rpgle` }, {
        provideDefinition: async (document, position, token) => {
          const doc = await Parser.getDocs(document.uri);
          const range = document.getWordRangeAtPosition(position);
          const line = position.line;
          const word = document.getText(range).toUpperCase();

          if (doc) {
            // If they're typing inside of a procedure, let's get the stuff from there too
            const currentProcedure = doc.procedures.find(proc => range.start.line >= proc.range.start && range.start.line <= proc.range.end);
            let def;
            
            if (currentProcedure) {
              def = currentProcedure.scope.find(word);
            }
            
            if (!def) {
              def = doc.find(word);
            }
            
            if (def) {
              let {finishedPath, type} = Generic.getPathInfo(document.uri, def.position.path);
              if (document.uri.path.startsWith(finishedPath)) {
                return new vscode.Location(
                  document.uri,
                  new vscode.Range(def.position.line, 0, def.position.line, 0)
                );

              } else {
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
        }}
      ),

      vscode.languages.registerReferenceProvider({ language: `rpgle` }, {
        provideReferences: async (document, position, context, token) => {

          /** @type {vscode.Location[]} */
          let refs = [];

          const range = document.getWordRangeAtPosition(position);
          const word = document.getText(range).toUpperCase();
            
          const lineNumber = position.line;
          const isFree = (document.getText(new vscode.Range(0, 0, 0, 6)).toUpperCase() === `**FREE`);
          const text = document.getText();
          if (isFree) {
            const docs = await Parser.getDocs(document.uri);

            // Updates docs
            Linter.getErrors(text, {
              CollectReferences: true,
            }, docs);

            // If they're typing inside of a procedure, let's get the stuff from there too
            const currentProcedure = docs.procedures.find(proc => lineNumber >= proc.range.start && lineNumber <= proc.range.end);

            if (currentProcedure) {
              const localDef = currentProcedure.scope.find(word);

              if (localDef) {
                localDef.references.forEach(ref => {
                  refs.push(new vscode.Location(
                    document.uri, 
                    Generic.calculateOffset(document, ref)
                  ))
                });
              }
            }

            // Otherwise, maybe it's a global variable
            if (refs.length === 0) {
              const globalDef = docs.find(word);

              if (globalDef) {
                globalDef.references.forEach(ref => {
                  refs.push(new vscode.Location(
                    document.uri, 
                    Generic.calculateOffset(document, ref)
                  ))
                });
              }
            }
          }
          
          return refs;
        }
      }),

      vscode.languages.registerCompletionItemProvider({language: `rpgle`, }, {
        provideCompletionItems: async (document, position) => {
          const text = document.getText();
          const lineNumber = position.line;
          const currentLine = document.getText(new vscode.Range(lineNumber, 0, lineNumber, position.character));
          const doc = await Parser.getDocs(document.uri, text);

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

              if (!struct.keywords.includes(`QUALIFIED`)) {
                struct.subItems.forEach(subItem => {
                  item = new vscode.CompletionItem(`${subItem.name}`, vscode.CompletionItemKind.Property);
                  item.insertText = new vscode.SnippetString(`${subItem.name}\$0`);
                  item.detail = subItem.keywords.join(` `);
                  item.documentation = subItem.description + ` (${struct.name})`;
                  items.push(item);
                });
              }
            }

            for (const constant of doc.constants) {
              item = new vscode.CompletionItem(`${constant.name}`, vscode.CompletionItemKind.Constant);
              item.insertText = new vscode.SnippetString(`${constant.name}\$0`);
              item.detail = constant.keywords.join(` `);
              item.documentation = constant.description;
              items.push(item);
            }

            // If they're typing inside of a procedure, let's get the stuff from there too
            const currentProcedure = doc.procedures.find(proc => lineNumber >= proc.range.start && lineNumber <= proc.range.end);

            if (currentProcedure) {
              for (const subItem of currentProcedure.subItems) {
                item = new vscode.CompletionItem(`${subItem.name}`, vscode.CompletionItemKind.Variable);
                item.insertText = new vscode.SnippetString(`${subItem.name}\$0`);
                item.detail = [`parameter`, ...subItem.keywords].join(` `);
                item.documentation = subItem.description;
                items.push(item);
              }

              if (currentProcedure.scope) {
                const scope = currentProcedure.scope;
                for (const variable of scope.variables) {
                  item = new vscode.CompletionItem(`${variable.name}`, vscode.CompletionItemKind.Variable);
                  item.insertText = new vscode.SnippetString(`${variable.name}\$0`);
                  item.detail = variable.keywords.join(` `);
                  item.documentation = variable.description;
                  items.push(item);
                }
  
                for (const struct of scope.structs) {
                  item = new vscode.CompletionItem(`${struct.name}`, vscode.CompletionItemKind.Struct);
                  item.insertText = new vscode.SnippetString(`${struct.name}\$0`);
                  item.detail = struct.keywords.join(` `);
                  item.documentation = struct.description;
                  items.push(item);

                  if (!struct.keywords.includes(`QUALIFIED`)) {
                    struct.subItems.forEach(subItem => {
                      item = new vscode.CompletionItem(`${subItem.name}`, vscode.CompletionItemKind.Property);
                      item.insertText = new vscode.SnippetString(`${subItem.name}\$0`);
                      item.detail = subItem.keywords.join(` `);
                      item.documentation = subItem.description + ` (${struct.name})`;
                      items.push(item);
                    });
                  }
                }
  
                for (const constant of scope.constants) {
                  item = new vscode.CompletionItem(`${constant.name}`, vscode.CompletionItemKind.Constant);
                  item.insertText = new vscode.SnippetString(`${constant.name}\$0`);
                  item.detail = constant.keywords.join(` `);
                  item.documentation = constant.description;
                  items.push(item);
                }
              }
            }
          }

          return items;
        }
      }),
    )
  }
}