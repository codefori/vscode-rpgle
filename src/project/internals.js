const vscode = require(`vscode`);

const Linter = require(`../language/linter`);
const {Parser} = require(`../parser`);

exports.initialise = async () => {
  const hasWorkspace = vscode.workspace.workspaceFolders.length > 0;

  if (hasWorkspace) {
    const sources = await vscode.workspace.findFiles(`**/*.{rpgle,sqlrpgle}`);
    this.parseUris(sources);
  }
}

/**
 * @param {vscode.Uri[]} sources 
 */
exports.parseUris = async (sources) => {
  const documents = await Promise.allSettled(sources.map(uri => vscode.workspace.openTextDocument(uri)));

  documents.forEach(docPromise => {
    if (docPromise.status === `fulfilled`) {
      const document = docPromise.value;
      const content = document.getText();
      Parser.getDocs(document.uri, content).then(cache => {

        // Linter / reference collector only works on free-format.
        if (content.length >= 6 && content.substring(0, 6).toUpperCase() === `**FREE`) {
          Linter.getErrors({
            uri: document.uri,
            content,
          }, {
            CollectReferences: true
          }, cache);
        }
      });
    }
  });
}

const typeKeyword = {
  program: `EXTPGM`,
  function: `EXTPROC`
};

/**
 * @param {string} name 
 * @returns {DefinitionPosition|void}
 */
exports.findExportDefinition = (name) => {  
  const upperName = name.toUpperCase();
  const parsedFiles = Object.keys(Parser.parsedCache);

  parsedFiles.forEach(keyPath => {
    const cache = Parser.getParsedCache(keyPath);

    cache.procedures.forEach(proc => {
      const keyword = proc.keyword[`EXPORT`];
      if (keyword) {
        if (proc.name.toUpperCase() === upperName) {
          return proc.position;
        }
      }
    })
  });
}

/**
 * @param {"program"|"function"} type 
 * @param {string} name 
 * @returns {DefinitionPosition[]}
 */
exports.findOtherPrototypes = (type, name) => {
  /** @type {DefinitionPosition[]} */
  const references = [];

  const upperName = name.toUpperCase();
  const requiredKeyword = typeKeyword[type];
  const parsedFiles = Object.keys(Parser.parsedCache);

  parsedFiles.forEach(keyPath => {
    const cache = Parser.getParsedCache(keyPath);

    cache.procedures.forEach(proc => {
      let addReference = false;
      const keyword = proc.keyword[requiredKeyword];
      if (keyword) {
        if (keyword === true) {
          if (proc.name.toUpperCase() === upperName) {
            
            addReference = true;
          }
        } else
        if (keyword.toUpperCase() === upperName) {
          addReference = true;
        }
      }

      if (addReference) {
        if (keyPath === proc.position.path) {
          // This means the PR is declared in the same source
          references.push(proc.position)
        } else {
          // This means the prototype is defined in an include
          // TODO: review this later
          references.push({
            path: keyPath,
            line: 1
          });
        }
      }
    })
  });

  return references;
}