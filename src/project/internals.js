const vscode = require(`vscode`);

const Linter = require(`../language/linter`);
const {Parser} = require(`../parser`);

exports.initialise = async () => {
  const hasWorkspace = vscode.workspace.workspaceFolders.length > 0;

  if (hasWorkspace) {
    const sources = await vscode.workspace.findFiles(`**/*.{rpgle,RPGLE,sqlrpgle,SQLRPGLE,rpgleinc,RPGLEINC}`);
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

/**
 * @param {string} name 
 */
exports.findProgramFile = (name) => {
  const lowerName = name.toLowerCase() + `.pgm.`;
  const foundUriKey = Object.keys(Parser.parsedCache).find(keyPath => keyPath.toLowerCase().includes(lowerName));

  return foundUriKey;
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

  try {
    for (const keyPath of parsedFiles) {
      const cache = Parser.getParsedCache(keyPath);
      for (const proc of cache.procedures) {
        const keyword = proc.keyword[`EXPORT`];
        if (keyword) {
          if (proc.name.toUpperCase() === upperName) {
            return proc.position;
          }
        }
      }
    }
  } catch (e) {
    console.log(e);
  }
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
        if (this.trimQuotes(keyword).toUpperCase() === upperName) {
          addReference = true;
        }
      } else
      if (type === `function` && !proc.keyword[`EXPORT`]) {
        if (proc.name.toUpperCase() === upperName) {
          addReference = true;
        }
      }

      if (addReference) {
        // Don't add duplicates
        if (!references.some(ref => ref.path === proc.position.path)) {
          references.push(proc.position);
          references.push(...proc.references.map(ref => ({path: keyPath, line: ref.range.start.line})));
        }
      }
    })
  });

  return references;
}

/**
 * @param {string} input 
 */
exports.trimQuotes = input => {
  if (input[0] === `'`) input = input.substring(1);
  if (input[input.length - 1] === `'`) input = input.substring(0, input.length - 1);
  return input;
}