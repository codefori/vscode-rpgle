const vscode = require(`vscode`);
const Output = require(`../output`);

const Linter = require(`../language/linter`);
const {Parser} = require(`../parser`);

exports.scanWorkspace = async () => {
  // Assumes that workspace exists.
  Output.write(`Project mode: scanning workspace for rpgle, sqlrpgle and rpgleinc.`);

  const sources = await vscode.workspace.findFiles(`**/*.{rpgle,RPGLE,sqlrpgle,SQLRPGLE,rpgleinc,RPGLEINC}`);
  this.parseUris(sources);

  Output.write(`Project mode: Found sources: ${sources.length}`);

  return sources.length > 0;
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
 * @param {vscode.Uri} uri 
 * @returns {vscode.Location[]}
 */
exports.findIncludeReferences = (uri) => {
  const filePath = uri.path;
  
  /** @type {vscode.Location[]} */
  const locations = [];

  const parsedFiles = Object.keys(Parser.parsedCache);

  for (const file of parsedFiles) {
    const cache = Parser.getParsedCache(file);

    const possibleIncludes = cache.includes
      .filter(include => include.toPath === filePath)
      .map(include => new vscode.Location(
        vscode.Uri.from({ scheme: uri.scheme, path: file }),
        new vscode.Range(include.line, 0, include.line, 0)
      ));

    locations.push(...possibleIncludes)
  }

  if (locations.length === 0) {
    vscode.window.showInformationMessage(`No references to this source found.`)
  }

  return locations;
}

/**
 * @param {string} input 
 */
exports.trimQuotes = input => {
  if (input[0] === `'`) input = input.substring(1);
  if (input[input.length - 1] === `'`) input = input.substring(0, input.length - 1);
  return input;
}