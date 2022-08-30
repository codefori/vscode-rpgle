// This script can be used to generate ILEExports from ./protos.rpgle into ./output.json

// 1. $ cd tests/exports
// 2. create ./protos.rpgle
// 3. $ node index
// 4. take ./output.json and add it in ILEExports.js

const vscode = require(`../models/vscode`);

// Force a polyfill for the test
let Module = require(`module`);
let originalRequire = Module.prototype.require;

/** @ts-ignore */
Module.prototype.require = function(){
  // We have to re-implement some VS Code APIs
  // due to the fact we have to support 3 file
  // systems (file, member, streamfile)
  switch (arguments[0]) {
  case `vscode`:
    return vscode;
  default:
    return originalRequire.apply(this, arguments);
  }
};

const fs = require(`fs/promises`);

const Parser = require(`../../src/language/parser`);
const Linter = require(`../../src/language/linter`);
const path = require(`path`);

const uri = vscode.Uri.parse(`source.rpgle`);

const doWork = async () => {
  const content = await  fs.readFile(`./protos.rpgle`, {encoding: `utf-8`});

  const parser = new Parser();
  const cache = await parser.getDocs(uri, content);

  /** @type {{[name: string]: APIInterface}} */
  const results = {};
  cache.procedures.forEach(proc => {
    let detail = Object.keys(proc.keyword).filter(k => !k.startsWith(`EXT`)).map(k => {
      if (proc.keyword[k] === true) {
        return k;
      } else {
        return `${k}(${proc.keyword[k]})`;
      }
    }).join(` `);

    if (detail === ``) detail = `void`;

    let parms = ``;
    if (proc.subItems.length > 0)
      parms = `\n${proc.subItems.map((p, i) => `  \${${i+1}:${p.name}}`).join(`:\n`)}\n`;
    
    results[proc.name] = {
      type: `function`,
      description: proc.description,
      detail,
      insertText: `${proc.name}(${parms})\$0`,
      prototype: [
        `///`,
        `// ${proc.name}`,
        `// ${proc.description}`,
        `///`,
        `Dcl-Pr ${proc.name} ${proc.keywords.join(` `)};`,
        ...proc.subItems.map(subItem => 
          `  ${subItem.name} ${subItem.keywords.join(` `)};`
        ),
        `End-Pr;`
      ]
    }
  });

  await fs.writeFile(`./output.json`, JSON.stringify(results, null, 2), {
    encoding: `utf-8`
  });
}

doWork();