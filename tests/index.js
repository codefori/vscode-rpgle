
const vscode = require(`./models/vscode`);

// Force a polyfill for the test

let Module = require(`module`);
let originalRequire = Module.prototype.require;

/** @ts-ignore */
Module.prototype.require = function(){
  // We have to re-implement some VS Code APIs
  // due to the fact we rely on the fact that
  // we have to support 3 file systems (file, member, streamfile)
  switch (arguments[0]) {
  case `vscode`:
    return vscode;
  default:
    return originalRequire.apply(this, arguments);
  }
};

async function run() {
  console.log(`Running tests...`);

  const suite = require(`./suite`);
  const testNames = Object.keys(suite);

  console.log(`Running ${testNames.length} tests:`);
  console.log(testNames.map(name => `\t${name}`).join(`\n`));

  const tests = testNames.map(name => suite[name]);
  Promise.all(tests);
};

run();