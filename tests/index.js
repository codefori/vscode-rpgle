// Force a polyfill for the test

let Module = require(`module`);
let originalRequire = Module.prototype.require;

/** @ts-ignore */
Module.prototype.require = function(){
  //do your thing here
  switch (arguments[0]) {
  case `vscode`:
    const vscode = originalRequire.apply(this, [`vscode-test`]);
    vscode.Uri = require(`vscode-uri`).URI;
    return vscode;
  default:
    return originalRequire.apply(this, arguments);
  }
};

async function run() {
  console.log(`Running tests...`);

  const suite = require(`./suite`);
  const testNames = Object.keys(suite);

  console.log(`Running ${testNames.length} tests: ${testNames.join(`, `)}`);

  for (const testName of testNames) {
    const test = suite[testName];
    console.log(`Running test ${testName}`);
    await test();
  }
};

run();