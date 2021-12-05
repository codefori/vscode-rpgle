// Force a polyfill for the test

let Module = require(`module`);
let originalRequire = Module.prototype.require;

/** @ts-ignore */
Module.prototype.require = function(){
  //do your thing here
  switch (arguments[0]) {
  case `vscode`:
    return originalRequire.apply(this, [`vscode-test`]);
  default:
    return originalRequire.apply(this, arguments);
  }
};

const tests = require(`./parser/test1`);

console.log(`Running tests...`);

const run = async () => {
  const testNames = Object.keys(tests);
  console.log(`Tests: ${testNames.join(`, `)}`);

  for (const testName of testNames) {
    const test = tests[testName];
    console.log(`Running test: ${testName}`);
    await test();
  }
}