
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

const specificTests = process.argv[2];

async function run() {
  console.log(`Running tests...`);

  const suite = require(`./suite`);
  let testNames = Object.keys(suite);

  if (specificTests) {
    testNames = testNames.filter(name => name.startsWith(specificTests));
  }

  console.log(`Running ${testNames.length} tests: ${testNames.join(`, `)}`);

  for (const testName of testNames) {
    const test = suite[testName];
    console.log(`Running ${testName}`);
    await test();
  }
};

run();