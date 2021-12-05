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

const Parser = require(`../src/parser`);
const Linter = require(`../src/linter`);

console.log(`Running tests...`);