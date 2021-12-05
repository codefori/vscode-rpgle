
const vscode = require(`vscode`);
const assert = require(`assert`);

const Parser = require(`../../src/parser`);
const Linter = require(`../../src/linter`);

const URI = vscode.Uri.parse(`source.rpgle`);

module.exports = {
  test1: async () => {
    const lines = [
      `**FREE`,
      `Dcl-s MyVariable Char(20);`
    ].join(`\n`);

    const parser = new Parser();
    const cache = await parser.getDocs(URI, lines);

    assert.strictEqual(cache.variables.length, 1, `Expect length of 1`);
  },

  test2: async () => {
    const lines = [
      `**FREE`,
      `Dcl-s MyVariable Char(20);`,
      `Dcl-s MyVariable2 Char(20);`
    ].join(`\n`);

    const parser = new Parser();
    const cache = await parser.getDocs(URI, lines);

    assert.strictEqual(cache.variables.length, 2, `Expect length of 2`);
  },

  test3: async () => {
    const lines = [
      `Dcl-s MyVariable2 Char(20);`,
      `Dcl-Ds astructure qualified;`,
      `  Subitem1 Char(20);`,
      `  Subitem2 Char(20);`,
      `End-ds;`,
      `Dcl-s MyVariable Char(20);`,
      `//Yes`
    ].join(`\n`);

    const parser = new Parser();
    const cache = await parser.getDocs(URI, lines);

    assert.strictEqual(cache.variables.length, 2, `Expect length of 2`);
    assert.strictEqual(cache.structs.length, 1, `Expect length of 1`);
    
    assert.strictEqual(cache.structs[0].subItems.length, 2, `Expect length of 2 subitems`);
  }
}