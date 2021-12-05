
const vscode = require(`vscode`);
const Parser = require(`../../src/parser`);
const Linter = require(`../../src/linter`);

console.log(vscode);
const URI = vscode.Uri.parse(`source.rpgle`);

module.exports = {
  test1: async () => {
    const lines = [
      `**FREE`,
      `Dcl-s MyVariable Char(20);`
    ];

    const parser = new Parser();
    const cache = await parser.getDocs(URI, lines.join(`\n`));

    console.log(cache);
  }
}