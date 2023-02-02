
const assert = require(`assert`);

const {default: parserSetup} = require(`../parserSetup`);
const uri = `source.rpgle`;

exports.issue_202 = async () => {
  const lines = [
    `**free`,
    `///`,
    `// Transform to lowercase`,
    `// This procedure will take a string and transform it to lowercase`,
    `//`,
    `// @param The string`,
    `// @return The lowercase value`,
    `///`,
    `Dcl-Proc ToLower Export;`,
    `  Dcl-Pi *N Char(20);`,
    `    stringIn Char(20);`,
    `  End-pi;`,
    ``,
    `  return STRLOWER(stringIn);`,
    `End-Proc;`,
  ].join(`\n`);

  const parser = parserSetup();
  const cache = await parser.getDocs(uri, lines);

  const toLower = cache.find(`ToLower`);

  assert.strictEqual(toLower.description, `This procedure will take a string and transform it to lowercase`);

  const tags = toLower.tags;
  assert.deepStrictEqual(tags[0], {
    tag: `param`,
    content: `The string`
  });

  assert.deepStrictEqual(tags[1], {
    tag: `return`,
    content: `The lowercase value`
  });

  const stringInParam = toLower.subItems[0];
  assert.strictEqual(stringInParam.description, `The string`);
}