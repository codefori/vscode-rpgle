
const assert = require(`assert`);

const { default: parserSetup } = require(`../parserSetup`);
const { default: Linter } = require(`../../language/linter`);

const parser = parserSetup();
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

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});

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

exports.issue_231 = async () => {
  const lines = [
    `**FREE`,
    `Ctl-Opt Main(MainLine);`,
    `/// -------------------------------------`,
    `// Main`,
    `/// -------------------------------------`,
    `Dcl-Proc MainLine;`,
    `  Dcl-Pi MainLine Extpgm('MAINTLINE');`,
    `    Iof Char(1);`,
    `  End-Pi;`,
    `  Dcl-S myString Varchar(20);`,
    ``,
    `  myString = CvtToMixed(myString);`,
    `End-Proc;`,
    ``,
    `/// -------------------------------------`,
    `// CvtToMixed`,
    `// Convert the passed string to mixed case or `,
    `// what is normally called Title case.`,
    `// @param  Source string`,
    `// @return  Title cased string`,
    `/// -------------------------------------`,
    `Dcl-Proc CvtToMixed;`,
    `  Dcl-Pi CvtToMixed Extpgm('MAINTLINE');`,
    `    theString Varchar(100);`,
    `  End-Pi;`,
    ``,
    `  return theString;`,
    `End-Proc;`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});

  const { indentErrors, errors } = Linter.getErrors({ uri, content: lines }, {
    indent: 2,
    PrettyComments: true,
  }, cache);

  assert.strictEqual(indentErrors.length, 0);
  assert.strictEqual(errors.length, 0);
}