
import setupParser from "../parserSetup";
import Linter from "../../language/linter";
import { test, expect } from "vitest";

const parser = setupParser();
const uri = `source.rpgle`;

test("issue_202", async () => {
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

  expect(toLower.description).toBe(`This procedure will take a string and transform it to lowercase`);

  const tags = toLower.tags;
  expect(tags[0]).toEqual({
    tag: `param`,
    content: `The string`
  });

  expect(tags[1]).toEqual({
    tag: `return`,
    content: `The lowercase value`
  });

  const stringInParam = toLower.subItems[0];
  expect(stringInParam.description).toBe(`The string`);
});

test("issue_231", async () => {
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

  expect(indentErrors.length).toBe(0);
  expect(errors.length).toBe(0);
});