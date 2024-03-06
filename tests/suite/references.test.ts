import setupParser from "../parserSetup";
import Linter from "../../language/linter";
import Cache from "../../language/models/cache";
import { test, expect } from "vitest";


const parser = setupParser();
const uri = `source.rpgle`;

const lines = [
  `**free`,
  ``,
  `dcl-c FALSE '0';`,
  `dcl-c TRUE '1';`,
  ``,
  `dcl-enum COLORS qualified;`,
  `  GREEN 1;`,
  `  RED 2;`,
  `  YELLOW 3;`,
  `end-enum;`,
  ``,
  `dcl-s var1 varchar(20);`,
  `dcl-s var4 varchar(20);`,
  ``,
  `dcl-ds varColors qualified dim(2);`,
  `  blue char(1);`,
  `  red char(1);`,
  `  yellow char(1);`,
  `end-ds;`,
  ``,
  `var1 = COLORS.GREEN;`,
  `var4 = varColors(1).red;`,
  ``,
  `if ( var1 = TRUE );`,
  `endif;`,
  ``,
  `var1 = 'RED';`,
  ``,
  `select var1;`,
  `  when-is 'RED';`,
  `    dsply 'works';`,
  `endsl;`,
  ``,
  `monitor;`,
  `  a = b;`,
  `on-error *ALL;`,
  `  dsply 'Error!';`,
  `endmon;`,
  `  `,
  `dcl-proc abc;`,
  `  dcl-pi *n ;`,
  `  end-pi;`,
  ``,
  `  on-exit;`,
  `    dsply 'out!';`,
  `end-proc;`,
].join(`\n`);

test("references_1_const", async () => {
  const cache = await parser.getDocs(uri, lines, {ignoreCache: true});

  Linter.getErrors({ uri, content: lines }, {
    CollectReferences: true,
  }, cache);

  const falseConstIndex = lines.indexOf(`dcl-c FALSE`) + 7;

  const falseConst = Cache.referenceByOffset(cache, falseConstIndex);
  expect(falseConst.name).toBe(`FALSE`);
  expect(falseConst.references.length).not.toBe(0);
});

test("references_2_const", async () => {
  const cache = await parser.getDocs(uri, lines, {ignoreCache: true});

  Linter.getErrors({ uri, content: lines }, {
    CollectReferences: true,
  }, cache);

  const trueConstIndex = lines.indexOf(`var1 = TRUE`) + 7;

  const trueConst = Cache.referenceByOffset(cache, trueConstIndex);
  expect(trueConst.name).toBe(`TRUE`);
  expect(trueConst.references.length).toBe(2);
});

test("references_3_enum", async () => {
  const cache = await parser.getDocs(uri, lines, {ignoreCache: true});

  Linter.getErrors({ uri, content: lines }, {
    CollectReferences: true,
  }, cache);

  const colorsConstIndex = lines.indexOf(`var1 = COLORS`) + 7;

  const colorsConst = Cache.referenceByOffset(cache, colorsConstIndex);
  expect(colorsConst.name).toBe(`COLORS`);
  expect(colorsConst.references.length).toBe(2);
});

test("references_4_subfield_a", async () => {
  const cache = await parser.getDocs(uri, lines, {ignoreCache: true});

  Linter.getErrors({ uri, content: lines }, {
    CollectReferences: true,
  }, cache);

  const greenSubfieldIndex = lines.indexOf(`var1 = COLORS.GREEN`) + 17;

  const greenConst = Cache.referenceByOffset(cache, greenSubfieldIndex);
  expect(greenConst.name).toBe(`GREEN`);
  expect(greenConst.references.length).toBe(2);
});

test("references_4_subfield_b", async () => {
  const cache = await parser.getDocs(uri, lines, {ignoreCache: true});

  Linter.getErrors({ uri, content: lines }, {
    CollectReferences: true,
  }, cache);

  const greenSubfieldIndex = lines.indexOf(` GREEN 1`) + 3;

  const greenConst = Cache.referenceByOffset(cache, greenSubfieldIndex);
  expect(greenConst.name).toBe(`GREEN`);
  expect(greenConst.references.length).toBe(2);

  const refSubfieldIndex = lines.indexOf(` RED 2`) + 3;

  const redConst = Cache.referenceByOffset(cache, refSubfieldIndex);
  expect(redConst.name).toBe(`RED`);
  expect(redConst.references.length).toBe(1);
});

test("references_5", async () => {
  const cache = await parser.getDocs(uri, lines, {ignoreCache: true});

  Linter.getErrors({ uri, content: lines }, {
    CollectReferences: true,
  }, cache);

  const var1Index = lines.indexOf(`var1 = TRUE`);

  const var1Var = Cache.referenceByOffset(cache, var1Index);
  expect(var1Var.name).toBe(`var1`);
  expect(var1Var.references.length).toBe(5);
});

test("references_6_subfield_dim", async () => {
  const cache = await parser.getDocs(uri, lines, {ignoreCache: true});

  Linter.getErrors({ uri, content: lines }, {
    CollectReferences: true,
  }, cache);

  const baseIndex = lines.indexOf(`var4 = varColors(1).red`);
  const varColorsIndex = baseIndex + 9;
  const redSubfieldIndex = baseIndex + 22;

  const varColors = Cache.referenceByOffset(cache, varColorsIndex);
  expect(varColors.name).toBe(`varColors`);
  expect(varColors.references.length).toBe(2);

  const redSubfield = Cache.referenceByOffset(cache, redSubfieldIndex);
  expect(redSubfield.name).toBe(`red`);
  expect(redSubfield.references.length).toBe(2);
});

test("references_7", async () => {
  const cache = await parser.getDocs(uri, lines, {ignoreCache: true});

  Linter.getErrors({ uri, content: lines }, {
    CollectReferences: true,
  }, cache);

  const declareAbcIndex = lines.indexOf(`dcl-proc abc`) + 10;

  const varColors = Cache.referenceByOffset(cache, declareAbcIndex);
  expect(varColors.name).toEqual(`abc`);
  expect(varColors.references.length).toEqual(1);
});
