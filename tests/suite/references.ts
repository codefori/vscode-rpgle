
import setupParser from "../parserSetup";
import Linter from "../../language/linter";
import Cache from "../../language/models/cache";
import assert from "assert";

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

export async function references_1_const() {
  const cache = await parser.getDocs(uri, lines, {ignoreCache: true});

  Linter.getErrors({ uri, content: lines }, {
    CollectReferences: true,
  }, cache);

  const falseConstIndex = lines.indexOf(`dcl-c FALSE`) + 7;

  const falseConst = Cache.referenceByOffset(cache, falseConstIndex);
  assert.strictEqual(falseConst.name, `FALSE`);
  assert.notStrictEqual(falseConst.references.length, 0);
}

export async function references_2_const() {
  const cache = await parser.getDocs(uri, lines, {ignoreCache: true});

  Linter.getErrors({ uri, content: lines }, {
    CollectReferences: true,
  }, cache);

  const trueConstIndex = lines.indexOf(`var1 = TRUE`) + 7;

  const trueConst = Cache.referenceByOffset(cache, trueConstIndex);
  assert.strictEqual(trueConst.name, `TRUE`);
  assert.strictEqual(trueConst.references.length, 2);
}

export async function references_3_enum() {
  const cache = await parser.getDocs(uri, lines, {ignoreCache: true});

  Linter.getErrors({ uri, content: lines }, {
    CollectReferences: true,
  }, cache);

  const colorsConstIndex = lines.indexOf(`var1 = COLORS`) + 7;

  const colorsConst = Cache.referenceByOffset(cache, colorsConstIndex);
  assert.strictEqual(colorsConst.name, `COLORS`);
  assert.strictEqual(colorsConst.references.length, 2);
}

export async function references_4_subfield_a() {
  const cache = await parser.getDocs(uri, lines, {ignoreCache: true});

  Linter.getErrors({ uri, content: lines }, {
    CollectReferences: true,
  }, cache);

  const greenSubfieldIndex = lines.indexOf(`var1 = COLORS.GREEN`) + 17;

  const greenConst = Cache.referenceByOffset(cache, greenSubfieldIndex);
  assert.strictEqual(greenConst.name, `GREEN`);
  assert.strictEqual(greenConst.references.length, 2);
}

export async function references_4_subfield_b() {
  const cache = await parser.getDocs(uri, lines, {ignoreCache: true});

  Linter.getErrors({ uri, content: lines }, {
    CollectReferences: true,
  }, cache);

  const greenSubfieldIndex = lines.indexOf(` GREEN 1`) + 3;

  const greenConst = Cache.referenceByOffset(cache, greenSubfieldIndex);
  assert.strictEqual(greenConst.name, `GREEN`);
  assert.strictEqual(greenConst.references.length, 2);

  const refSubfieldIndex = lines.indexOf(` RED 2`) + 3;

  const redConst = Cache.referenceByOffset(cache, refSubfieldIndex);
  assert.strictEqual(redConst.name, `RED`);
  assert.strictEqual(redConst.references.length, 1);
}

export async function references_5() {
  const cache = await parser.getDocs(uri, lines, {ignoreCache: true});

  Linter.getErrors({ uri, content: lines }, {
    CollectReferences: true,
  }, cache);

  const var1Index = lines.indexOf(`var1 = TRUE`);

  const var1Var = Cache.referenceByOffset(cache, var1Index);
  assert.strictEqual(var1Var.name, `var1`);
  assert.strictEqual(var1Var.references.length, 5);
}

export async function references_6_subfield_dim() {
  const cache = await parser.getDocs(uri, lines, {ignoreCache: true});

  Linter.getErrors({ uri, content: lines }, {
    CollectReferences: true,
  }, cache);

  const baseIndex = lines.indexOf(`var4 = varColors(1).red`);
  const varColorsIndex = baseIndex + 9;
  const redSubfieldIndex = baseIndex + 22;

  const varColors = Cache.referenceByOffset(cache, varColorsIndex);
  assert.strictEqual(varColors.name, `varColors`);
  assert.strictEqual(varColors.references.length, 2);

  const redSubfield = Cache.referenceByOffset(cache, redSubfieldIndex);
  assert.strictEqual(redSubfield.name, `red`);
  assert.strictEqual(redSubfield.references.length, 2);
}

export async function references_7() {
  const cache = await parser.getDocs(uri, lines, {ignoreCache: true});

  Linter.getErrors({ uri, content: lines }, {
    CollectReferences: true,
  }, cache);

  const declareAbcIndex = lines.indexOf(`dcl-proc abc`) + 10;

  const varColors = Cache.referenceByOffset(cache, declareAbcIndex);
  assert.strictEqual(varColors.name, `abc`);
  assert.strictEqual(varColors.references.length, 1);
}