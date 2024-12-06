import setupParser from "../parserSetup";
import Cache from "../../language/models/cache";
import { test, expect } from "vitest";


const parser = setupParser();
const uri = `source.rpgle`;

const bigLines = [
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
  const cache = await parser.getDocs(uri, bigLines, {ignoreCache: true, collectReferences: true});

  const falseConstIndex = bigLines.indexOf(`dcl-c FALSE`) + 7;

  const falseConst = Cache.referenceByOffset(cache, falseConstIndex);
  expect(falseConst.name).toBe(`FALSE`);
  expect(falseConst.references.length).not.toBe(0);
});

test("references_2_const", async () => {
  const cache = await parser.getDocs(uri, bigLines, {ignoreCache: true, collectReferences: true});

  const trueConstIndex = bigLines.indexOf(`var1 = TRUE`) + 7;

  const trueConst = Cache.referenceByOffset(cache, trueConstIndex);
  expect(trueConst.name).toBe(`TRUE`);
  expect(trueConst.references.length).toBe(2);
});

test("references_3_enum", async () => {
  const cache = await parser.getDocs(uri, bigLines, {ignoreCache: true, collectReferences: true});

  const colorsConstIndex = bigLines.indexOf(`var1 = COLORS`) + 7;

  const colorsConst = Cache.referenceByOffset(cache, colorsConstIndex);
  expect(colorsConst.name).toBe(`COLORS`);
  expect(colorsConst.references.length).toBe(2);
});

test("references_4_subfield_a", async () => {
  const cache = await parser.getDocs(uri, bigLines, {ignoreCache: true, collectReferences: true});

  const greenSubfieldIndex = bigLines.indexOf(`var1 = COLORS.GREEN`) + 17;

  const greenConst = Cache.referenceByOffset(cache, greenSubfieldIndex);

  expect(greenConst.references.length).toBe(2);
});

test("references_4_subfield_b", async () => {
  const cache = await parser.getDocs(uri, bigLines, {ignoreCache: true, collectReferences: true});

  const greenSubfieldIndex = bigLines.indexOf(` GREEN 1`) + 3;

  const greenConst = Cache.referenceByOffset(cache, greenSubfieldIndex);
  expect(greenConst.name).toBe(`GREEN`);
  expect(greenConst.references.length).toBe(2);



  const colours = cache.find(`COLORS`);
  const red = colours.subItems.find(sub => sub.name === `RED`);



  const refSubfieldIndex = bigLines.indexOf(` RED 2`) + 3;
  const redConst = Cache.referenceByOffset(cache, refSubfieldIndex);

  expect(redConst.name).toBe(`RED`);
  expect(redConst.references.length).toBe(1);
});

test("references_5", async () => {
  const cache = await parser.getDocs(uri, bigLines, {ignoreCache: true, collectReferences: true});

  const var1Index = bigLines.indexOf(`var1 = TRUE`);

  const var1Var = Cache.referenceByOffset(cache, var1Index);
  expect(var1Var.name).toBe(`var1`);
  expect(var1Var.references.length).toBe(5);
});

test("references_6_subfield_dim", async () => {
  const cache = await parser.getDocs(uri, bigLines, {ignoreCache: true, collectReferences: true});

  const baseIndex = bigLines.indexOf(`var4 = varColors(1).red`);
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
  const cache = await parser.getDocs(uri, bigLines, {ignoreCache: true, collectReferences: true});

  const declareAbcIndex = bigLines.indexOf(`dcl-proc abc`) + 10;

  const varColors = Cache.referenceByOffset(cache, declareAbcIndex);
  expect(varColors.name).toEqual(`abc`);
  expect(varColors.references.length).toEqual(1);
});

test("references_8", async () => {
  const lines = [
    `**free`,
    ``,
    `dcl-s localVarYes Char(1);`,
    `Dcl-s localVarForProc Int(20);`,
    `dcl-s localVarNo Ind;`,
    ``,
    `dcl-ds structYes;`,
    `    subfa varchar(12);`,
    `End-ds;`,
    ``,
    `dcl-ds structNo;`,
    `    subfb packed(12);`,
    `End-ds;`,
    ``,
    `Dcl-ds structYesAlso;`,
    `    subfc char(20);`,
    `End-Ds;`,
    ``,
    `dcl-ds qualStructYes Qualified;`,
    `    qualsubA zoned(5);`,
    `end-ds;`,
    ``,
    `dcl-ds qualStructNo Qualified;`,
    `    qualsubA zoned(5);`,
    `end-ds;`,
    ``,
    `dcl-ds qualDimStructYup Qualified Dim(2);`,
    `    boopABC zoned(5);`,
    `end-ds;`,
    ``,
    `localVarYes = 'Y';`,
    `procYes();`,
    ``,
    `subfa = 'Yes!';`,
    `structYesAlso = 'Really yes';`,
    ``,
    `qualStructYes.qualsubA = 5;`,
    ``,
    `qualDimStructYup(1).boopabc = 5;`,
    `qualDimStructYup(localVarForProc).boopAbc = 5;`,
    `qualDimStructYup(localVarForProc - 1).boopABC = 5;`,
    ``,
    `return;`,
    ``,
    `Dcl-Proc procYes;`,
    `    dcl-s reallyLocalYes bindec(9);`,
    `    dcl-s reallyLocalNo Char(1);`,
    ``,
    `    dcl-ds localStructYes;`,
    `        subfd char(12);`,
    `    end-ds;`,
    ``,
    `    dcl-ds localStructAlsoYes;`,
    `        subfe char(12);`,
    `    end-ds;`,
    ``,
    `    dcl-ds localStructNo;`,
    `        subfg char(12);`,
    `    end-ds;`,
    ``,
    `    dcl-ds localQualStructYes Qualified;`,
    `        qualsubA zoned(5);`,
    `    end-ds;`,
    ``,
    `    dcl-ds localQualStructNo Qualified;`,
    `        qualsubA zoned(5);`,
    `    end-ds;`,
    ``,
    `    reallyLocalYes = 1;`,
    `    localStructYes = 'Helloworld';`,
    `    subfe = 'Otherworld';`,
    `    localQualStructYes.qualsubA = 55;`,
    ``,
    `    localVarForProc = 12398;`,
    `End-Proc;`,
    ``,
    `Dcl-Proc procNo;`,
    `    localVarForProc = 1190348;`,
    `End-Proc;`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true, collectReferences: true});

  const subfa = cache.find(`subfa`);
  expect(subfa.references.length).toBe(2);
  expect(subfa.references[1]).toEqual({
    offset: { position: 469, end: 474 }
  });

  const structYesAlso = cache.find(`structYesAlso`);
  expect(structYesAlso.references.length).toBe(2);
  expect(structYesAlso.references[1]).toEqual({
    offset: { position: 485, end: 498 }
  });

  const subfc = structYesAlso.subItems[0];
  expect(subfc.name).toBe(`subfc`);
  expect(subfc.references.length).toBe(1);

  const qualStructYes = cache.find(`qualStructYes`);
  expect(qualStructYes.references.length).toBe(2);
  expect(qualStructYes.references[1]).toEqual({
    offset: { position: 516, end: 529 }
  });

  const qualsubA = qualStructYes.subItems[0];
  expect(qualsubA.name).toBe(`qualsubA`);
  expect(qualsubA.references.length).toBe(2);

  expect(qualsubA.references[0]).toEqual({
    offset: { position: 274, end: 282 }
  });

  expect(qualsubA.references[1]).toEqual({
    offset: { position: 530, end: 538 }
  });

  const procYes = cache.find(`procYes`);
  const subProc = procYes.scope;

  const localStructYes = subProc.find(`localStructYes`);
  expect(localStructYes.references.length).toBe(2);
  expect(localStructYes.references[1]).toEqual({
    offset: { position: 1158, end: 1172 }
  });

  const localStructAlsoYes = subProc.find(`localStructAlsoYes`);
  expect(localStructAlsoYes.references.length).toBe(1);

  const subfe = localStructAlsoYes.subItems[0];
  expect(subfe.name).toBe(`subfe`);
  expect(subfe.references.length).toBe(2);
  expect(subfe.references[1]).toEqual({
    offset: { position: 1193, end: 1198 }
  });

  const qualDimStructYup = cache.find(`qualDimStructYup`);
  expect(qualDimStructYup.references.length).toBe(4)

  expect(qualDimStructYup.references[1]).toEqual({
    offset: { position: 545, end: 561 }
  });

  expect(qualDimStructYup.references[2]).toEqual({
    offset: { position: 578, end: 594 }
  });

  expect(qualDimStructYup.references[3]).toEqual({
    offset: { position: 625, end: 641 }
  });

  const boopABC = qualDimStructYup.subItems[0];
  expect(boopABC.name).toBe(`boopABC`);
  expect(boopABC.references.length).toBe(4);

  expect(boopABC.references[0]).toEqual({
    offset: { position: 411, end: 418 }
  });

  expect(boopABC.references[1]).toEqual({
    offset: { position: 565, end: 572 }
  });

  expect(boopABC.references[2]).toEqual({
    offset: { position: 612, end: 619 } 
  });

  expect(boopABC.references[3]).toEqual({
    offset: { position: 663, end: 670 }
  });
});

test("references_9", async () => {
  const lines = [
    `**free`,
    `Dcl-Proc InputIsValid;`,
    `  Dcl-PI InputIsValid likeds(validationResult);`,
    `    comp Char(1);`,
    `  End-PI;`,
    ``,
    `  Dcl-S isValid Ind inz(*on);`,
    `  Dcl-S isFound Ind inz(*on);`,
    ``,
    `  Dcl-DS validationResult Qualified;`,
    `    isValid Ind inz(*on);`,
    `    errorField Char(20) inz(*blanks);`,
    `    errorMessage Char(100) inz(*blanks);`,
    `  End-DS;`,
    ``,
    `  // Validate company value`,
    `  isFound = company_getrecord(comp);`,
    `  if (isFound = *off);`,
    `    validationResult.isValid = *off;`,
    `    validationResult.errorField = 'comp';`,
    `    validationResult.errorMessage = 'Company value inva lid';`,
    ``,
    `    return validationResult;`,
    `  endif;`,
    ``,
    `  // Validate other input parameters...`,
    ``,
    `  return validationResult;`,
    ``,
    `End-Proc;`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true, collectReferences: true});

  const procedure = cache.find(`InputIsValid`);
  const validationResult = procedure.scope.find(`validationResult`);

  expect(validationResult.references.length).toEqual(7);
  expect(validationResult.references.every(ref => lines.substring(ref.offset.position, ref.offset.end) === `validationResult`)).toBe(true);
});

test('references_10', async () => {
  const lines = [
    `**free`,
    `ctl-opt debug  option(*nodebugio: *srcstmt) dftactgrp(*no) actgrp(*caller)`,
    `main(Main);`,
    `dcl-s x timestamp;`,
    `dcl-s y timestamp;`,
    `dcl-proc Main;`,
    `  dsply %CHAR(CalcDiscount(10000));`,
    `  dsply %char(CalcDiscount(1000));`,
    `  x = %TIMESTAMP(y);`,
    `  y = %TimeStamp(x);`,
    `  return;`,
    `end-proc;`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true, collectReferences: true});

  const rangeRefs = cache.referencesInRange({position: 220, end: 260});
  expect(rangeRefs.length).toBe(2);
  expect(rangeRefs[0].dec.name).toBe(`x`);
  expect(rangeRefs[1].dec.name).toBe(`y`);

  expect(rangeRefs[0].refs).toEqual([
    { position: 220, end: 221 },
    { position: 256, end: 257 }
  ]);

  expect(rangeRefs[1].refs).toEqual([
    { position: 235, end: 236 },
    { position: 241, end: 242 }
  ]);
});

test("references_11_issue_175", async () => {
  const lines = [
    `**FREE`,
    ``,
    `Dcl-S Field Char(1);`,
    ``,
    `Field = SubProc('A');`,
    ``,
    `*INLR = *ON;`,
    `Return;`,
    ``,
    `  ///`,
    `  // SubProc`,
    `  // Description of SubProc()`,
    `  // Description can be multiline`,
    `  // @param Parm_A`,
    `  // @return Return_1`,
    `  ///`,
    `Dcl-Proc SubProc;`,
    `  Dcl-Pi *N Like( ReturnValue );`,
    `    PP_PARM1 Char(1);`,
    `  End-Pi;`,
    `  Dcl-S ReturnValue Char(1);`,
    `  // Your code goes here`,
    `  ReturnValue = PP_PARM1;`,
    `  ReturnValue= 'Q';`,
    `  Return ReturnValue;`,
    `  Begsr *PSSR;`,
    `    *INLR = *ON;`,
    `    Return;`,
    `  Endsr;`,
    `End-Proc;`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true, collectReferences: true});

  const procedure = cache.find(`SubProc`);
  expect(procedure).toBeDefined();
  expect(procedure.references.length).toBe(2);
});

test('references_12_fixed_1', async () => {
  const lines = [
    ``,
    `     FINVMST    IF   E           K DISK`,
    `   `,
    `     D wkCorp          S             10    inz('100')`,
    `     D wkInvoice       S             15`,
    `   `,
    `     C                   eval      wkInvoice = 'I035552120'`,
    `   `,
    `     C                   eval      *inlr = *on`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, { ignoreCache: true, withIncludes: true, collectReferences: true });

  expect(cache.variables.length).to.equal(2);

  const wkInvoice = cache.find(`wkInvoice`);
  expect(wkInvoice.references.length).to.equal(2);
});