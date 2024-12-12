import setupParser, { getFileContent } from "../parserSetup";
import Cache from "../../language/models/cache";
import { test, expect } from "vitest";
import { readFile } from "fs/promises";


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

  const falseConst = Cache.referenceByOffset(uri, cache, falseConstIndex);
  expect(falseConst.name).toBe(`FALSE`);
  expect(falseConst.references.length).not.toBe(0);
});

test("references_2_const", async () => {
  const cache = await parser.getDocs(uri, bigLines, {ignoreCache: true, collectReferences: true});

  const trueConstIndex = bigLines.indexOf(`var1 = TRUE`) + 7;

  const trueConst = Cache.referenceByOffset(uri, cache, trueConstIndex);
  expect(trueConst.name).toBe(`TRUE`);
  expect(trueConst.references.length).toBe(2);
});

test("references_3_enum", async () => {
  const cache = await parser.getDocs(uri, bigLines, {ignoreCache: true, collectReferences: true});

  const colorsConstIndex = bigLines.indexOf(`var1 = COLORS`) + 7;

  const colorsConst = Cache.referenceByOffset(uri, cache, colorsConstIndex);
  expect(colorsConst.name).toBe(`COLORS`);
  expect(colorsConst.references.length).toBe(2);
});

test("references_4_subfield_a", async () => {
  const cache = await parser.getDocs(uri, bigLines, {ignoreCache: true, collectReferences: true});

  const greenSubfieldIndex = bigLines.indexOf(`var1 = COLORS.GREEN`) + 17;

  const greenConst = Cache.referenceByOffset(uri, cache, greenSubfieldIndex);

  expect(greenConst.references.length).toBe(2);
});

test("references_4_subfield_b", async () => {
  const cache = await parser.getDocs(uri, bigLines, {ignoreCache: true, collectReferences: true});

  const greenSubfieldIndex = bigLines.indexOf(` GREEN 1`) + 3;

  const greenConst = Cache.referenceByOffset(uri, cache, greenSubfieldIndex);
  expect(greenConst.name).toBe(`GREEN`);
  expect(greenConst.references.length).toBe(2);



  const colours = cache.find(`COLORS`);
  const red = colours.subItems.find(sub => sub.name === `RED`);



  const refSubfieldIndex = bigLines.indexOf(` RED 2`) + 3;
  const redConst = Cache.referenceByOffset(uri, cache, refSubfieldIndex);

  expect(redConst.name).toBe(`RED`);
  expect(redConst.references.length).toBe(1);
});

test("references_5", async () => {
  const cache = await parser.getDocs(uri, bigLines, {ignoreCache: true, collectReferences: true});

  const var1Index = bigLines.indexOf(`var1 = TRUE`);

  const var1Var = Cache.referenceByOffset(uri, cache, var1Index);
  expect(var1Var.name).toBe(`var1`);
  expect(var1Var.references.length).toBe(5);
});

test("references_6_subfield_dim", async () => {
  const cache = await parser.getDocs(uri, bigLines, {ignoreCache: true, collectReferences: true});

  const baseIndex = bigLines.indexOf(`var4 = varColors(1).red`);
  const varColorsIndex = baseIndex + 9;
  const redSubfieldIndex = baseIndex + 22;

  const varColors = Cache.referenceByOffset(uri, cache, varColorsIndex);
  expect(varColors.name).toBe(`varColors`);
  
  expect(varColors.references.length).toBe(2);

  const redSubfield = Cache.referenceByOffset(uri, cache, redSubfieldIndex);
  expect(redSubfield.name).toBe(`red`);
  expect(redSubfield.references.length).toBe(2);
});

test("references_7", async () => {
  const cache = await parser.getDocs(uri, bigLines, {ignoreCache: true, collectReferences: true});

  const declareAbcIndex = bigLines.indexOf(`dcl-proc abc`) + 10;

  const varColors = Cache.referenceByOffset(uri, cache, declareAbcIndex);
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
    offset: { start: 469, end: 474, line: 33 },
    uri: uri
  });

  const structYesAlso = cache.find(`structYesAlso`);
  expect(structYesAlso.references.length).toBe(2);
  expect(structYesAlso.references[1]).toEqual({
    offset: { start: 485, end: 498, line: 34 },
    uri: uri
  });

  const subfc = structYesAlso.subItems[0];
  expect(subfc.name).toBe(`subfc`);
  expect(subfc.references.length).toBe(1);

  const qualStructYes = cache.find(`qualStructYes`);
  expect(qualStructYes.references.length).toBe(2);
  expect(qualStructYes.references[1]).toEqual({
    offset: { start: 516, end: 529, line: 36 },
    uri: uri
  });

  const qualsubA = qualStructYes.subItems[0];
  expect(qualsubA.name).toBe(`qualsubA`);
  expect(qualsubA.references.length).toBe(2);

  expect(qualsubA.references[0]).toEqual({
    offset: { start: 274, end: 282, line: 19 },
    uri: uri
  });

  expect(qualsubA.references[1]).toEqual({
    offset: { start: 530, end: 538, line: 36 },
    uri: uri
  });

  const procYes = cache.find(`procYes`);
  const subProc = procYes.scope;

  const localStructYes = subProc.find(`localStructYes`);
  expect(localStructYes.references.length).toBe(2);
  expect(localStructYes.references[1]).toEqual({
    offset: { start: 1158, end: 1172, line: 69 },
    uri: uri
  });

  const localStructAlsoYes = subProc.find(`localStructAlsoYes`);
  expect(localStructAlsoYes.references.length).toBe(1);

  const subfe = localStructAlsoYes.subItems[0];
  expect(subfe.name).toBe(`subfe`);
  expect(subfe.references.length).toBe(2);
  expect(subfe.references[1]).toEqual({
    offset: { start: 1193, end: 1198, line: 70 },
    uri: uri
  });

  const qualDimStructYup = cache.find(`qualDimStructYup`);
  expect(qualDimStructYup.references.length).toBe(4)

  expect(qualDimStructYup.references[1]).toEqual({
    offset: { start: 545, end: 561, line: 38 },
    uri: uri
  });

  expect(qualDimStructYup.references[2]).toEqual({
    offset: { start: 578, end: 594, line: 39 },
    uri: uri
  });

  expect(qualDimStructYup.references[3]).toEqual({
    offset: { start: 625, end: 641, line: 40 },
    uri: uri
  });

  const boopABC = qualDimStructYup.subItems[0];
  expect(boopABC.name).toBe(`boopABC`);
  expect(boopABC.references.length).toBe(4);

  expect(boopABC.references[0]).toEqual({
    offset: { start: 411, end: 418, line: 27 },
    uri: uri
  });

  expect(boopABC.references[1]).toEqual({
    offset: { start: 565, end: 572, line: 38 },
    uri: uri
  });

  expect(boopABC.references[2]).toEqual({
    offset: { start: 612, end: 619, line: 39 },
    uri: uri
  });

  expect(boopABC.references[3]).toEqual({
    offset: { start: 663, end: 670, line: 40 },
    uri: uri
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
  expect(validationResult.references.every(ref => lines.substring(ref.offset.start, ref.offset.end) === `validationResult`)).toBe(true);

  const comp = procedure.scope.find(`comp`);
  expect(comp.references.length).toEqual(2);
  expect(comp.references.every(ref => lines.substring(ref.offset.start, ref.offset.end) === `comp`)).toBe(true);
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

  const rangeRefs = cache.referencesInRange(uri, {start: 220, end: 260});
  expect(rangeRefs.length).toBe(2);
  expect(rangeRefs[0].dec.name).toBe(`x`);
  expect(rangeRefs[1].dec.name).toBe(`y`);

  expect(rangeRefs[0].refs).toEqual([
    { start: 220, end: 221, line: 8 },
    { start: 256, end: 257, line: 9 }
  ]);

  expect(rangeRefs[1].refs).toEqual([
    { start: 235, end: 236, line: 8 },
    { start: 241, end: 242, line: 9 }
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

  for (const ref of wkInvoice.references) {
    // console.log({
    //   ref,
    //   text: lines.substring(ref.offset.start, ref.offset.end),
    //   about: lines.substring(ref.offset.start - 10, ref.offset.end + 10)
    // });
    expect(lines.substring(ref.offset.start, ref.offset.end)).to.equal(`wkInvoice`);
  }

  expect(wkInvoice.references.length).to.equal(2);
});


test('references_13_fixed_2', async () => {
  const lines = [
    `      * ********************************************************************/`,
    `      *                                                                    *`,
    `      *  Last Amend No. MIDAS2122          Date 26Jul18    Author ABDN198  *`,
    `      *  Prev Amend No. MIDAS1939          Date 25May18    Author ABDN198  *`,
    `      *  Prev Amend No. MIDAS841           Date 23Jun17    Author ABDN198  *`,
    `      *                                                                    *`,
    `      * ********************************************************************/`,
    `      *                                                                    *`,
    `      *  MIDAS2122  - Added $F4_TYPEMST procedure                          *`,
    `      *  MIDAS1939  - Added $Close_ procedure                              *`,
    `      *  MIDAS841   - Add additional columns                               *`,
    `      *                                                                    *`,
    `      * ********************************************************************/`,
    `     D* -------------------------------------------------------------------`,
    `     D* TYPEMSTPF`,
    `     D* -------------------------------------------------------------------`,
    `     D TYPEMST_T     E Ds                  ExtName(TYPEMSTPF) Qualified Template`,
    `     D`,
    `     D TYPEMST_P       s               *`,
    `     D/IF DEFINED(TYPEMSTPF)`,
    `     D TYPEMST_K     E Ds                  ExtName(TYPEMSTPF: *KEY)`,
    `     D                                     Qualified`,
    `     D TYPEMST_R     E Ds                  ExtName(TYPEMSTPF)`,
    `     D                                     Based(TYPEMST_P)`,
    `     D/ELSEIF DEFINED(TYPEMSTPF_PREFIX)`,
    `     D TYPEMST_K     E Ds                  ExtName(TYPEMSTPF: *KEY)`,
    `     D                                     Prefix('KTM1')`,
    `     D TYPEMST_R     E Ds                  ExtName(TYPEMSTPF)`,
    `     D                                     Based(TYPEMST_P)`,
    `     D                                     Prefix('TM1')`,
    `     D/ELSE`,
    `     D TYPEMST_K     E Ds                  ExtName(TYPEMSTPF: *KEY)`,
    `     D                                     Qualified`,
    `     D TYPEMST_R     E Ds                  ExtName(TYPEMSTPF)`,
    `     D                                     Based(TYPEMST_P)`,
    `     D                                     Qualified`,
    `     D/ENDIF`,
    `     D TYPEMST_Ds    E Ds                  ExtName(TYPEMSTPF)`,
    `     D                                     Qualified`,
    `     D                                     Dim(TYPEMST_Dim)`,
    `     D`,
    `     D TYPEMST_F       Ds                  LikeDs(TYPEMST_T)`,
    `     D* -------------------------------------------------------------------`,
    `     D* Service Program Procedures`,
    `     D* -------------------------------------------------------------------`,
    `     D $Validate_TYPEMST...`,
    `     D                 Pr              n`,
    `     D  $i_Action                     4    Const`,
    `     D  $i_Pointer                     *   Const`,
    `     D`,
    `     D $GetError_TYPEMST...`,
    `     D                 Pr            80a   Varying`,
    `     D  $o_ErrNo                     10i 0 Options(*NoPass: *Omit)`,
    `     D`,
    `     D $GetErrors_TYPEMST...`,
    `     D                 Pr            10i 0`,
    `     D  $o_ErrDs                           likeds($ErrorDs_TYPEMST)`,
    `     D                                     Dim(TYPEMST_Dim)`,
    `     D`,
    `     D* Input Handler`,
    `     D`,
    `     D $SetLL_TYPEMST  Pr              n`,
    `     D  $i_Pointer                     *   Const`,
    `     D  $i_Key                             Const`,
    `     D                                     likeds(TYPEMST_K)`,
    `     D`,
    `     D $Read_TYPEMST   Pr              n`,
    `     D  $i_Pointer                     *   Const`,
    `     D`,
    `     D $ReadE_TYPEMST  Pr              n`,
    `     D  $i_Pointer                     *   Const`,
    `     D  $i_Key                             LikeDs(TYPEMST_K)`,
    `     D                                     Const Options(*NoPass)`,
    `     D`,
    `     D $Chain_TYPEMST  Pr              n`,
    `     D  $i_Pointer                     *   Const`,
    `     D  $i_Key                             LikeDs(TYPEMST_K)`,
    `     D                                     Const Options(*NoPass)`,
    `     D`,
    `     D $CloseI_TYPEMST...`,
    `     D                 Pr`,
    `     D`,
    `     D $Close_TYPEMST...`,
    `     D                 Pr`,
    `     D`,
    `     D $SetGT_TYPEMST  Pr              n`,
    `     D  $i_Pointer                     *   Const`,
    `     D  $i_Key                             LikeDs(TYPEMST_K)`,
    `     D                                     Const Options(*NoPass)`,
    `     D`,
    `     D $ReadPE_TYPEMST...`,
    `     D                 Pr              n`,
    `     D  $i_Pointer                     *   Const`,
    `     D  $i_Key                             LikeDs(TYPEMST_K)`,
    `     D                                     Const Options(*NoPass)`,
    `     D`,
    `     D $ReadP_TYPEMST  Pr              n`,
    `     D  $i_Pointer                     *   Const`,
    `     D`,
    `     D $SaveKeys_TYPEMST...`,
    `     D                 Pr              n`,
    `     D  $i_Pointer                     *   Const`,
    `     D  $i_Key                             LikeDs(TYPEMST_K)`,
    `     D                                     Const Options(*NoPass)`,
    `     D`,
    `     D $Restore_TYPEMST...`,
    `     D                 Pr              n`,
    `     D`,
    `     D* Update Handler`,
    `     D`,
    `     D $CloseU_TYPEMST...`,
    `     D                 Pr`,
    `     D`,
    `     D $Write_TYPEMST  Pr              n`,
    `     D  $i_Pointer                     *   Const`,
    `     D  $i_Key                             LikeDs(TYPEMST_K)`,
    `     D                                     Const Options(*NoPass)`,
    `     D`,
    `     D $Update_TYPEMST...`,
    `     D                 Pr              n`,
    `     D  $i_Pointer                     *   Const`,
    `     D  $i_Key                             LikeDs(TYPEMST_K)`,
    `     D                                     Const Options(*NoPass)`,
    `     D`,
    `     D $Delete_TYPEMST...`,
    `     D                 Pr              n`,
    `     D  $i_Pointer                     *   Const`,
    `     D  $i_Key                             LikeDs(TYPEMST_K)`,
    `     D                                     Const Options(*NoPass)`,
    `     D`,
    `     D* SQL Handler`,
    `     D`,
    `     D $SQLRead_TYPEMST...`,
    `     D                 Pr              n`,
    `     D $i_Pointer                      *   Const`,
    `     D $i_Statement                 500a   Const Options(*NoPass: *VarSize)`,
    `     D`,
    `     D $Select_TYPEMST...`,
    `     D                 Pr              n`,
    `     D $o_TYPEMST_Ds                       LikeDs(TYPEMST_R) Dim(TYPEMST_Dim)`,
    `     D $o_TYPEMST_Elem...`,
    `     D                               10i 0`,
    `     D $i_SQLWhere                  200a   Const Options(*NoPass)`,
    `     D $i_SQLOrder                  200a   Const Options(*NoPass)`,
    `     D`,
    `     D $SQLFetch_TYPEMST...`,
    `     D                 Pr              n`,
    `     D $i_Pointer                      *   Const`,
    `     D $i_Procedure                  10a   Const`,
    `     D`,
    `     D $F4_TYPEMST...`,
    `     D                 Pr                  LikeDs(TYPEMST_K)`,
    `     D  $i_Filter                          LikeDs(TYPEMST_F)`,
    `     D                                     Const`,
    `     D  $i_Row                        3s 0 Const Options(*NoPass)`,
    `     D  $i_Col                        3s 0 Const Options(*NoPass)`,
    `     D`,
    `     D $GetFilter_TYPEMST...`,
    `     D                 Pr          5000a   Varying`,
    `     D  $i_Filter                          LikeDs(TYPEMST_F)`,
    `     D                                     Const`,
    `     D* -------------------------------------------------------------------`,
    `     D* Data Structure`,
    `     D* -------------------------------------------------------------------`,
    `     D $ErrorDS_TYPEMST...`,
    `     D                 Ds                  Qualified Dim(TYPEMST_Dim)`,
    `     D  Column                       10a`,
    `     D  Message                      70a`,
    `     D`,
    `     D* -------------------------------------------------------------------`,
    `     D* Constants`,
    `     D* -------------------------------------------------------------------`,
    `     D TYPEMST_FILENAME...`,
    `     D                 c                   'TYPEMSTPF'                          FILENAME`,
    `     D TYPEMST_Dim     c                   100`,
    `     D TYPEMST_IN51    c                   51                                   FILENAME`,
    `     D TYPEMST_IN52    c                   52                                   TYPE`,
    `     D TYPEMST_IN53    c                   53                                   TYPNAME`,
    `     D TYPEMST_IN54    c                   54                                   TYPSNAME`,
    `     D TYPEMST_IN55    c                   55                                   ACTION`,
    `     D TYPEMST_IN56    c                   56                                   PROC1`,
    `     D TYPEMST_IN57    c                   57                                   PROC2`,
    `     D TYPEMST_IN58    c                   58                                   PROC3`,
    `     D`,
    `     /*MIDAS560   ABDN198   */`,
    `     /*MIDAS1939  ABDN198   */`,
    ``,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, { ignoreCache: true, withIncludes: true, collectReferences: true });

  const TYPEMST_T = cache.find(`TYPEMST_T`);
  expect(TYPEMST_T.references.length).toBe(2);
  expect(TYPEMST_T.references.every(ref => lines.substring(ref.offset.start, ref.offset.end) === `TYPEMST_T`)).toBe(true);

  const TYPEMST_Ds = cache.find(`TYPEMST_Ds`);
  expect(TYPEMST_Ds.references.length).toBe(1);
  expect(TYPEMST_Ds.references.every(ref => lines.substring(ref.offset.start, ref.offset.end) === `TYPEMST_Ds`)).toBe(true);

  const TYPEMST_Dim = cache.find(`TYPEMST_Dim`);
  expect(TYPEMST_Dim.references.length).toBe(5);
  expect(TYPEMST_Dim.references.every(ref => lines.substring(ref.offset.start, ref.offset.end) === `TYPEMST_Dim`)).toBe(true);
});

test('references_14_fixed_3', async () => {
  const lines = [
    `      *?--------------------------------------------------------------?`,
    `     D  frdt           s              7  0`,
    `     D  todt           s              7  0`,
    `     D  per            s              6`,
    `     D  year           s              2  0`,
    `     D  month          s              2  0`,
    `  `,
    `      *?--------------------------------------------------------------?`,
    `     D                 DS`,
    `     D  filedt_c               1      1  0`,
    `     D  filedt_yy              2      3  0`,
    `     D  filedt_mm              4      5  0`,
    `     D  filedt_dd              6      7  0`,
    `     D  filedt                 1      7  0`,
    `      *?--------------------------------------------------------------?`,
    `     D                 DS`,
    `     D  today                  1      7  0`,
    `     D  udatc                  1      1  0`,
    `     D  udatyy                 2      3  0`,
    `     D  udatmm                 4      5  0`,
    `     D  udatdd                 6      7  0`,
    `      *?--------------------------------------------------------------?`,
    `     D PARMDS          DS             6`,
    `     D  pmcc                   1      2  0`,
    `     D  pmyy                   3      4  0`,
    `     D  pmmm                   5      6  0`,
    `      *?---?`,
    `      *?--------------------------------------------------------------?`,
    `     C     *ENTRY        plist`,
    `     C                   parm                    parmds`,
    `      *??`,
    `      *??`,
    `      *?---?`,
    `     C                   if        parmds = *blank`,
    `     c                   eval      year  = Uyear`,
    `     C                   eval      month  = umonth`,
    `     C                   movel     *year         per                            --> cyymmdd`,
    `     C                   move      umonth        per`,
    `     C                   else`,
    `     C                   eval      year    = pmyy`,
    `     C                   eval      month   = pmmm`,
    `     C                   eval      per = parmds                                 --> cyymmdd`,
    `     C                   endif`,
    `      *?---?`,
    `     C                   eval      filedt_c  = 1`,
    `     C                   eval      filedt_yy = year`,
    `     C                   eval      filedt_mm = month`,
    `     C                   eval      filedt_dd = 1`,
    `     C                   eval      frdt = filedt                                --> cyymmdd`,
    `      *??`,
    `     C                   eval      filedt_dd = 31`,
    `     C                   eval      todt = filedt                                --> cyymmdd`,
    `      *??`,
    `      *?SQL-Delete if there are already records for given period?`,
    `     C/EXEC SQL`,
    `     C+ delete from WOOPS/SCOOBYDO where period = :per`,
    `     C/END-EXEC`,
    `      *?==============================================================?`,
    `      *?SQL-Insert in file SCOOBYDO for the  given period?`,
    `     C/EXEC SQL`,
    `     C+ insert into WOOPS/SCOOBYDO  (geco,nuco,period,lati,cicn,cdt3,nao2,`,
    `     C* this is intentially broken, because we don't parse SQL`,
    `     C+ substr(rtrim('0000000' concat cast(fhnuco as char(7))),`,
    `     C+ length(rtrim('0000000' concat cast(fhnuco as char(7))))-6, 5 )`,
    `     C+ concat '-'`,
    `     C+ concat substr(rtrim('0000000' concat cast(fhnuco as char(7))),`,
    `     C+ length(rtrim('0000000' concat cast(fhnuco as char(7))))-1, 2 ),`,
    `     C+ ftlet1, ftlet2        from     pcsiti,   pchico,   pcsiko`,
    `     C+ where fhgeco = fkgeco and fhnuco = fknuco`,
    `     C+ and fkgeco = 2 and (fkcgko in ('B', 'C'))`,
    `     C+ and fkrpko not in ('110', '130', '135', '140', '199', '235')`,
    `     C+ and fhnao1 in ('C', 'H', 'O', 'S')`,
    `     C+ and fhanop = ' ' and fhdaop between :frdt and :todt`,
    `     C+ and fhcdt3 = ftcdt3`,
    `     C+ and fhssor = 'T'`,
    `     C+ and fhfcds <> 0`,
    `     C+ group by fhgeco, fhnuco, fkleti, ftlati,`,
    `     C+ fhcicn, fhnao2, fhlads, fhcdt3, ftlet1, ftlet2`,
    `     C+ order by fhnuco`,
    `     C/END-EXEC`,
    `      *?==============================================================?`,
    `     C                   call      'HICO_TAXE'`,
    `     C                   parm                    per`,
    `     C                   call      'HICO_BRK2'`,
    `     C                   parm                    per`,
    `     C                   seton                                        LR`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, { ignoreCache: true, withIncludes: true, collectReferences: true });

  const per = cache.find(`per`);
  expect(per.references.length).toBe(7);

  // for (const ref of per.references) {
  //   console.log({
  //     ref,
  //     text: lines.substring(ref.offset.start, ref.offset.end),
  //     about: lines.substring(ref.offset.start - 10, ref.offset.end + 10)
  //   })
  // }

  expect(per.references.every(ref => lines.substring(ref.offset.start, ref.offset.end) === `per`)).toBe(true);

  const pmyy = cache.find(`pmyy`);
  expect(pmyy.references.length).toBe(2);
  expect(pmyy.references.every(ref => lines.substring(ref.offset.start, ref.offset.end) === `pmyy`)).toBe(true);

  const filedt = cache.find(`filedt`);
  expect(filedt.references.length).toBe(3);
  expect(filedt.references.every(ref => lines.substring(ref.offset.start, ref.offset.end) === `filedt`)).toBe(true);

  const lr = cache.find(`INLR`);
  expect(lr.references.length).toBe(1);
});

test('indicators1', async () => {
  const lines = [
    `**FREE`,
    `Dcl-S MyVar char(10);`,
    ``,
    `*IN10 = *ON;`,
    `MyVar = 'Hi';`,
    ``,
    `DSply Myvar;`,
    `*INLR = *IN10;`,
    `Return;`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {withIncludes: true, ignoreCache: true, collectReferences: true});

  const in10 = cache.find(`IN10`);
  expect(in10.references.length).toBe(2);
});

test('references_15_fixed_4', async () => {
  const lines = [
    `      *****************************************************************?`,
    `     FGltbsVar  UF   e           k disk`,
    `     FGlRcvvar  IF a e           k disk`,
    `     FGllogvar  o  a f  500        disk`,
    `      *---------------------------------------------------------------*?`,
    `      `,
    `     D EntryParm       ds            19`,
    `     D   Type_SLX                     3                                         :SLE, SLC...`,
    `     D   AdrIP                        2                                         :last chars adr ip`,
    `     D   Version                      2                                         :V4 ou V5`,
    `     D   PortNumber                   6                                         :P + port nr`,
    `     D   SubNode                      6                                         :S + sub-node`,
    `      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*?`,
    `     D RCV             s           5083                                         Received`,
    `     D*  GLrcv1                    1694`,
    `     D*  GLrcv2                    1694`,
    `     D*  GLrcv3                    1694`,
    `     D*  GLrcvLibre                   1`,
    `      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*?`,
    `     D ebcd            s              1    dim(223)                             ebcdic "length"`,
    `     D Hms             s               t   timfmt(*HMS)`,
    `     D HmsLast         s               t   timfmt(*HMS)`,
    `      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*?`,
    `     D r_saved_lg      s              5  0                                      longueur r_saved`,
    `     D r_saved         s          19600                                         saved buffer`,
    `     D r_transf        s          19600                                         transfer data`,
    `     D r_lg5           s              5  0                                      longueur modulo 256`,
    `      *---------------------------------------------------------------*?`,
    `     C     *entry        plist`,
    `     C                   parm                    EntryParm`,
    ``,
    `      *---------------------------------------------------------------*?`,
    `     C     Decalage      begsr`,
    `     C                   clear                   r_long_lue        1 0`,
    `     C                   if        r_saved_lg = r_lg5`,
    `     C                   clear                   r_saved_lg`,
    `     C                   clear                   r_saved`,
    `     C                   movel     x'0000'       r_saved`,
    `     C                   else`,
    `     C                   eval      r_transf = r_saved`,
    `     C                   eval      r_saved = %subst(r_transf:(r_lg5+1))`,
    `     C                   eval      r_saved_lg = r_saved_lg - r_lg5`,
    `     C                   endif`,
    `     C                   clear                   r_lg5`,
    `     C                   endsr`,
    `      *---------------------------------------------------------------*?`,
  ].join(`\r\n`);

  const cache = await parser.getDocs(uri, lines, { ignoreCache: true, withIncludes: true, collectReferences: true });

  const EntryParm = cache.find(`EntryParm`)
  expect(EntryParm.references.length).toBe(2);
  expect(EntryParm.references.every(ref => lines.substring(ref.offset.start, ref.offset.end) === `EntryParm`)).toBe(true);

  const r_transf = cache.find(`r_transf`);
  expect(r_transf.references.length).toBe(3);
  expect(r_transf.references.every(ref => lines.substring(ref.offset.start, ref.offset.end) === `r_transf`)).toBe(true);

  const r_long_lue = cache.find(`r_long_lue`);
  expect(r_long_lue.references.length).toBe(1);
});

test('references_16_fixed_5', async () => {
  const lines = [
    `     Fqlaser    O    F  198        PRINTER OFLIND(*INOV) usropn`,
    `      *---------------------------------------------------------------*?`,
    `     D USRS            S             35    DIM(10)`,
    `     D Dadp            s              6  0`,
    `      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*?`,
    `     D MinEur          s              1  0`,
    `     D MinEur1         c                   1`,
    `     D MinEur2         c                   2`,
    `     D MinEur3         c                   5`,
    `     D MinEur4         c                   10`,
    `     D MinEur5         c                   20`,
    `     D MinEur6         c                   50`,
    `     D MinEur7         c                   100`,
    `     D MinEur8         c                   200`,
    `     D MinEur9         c                   500`,
    `      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*?`,
    `     D line            s              3  0`,
    `     D date7sub1       s              7  0`,
    `     D date7           s              7  0`,
    `     D jour            s              2`,
    `     D tcds            s             15  7`,
    `     D tcde            s             15  7`,
    `     D tc$             s             24`,
    ``,
    `     D mntA            s             13  2`,
    `     D mntB            s             15  2`,
    `     D mntC            s             15  2`,
    `     D mntD            s             15  7`,
    `     D mntD$           s             19`,
    `     D mntE            s             15  7`,
    `     D mntE$           s             19`,
    `     D mntF            s             15  2`,
    `     D mntG            s             15  2`,
    `     D mntH            s             15  2`,
    `     D mntI            s             15  2`,
    `     D mntJ            s             15  2`,
    `     D mntJtot         s             15  2`,
    `     D mntK            s             15  2`,
    `     D PrintTotal      s              1`,
    `      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*?`,
    `     D                UDS`,
    `     D  DATE                   1      6  0`,
    `     D  INTER                  7      7`,
    `     D  DadpIn                11     16`,
    `     D  CodeIn                17     17`,
    `     D  #p4                   84     84`,
    `      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*?`,
    `      /copy i,#$edti`,
    `      /copy i,#$rnse`,
    `      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*?`,
    `      /copy i,#$rnhi`,
    `     D mmdshi          ds           900`,
    `     D  mmREJO               100    101P 0`,
    `     D  mmNUEN               102    105P 0`,
    `     D  mmAEAN               106    109P 0`,
    `      *---------------------------------------------------------------*?`,
    `     C                   eval      *inLR = *ON`,
    `      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*?`,
    `     C                   if        #p4 = '$'`,
    ``,
    `     C                   if           %check('0123456789':dadpin) > 0`,
    `     C                             or %check('0123456789':codein) > 0`,
    `     C                   return`,
    `     C                   endif`,
    ``,
    `     C                   else`,
    `     C                   movel     date          dadpin`,
    `     C                   eval      codein = '5'`,
    `     C                   endif`,
    `      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*?`,
    `     C                   movel     dadpin        dadp`,
    `     C                   if        dadp < 010101`,
    `     C                   return`,
    `     C                   endif`,
    `     C                   movel     codein        MinEur`,
    `      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*?`,
    `     C                   call      'DAIS7'`,
    `     C                   parm      dadp          date7`,
    `     C                   if        date7 < 1040102 or date7 > 1991230`,
    `     C                   return`,
    `     C                   endif`,
    `      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*?`,
    `      *?sub 1 day?`,
    `     C                   call      'DACLTT7'`,
    `     C                   parm      date7         date7sub1`,
    `     C                   parm      -1            JOURS             3 0`,
    `     C                   parm      '-'           RTNCOD            1`,
    `      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*?`,
    `     C                   if        INTER = 'I'`,
    `     C                   eval      EDTIMC = 'N'`,
    `     C                   endif`,
    `      /copy i,##edti#ini`,
    `      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*?`,
    `      *?Compte de start:?lecture de tous les soldes (1 par devise)`,
    `     C                   eval      FSDADP = date7sub1`,
    `     C                   eval      secode = 'S'`,
    `     C                   eval      FSGECO = 1`,
    `     C                   eval      FSNUCO = 9000074`,
    `     C                   eval      fssuco = 'C'`,
    `     C                   eval      sezone = '+S  '`,
    `     C                   eval      sekylf = 'GNLS '`,
    `     C                   call      'RNSE'`,
    `     C                   parm                    DSSE`,
    ``,
    `     C                   do        *hival`,
    `     C                   eval      secode = '%'`,
    `     C                   call      'RNSE'`,
    `     C                   parm                    DSSE`,
    ``,
    `     C                   if        SECODE = 'E'`,
    `     C                   leave`,
    `     C                   endif`,
    ``,
    `     C                   if        FSLADE = 'EUR'`,
    `     C                   iter`,
    `     C                   endif`,
    ``,
    `     C                   clear                   PrintTotal`,
    `     C                   eval      mntA = FSMOSO`,
    ``,
    `      *?Conversion du solde de la veille, au taux de la veille?`,
    `     c                   call      'EURMODV15'`,
    `     c                   parm                    fslade`,
    `     c                   parm      'EUR'         syde              3`,
    `     c                   parm      date7sub1     dacr              7 0`,
    `     c                   parm      0             mntD`,
    `     c                   parm      0             dvfrto            1 0`,
    `     c                   parm      ' '           isfrto            1`,
    `     C                   parm      fsmoso        mntF`,
    ``,
    `      *?Conversion du solde de la veille, au taux du jour?`,
    `     c                   call      'EURMODV15'`,
    `     c                   parm                    fslade`,
    `     c                   parm      'EUR'         syde              3`,
    `     c                   parm      date7         dacr              7 0`,
    `     c                   parm      0             mntE`,
    `     c                   parm      0             dvfrto            1 0`,
    `     c                   parm      ' '           isfrto            1`,
    `     C                   parm      fsmoso        mntG`,
    ``,
    `     C                   eval      mntH = mntG - mntF`,
    ``,
    `     C                   if        line = 0 or line > edtiov`,
    `     C                   exsr      newpag`,
    `     C                   endif`,
    ``,
    `     C                   if           MinEur = 0 and %abs(mntH) > 0`,
    `     C                             or MinEur = 1 and %abs(mntH) >= MinEur1`,
    `     C                             or MinEur = 2 and %abs(mntH) >= MinEur2`,
    `     C                             or MinEur = 3 and %abs(mntH) >= MinEur3`,
    `     C                             or MinEur = 4 and %abs(mntH) >= MinEur4`,
    `     C                             or MinEur = 5 and %abs(mntH) >= MinEur5`,
    `     C                             or MinEur = 6 and %abs(mntH) >= MinEur6`,
    `     C                             or MinEur = 7 and %abs(mntH) >= MinEur7`,
    `     C                             or MinEur = 8 and %abs(mntH) >= MinEur8`,
    `     C                             or MinEur = 9 and %abs(mntH) >= MinEur9`,
    `     C                   except    solde`,
    `     C                   add       1             line`,
    `     C                   eval      PrintTotal = '*'`,
    `     C                   endif`,
    ``,
    `     C                   clear                   mntJtot`,
    ``,
    `      *?Historique du compte du jour, dans la devise?`,
    `     C                   clear                   dshi`,
    `     C                   eval      hilf = 'LHIGLDP'`,
    `     C                   eval      higenu = '+'`,
    `     C                   eval      fhgeco = fsgeco`,
    `     C                   eval      fhnuco = fsnuco`,
    `     C                   eval      hidadp = '='`,
    `     C                   eval      fhdadp = date7`,
    `     C                   eval      hilade = '='`,
    `     C                   eval      fhlade = fslade`,
    `      *?setll histo:?`,
    `     C                   eval      hicode = 'S'`,
    `     C                   call      'RNHI'`,
    `     C                   parm                    dshi`,
    `      *?read histo:?`,
    `     C                   do        *hival`,
    `     C                   eval      hicode = 'R'`,
    `     C                   call      'RNHI'`,
    `     C                   parm                    dshi`,
    ``,
    `     C                   if        hicode = 'E'`,
    `     C                   exsr      PrtTotal`,
    `     C                   leave`,
    `     C                   endif`,
    ``,
    `     C                   eval      mmdshi = dshi`,
    `     C                   exsr      operation`,
    `     C                   eval      dshi = mmdshi`,
    ``,
    `     C                   enddo`,
    ``,
    `     C                   enddo`,
  ].join(`\r\n`);

  const cache = await parser.getDocs(uri, lines, { ignoreCache: true, withIncludes: true, collectReferences: true });

  const MinEur2 = cache.find(`MinEur2`);
  expect(MinEur2.references.length).toBe(2);
  expect(MinEur2.references.every(ref => lines.substring(ref.offset.start, ref.offset.end).toUpperCase() === `MINEUR2`)).toBe(true);

  const MinEur = cache.find(`MinEur`);
  expect(MinEur.references.length).toBe(12);
  expect(MinEur.references.every(ref => lines.substring(ref.offset.start, ref.offset.end).toUpperCase() === `MINEUR`)).toBe(true);

  const line = cache.find(`line`);
  expect(line.references.length).toBe(4);
  expect(line.references.every(ref => lines.substring(ref.offset.start, ref.offset.end) === `line`)).toBe(true);

  const DadpIn = cache.find(`DadpIn`);
  expect(DadpIn.references.length).toBe(4);
  expect(DadpIn.references.every(ref => lines.substring(ref.offset.start, ref.offset.end).toUpperCase() === `DADPIN`)).toBe(true);

  const mntJtot = cache.find(`mntJtot`);
  expect(mntJtot.references.length).toBe(2);
  expect(mntJtot.references.every(ref => lines.substring(ref.offset.start, ref.offset.end) === `mntJtot`)).toBe(true);

  const syde = cache.find(`syde`);
  expect(syde.references.length).toBe(2);
  expect(syde.references.every(ref => lines.substring(ref.offset.start, ref.offset.end) === `syde`)).toBe(true);
});

test('references_17_fixed_6', async () => {
  const lines = [
    `      *%CSTD===========================================================*`,
    `      ** Application. : PEC PECAM                                      *`,
    `      ** Component. . : AUD003                        Type: RPGLE      *`,
    `      **===============================================================*`,
    `      ** Sub-system . :                                                *`,
    `      ** Function . . :                                                *`,
    `      ** Sub-function :                                                *`,
    `      **%S=============================================================*`,
    `      ** Description of functions:                                     *`,
    `      **                                                               *`,
    `      **                                                               *`,
    `      **                                                               *`,
    `      **%E=============================================================*`,
    `      *%ECSTD==========================================================*`,
    ``,
    ``,
    `     fLSIKODO   iP   e           k disk`,
    `     faud003O   o    e           k disk    usropn extfile('QTEMP/AUD003O')`,
    ``,
    `     d AUD003          PR`,
    `     d  type                          5    const`,
    `     d  req                           1    const`,
    ``,
    `     d AUD003          PI`,
    `     d  type                          5    const`,
    `     d  req                           1    const`,
    ``,
    `      /copy qprotosrc,excproto`,
    `      /copy qprotosrc,mail`,
    `      /copy qprotosrc,maildist`,
    `      /copy qprotosrc,toolproc`,
    `      /copy qprotosrc,user`,
    `      /copy qprotosrc,path`,
    `      /copy qprotosrc,list`,
    `      /copy qprotosrc,siko`,
    `      /copy qprotosrc,adpo`,
    ``,
    `     d  user           s             10     inz(*user)`,
    `     d  path           s            100`,
    `     d  file           s            150`,
    `     d msgkey          s              4`,
    `     d dest            s              4`,
    `     d datiso          s              5i 0`,
    `     d dec2            s              5i 0`,
    `     d cent            s              5i 0`,
    `     d today           s               d    inz(*sys)`,
    `     d automat         s               n`,
    `     d catbil          s              6`,
    ``,
    `      * common error data structure`,
    ``,
    `     D errcod          ds`,
    `     D  bytpro                       10i 0 INZ(256)`,
    `     D  bytava                       10i 0`,
    `     D  errmsgid                      7`,
    `     D                                1`,
    `     D  errmsgdta                   240`,
    ``,
    `      /free`,
    ``,
    ``,
    `        begsr *inzsr;`,
    `          // si rapport automatique envoi à la liste et chaque gestionnaire`,
    `          automat = req <> 'Y';`,
    `          if automat;`,
    `            callp(e) docmd('x');`,
    `            if %error;`,
    `              callp(e) docmd('clrpfm x ');`,
    `            endif;`,
    `            open aud003o;`,
    `          endif;`,
    `          path = getpath('*DFT');`,
    `          file = %trim(path) + 'x ' + %trim(type) + '.xls';`,
    `          exopenfile(file);`,
    `           // creation des styles`,
    `          datiso = ExCreateStyle ();`,
    `          ExSetFormat(datiso :FORMAT_CUSTOM :'d/m/yyyy');`,
    `          cent  =  ExCreateStyle ();`,
    `          exsetAlign(cent:ALIGN_CENTER);`,
    `          dec2  =  ExCreateStyle ();`,
    `          ExSetFormat(dec2 :FORMAT_4);`,
    `          exaddsheet();`,
    `          exFreezePane(2:2);`,
    `          exsethead(HF_FILE);`,
    `          exsetfoot(HF_CURTOTPAGE:POS_RIGHT);`,
    `          exsetlandscape(*ON);`,
    `          exsetgridprint(*ON);`,
    `          exSetRepRowCol(1:1:0:0);`,
    `          exaddrow(0:3);`,
    ``,
    `        endsr;`,
    `        begsr srend;`,
    `          exclosefile();`,
    `          // Si appelé par menu envoi au requester`,
    `          if automat;`,
    `            if type = 'ALL'`,
    `              or type = 'GEST' and %subst(catbil:1:4) = 'GEST'`,
    `              or type = 'OTHER' and %subst(catbil:1:4) <> 'GEST';`,
    `              crtdstmail('SLEE1_GEST');`,
    `            elseif type = 'OTHER';`,
    `              crtdstmail('SLEE1_OTH');`,
    `            else;`,
    `              crtdstmail('SLEEPING');`,
    `            endif;`,
    `          else;`,
    `            openMail('x');`,
    `            MailAddSender(' ' :getusremail());`,
    `            MailAddDist(' ':getUsreMail():0);`,
    `          endif;`,
    ``,
    `        endsr ;`,
    `      /end-free`,
  ].join(`\r\n`);

  const cache = await parser.getDocs(uri, lines, { ignoreCache: true, withIncludes: true, collectReferences: true });

  const automat = cache.find(`automat`);
  expect(automat.references.length).toBe(4);
  expect(automat.references.every(ref => lines.substring(ref.offset.start, ref.offset.end) === `automat`)).toBe(true);
});

test('references_18_fixed_7', async () => {
  const lines = [
    ``,
    `      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*?`,
    `     D Z160            DS           160`,
    `     D  SRCDTA                 1    120`,
    `     D  LAS                    1    132    dim(132)`,
    `      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*?`,
    `     C                   movea     'Solde titres'las(ll)`,
    `     C                   add       12            ll`,
    `     C                   movea     ' du Client: 'las(ll)`,
    `     C                   add       12            ll`,
    ``,
  ].join(`\r\n`);

  const cache = await parser.getDocs(uri, lines, { ignoreCache: true, withIncludes: true, collectReferences: true });

  const las = cache.find(`LAS`);
  expect(las.references.length).toBe(3);
  expect(las.references.every(ref => lines.substring(ref.offset.start, ref.offset.end).toUpperCase() === `LAS`)).toBe(true);
});

test('references_19_fixed_8', async () => {
  const lines = [
    `     d data6g          ds                  dim(7) export qualified`,
    `     d                                     inz`,
    `     d rg                             9  4 dim(12)`,
    ``,
    `     d data6d          s              7  0 dim(12) export`,
    ``,
    `     d  dat2           s              7  0  import`,
    ``,
    `     d  xdate          s               d`,
    `     d  sdat           s              7  0`,
    `     d  i              s              5i 0`,
    `     d  j              s              5i 0`,
    `     d data            ds                  inz`,
    `     d   sumx                        13  0`,
    `     d   retx                        13  0`,
    `     d   sumy                        13  0`,
    `     d   rety                        13  0`,
    `     d   sum1                        13  0`,
    `     d   ret1                        13  0`,
    `     d   sum2                        13  0`,
    `     d   ret2                        13  0`,
    `     d   sum3                        13  0`,
    `     d   ret3                        13  0`,
    `     d   sum5                        13  0`,
    `     d   ret5                        13  0`,
    `     d   sum0                        13  0`,
    `     d   ret0                        13  0`,
    `      /free`,
    `        reset data6g;`,
    `        // hello world`,
    `        i = 1;`,
    `        data6d(i) = dat2;`,
    `        sdat = data6d(i);`,
    `        exsr sql1;`,
    `        xdate = %date(dat2:*cymd);`,
    `        for i = 2 to 12;`,
    `          xdate -= %days(%subdt(xdate:*d));`,
    `          data6d(i) = %dec(xdate:*cymd);`,
    `          sdat = data6d(i);`,
    `          exsr sql1;`,
    `        endfor;`,
    `        *inlr = *on;`,
    ``,
    `        begsr sql1;`,
    `          reset data;`,
    `          exec sql SELECT`,
    ``,
    `          sum(whoops) as sumx,`,
    `          sum(awesome) as retx,`,
    `          sum(case when scooby <> ' ' then whoops else 0 end ) as sumy,`,
    `          sum(case when scooby <> ' ' then awesome else 0 end ) as rety,`,
    `          sum(case when scooby = '1' then whoops else 0 end ) as sum1,`,
    `          sum(case when scooby = '1' then awesome else 0 end ) as ret1,`,
    `          sum(case when scooby = '2' then whoops else 0 end ) as sum2,`,
    `          sum(case when scooby = '2' then awesome else 0 end ) as ret2,`,
    `          sum(case when scooby = '3' then whoops else 0 end ) as sum3,`,
    `          sum(case when scooby = '3' then awesome else 0 end ) as ret3,`,
    `          sum(case when scooby = '5' then whoops else 0 end ) as sum5,`,
    `          sum(case when scooby = '5' then awesome else 0 end ) as ret5,`,
    `          sum(case when scooby = ' ' then whoops else 0 end ) as sum0,`,
    `          sum(case when scooby = ' ' then awesome else 0 end ) as ret0`,
    `          into :data`,
    `          FROM pcstatko`,
    `          WHERE skprive = '1' and skdate = :sdat and`,
    `            skgest in (select cluser from pcusrlst where clname = 'RRO')`,
    `            and scooby in (' ', '1', '2', '3', '5')`,
    `            and whoops > 0 ;`,
  ].join(`\r\n`);

  const cache = await parser.getDocs(uri, lines, { ignoreCache: true, withIncludes: true, collectReferences: true });

  const data = cache.find(`data`);
  expect(data.references.length).toBe(3);
  expect(data.references.every(ref => lines.substring(ref.offset.start, ref.offset.end) === `data`)).toBe(true);

  const sumy = cache.find(`sumy`);
  expect(sumy.references.length).toBe(1);
  const aroundSumy = lines.substring(sumy.references[0].offset.start - 10, sumy.references[0].offset.end + 10);
  expect(aroundSumy).toContain(`d   sumy`);
});

test(`references_20`, async () => {
  const lines = [
    `**FREE`,
    `Ctl-Opt DftActGrp(*No);`,
    `/copy './rpgle/copy5.rpgleinc'`,
    ``,
    `dcl-s MyVar char(LENGTH_t);`,
    ``,
    `dsply MyVar;`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, { ignoreCache: true, withIncludes: true, collectReferences: true });

  const Length_t = cache.find(`LENGTH_t`);
  expect(Length_t.references.length).toBe(3);

  expect(Length_t.position.path).not.toBe(uri);

  const uniqueUris = Length_t.references.map(ref => ref.uri).filter((value, index, self) => self.indexOf(value) === index);
  expect(uniqueUris.length).toBe(2);

  const include = uniqueUris.find(uri => uri.endsWith(`copy5.rpgleinc`));
  expect(include).toBeDefined();

  const rawCopyBook = await readFile(include, 'utf-8');
  const copyRefs = Length_t.references.filter(ref => ref.uri === include);
  expect(copyRefs.length).toBe(2);
  expect(copyRefs.every(ref => rawCopyBook.substring(ref.offset.start, ref.offset.end).toUpperCase() === `LENGTH_T`)).toBe(true);

  const baseRefs = Length_t.references.filter(ref => ref.uri === uri);
  expect(baseRefs.length).toBe(1);
  expect(baseRefs.every(ref => lines.substring(ref.offset.start, ref.offset.end).toUpperCase() === `LENGTH_T`)).toBe(true);
});

test(`references_21_fixed_exec1`, async () => {
  const lines = [
    ``,
    `     d  tlst           s             10    inz('RRO')`,
    ``,
    `     c     sropen        begsr`,
    `     c                   if        byList  or  gest = '*'`,
    `     c                   if        byList`,
    `     c                   eval      tlst = liste`,
    `     c                   else`,
    `     c                   reset                   tlst`,
    `     c                   endif`,
    `     c/exec sql`,
    `     C+ declare C1 cursor for`,
    `      /copy copy,rro100k1`,
    `     c+  and  skgest in (select cluser from pcusrlst where clname = :tlst)`,
    `     C+ group by stcdpy`,
    `     C+ order by xx  desc`,
    `     c/end-exec`,
    `     c/exec sql`,
    `     C+ open C1`,
    `     c/end-exec`,
  ].join(`\r\n`);

  const cache = await parser.getDocs(uri, lines, { ignoreCache: true, withIncludes: true, collectReferences: true });
  
  const tlst = cache.find(`tlst`);
  expect(tlst.references.length).toBe(4);
  expect(tlst.references.every(ref => lines.substring(ref.offset.start, ref.offset.end) === `tlst`)).toBe(true);
});


// Test case is from noxDb
test(`references_22_long_lines`, async () => {
  const lines = [
    `**free`,
  `// --------------------------------------------------------------`,
  `// Next departure from Mols Linien`,
  `// --------------------------------------------------------------`,
  `dcl-proc jsonRequest;`,
  ``,
  `   dcl-s  pReq   	  	pointer;`,
  `   dcl-s  pResponse 	pointer;`,
  `   dcl-s  url  	  	varchar(1024);`,
  ``,
  `   // parameters on URL`,
  `   url = 'https://www.molslinjen.dk/umbraco/api/departure/getnextdepartures?departureRegionId=JYL';`,
  ``,
  `   // Note: No payload in the request. use *null - here we pass a null pointer  `,
  `   // Note: No options in the request. use *null - here we pass the *null literal value`,
  `   `,
  `   // Do the http request to get next depature`,
  `   // Use YUM to install curl, which is the tool used by httpRequest`,
  `   pResponse = json_httpRequest (url: pReq:*null:'JSON');`,
  ``,
  `   json_WriteJsonStmf(pResponse:'/prj/noxdb/testout/httpdump.json':1208:*OFF);`,
  ``,
  `   json_delete(pReq);`,
  `   json_delete(pResponse);`,
  ``,
  `end-proc;`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, { ignoreCache: true, withIncludes: true, collectReferences: true });

  const jsonRequest = cache.find(`jsonRequest`);
  const pReq = jsonRequest.scope.find(`pReq`);
  expect(pReq.references.length).toBe(3);
  expect(pReq.references.every(ref => lines.substring(ref.offset.start, ref.offset.end) === `pReq`)).toBe(true);
});

// Test case is from noxDb
test('references_23_before_spaces', async () => {
  const lines = [
    `**free`,
    ``,
    `dcl-s  err                ind;`,
    `dcl-s  row                varChar(32766);`,
    `dcl-s  pRow               pointer;`,
    `dcl-s  id                 int(10);`,
    ``,
    `   // now get that row: here we use the a stringed object to build the where statement via the`,
    `   pRow = json_sqlResultRow ((`,
    `      'Select *                -`,
    `       from noxdbdemo.note2    -`,
    `       where id = $id          -`,
    `      ')`,
    `      :'{id:' + %char(id) +'}'`,
    `   );`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, { ignoreCache: true, withIncludes: true, collectReferences: true });

  const id = cache.find(`id`);
  expect(id.references.length).toBe(2);
  expect(id.references.every(ref => lines.substring(ref.offset.start, ref.offset.end) === `id`)).toBe(true);
});

// Test case is from rpgle-repl
test('references_24_comment_in_statement', async () => {
  const lines = [
    `**free`,
    ``,
    `dcl-proc freeFormatEvaluationFound export;`,
    `  dcl-pi *n ind;`,
    `    code like(t_longLineOfCode) const;`,
    `    triggerType like(t_triggerType);`,
    `  end-pi;`,
    ``,
    `  if %len(%trim(code)) >= 10 `,
    `  and %lower(%subst(%trim(code): 1: 10)) = 'replprint(';`,
    `    triggerType = c_replPrintStatement;`,
    `    return *on;`,
    `  endif;`,
    ``,
    `  if %len(%trim(code)) >= 11`,
    `  and %lower(%subst(%trim(code): 1: 11)) = 'replequals(';`,
    `    triggerType = c_replEqualsStatement;`,
    `    return *on;`,
    `  endif;`,
    ``,
    `  if %scan('IF ': %trim(toUpperCase(code))) = 1`,
    `  or %scan('IF%': %trim(toUpperCase(code))) = 1`,
    `  or %scan('ELSE ': %trim(toUpperCase(code))) = 1`,
    `  // why do we need the next case?`,
    `  //or %scan('ELSE%': %trim(toUpperCase(code))) = 1`,
    `  or %scan('ELSE;': %trim(toUpperCase(code))) = 1`,
    `  or %scan('ELSEIF ': %trim(toUpperCase(code))) = 1`,
    `  or %scan('ELSEIF%': %trim(toUpperCase(code))) = 1`,
    `  or %scan('WHEN ': %trim(toUpperCase(code))) = 1`,
    `  or %scan('WHEN%': %trim(toUpperCase(code))) = 1`,
    `  or %scan('OTHER ': %trim(toUpperCase(code))) = 1`,
    `  or %scan('OTHER;': %trim(toUpperCase(code))) = 1`,
    `  or %scan('ON-ERROR ': %trim(toUpperCase(code))) = 1`,
    `  or %scan('ON-ERROR;': %trim(toUpperCase(code))) = 1;`,
    ``,
    `    triggerType = c_conditionalStatement;`,
    `    return *on;`,
    ``,
    `  endif;`,
    ``,
    `  if %scan('EXEC SQL': %trim(toUpperCase(code))) = 1;`,
    ``,
    `    // DECLARE statement are non-executable, so don't`,
    `    //  evaluate them - check by removing blanks, uppercasing,`,
    `    //  and trimming.`,
    `    if %scan('EXECSQLDECLARE':`,
    `             %trim(toUpperCase(%scanrpl(' ':'':code)))`,
    `            ) <> 1;`,
    ``,
    `      triggerType = c_sqlStatement;`,
    `      return *on;`,
    ``,
    `    endif;`,
    ``,
    `  endif;`,
    ``,
    `  if %scan('DOW ': %trim(toUpperCase(code))) = 1`,
    `  or %scan('DOW%': %trim(toUpperCase(code))) = 1`,
    `  or %scan('DOU ': %trim(toUpperCase(code))) = 1`,
    `  or %scan('DOU%': %trim(toUpperCase(code))) = 1`,
    `  or %scan('FOR ': %trim(toUpperCase(code))) = 1;`,
    `  // why do we need the next case?`,
    `  //or %scan('FOR%': %trim(toUpperCase(code))) = 1;`,
    ``,
    `    triggerType = c_loopStatement;`,
    `    return *on;`,
    ``,
    `  endif;`,
    ``,
    `  if %scan('=': %trim(code)) <> 0;`,
    ``,
    `    triggerType = c_setValueStatement;`,
    `    return *on;`,
    ``,
    `  endif;`,
    ``,
    `  return *off;`,
    ``,
    `end-proc;`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, { ignoreCache: true, withIncludes: true, collectReferences: true });
  const freeFormatEvaluationFound = cache.find(`freeFormatEvaluationFound`);
  const code = freeFormatEvaluationFound.scope.find(`code`);
  expect(code.references.length).toBe(25);
  expect(code.references.every(ref => lines.substring(ref.offset.start, ref.offset.end) === `code`)).toBe(true);
});

// Test case is from httpapi
test('references_25_fixed_string', async () => {
  const lines = [
    `     D http            s           5050a   varying`,
    `     D rc              s             10i 0`,
    ``,
    `      /free`,
    `         http_debug(*on: '/tmp/example9-debug.txt');`,
    ``,
    `         if %parms < 3;`,
    `            http_comp('To call, type: +`,
    `         EXAMPLE9 URL(''http://google.com'') STMF(''/tmp/google.pdf'')');`,
    `            return;`,
    `         endif;`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, { ignoreCache: true, withIncludes: true, collectReferences: true });
  const http = cache.find(`http`);
  expect(http.references.length).toBe(1);
  expect(http.references.every(ref => lines.substring(ref.offset.start, ref.offset.end) === `http`)).toBe(true);
});

test('references_26_fixed_tag', async () => {
  const lines = [
    `     C     SCRNFY        IFNE      wCfgNUSRNF`,
    `     C                   EVAL      wCfgKey = 'NUSRNF'`,
    `     C     wCfgKey       CHAIN     PCONFIG                            81`,
    `     C  N81              EVAL      CNFVAL = SCRNFY`,
    `     C  N81              UPDATE    CONFIG                               81`,
    `     C   81              GOTO      UPDKO`,
    `     C                   ENDIF`,
    `     C     UPDOK         TAG`,
    `     C                   EVAL      MSGLIN = cSavedOK`,
    `     C                   GOTO      UPDEND`,
    `     C     UPDKO         TAG`,
    `     C                   EVAL      MSGLIN = cSavedKO`,
    `     C     UPDEND        TAG`,
    `     C                   EVAL      *IN80 = *ON`,
    `     C                   ENDSR`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, { ignoreCache: true, withIncludes: true, collectReferences: true });
  const updok = cache.find(`UPDKO`);
  expect(updok.references.length).toBe(2);
  expect(updok.references.every(ref => lines.substring(ref.offset.start, ref.offset.end) === `UPDKO`)).toBe(true);
});

// This test case is from bbs400
test('references_27_fixed_reference', async () => {
  const lines = [
    `     H/TITLE Administration - General Configuration`,
    `     H COPYRIGHT('(C) 2020 David Asta under MIT License')`,
    `      * SYSTEM      : V4R5`,
    `      * PROGRAMMER  : David Asta`,
    `      * DATE-WRITTEN: 12/NOV/2020`,
    `      *`,
    `      * This program allows an Administrator user to display/change the`,
    `      *   general Configuration values of the BBS stored in PCONFIG`,
    `      **********************************************************************`,
    `      /copy 'CBKOPTIMIZ.rpgle'`,
    `      **********************************************************************`,
    `      * INDICATORS USED:`,
    `      * 80 - *ON turns DSPATR(PR), which protects fields from being changed`,
    `      * 81 - CBKPCFGREA CHAIN Not Found`,
    `      **********************************************************************`,
    `     FBBSADGCD  CF   E             WORKSTN`,
    `     FPCONFIG   UF   E           K DISK`,
    `      **********************************************************************`,
    `      * Data structures`,
    `      /copy 'CBKDTAARA.rpgle'`,
    `      * Constants`,
    `     D cKeysDft        C                   CONST('F10=Edit   F12=Go back')`,
    `     D cKeysEdit       C                   CONST('F10=Confirm Changes   F12=Can-`,
    `     D                                     cel')`,
    `     D cSavedOK        C                   CONST('Configuration was changed suc-`,
    `     D                                     cessfully.')`,
    `     D cSavedKO        C                   CONST('There was an error while writ-`,
    `     D                                     ting to PCONFIG.')`,
    `      * Variables`,
    `      /copy 'CBKPCFGDCL.rpgle'`,
    `     D wCfgKey         S              6A`,
    `     D wShowWelcome    S              1A`,
    `     ***********************************************************************`,
    `     C                   WRITE     HEADER`,
    `     C   80              EVAL      KEYSLS = cKeysDft`,
    `     C  N80              EVAL      KEYSLS = cKeysEdit`,
    `     C                   WRITE     FOOTER`,
    `     C                   EXFMT     BODY`,
    `     C                   CLEAR                   MSGLIN`,
    `     C                   EXSR      CheckFkeys`,
    `      **********************************************************************`,
    `      * Subroutine called automatically at startup`,
    `      **********************************************************************`,
    `     C     *INZSR        BEGSR`,
    `     C                   EVAL      SCRSCR = 'BBSADGC'`,
    `      * Protect fields from being modified`,
    `     C                   EVAL      *IN80 = *ON`,
    `      * Get values from PCONFIG and show them on screen`,
    `     C                   EXSR      GetConfig`,
    `     C                   EVAL      SCRNAM = wCfgBBSNAM`,
    `     C                   EVAL      SCRLCR = WCfgLOCCRY`,
    `     C                   EVAL      SCRLCT = wCfgLOCCTY`,
    `     C                   EVAL      SCRTZC = wCfgTIMZON`,
    `     C                   EVAL      SCRCLO = wCfgCLOSED`,
    `     C                   EVAL      SCRSAL = wCfgSHWALD`,
    `     C                   EVAL      SCRSWE = wCfgSHWWEL`,
    `     C                   EVAL      SCRHID = wCfgHIDESO`,
    `     C                   EVAL      SCRHLS = wCfgHLSOMS`,
    `     C                   EVAL      SCRNFY = wCfgNUSRNF`,
    `      * Get values from DATAARA and show them on screen`,
    `      /copy 'CBKHEADER.rpgle'`,
    `     C                   ENDSR`,
    `      **********************************************************************`,
    `      * Check Function keys pressed by the user`,
    `      **********************************************************************`,
    `     C     CheckFkeys    BEGSR`,
    `      * F10=Edit`,
    `     C                   IF        *IN10 = *ON`,
    `     C* N80              EXSR      SavePCONFIG`,
    `     C*  80              EVAL      *IN80 = *OFF`,
    `     C                   IF        *IN80 = *ON`,
    `     C                   EVAL      *IN80 = *OFF`,
    `     C                   ELSE`,
    `     C                   EXSR      SavePCONFIG`,
    `     C                   ENDIF`,
    `     C                   ENDIF`,
    `      * F12=Go back or F12=Cancel`,
    `     C                   IF        *IN12 = *ON`,
    `     C   80              MOVE      *ON           *INLR`,
    `     C   80              RETURN`,
    `     C  N80              EVAL      *IN80 = *ON`,
    `     C                   ENDIF`,
    `     C                   ENDSR`,
    `      **********************************************************************`,
    `      * Save changed values to PCONFIG`,
    `      **********************************************************************`,
    `     C     SavePCONFIG   BEGSR`,
    `      * BBS Name`,
    `     C     SCRNAM        IFNE      wCfgBBSNAM`,
    `     C                   EVAL      wCfgKey = 'BBSNAM'`,
    `     C     wCfgKey       CHAIN     PCONFIG                            81`,
    `     C  N81              EVAL      CNFVAL = SCRNAM`,
    `     C  N81              UPDATE    CONFIG                               81`,
    `     C   81              GOTO      UPDKO`,
    `     C                   ENDIF`,
    `      * BBS Location Country`,
    `     C     SCRLCR        IFNE      WCfgLOCCRY`,
    `     C                   EVAL      wCfgKey = 'LOCCRY'`,
    `     C     wCfgKey       CHAIN     PCONFIG                            81`,
    `     C  N81              EVAL      CNFVAL = SCRLCR`,
    `     C  N81              UPDATE    CONFIG                               81`,
    `     C   81              GOTO      UPDKO`,
    `     C                   ENDIF`,
    `      * BBS Location City`,
    `     C     SCRLCT        IFNE      wCfgLOCCTY`,
    `     C                   EVAL      wCfgKey = 'LOCCTY'`,
    `     C     wCfgKey       CHAIN     PCONFIG                            81`,
    `     C  N81              EVAL      CNFVAL = SCRLCT`,
    `     C  N81              UPDATE    CONFIG                               81`,
    `     C   81              GOTO      UPDKO`,
    `     C                   ENDIF`,
    `      * BBS Time Zone`,
    `     C     SCRTZC        IFNE      wCfgTIMZON`,
    `     C                   EVAL      wCfgKey = 'TIMZON'`,
    `     C     wCfgKey       CHAIN     PCONFIG                            81`,
    `     C  N81              EVAL      CNFVAL = SCRTZC`,
    `     C  N81              UPDATE    CONFIG                               81`,
    `     C   81              GOTO      UPDKO`,
    `     C                   ENDIF`,
    `      * Closed to New Users?`,
    `     C     SCRCLO        IFNE      wCfgCLOSED`,
    `     C                   EVAL      wCfgKey = 'CLOSED'`,
    `     C     wCfgKey       CHAIN     PCONFIG                            81`,
    `     C  N81              EVAL      CNFVAL = SCRCLO`,
    `     C  N81              UPDATE    CONFIG                               81`,
    `     C   81              GOTO      UPDKO`,
    `     C                   ENDIF`,
    `      * Show Access Level Description?`,
    `     C     SCRSAL        IFNE      wCfgSHWALD`,
    `     C                   EVAL      wCfgKey = 'SHWALD'`,
    `     C     wCfgKey       CHAIN     PCONFIG                            81`,
    `     C  N81              EVAL      CNFVAL = SCRSAL`,
    `     C  N81              UPDATE    CONFIG                               81`,
    `     C   81              GOTO      UPDKO`,
    `     C                   ENDIF`,
    `      * Show Welcome screen?`,
    `     C     SCRSWE        IFNE      wCfgSHWWEL`,
    `     C                   EVAL      wCfgKey = 'SHWWEL'`,
    `     C     wCfgKey       CHAIN     PCONFIG                            81`,
    `     C  N81              EVAL      CNFVAL = SCRSWE`,
    `     C  N81              UPDATE    CONFIG                               81`,
    `     C   81              GOTO      UPDKO`,
    `     C                   ENDIF`,
    `      * Hide SysOp from User Lists?`,
    `     C     SCRHID        IFNE      wCfgHIDESO`,
    `     C                   EVAL      wCfgKey = 'HIDESO'`,
    `     C     wCfgKey       CHAIN     PCONFIG                            81`,
    `     C  N81              EVAL      CNFVAL = SCRHID`,
    `     C  N81              UPDATE    CONFIG                               81`,
    `     C   81              GOTO      UPDKO`,
    `     C                   ENDIF`,
    `      * Hide SysOp from User Lists?`,
    `     C     SCRHLS        IFNE      wCfgHLSOMS`,
    `     C                   EVAL      wCfgKey = 'HLSOMS'`,
    `     C     wCfgKey       CHAIN     PCONFIG                            81`,
    `     C  N81              EVAL      CNFVAL = SCRHLS`,
    `     C  N81              UPDATE    CONFIG                               81`,
    `     C   81              GOTO      UPDKO`,
    `     C                   ENDIF`,
    `      * Notify New User Registration`,
    `     C     SCRNFY        IFNE      wCfgNUSRNF`,
    `     C                   EVAL      wCfgKey = 'NUSRNF'`,
    `     C     wCfgKey       CHAIN     PCONFIG                            81`,
    `     C  N81              EVAL      CNFVAL = SCRNFY`,
    `     C  N81              UPDATE    CONFIG                               81`,
    `     C   81              GOTO      UPDKO`,
    `     C                   ENDIF`,
    `     C     UPDOK         TAG`,
    `     C                   EVAL      MSGLIN = cSavedOK`,
    `     C                   GOTO      UPDEND`,
    `     C     UPDKO         TAG`,
    `     C                   EVAL      MSGLIN = cSavedKO`,
    `     C     UPDEND        TAG`,
    `     C                   EVAL      *IN80 = *ON`,
    `     C                   ENDSR`,
    `      **********************************************************************`,
    `      /copy 'CBKPCFGREA.rpgle'`,
    ``,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, { ignoreCache: true, withIncludes: true, collectReferences: true });
  expect(cache.includes.length).toBe (5);

  const wCfgKey = cache.find(`wCfgKey`);
  const baseRefs = wCfgKey.references.filter(r => r.uri === uri);
  expect(baseRefs.every(ref => lines.substring(ref.offset.start, ref.offset.end).toUpperCase() === `WCFGKEY`)).toBe(true);
  expect(baseRefs.length).toBe(21);

  const uniqueUris = wCfgKey.references.map(r => r.uri).filter((value, index, self) => self.indexOf(value) === index);
  let cachedFiles: {[uri: string]: string} = {};

  for (const refUri of uniqueUris) {
    if (refUri === uri) {
      cachedFiles[refUri] = lines;
    }
    
    if (!cachedFiles[refUri]) {
      cachedFiles[refUri] = await getFileContent(refUri);
    }
  }

  for (const ref of wCfgKey.references) {
    const offsetContent = cachedFiles[ref.uri].substring(ref.offset.start, ref.offset.end);

    expect(offsetContent.toUpperCase()).toBe(`WCFGKEY`);
  }
});

test('reference_28_parameters', async () => {
  const lines = [
    `**free`,
    ``,
    `ctl-opt dftactgrp(*no);`,
    ``,
    `dcl-pi upddept;`,
    `  deptno char(3);`,
    `  deptname char(36);`,
    `  mgrno char(6);`,
    `  admrdept char(3);`,
    `  location char(16);`,
    `end-pi;`,
    ``,
    `dcl-ds result qualified dim(1);`,
    `  success char(1);`,
    `end-ds;`,
    ``,
    `exec sql`,
    `  update dept`,
    `    set deptname = :deptname,`,
    `        mgrno = :mgrno,`,
    `        admrdept = :admrdept,`,
    `        location = :location`,
    `    where deptno = :deptno;`,
    ``,
    `if (SQLCOD = 0);`,
    `  result(1).success = 'Y';`,
    `else;`,
    `  result(1).success = 'N';`,
    `endif;`,
    ``,
    `dcl-s return char(length) inz('Y');`,
    ``,
    `exec sql set result sets array :result for 1 rows;`,
    `// Hello`,
    `return;`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, { ignoreCache: true, withIncludes: true, collectReferences: true });

  const deptno = cache.find(`deptno`);
  expect(deptno).toBeDefined();
  expect(deptno.references.length).toBe(2);
  expect(deptno.references.every(ref => lines.substring(ref.offset.start, ref.offset.end) === `deptno`)).toBe(true);
});