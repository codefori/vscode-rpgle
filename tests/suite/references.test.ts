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

  for (const ref of wkInvoice.references) {
    // console.log({
    //   ref,
    //   text: lines.substring(ref.offset.position, ref.offset.end),
    //   about: lines.substring(ref.offset.position - 10, ref.offset.end + 10)
    // });
    expect(lines.substring(ref.offset.position, ref.offset.end)).to.equal(`wkInvoice`);
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
  expect(TYPEMST_T.references.every(ref => lines.substring(ref.offset.position, ref.offset.end) === `TYPEMST_T`)).toBe(true);

  const TYPEMST_Ds = cache.find(`TYPEMST_Ds`);
  expect(TYPEMST_Ds.references.length).toBe(1);
  expect(TYPEMST_Ds.references.every(ref => lines.substring(ref.offset.position, ref.offset.end) === `TYPEMST_Ds`)).toBe(true);

  const TYPEMST_Dim = cache.find(`TYPEMST_Dim`);
  expect(TYPEMST_Dim.references.length).toBe(5);
  expect(TYPEMST_Dim.references.every(ref => lines.substring(ref.offset.position, ref.offset.end) === `TYPEMST_Dim`)).toBe(true);
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
  expect(per.references.length).toBe(6);
  expect(per.references.every(ref => lines.substring(ref.offset.position, ref.offset.end) === `per`)).toBe(true);

  const pmyy = cache.find(`pmyy`);
  expect(pmyy.references.length).toBe(2);
  expect(pmyy.references.every(ref => lines.substring(ref.offset.position, ref.offset.end) === `pmyy`)).toBe(true);

  const filedt = cache.find(`filedt`);
  expect(filedt.references.length).toBe(3);
  expect(filedt.references.every(ref => lines.substring(ref.offset.position, ref.offset.end) === `filedt`)).toBe(true);

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

  for (const ref of in10.references) {
    console.log(lines.substring(ref.offset.position, ref.offset.end));
  }
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
  expect(EntryParm.references.every(ref => lines.substring(ref.offset.position, ref.offset.end) === `EntryParm`)).toBe(true);

  const r_transf = cache.find(`r_transf`);
  expect(r_transf.references.length).toBe(3);
  expect(r_transf.references.every(ref => lines.substring(ref.offset.position, ref.offset.end) === `r_transf`)).toBe(true);
})