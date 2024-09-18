
import path from "path";
import setupParser from "../parserSetup";
import Linter from "../../language/linter";
import { test, expect } from "vitest";

const parser = setupParser();
const uri = `source.rpgle`;

test('vitestTest1', async () => {
  const lines = [
    `**FREE`,
    `Dcl-s MyVariable CHAR(20);`
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {withIncludes: true, ignoreCache: true});

  expect(cache.variables.length).toBe(1);
  expect(cache.variables[0].position.line).toBe(1);
});

/**
   * Multiple variable definition test
   */
test('vitestTest2', async () => {
  const lines = [
    `**FREE`,
    `Dcl-s MyVariable CHAR(20);`,
    ``,
    `Dcl-s MyVariable2 CHAR(20);`
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {withIncludes: true, ignoreCache: true});

  expect(cache.variables.length).toBe(2);
  expect(cache.variables[0].position.line).toBe(1);
  expect(cache.variables[1].position.line).toBe(3);
});

test('vitestTest3', async () => {
  const lines = [
    `**FREE`,
    `Dcl-s MyVariable2 CHAR(20);`,
    `Dcl-Ds astructure qualified;`,
    `  Subitem1 CHAR(20);`,
    `  Subitem2 CHAR(20);`,
    `End-ds;`,
    `Dcl-s MyVariable CHAR(20);`,
    `//Yes`
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {withIncludes: true, ignoreCache: true});

  expect(cache.variables.length).toBe(2);
  expect(cache.structs.length).toBe(1);

  expect(cache.structs[0].subItems.length).toBe(2);

  expect(cache.variables[0].position.line).toBe(1);
  expect(cache.variables[1].position.line).toBe(6);
  expect(cache.structs[0].position.line).toBe(2);

  expect(cache.structs[0].range).toEqual({
    start: 2,
    end: 5
  });
});

test('vitestTest4', async () => {
  const lines = [
    `**FREE`,
    `Dcl-s MyVariable2 CHAR(20);`,
    ``,
    ``,
    `Begsr theSubroutine;`,
    `  MyVariable2 = 'Hello world';`,
    `Endsr;`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {withIncludes: true, ignoreCache: true});

  expect(cache.variables.length).toBe(1);
  expect(cache.subroutines.length).toBe(1);

  expect(cache.variables[0].position.line).toBe(1);
  expect(cache.subroutines[0].range.start).toBe(4);
  expect(cache.subroutines[0].range.end).toBe(6);
});

/**
  * Variable and procedure definition test
  */
test('vitestTest5', async () => {
  const lines = [
   `**FREE`,
   ``,
   `Dcl-s MyVariable2 CHAR(20);`,
   ``,
   `Dcl-Proc theProcedure;`,
   `  MyVariable2 = 'Hello world';`,
   `End-Proc;`,
   ``,
   `Dcl-Proc setValue;`,
   `  Dcl-Pi *N;`,
   `    newValue CHAR(20);`,
   `  End-Pi;`,
   `  MyVariable2 = newValue;`,
   `End-Proc;`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {withIncludes: true, ignoreCache: true});

  expect(cache.variables.length).toBe(1);
  expect(cache.procedures.length).toBe(2);

  expect(cache.variables[0].position.line).toBe(2);
  expect(cache.procedures[0].position.line).toBe(4);
  expect(cache.procedures[1].position.line).toBe(8);

  expect(cache.procedures[0].subItems.length).toBe(0);
  expect(cache.procedures[1].subItems.length).toBe(1);
});

test('vitestTest6', async () => {
  const lines = [
   `**FREE`,
   `Dcl-s MyVariable2 CHAR(20);`,
   ``,
   `Dcl-Pr TheProcedure;`,
   `  parmA CHAR(20);`,
   `End-Pr;`,
   ``,
   `MyVariable2 = 'Hello world';`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {withIncludes: true, ignoreCache: true});

  expect(cache.variables.length).toBe(1);
  expect(cache.procedures.length).toBe(1);
});

/**
  * Test procedure length.
  * When Procedure is defined, the prototype is overridden.
  */
test('vitestTest7', async () => {
  const lines = [
   `**FREE`,
   ``,
   `Dcl-Pr TheProcedure;`,
   `  parmA CHAR(20);`,
   `End-Pr;`,
   ``,
   `Dcl-S theVar CHAR(20);`,
   ``,
   `Dcl-Proc theProcedure;`,
   `  Dcl-Pi *N;`,
   `    newValue Char(20);`,
   `  End-Pi;`,
   `  theVar = newValue;`,
   `End-Proc;`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {withIncludes: true, ignoreCache: true});

  expect(cache.variables.length).toBe(1);
  expect(cache.procedures.length).toBe(1);
  expect(cache.procedures[0].subItems.length).toBe(1);
});

/**
  * Test procedure length.
  * When Procedure is defined, the prototype is overridden.
  */
test('vitestTest7_fixed', async () => {
  const lines = [
   `     DGetArtDesc       PR            50A    extproc`,
   `     D ARID                           6A    value`,
   ``,
   `     PGetArtDesc       B                     export`,
   `     DGetArtDesc       PI                   like(ardesc)`,
   `     D P_ARID                         6A    value`,
   `      /free`,
   `         chainARTICLE1(P_ARID`,
   `               );`,
   `         return ARDESC;`,
   `      /end-free`,
   `     pGetArtDesc       e`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {withIncludes: true, ignoreCache: true});

  expect(cache.procedures.length).toBe(1);
  expect(cache.procedures[0].subItems.length).toBe(1);
});

/**
  * Constant definition test
  * */
test('vitestTest8', async () => {
  const lines = [
   `**FREE`,
   `Dcl-s MyVariable2 Char(20);`,
   ``,
   `Dcl-C theConstant 'Hello world';`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {withIncludes: true, ignoreCache: true});

  expect(cache.variables.length).toBe(1);
  expect(cache.constants.length).toBe(1);
});

/**
  * Check that local variables are not in global scope
  */
test('vitestTest9', async () => {
  const lines = [
   `**FREE`,
   `Dcl-s MyVariable2 Char(20);`,
   ``,
   `Dcl-C theConstant 'Hello world';`,
   ``,
   `Dcl-Proc theProcedure;`,
   `  Dcl-Pi *N;`,
   `    newValue Char(20);`,
   `  End-Pi;`,
   `  Dcl-S localVar Char(20);`,
   `  localVar = newValue;`,
   `  MyVariable2 = localVar;`,
   `End-Proc;`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {withIncludes: true, ignoreCache: true});

  expect(cache.variables.length).toBe(1);
  expect(cache.constants.length).toBe(1);
  expect(cache.procedures.length).toBe(1);
  expect(cache.procedures[0].subItems.length).toBe(1);
});

/**
  * Test copybook
  * */
test('vitestTest10', async () => {
  const lines = [
   `**FREE`,
   ``,
   `Ctl-Opt DftActGrp(*No);`,
   ``,
   `/copy './tests/rpgle/copy1.rpgle'`,
   ``,
   `Dcl-s MyVariable2 Char(20);`,
   ``,
   `Dcl-C theConstant 'Hello world';`,
   ``,
   `CallP theExtProcedure(myVariable);`,
   ``,
   `Return;`
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {withIncludes: true, ignoreCache: true});

  expect(Object.keys(cache.keyword).length).toBe(1);
  expect(cache.keyword[`DFTACTGRP`]).toBe(`*NO`);
  expect(cache.includes.length).toBe(1);
  expect(cache.variables.length).toBe(1);
  expect(cache.constants.length).toBe(1);
  expect(cache.procedures.length).toBe(1);
  expect(cache.procedures[0].subItems.length).toBe(1);

  const baseNameInclude = path.basename(cache.procedures[0].position.path);
  expect(baseNameInclude).toBe(`copy1.rpgle`);
  expect(cache.procedures[0].position.line).toBe(2);
});

test('test10_local_fixedcopy', async () => {
  const lines = [
    `**FREE`,
    ``,
    `Ctl-Opt DftActGrp(*No);`,
    ``,
    `/copy tests,eof4`,
    ``,
    `Dcl-s MyVariable2 Char(20);`,
    ``,
    `Dcl-C theConstant 'Hello world';`,
    ``,
    `dsply theConstant;`,
    ``,
    `Return;`
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {withIncludes: true, ignoreCache: true});

  expect(cache.includes.length).toBe(1);
  expect(cache.variables.length).toBe(1);
  expect(cache.constants.length).toBe(1);
  expect(cache.procedures.length).toBe(1);

  const uppercase = cache.find(`UPPERCASE`);

  const baseNameInclude = path.basename(uppercase.position.path);
  expect(baseNameInclude).toBe(`eof4.rpgle`);
});

/**
   * Test many copybooks
   * */
test('test11', async () => {
  const lines = [
    `**FREE`,
    ``,
    `Ctl-Opt DftActGrp(*No);`,
    ``,
    `/copy './tests/rpgle/copy1.rpgle'`,
    `/include './tests/rpgle/copy2.rpgle'`,
    ``,
    `Dcl-s MyVariable2 Char(20);`,
    ``,
    `Dcl-C theConstant 'Hello world';`,
    ``,
    `CallP theExtProcedure(myVariable);`,
    ``,
    `TheStruct.SubItem = theConstant;`,
    ``,
    `Return;`
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {withIncludes: true, ignoreCache: true});

  expect(cache.includes.length).toBe(2);
  expect(cache.variables.length).toBe(1);
  expect(cache.constants.length).toBe(2);
  expect(cache.structs.length).toBe(1);
  expect(cache.structs[0].subItems.length).toBe(1);
  expect(cache.procedures.length).toBe(1);
  expect(cache.procedures[0].subItems.length).toBe(1);
});

test('test12', async () => {
  const lines = [
    `**FREE`,
    ``,
    `Ctl-Opt DftActGrp(*No);`,
    ``,
    `/copy './tests/rpgle/copy1.rpgle'`,
    ``,
    `Dcl-S globalVar Char(20);`,
    ``,
    `Dcl-C theConstant 'Hello world';`,
    ``,
    `globalVar = theConstant;`,
    ``,
    `theLocalProc(globalVar);`,
    ``,
    `Return;`,
    ``,
    `Dcl-Proc theLocalProc;`,
    `  Dcl-Pi *N;`,
    `    newValue Char(20);`,
    `  End-Pi;`,
    `  Dcl-S localVar Char(20);`,
    `  localVar = %trimr(newValue) + '!';`,
    `  globalVar = localVar;`,
    `End-Proc;`,
    ``
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {withIncludes: true, ignoreCache: true});

  expect(cache.includes.length).toBe(1);

  expect(cache.variables.length).toBe(1);
  expect(cache.constants.length).toBe(1);

  // One prototype and one declared
  expect(cache.procedures.length).toBe(2);

  // Valid names
  expect(cache.procedures[0].name).toBe(`theExtProcedure`);
  expect(cache.procedures[1].name).toBe(`theLocalProc`);

  const theLocalProc = cache.find(`theLocalProc`);

  expect(theLocalProc.range).toEqual({
    start: 16,
    end: 23
  });

  // Has a parameter
  expect(theLocalProc.subItems.length).toBe(1);

  // Has a local scope
  expect(theLocalProc.scope !== undefined).toBe(true);

  // Should have a local variable
  expect(theLocalProc.scope.variables.length).toBe(1);
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

  const cache = await parser.getDocs(uri, lines, {withIncludes: true, ignoreCache: true});

  Linter.getErrors({ uri, content: lines }, {
    CollectReferences: true,
  }, cache);

  const in10 = cache.find(`IN10`);

  expect(in10.references.length).toBe(2);
});


test('subds1', async () => {
  const lines = [
    `**FREE`,
    ``,
    `Dcl-Ds DsChangingNodeRole Qualified;`,
    `  *n Int(10) Inz(0);`,
    `  *n VarChar(20) Inz('Primary Node');`,
    `  *n Int(10) Inz(1);`,
    `  *n VarChar(20) Inz('Backup node');`,
    `  *n Int(10) Inz(-1);`,
    `  *n VarChar(20) Inz('Replicate Node');`,
    `  *n Int(10) Inz(-2);`,
    `  *n VarChar(20) Inz('Changing Node');`,
    `  *n Int(10) Inz(-3);`,
    `  *n VarChar(20) Inz('*List');`,
    `  *n Int(10) Inz(-4);`,
    `  *n VarChar(20) Inz('Peer Node');`,
    `  Dcl-Ds Role Dim(1) Pos(1);`,
    `    Values Int(10);`,
    `    Descriotion VarChar(20);`,
    `  End-Ds Role;`,
    `End-Ds DsChangingNodeRole;`,
    ``,
    `Return;`,
    ``,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {withIncludes: true, ignoreCache: true});

  expect(cache.structs.length).toBe(1);

  const DsChangingNodeRole = cache.find(`DsChangingNodeRole`);
  expect(DsChangingNodeRole.name).toBe(`DsChangingNodeRole`);
  expect(DsChangingNodeRole.position.line).toBe(2);

  expect(DsChangingNodeRole.subItems.length).toBe(13);
  expect(DsChangingNodeRole.subItems[12].name).toBe(`Role`);

  expect(DsChangingNodeRole.range).toEqual({
    start: 2,
    end: 19
  });
});

test('range1', async () => {
  const lines = [
    `**FREE`,
    `///`,
    `// Get delimiters`,
    `//`,
    `// Returns a pointer to the used delimiters for parsing the JSON and XML data.`,
    `// The pointer points to a data structure in the format <em>jx_DelimiterDS</em>.`,
    `//`,
    `// @return Pointer to the delimiters data structure (jx_DelimiterDS)`,
    `//`,
    `// @warning Do not deallocate the returned pointer!`,
    `///`,
    `Dcl-PR json_getDelims Pointer extproc(*CWIDEN : 'jx_GetDelimiters') End-PR;`,
    ``,
    `///`,
    `// Set delimiters`,
    `//`,
    `// Sets the delimiters used for parsing the JSON and XML data.`,
    `// For the default delimiters see constant json_DELIMITERS.`,
    `//`,
    `// @param Delimiters`,
    `///`,
    `Dcl-PR json_setDelimiters extproc(*CWIDEN : 'jx_SetDelimiters');`,
    `  delimiters pointer value options(*string);`,
    `End-PR;`,
    ``,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {withIncludes: true, ignoreCache: true});

  expect(cache.procedures.length).toBe(2);

  const json_getDelims = cache.find(`json_getDelims`);
  expect(json_getDelims.range).toEqual({
    start: 11,
    end: 11
  });

  const json_getDelimiters = cache.find(`json_setDelimiters`);
  expect(json_getDelimiters.subItems.length).toBe(1);
  expect(json_getDelimiters.range).toEqual({
    start: 21,
    end: 23
  });
});

test('range2', async () => {
  const lines = [
    `**free`,
    `Dcl-S  FullCmd      Char(32);`,
    `Dcl-DS ABCD  LikeDS(BBOOP);`,
    ``,
    ``,
    `Dcl-S Eod          Ind;`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {withIncludes: true, ignoreCache: true});

  expect(cache.variables.length).toBe(2);
  expect(cache.structs.length).toBe(1);

  const ABCD = cache.find(`ABCD`);
  expect(ABCD.keyword[`LIKEDS`]).toBe(`BBOOP`);
  expect(ABCD.range).toEqual({
    start: 2,
    end: 2
  });
});

test('inline_end_pi', async () => {
  const lines = [
    `       dcl-proc getHandle;`,
    ``,
    `       dcl-pi *n like(handle_t) end-pi;`,
    ``,
    `       dcl-s rcdHandle like(handle_t) inz;`,
    ``,
    `       rcdHandle = %lookup(' ' :handles);`,
    `       if (rcdHandle > 0);`,
    `         handles(rcdHandle) = 'A';`,
    `       endif;`,
    ``,
    `       return rcdHandle;`,
    ``,
    `       end-proc getHandle;`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {withIncludes: true, ignoreCache: true});

  const getHandle = cache.find(`getHandle`);

  expect(getHandle.keyword[`LIKE`]).toBe(`HANDLE_T`);
  expect(getHandle.keyword[`END-PI`]).toBeUndefined();
});

test('issue_168', async () => {
  const lines = [
    `**free`,
    `Ctl-opt datfmt(*iso) timfmt(*iso) alwnull(*usrctl) debug;`,
    ``,
    `Dcl-F TESTFILE3 Keyed Usage(*Update :*Delete);`,
    ``,
    `Dcl-Pr TESTCHAIN1 ExtPgm('TESTCHAIN1');`,
    `  Parm1 Char(1);`,
    `End-Pr TESTCHAIN1;`,
    ``,
    `Dcl-Pi TESTCHAIN1;`,
    `  Parm1 Char(1);`,
    `End-Pi TESTCHAIN1;`,
    ``,
    `Dcl-DS AAA;`,
    `  a Char(10);`,
    `End-D AAA;`,
    ``,
    `If (Parm1 = 'N');`,
    `  Chain ('CHIAVE' :1) TESTFILE3;`,
    `Else;`,
    `  Chain ('CHIAVE' :1) TESTFILE3;`,
    `EndIf; `,
    ``,
    `job_name = 'TESTFILE1';`,
    ``,
    `Update TESTREC;`,
    ``,
    `Return; `,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {withIncludes: true, ignoreCache: true});
});

test('issues_168a', async () => {
  const lines = [
    `**free`,
    `Ctl-opt datfmt(*iso) timfmt(*iso) alwnull(*usrctl) debug;`,
    ``,
    `Dcl-F TESTFILE3 Keyed Usage(*Update :*Delete);`,
    ``,
    `Dcl-Pr TESTCHAIN1 ExtPgm('TESTCHAIN1');`,
    `Parm1 Char(1);`,
    `End-Pr TESTCHAIN1;`,
    ``,
    `Dcl-Pi TESTCHAIN1;`,
    `Parm1 Char(1);`,
    `End-Pi TESTCHAIN1;`,
    ``,
    `Dcl-DS AAA;`,
    `a Char(10);`,
    `Dcl-ds a;`,
    `End-ds a;`,
    `End-Ds AAA;`,
    ``,
    `If (Parm1 = 'N');`,
    `Chain ('CHIAVE' :1) TESTFILE3;`,
    `Else;`,
    `Chain ('CHIAVE' :1) TESTFILE3;`,
    `EndIf;`,
    ``,
    `job_name = 'TESTFILE1';`,
    ``,
    `Update TESTREC;`,
    ``,
    `Return;`,
    ``,
    `// ____________________________________________________________________________`,
    `Dcl-Proc aaa;`,
    ``,
    `Dcl-Pi aaa;`,
    `end-proc;`,
    `End-Pi aaa;`,
    `// ____________________________________________________________________________`,
    ``,
    `End-Proc aaa;`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {withIncludes: true, ignoreCache: true});
});

test('issues_170b', async () => {
  const lines = [
    `**free`,
    ``,
    `Dcl-DS WkStnInd;`,
    `  ProcessSCF     Ind        Pos(21);`,
    `  ReprintScf     Ind        Pos(22);`,
    `  Error         `,
    `   Ind      `,
    `     Pos(25);`,
    `  PageDown       Ind      `,
    `    Pos(30);`,
    `  PageUp         Ind        Pos(31);`,
    `  SflEnd         Ind        Pos(40);`,
    `  SflBegin       Ind        Pos(41);`,
    `  NoRecord       Ind        Pos(60);`,
    `  SflDspCtl      Ind        Pos(85);`,
    `  SflClr         Ind        Pos(75);`,
    `  SflDsp         Ind        Pos(95);`,
    `End-DS;`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {withIncludes: true, ignoreCache: true});

  const WkStnInd = cache.find(`WkStnInd`);
  expect(WkStnInd.name).toBe(`WkStnInd`);
  expect(WkStnInd.subItems.length).toBe(11);

  const error = cache.find(`Error`);
  expect(error.name).toBe(`Error`);
  expect(error.keyword[`IND`]).toBe(true);
  expect(error.keyword[`POS`]).toBe(`25`);
});

test('issues_dcl_subf', async () => {
  const lines = [
    `**free`,
    ``,
    `dcl-ds inputsYo;`,
    `  dcl-subf boop_Addr1 char(30);`,
    `  dcl-subf boop_Addr2 char(30);`,
    `  dcl-subf boop_City char(18);`,
    `  dcl-subf boop_State char(2);`,
    `  dcl-subf boop_Zip char(15);`,
    `  dcl-subf boop_Cntry char(4);`,
    `  dcl-subf boop_Foreign char(1);`,
    `end-ds;`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {withIncludes: true, ignoreCache: true});

  const inputsYo = cache.find(`inputsYo`);
  expect(inputsYo.subItems.length).toBe(7);

  const boop_Addr1 = inputsYo.subItems[0];
  expect(boop_Addr1.name).toBe(`boop_Addr1`);
});

test('issue_195a', async () => {
  const lines = [
    `**free`,
    `//***********************************************************************`,
    `//*** Scompone la stringa per sql ***`,
    `//***********************************************************************`,
    `Dcl-Proc ScomponiStringa;`,
    ``,
    `Dcl-Pi ScomponiStringa varchar(2000);`,
    `pstringa varchar(999);`,
    `pLunghezza zoned(3);`,
    `End-Pr;`,
    `dcl-s lStringa varchar(2000);`,
    `dcl-s linizio zoned(4);`,
    `lInizio = 1;`,
    `Dow pStringa(lInizio:pLunghezza)<>*blank;`,
    `lStringa +=sQuote+ pstringa(linizio:pLunghezza)+sQuote;`,
    `linizio=linizio+pLunghezza`,
    `If pstringa(linizio:1) <> *blank;`,
    `lStringa +=pstringa(linizio:1);`,
    `EndIf;`,
    `linizio+=1;`,
    `EndDo;`,
    `Return lStringa;`,
    `End-Proc ScomponiStringa;`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {withIncludes: true, ignoreCache: true});

  cache.clearReferences();
});

test('issue_195b', async () => {
  const lines = [
    `**FREE`,
    ``,
    `Dcl-Pi AUTH;`,
    `  pUserID   Char(10);`,
    `  pPassword Char(32);`,
    `  Result    Char(1);`,
    `End-Pi;`,
    ``,
    `DoProfileStuff();`,
    ``,
    `*InLR = *On;`,
    `Return;`,
    ``,
    `Dcl-Proc DoProfileStuff;`,
    `  Dcl-PR GetProfile  ExtPgm('QSYGETPH');`,
    `    UserID         Char(10)   const;`,
    `    Password       Char(32767) const options(*varsize);`,
    `    Handle         Char(12);`,
    `    ErrorCode      Char(256)  Options(*Varsize : *NoPass);`,
    `    PswLength      Int(10)    const Options(*NoPass);`,
    `    CCSIDCode      Int(10)    const Options(*NoPass);`,
    `  End-PR;`,
    `  `,
    `  Dcl-Pr CloseProfile ExtPgm('QSYRLSPH');`,
    `    Handle         Char(12);`,
    `  End-Pr;`,
    `  `,
    `  Dcl-S ResultHandle Char(12);`,
    `  `,
    `  Dcl-S errorOut Char(256);`,
    `  Dcl-S pwLength Int(3);`,
    `  `,
    `  pwLength = %Len(%Trim(pPassword));`,
    `  `,
    `  //pPassword = %Trim(pPassword);`,
    `  ResultHandle = '';`,
    `  Result = *Off;`,
    `  `,
    `  GetProfile(pUserID:pPassword:ResultHandle:errorOut:pwLength:37);`,
    `  `,
    `  //Indicates is incorrect`,
    `  If ResultHandle <> x'000000000000000000000000';`,
    `    Result = *On;`,
    `    //We don't want to keep handles open`,
    `    `,
    `    CloseProfile(ResultHandle);`,
    `  Endif;`,
    `End-Proc;`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {withIncludes: true, ignoreCache: true});

  expect(cache.procedures.length).toBe(1);

  const DoProfileStuff = cache.find(`DoProfileStuff`);
  expect(DoProfileStuff.scope.procedures.length).toBe(2);
});

test('exec_1', async () => {
  const lines = [
    `**FREE`,
    ``,
    `Ctl-Opt DFTACTGRP(*No);`,
    ``,
    `Dsply 'aaa';`,
    `DSPLY '';`,
    `Dsply 'aaa';`,
    ``,
    `EXEC SQL`,
    `   Select nullif('aaa', '') from sysibm/sysdummy1;`,
    `Return;`
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {withIncludes: true, ignoreCache: true});

  expect(cache.sqlReferences.length).toBe(1);
  expect(cache.sqlReferences[0].name).toBe(`sysdummy1`);
  expect(cache.sqlReferences[0].description).toBe(`sysibm`);
});

test('exec_2', async () => {
  const lines = [
    `**free`,
    `Dcl-s DeptNum Char(3);`,
    ``,
    `DeptNum = 'ABC';`,
    ``,
    `ClearSubfile();`,
    ``,
    `EXEC SQL DECLARE empCurA CURSOR FOR`,
    `    SELECT EMPNO, FIRSTNME, LASTNAME, JOB`,
    `    FROM Employee`,
    `    WHERE WORKDEPT = Deptnum;`,
    ``,
    `EXEC SQL DECLARE empCurB CURSOR FOR`,
    `    SELECT EMPNO, FIRSTNME, LASTNAME, JOB`,
    ``,
    `    FROM sample.Employee`,
    `    WHERE WORKDEPT = :deptNum;`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {withIncludes: true, ignoreCache: true});

  expect(cache.sqlReferences.length).toBe(2);
  expect(cache.sqlReferences[0].name).toBe(`Employee`);
  expect(cache.sqlReferences[0].description).toBe(``);

  expect(cache.sqlReferences[1].name).toBe(`Employee`);
  expect(cache.sqlReferences[1].description).toBe(`sample`);
});

test('exec_3', async () => {
  const lines = [
    `**FREE`,
    ``,
    `Dcl-s MyVariable2 Char(20);  `,
    ``,
    `EXEC SQL`,
    `    FETCH NEXT FROM empCur       `,
    `    INTO :myvariable2;`,
    `EXEC SQL`,
    `    EXECUTE IMMEDIATE :myvariable2;`,
    ``,
    `return;`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {withIncludes: true, ignoreCache: true});

  expect(cache.sqlReferences.length).toBe(0);
});

test('exec_4', async () => {
  const lines = [
    `        dcl-s NULL pointer inz(*NULL);`,
    `        dcl-s amount1 packed(7:2);`,
    `        dcl-s amount2 packed(7:2);`,
    `        dcl-s amount3 packed(7:2);`,
    `        dcl-s amount4 packed(7:2);`,
    `        dcl-s amount5 packed(7:2);`,
    `        `,
    `        // Watch null move left`,
    `        Exec Sql`,
    `          select`,
    `            max(case when bonus < 900 then bonus else null end),`,
    `        `,
    `            max(case when bonus < 800 then bonus else null end),`,
    `        `,
    `            max(case when bonus < 700 then bonus else null end),`,
    `        `,
    `            max(case when bonus < 600 then bonus else null end),`,
    `        `,
    `            max(case when bonus < 500 then bonus else null end)`,
    `          into`,
    `            :amount1,:amount2,:amount3,:amount4,:amount5`,
    `          from`,
    `            sample.employee;`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {withIncludes: true, ignoreCache: true});

  expect(cache.sqlReferences.length).toBe(1);
  expect(cache.sqlReferences[0].name).toBe(`employee`);
  expect(cache.sqlReferences[0].description).toBe(`sample`);
});

test('exec_5', async () => {
  const lines = [
    `**FREE`,
    ``,
    `Dcl-s MyVariable2 Char(20);  `,
    ``,
    `EXEC SQL`,
    `  INSERT INTO sample.mytable(column) values(:MyVariable2);`,
    ``,
    `EXEC SQL`,
    `  INSERT INTO othertable(column) values(:MyVariable2);`,
    ``,
    `return;`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {withIncludes: true, ignoreCache: true});

  expect(cache.sqlReferences.length).toBe(2);

  expect(cache.sqlReferences[0].name).toBe(`mytable`);
  expect(cache.sqlReferences[0].description).toBe(`sample`);

  expect(cache.sqlReferences[1].name).toBe(`othertable`);
  expect(cache.sqlReferences[1].description).toBe(``);
});

test('exec_6', async () => {
  const lines = [
    `**FREE`,
    ``,
    `Dcl-s MyVariable2 Char(20);  `,
    ``,
    `EXEC SQL`,
    `  INSERT INTO sample.mytable`,
    `  (column) values(:MyVariable2);`,
    ``,
    `EXEC SQL`,
    `  INSERT INTO othertable`,
    `  (column) values(:MyVariable2);`,
    ``,
    `return;`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {withIncludes: true, ignoreCache: true});

  expect(cache.sqlReferences.length).toBe(2);

  expect(cache.sqlReferences[0].name).toBe(`mytable`);
  expect(cache.sqlReferences[0].description).toBe(`sample`);

  expect(cache.sqlReferences[1].name).toBe(`othertable`);
  expect(cache.sqlReferences[1].description).toBe(``);
});

test('exec_7', async () => {
  const lines = [
    `**FREE`,
    ``,
    `Dcl-s MyVariable2 Char(20);  `,
    ``,
    `EXEC SQL`,
    `  UPDATE sample.thetable set a=:MyVariable2`,
    `  where id = 6;`,
    ``,
    `EXEC SQL`,
    `  UPDATE cooltable set a=:MyVariable2`,
    `  where id = 6;`,
    ``,
    `return;`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {withIncludes: true, ignoreCache: true});

  expect(cache.sqlReferences.length).toBe(2);

  expect(cache.sqlReferences[0].name).toBe(`thetable`);
  expect(cache.sqlReferences[0].description).toBe(`sample`);

  expect(cache.sqlReferences[1].name).toBe(`cooltable`);
  expect(cache.sqlReferences[1].description).toBe(``);
});

test('exec_8', async () => {
  const lines = [
    `**FREE`,
    ``,
    `Dcl-s MyVariable2 Char(20);  `,
    ``,
    `EXEC SQL`,
    `  DELETE from sample.thetable`,
    `  where id = 6;`,
    ``,
    `EXEC SQL`,
    `  DELETE from wooptable`,
    `  where id = 6;`,
    ``,
    `return;`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {withIncludes: true, ignoreCache: true});

  expect(cache.sqlReferences.length).toBe(2);

  expect(cache.sqlReferences[0].name).toBe(`thetable`);
  expect(cache.sqlReferences[0].description).toBe(`sample`);

  expect(cache.sqlReferences[1].name).toBe(`wooptable`);
  expect(cache.sqlReferences[1].description).toBe(``);
});

test('exec_9', async () => {
  const lines = [
    `**FREE`,
    ``,
    `Dcl-s MyVariable2 Char(20);  `,
    ``,
    `EXEC SQL`,
    `  CALL sample.MyRandomProc(:MyVariable2);`,
    ``,
    `EXEC SQL`,
    `  CALL OtherCoolProc(:MyVariable2);`,
    ``,
    `return;`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {withIncludes: true, ignoreCache: true});

  expect(cache.sqlReferences.length).toBe(2);

  expect(cache.sqlReferences[0].name).toBe(`MyRandomProc`);
  expect(cache.sqlReferences[0].description).toBe(`sample`);

  expect(cache.sqlReferences[1].name).toBe(`OtherCoolProc`);
  expect(cache.sqlReferences[1].description).toBe(``);
});

test('exec_10', async () => {
  const lines = [
    `        EXEC SQL`,
    `          DECLARE C1 CURSOR FOR`,
    `            SELECT MSGBRD, MSGSBD, MSGUID, MSGSBJ, MSGDAT, MSGTIM, MSGSND`,
    `            FROM PSBORDS AS S JOIN PMESSGS AS M`,
    `              ON S.SBRBRD = M.MSGBRD`,
    `               AND S.SBRSHT = M.MSGSBD`,
    `            WHERE MSGDAT = :cAuthUser`,
    `            ORDER BY MSGDAT DESC, MSGTIM DESC;`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {withIncludes: true, ignoreCache: true});

  expect(cache.sqlReferences.length).toBe(2);

  expect(cache.sqlReferences[0].name).toBe(`PSBORDS`);
  expect(cache.sqlReferences[0].description).toBe(``);

  expect(cache.sqlReferences[1].name).toBe(`PMESSGS`);
  expect(cache.sqlReferences[1].description).toBe(``);
});

test('exec_11', async () => {
  const lines = [
    `**FREE`,
    ``,
    `// sql statement causing the the bug, when you find the reference you are also bringing in the ) at the end.`,
    `        Exec Sql Update PrdBlock`,
    `            Set cgday     = :leastUtilDay,`,
    `                cgcnstdpt = :pd$dept,`,
    `                cgcnstjob = :pd$jobc`,
    `            Where cgblock in (Select b2addid`,
    `                                from prdblk2add) and`,
    `                    cgday = 0;`,
    ``,
    `return;`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {withIncludes: true, ignoreCache: true});

  expect(cache.sqlReferences.length).toBe(2);
  expect(cache.sqlReferences[0].name).toBe(`PrdBlock`);
  expect(cache.sqlReferences[1].name).toBe(`prdblk2add`);
});

test('exec_12_a', async () => {
  const lines = [
    `**free`,
    `exec sql declare c2 cursor for`,
    `SELECT ARID, ARDESC, arstock, ARMINQTY,`,
    `       ARCUSQTY, ARPURQTY, APPRICE, apref`,
    `FROM article, artiprov`,
    `WHERE arstock < ARMINQTY - arcusqty + arpurqty`,
    `  and arid = aparid`,
    `  AND apprid = :id`,
    `order by arid ;`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {withIncludes: true, ignoreCache: true});

  expect(cache.sqlReferences[0].name).toBe(`article`);
  expect(cache.sqlReferences[1].name).toBe(`artiprov`);
  expect(cache.sqlReferences.length).toBe(2);
});

test('exec_12_b', async () => {
  const lines = [
    `**free`,
    `exec sql declare c2 cursor for`,
    `SELECT ARID, ARDESC, arstock, ARMINQTY,`,
    `       ARCUSQTY, ARPURQTY, APPRICE, apref`,
    `FROM article , artiprov`,
    `WHERE arstock < ARMINQTY - arcusqty + arpurqty`,
    `  and arid = aparid`,
    `  AND apprid = :id`,
    `order by arid ;`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {withIncludes: true, ignoreCache: true});

  expect(cache.sqlReferences[0].name).toBe(`article`);
  expect(cache.sqlReferences[1].name).toBe(`artiprov`);
  expect(cache.sqlReferences.length).toBe(2);
});

test('exec_13', async () => {
  const lines = [
    `**FREE`,
    `EXEC SQL`,
    ` SELECT TRID `,
    ` into :transaction.TRID `,
    ` FROM FINAL TABLE (`,
    `   INSERT INTO TRANSACTION`,
    `   (TCUS, TDESC, TAMT)`,
    `   VALUES`,
    `   (`,
    `     :cusno, :transaction.TDESC, :amount`,
    `   )`,
    ` );`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {withIncludes: true, ignoreCache: true});

  expect(cache.sqlReferences.length).toBe(1);
  expect(cache.sqlReferences[0].name).toBe(`TRANSACTION`);
});

test(`exec_14`, async () => {
  const lines = [
    `**FREE`,
    ``,
    `exec sql declare c1 cursor for `,
    `        with temp as (`,
    `          select`,
    `           field1,`,
    `           field2`,
    `          from table1`,
    `        )`,
    `Select * from temp;`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {withIncludes: true, ignoreCache: true});

  console.log(cache.sqlReferences);
  expect(cache.sqlReferences.length).toBe(1);
  expect(cache.sqlReferences[0].name).toBe(`table1`);
});

test('enum_1', async () => {
  const lines = [
    `**free`,
    ``,
    `DCL-ENUM sizes; //  1 `,
    `  tiny -1;`,
    `  small 0;`,
    `  medium 1;`,
    `  large 2;`,
    `END-ENUM;`,
    ``,
    `DCL-ENUM jobMsgQ QUALIFIED; //  2 `,
    `  noWrap '*NOWRAP';`,
    `  wrap '*WRAP';`,
    `  prtWrap '*PRTWRAP';`,
    `END-ENUM;`,
    ``,
    `DCL-S cmd VARCHAR(100);`,
    `DCL-S array INT(10) DIM(5);`,
    `DCL-DS item QUALIFIED;`,
    `  size packed(5);`,
    `END-DS;`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {withIncludes: true, ignoreCache: true});

  expect(cache.constants.length).toBe(2);

  const sizes = cache.find(`sizes`);
  expect(sizes.name).toBe(`sizes`);
  expect(sizes.subItems.length).toBe(4);

  const jobMsgQ = cache.find(`jobMsgQ`);
  expect(jobMsgQ.name).toBe(`jobMsgQ`);
  expect(jobMsgQ.subItems.length).toBe(3);
});

test('keywords over multiple lines', async () => {
  const lines = [
    `       Dcl-PR invoice_get_invoice;`,
    `         store Zoned(3:0) const;`,
    `         invoice Zoned(7:0) const;`,
    `         details LikeDS(invoice_get_invoice_sales_detail_ds)`,
    `           dim(invoice_max_details);`,
    `         count_details Zoned(3:0);`,
    `         error Like(TError);`,
    `       End-Pr;`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {withIncludes: true, ignoreCache: true});

  expect(cache.procedures.length).toBe(1);
  const invoice_get_invoice = cache.find(`invoice_get_invoice`);
  expect(invoice_get_invoice.subItems.length).toBe(5);

  const storeParm = invoice_get_invoice.subItems[0];
  expect(storeParm.name).toBe(`store`);
  expect(storeParm.keyword[`ZONED`]).toBe(`3:0`);
  expect(storeParm.keyword[`CONST`]).toBe(true);

  const invoiceParm = invoice_get_invoice.subItems[1];
  expect(invoiceParm.name).toBe(`invoice`);
  expect(invoiceParm.keyword[`ZONED`]).toBe(`7:0`);
  expect(invoiceParm.keyword[`CONST`]).toBe(true);

  const detailParm = invoice_get_invoice.subItems[2];
  console.log(detailParm);
  expect(detailParm.name).toBe(`details`);
  expect(detailParm.keyword[`LIKEDS`]).toBe(`INVOICE_GET_INVOICE_SALES_DETAIL_DS`);
  expect(detailParm.keyword[`DIM`]).toBe(`INVOICE_MAX_DETAILS`);

  const count_details = invoice_get_invoice.subItems[3];
  expect(count_details.name).toBe(`count_details`);
  expect(count_details.keyword[`ZONED`]).toBe(`3:0`);

  const error = invoice_get_invoice.subItems[4];
  expect(error.name).toBe(`error`);
  expect(error.keyword[`LIKE`]).toBe(`TERROR`);
})