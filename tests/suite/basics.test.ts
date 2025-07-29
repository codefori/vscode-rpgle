
import path from "path";
import setupParser, { getFileContent } from "../parserSetup";
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
  expect(cache.variables[0].position.range.line).toBe(1);
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
  expect(cache.variables[0].position.range.line).toBe(1);
  expect(cache.variables[1].position.range.line).toBe(3); 
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

  expect(cache.variables[0].position.range.line).toBe(1);
  expect(cache.variables[1].position.range.line).toBe(6);
  expect(cache.structs[0].position.range.line).toBe(2);

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

  expect(cache.variables[0].position.range.line).toBe(1);
  expect(cache.subroutines[0].range.start).toBe(4);
  expect(cache.subroutines[0].range.end).toBe(6);

  const typeData = cache.resolveType(cache.variables[0]);
  expect(typeData.type).toBeDefined();
  expect(typeData.type.name).toBe(`char`);
  expect(typeData.type.value).toBe(`20`);
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

  expect(cache.variables[0].position.range.line).toBe(2);
  expect(cache.procedures[0].position.range.line).toBe(4);
  expect(cache.procedures[1].position.range.line).toBe(8);

  expect(cache.procedures[0].subItems.length).toBe(0);
  expect(cache.procedures[1].subItems.length).toBe(1);

  const setValue = cache.procedures[1];
  expect(setValue.name).toBe(`setValue`);
  expect(setValue.subItems[0].name).toBe(`newValue`);
  expect(setValue.subItems[0].range.start).toBe(10);
  expect(setValue.subItems[0].range.end).toBe(10);

  let typeData;
  for (const proc of cache.procedures) {
    typeData = cache.resolveType(cache.procedures[0]);
    expect(typeData).toBeDefined();
    expect(typeData.type).toBeUndefined();
    expect(typeData.reference).toBeUndefined();
  }
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

  const resolved = cache.find(`TheProcedure`);
  expect(resolved).toBeDefined();
  expect(resolved.name).toBe(`TheProcedure`);
  expect(resolved.prototype).toBeTruthy();
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
  expect(cache.procedures.length).toBe(2);

  expect(cache.procedures[0].name).toBe(`TheProcedure`);
  expect(cache.procedures[0].prototype).toBeTruthy();
  expect(cache.procedures[0].subItems.length).toBe(1);

  expect(cache.procedures[1].name).toBe(`theProcedure`);
  expect(cache.procedures[1].prototype).toBeFalsy();
  expect(cache.procedures[1].subItems.length).toBe(1);

  const resolved = cache.find(`TheProcedure`);
  expect(resolved).toBeDefined();
  expect(resolved.name).toBe(`theProcedure`);
  expect(resolved.prototype).toBeFalsy();
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

  expect(cache.procedures.length).toBe(2);
  
  expect(cache.procedures[0].name).toBe(`GetArtDesc`);
  expect(cache.procedures[0].subItems.length).toBe(1);
  expect(cache.procedures[0].prototype).toBeTruthy();

  expect(cache.procedures[1].name).toBe(`GetArtDesc`);
  expect(cache.procedures[1].subItems.length).toBe(1);
  expect(cache.procedures[1].prototype).toBeFalsy();

  const resolved = cache.find(`GetArtDesc`);
  expect(resolved).toBeDefined();
  expect(resolved.name).toBe(`GetArtDesc`);
  expect(resolved.prototype).toBeFalsy();
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

  const typeData = cache.resolveType(cache.variables[0]);
  expect(typeData.type).toBeDefined();
  expect(typeData.type.name).toBe(`char`);
  expect(typeData.type.value).toBe(`20`);
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
   `/copy './rpgle/copy1.rpgle'`,
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
  expect(cache.procedures[0].position.range.line).toBe(2);
});

test('test10_local_fixedcopy', async () => {
  const lines = [
    `**FREE`,
    ``,
    `Ctl-Opt DftActGrp(*No);`,
    ``,
    `/copy eof4`,
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
    `/copy './rpgle/copy1.rpgle'`,
    `/include './rpgle/copy2.rpgle'`,
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
    `/copy './rpgle/copy1.rpgle'`,
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

  const typeData = cache.resolveType(theLocalProc);
  expect(typeData).toBeDefined();
  expect(typeData.type).toBeUndefined();
  expect(typeData.reference).toBeUndefined();
});

test('test13', async () => {
  const lines = [
    `**FREE`,
    ``,
    `Ctl-Opt DftActGrp(*No);`,
    ``,
    `/copy './rpgle/copy1.rpgle' // Test copy`,
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
  expect(DsChangingNodeRole.position.range.line).toBe(2);

  expect(DsChangingNodeRole.subItems.length).toBe(13);
  expect(DsChangingNodeRole.subItems[12].name).toBe(`Role`);
  expect(DsChangingNodeRole.subItems[12].subItems.length).toBe(2);

  expect(DsChangingNodeRole.range).toEqual({
    start: 2,
    end: 19
  });
});

test('subds2 likeds', async () => {
  const lines = [
    `**free`,
    ``,
    `dcl-ds t_fileinfo template qualified inz;`,
    `  fielda char(10);`,
    `  fieldb int(10) pos(0);`,
    `  fieldc zoned(10) pos(0);`,
    `end-ds;`,
    ``,
    `dcl-ds t_mysimpleDs template qualified inz;`,
    `  fieldd char(20);`,
    `  fielde char(20);`,
    `  dcl-ds fieldDs likeds(t_fileinfo);`,
    `  fieldf char(20);`,
    `  fieldg char(20);`,
    `end-ds;`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {withIncludes: true, ignoreCache: true});
  expect(cache.structs.length).toBe(2);

  const t_fileinfo = cache.find(`t_fileinfo`);
  expect(t_fileinfo.name).toBe(`t_fileinfo`);
  expect(t_fileinfo.subItems.length).toBe(3);

  const t_mysimpleDs = cache.find(`t_mysimpleDs`);
  expect(t_mysimpleDs.name).toBe(`t_mysimpleDs`);
  expect(t_mysimpleDs.subItems.length).toBe(5);

  const fieldDs = t_mysimpleDs.subItems.find(item => item.name === `fieldDs`);
  expect(fieldDs).toBeDefined();
  expect(fieldDs.keyword[`LIKEDS`]).toBe(`t_fileinfo`);
  expect(fieldDs.subItems.length).toBe(3);
})

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

  const typeDataA = cache.resolveType(json_getDelimiters);
  expect(typeDataA).toBeDefined();
  expect(typeDataA.type).toBeUndefined();
  expect(typeDataA.reference).toBeUndefined();

  const typeDataB = cache.resolveType(json_getDelims);
  expect(typeDataB).toBeDefined();
  expect(typeDataB.type).toMatchObject({name: `pointer`, value: true});
  expect(typeDataB.reference).toBeUndefined();
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

  const typeDataB = cache.resolveType(ABCD);
  expect(typeDataB).toBeDefined();
  expect(typeDataB.type).toBeUndefined();
  expect(typeDataB.reference).toBeUndefined(); //Because BBOOP is not defined
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

  expect(getHandle.keyword[`LIKE`]).toBe(`handle_t`);
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

  const testFile = cache.find(`TESTFILE3`);

  expect(testFile.range.start).toBe(3);
  expect(testFile.range.end).toBe(3);
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

  expect(WkStnInd.range.start).toBe(2);
  expect(WkStnInd.range.end).toBe(17);

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

  const cache = await parser.getDocs(uri, lines, {withIncludes: true, ignoreCache: true, collectReferences: true});

  const typeDataB = cache.resolveType(cache.find(`ScomponiStringa`));
  expect(typeDataB).toBeDefined();
  expect(typeDataB.type).toMatchObject({name: `varchar`, value: `2000`});
  expect(typeDataB.reference).toBeUndefined();
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
  expect(cache.sqlReferences[0].tags[0]).toEqual({
    tag: `description`,
    content: `sysibm`
  });
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
  expect(cache.sqlReferences[0].tags.length).toBe(0);

  expect(cache.sqlReferences[1].name).toBe(`Employee`);
  expect(cache.sqlReferences[1].tags[0]).toEqual({
    tag: `description`,
    content: `sample`
  });
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
  expect(cache.sqlReferences[0].tags[0]).toEqual({
    tag: `description`,
    content: `sample`
  });
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
  expect(cache.sqlReferences[0].tags[0]).toEqual({
    tag: `description`,
    content: `sample`
  });

  expect(cache.sqlReferences[1].name).toBe(`othertable`);
  expect(cache.sqlReferences[1].tags.length).toBe(0);
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
  expect(cache.sqlReferences[0].tags[0]).toEqual({
    tag: `description`,
    content: `sample`
  });

  expect(cache.sqlReferences[1].name).toBe(`othertable`);
  expect(cache.sqlReferences[1].tags.length).toBe(0);
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
  expect(cache.sqlReferences[0].tags[0]).toEqual({
    tag: `description`,
    content: `sample`
  });

  expect(cache.sqlReferences[1].name).toBe(`cooltable`);
  expect(cache.sqlReferences[1].tags.length).toBe(0);
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
  expect(cache.sqlReferences[0].tags[0]).toEqual({
    tag: `description`,
    content: `sample`
  });

  expect(cache.sqlReferences[1].name).toBe(`wooptable`);
  expect(cache.sqlReferences[1].tags.length).toBe(0);
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
  expect(cache.sqlReferences[0].tags[0]).toEqual({
    tag: `description`,
    content: `sample`
  });

  expect(cache.sqlReferences[1].name).toBe(`OtherCoolProc`);
  expect(cache.sqlReferences[1].tags.length).toBe(0);
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
  expect(cache.sqlReferences[0].tags.length).toBe(0);

  expect(cache.sqlReferences[1].name).toBe(`PMESSGS`);
  expect(cache.sqlReferences[1].tags.length).toBe(0);
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
  expect(detailParm.name).toBe(`details`);
  expect(detailParm.keyword[`LIKEDS`]).toBe(`invoice_get_invoice_sales_detail_ds`);
  expect(detailParm.keyword[`DIM`]).toBe(`invoice_max_details`);

  const count_details = invoice_get_invoice.subItems[3];
  expect(count_details.name).toBe(`count_details`);
  expect(count_details.keyword[`ZONED`]).toBe(`3:0`);

  const error = invoice_get_invoice.subItems[4];
  expect(error.name).toBe(`error`);
  expect(error.keyword[`LIKE`]).toBe(`TError`);
});

test(`const keyword check`, async () => {
  const lines = [
    ``,
    `           dcl-c hello 556;`,
    `     d act             c                   'act'`,
    ``,
  ].join(`\r\n`);

  const cache = await parser.getDocs(uri, lines, {withIncludes: true, ignoreCache: true});

  expect(cache.constants.length).toBe(2);

  const act = cache.find(`act`);
  expect(act.name).toBe(`act`);
  expect(act.keyword[`CONST`]).toBe(`'act'`);

  const hello = cache.find(`hello`);
  expect(hello.name).toBe(`hello`);
  expect(hello.keyword[`CONST`]).toBe(`556`);
});

test('issue_353_comments', async () => {
  const lines = [
    `**free`,
    `dcl-ds HEDINF                     based(p1@);`,
    `  HRLEN                 Int(10:0);                            // Record length`,
    `  HCRRN                 Int(10:0);                            // Cursor's RRN`,
    `  HCPOS                 Int(10:0);                            // Cursor's column`,
    `  HCCSID                Int(10:0);                            // CCSID of source`,
    `  HRECI                 Int(10:0);                            // Nbr of input rcds`,
    `end-ds;`,
    `dcl-s p2@          Pointer;`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, { ignoreCache: true, withIncludes: false });

  const hedinf = cache.find(`HEDINF`);
  expect(hedinf).toBeDefined();
  expect(hedinf.subItems.length).toBe(5);
  const p2at = cache.find(`p2@`);
  expect(p2at).toBeDefined();
});

test('header file parse', async () => {
  const lines = [
    `**free`,
    `      //%METADATA                                                      *`,
    `      // %TEXT API main validation procedure (IWS)                     *`,
    `      //%EMETADATA                                                     *`,
    `///`,
    `// @Program APIVAL01S`,
    `//`,
    `// @Purpose IWS API validation procedures`,
    `//`,
    `// @author JHEI`,
    `// @Date 22 May 2024`,
    `//`,
    `///`,
    `ctl-opt option(*srcstmt:*nodebugio);`,
    `ctl-opt debug(*retval:*constants);`,
    `ctl-opt reqprexp(*require);`,
    ``,
    `// includes`,
    `/copy './rpgle/apival01s.rpgleinc'`,
    ``,
    `dsply 'hello';`,
    ``,
    `return;`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, { ignoreCache: true, withIncludes: true });

  expect(cache.includes.length).toBe(1);
  expect(cache.constants.length).toBe(8);
  expect(cache.procedures.length).toBe(1);

  expect(cache.procedures[0].name).toBe(`APIVAL01S_iws_validate`);
});

test('can define on the first line', async () => {
  const lines = [
    `        begsr sub1;`,
    `        endsr;`,
    `        `,
    `        begsr sub2;`,
    `        endsr;`,
    ``,
    `        `,
    ``,
    `        begsr sub3;`,
    `        endsr;`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, { ignoreCache: true, withIncludes: false });
  expect(cache.subroutines.length).toBe(3);
});

test('fixed-format c spec', async () => {
  const lines = [
    `     Farticle1  UF   E           K DISK`,
    ``,
    `     d UpdArt          pr`,
    `     d   qty                          5  0 value`,
    `     d   id                                like(new.ODARID)`,
    ``,
    `     D PARM1           DS`,
    `      * Physical file name`,
    `     D  FNAME                        10`,
    `      * Physical file library`,
    `     D  LNAME                        10`,
    `      * Member name`,
    `     D  MNAME                        10`,
    `      * Trigger event 1=Ins, 2=Del, 3=Upd`,
    `     D  TEVEN                         1`,
    `      * Trigger time  1=After, 2=Before`,
    `     D  TTIME                         1`,
    `      * Commit lock level`,
    `     D  CMTLCK                        1`,
    `      * Reserved`,
    `     D                                3`,
    `      * CCSID`,
    `     D  CCSID                        10i 0`,
    `      * Reserved`,
    `     D                                8`,
    `      * Offset to the original record`,
    `     D  OLDOFF                       10i 0`,
    `      * length of the original record`,
    `     D  OLDLEN                       10i 0`,
    `      * Offset to the original record null byte map`,
    `     D  ONOFF                        10i 0`,
    `      * length of the null byte map`,
    `     D  ONLEN                        10i 0`,
    `      * Offset to the new record`,
    `     D  NEWOFF                       10i 0`,
    `      * length of the new record`,
    `     D  NEWLEN                       10i 0`,
    `      * Offset to the new record null byte map`,
    `     D  NNOFF                        10i 0`,
    `      * length of the null byte map`,
    `     D  NNLEN                        10i 0`,
    ``,
    `      * Trigger Buffer Length`,
    `     D  parm2          s             10i 0`,
    ``,
    `      * Record to be inserted or new values`,
    `     D NEW           E DS                  EXTNAME(detord)`,
    `     D                                     qualified`,
    `     D                                     based(pn)`,
    ``,
    `      * Record to be deleted or old values`,
    `     D OLD           E DS                  EXTNAME(detord)`,
    `     D                                     qualified`,
    `     D                                     based(po)`,
    `      * SET UP THE ENTRY PARAMETER LIST.`,
    ``,
    `     C     *ENTRY        PLIST`,
    `     C                   PARM                    PARM1`,
    `     C                   PARM                    PARM2`,
    `     C                   if                      %parms = 0`,
    `     C                   seton                                        lr`,
    `     C                   return`,
    `     C                   ENDIF`,
    `     C                   select`,
    `     c                   when      teven = '1'`,
    `     c                   eval      pn = %addr(parm1) + newoff`,
    `     c                   callp     UpdArt(new.odqty:new.odarid)`,
    `     c                   when      teven = '2'`,
    `     c                   eval      po = %addr(parm1) + oldoff`,
    `     c                   callp(e)  addlogEntry('ORD700:Order Line deleted ' +`,
    `     c                             %char(Old.odorid) + ' ' + %char(Old.odline)`,
    `     c                              + ' article : ' + old.odarid`,
    `     c                              + ' quantity : ' + %char(old.odqty))`,
    `     c                   callp     UpdArt(-Old.odqty + Old.odqtyliv:old.odarid)`,
    `     c                   when      teven = '3'`,
    `     c                   eval      pn = %addr(parm1) + newoff`,
    `     c                   eval      po = %addr(parm1) + oldoff`,
    `     c                   if        new.odarid = Old.odarid`,
    `     c                   callp     UpdArt((New.odqty - Old.odqty)`,
    `     c                              - (New.odqtyLiv - Old.odqtyLiv)`,
    `     c                             :new.odarid)`,
    `     c                   else`,
    `     c                   callp     UpdArt(new.odqty- new.odqtyliv:new.odarid)`,
    `     c                   callp     UpdArt(-Old.odqty + Old.odqtyliv:old.odarid)`,
    `     c                   endif`,
    `     c                   endsl`,
    `     c                   return`,
    ``,
    `     P UpdArt          b`,
    `     d UpdArt          pi`,
    `     d   qty                          5  0 value`,
    `     d   id                                like(new.ODARID)`,
    `     c                   if        qty = 0`,
    `     c                   return`,
    `     c                   ENDIF`,
    `     c     id            chain     article1`,
    `     c                   if        not %found`,
    `     c                   return`,
    `     c                   ENDIF`,
    `     c                   eval      ARCUSQTY += qty`,
    `     c                   update    farti`,
    `     P UpdArt          e`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, { ignoreCache: true, withIncludes: false });
  expect(cache.constants.length).toBe(0);
  expect(cache.procedures.length).toBe(2);
  
  expect(cache.procedures[0].name).toBe(`UpdArt`);
  expect(cache.procedures[0].prototype).toBeTruthy();
  expect(cache.procedures[0].subItems.length).toBe(2);

  expect(cache.procedures[1].name).toBe(`UpdArt`);
  expect(cache.procedures[1].prototype).toBeFalsy();
  expect(cache.procedures[1].subItems.length).toBe(2);

  const resolved = cache.find(`UpdArt`);
  expect(resolved).toBeDefined();
  expect(resolved.name).toBe(`UpdArt`);
  expect(resolved.prototype).toBeFalsy();

  expect(cache.structs.length).toBe(3);
  expect(cache.variables.length).toBe(1);
});

test('can resolve return structure correctly', async () => {
  const lines = [
    `**free`,
    ``,
    `dcl-ds return_t qualified template;`,
    `  valueA char(1);`,
    `  valueB char(2);`,
    `end-ds;`,
    ``,
    `dcl-s simpleReturn like(simpleProc);`,
    ``,
    `dcl-proc someProc export;`,
    `  dcl-pi *n likeds(return_t) end-pi;`,
    ``,
    `  // Declare a variable of the return type`,
    `  dcl-ds result likeds(return_t);`,
    ``,
    `  // Assign values to the fields`,
    `  result.valueA = valueA;`,
    `  result.valueB = valueB;`,
    ``,
    `  // Return the structure`,
    `  return result;`,
    `end-proc;`,
    ``,
    `dcl-proc dumbLikeReturn export;`,
    `  dcl-pi *n like(return_t) end-pi;`,
    ``,
    `  // Declare a variable of the return type`,
    `  dcl-ds result likeds(return_t);`,
    ``,
    `  // Assign values to the fields`,
    `  result.valueA = valueA;`,
    `  result.valueB = valueB;`,
    ``,
    `  // Return the structure`,
    `  return result;`,
    `end-proc;`,
    ``,
    `dcl-proc simpleProc export;`,
    `  dcl-pi *n char(10) end-pi;`,
    ``,
    `  return 'hello';`,
    `end-proc;`,
    ``,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, { ignoreCache: true, withIncludes: false });

  expect(cache.procedures.length).toBe(3);

  const someProc = cache.find(`someProc`);
  expect(someProc.name).toBe(`someProc`);

  const typeDataA = cache.resolveType(someProc);
  expect(typeDataA).toBeDefined();
  expect(typeDataA.type).toBeUndefined();
  expect(typeDataA.reference).toBeDefined();
  expect(typeDataA.reference.name).toBe(`return_t`);
  expect(typeDataA.reference.type).toBe(`struct`);
  expect(typeDataA.reference.subItems.length).toBe(2);

  const simpleProc = cache.find(`simpleProc`);
  expect(simpleProc.name).toBe(`simpleProc`);

  const typeDataB = cache.resolveType(simpleProc);
  expect(typeDataB).toBeDefined();
  expect(typeDataB.type).toMatchObject({name: `char`, value: `10`});
  expect(typeDataB.reference).toBeUndefined();

  const simpleReturn = cache.find(`simpleReturn`);
  expect(simpleReturn.name).toBe(`simpleReturn`);

  const typeDataC = cache.resolveType(simpleReturn);
  expect(typeDataC).toBeDefined();
  expect(typeDataC).toBeDefined();
  expect(typeDataC.type).toMatchObject({name: `char`, value: `10`});
  expect(typeDataC.reference).toBeUndefined();

  const dumbLikeReturn = cache.find(`dumbLikeReturn`);
  expect(dumbLikeReturn.name).toBe(`dumbLikeReturn`);

  const typeDataD = cache.resolveType(dumbLikeReturn);
  expect(typeDataD).toBeDefined();
  expect(typeDataD.type).toBeUndefined();
  expect(typeDataD.reference).toBeDefined();
  expect(typeDataD.reference.name).toBe(`return_t`);
  expect(typeDataD.reference.type).toBe(`struct`);
  expect(typeDataD.reference.subItems.length).toBe(2);
});

test('const value #400', async () => {
  const lines = [
    `**free`,
    `Dcl-C PI         3.14159;`
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, { ignoreCache: true, withIncludes: false });

  expect(cache.constants.length).toBe(1);
  const pi = cache.find(`PI`);
  expect(pi.name).toBe(`PI`);
  expect(pi.keyword[`CONST`]).toBe(`3.14159`);
  expect(pi.range).toMatchObject({ start: 1, end: 1 });
});

test('dcl-enum range (#425)', async () => {
  const lines = [
    `**free`,
    ``,
    `dcl-s myprogramText char(20);`,
    ``,
    ``,
    `dcl-enum myenum qualified;`,
    `  CONSTANT1 'value1';`,
    `  CONSTANT2 55;`,
    `end-enum;`,
    ``,
    `dsply myprogramText;`,
  ];

  const cache = await parser.getDocs(uri, lines.join(`\n`), { ignoreCache: true, withIncludes: false });

  const constants = cache.constants;
  expect(constants.length).toBe(1);
  expect(constants[0].name).toBe(`myenum`);
  expect(constants[0].subItems.length).toBe(2);
  expect(constants[0].range).toMatchObject({ start: 5, end: 8 });
  expect(lines[constants[0].range.start]).toBe(`dcl-enum myenum qualified;`);
  expect(lines[constants[0].range.end]).toBe(`end-enum;`);
});

test('correct ranges (#427)', async () => {
  const lines = [
    `     d                                     extproc('main')`,
    `     d  io_test                 12345a         options(*varsize)`,
    `     d testing...`,
    `     d                 pr`,
    `     d                                     extproc('testing')`,
    `     d msgInfo                      787a   const varying options(*varsize)`,
    `     d tester          s                   like(ai_tester)`,
    `     d system          s                   like(ai_system)`,
    `     d fmtType         s                   like(ai_fmtType)`,
    `     d TYPE_VAR_CHAR...`,
    `     d                 c                   x'8004'`,
    `      /free`,
    ``,
    `       *inlr = *on;`,
    ``,
    `       if (%parms() >= p_tester and %addr(ai_tester) <> *null);`,
    `          tester = ai_tester;`,
    `       endif;`,
    ``,
    ``,
    `      //*==========================================================================================*`,
    `      //* Program entry point                                                                      *`,
    `      //*==========================================================================================*`,
    ``,
    `     d TESTING...`,
    `     d                 pi`,
    `     d  aio_test                  12345a         options(*varsize)`,
    `     d  ai_lenFldInf                 10i 0 const`,
    `     d  ai_format                     8a   const`,
    `     d  ai_qFile                           const likeds(qObj_t)`,
    `     d  ai_rcdFmt                    10a   const`,
    `     d  ai_tester                     1a   const options(*nopass)`,
    `     d  ai_system                    10a   const options(*nopass)`,
    `     d  ai_fmtType                   10a   const options(*nopass)`,
    ``,
    `     d p_tester        c                   7`,
    `     d p_system        c                   8`,
    `     d p_fmtType       c                   9`,
    ``,
    `     d tester          s                   like(ai_tester)`,
    `     d system          s                   like(ai_system)`,
    `     d fmtType         s                   like(ai_fmtType)`,
    ``,
    ``,
    `      //*==========================================================================================*`,
    `      //* Main procedure                                                                           *`,
    `      //*==========================================================================================*`,
    `     p main...`,
    `     p                 b`,
    `     d                 pi`,
    `     d  io_test                 12345a         options(*varsize)`,
    `     p                 e`,
  ];

  const cache = await parser.getDocs(uri, lines.join(`\n`), { ignoreCache: true, withIncludes: false });

  const testingPr = cache.find(`testing`);
  expect(testingPr.range.start).toBe(2);
  expect(testingPr.range.end).toBe(5);

  expect(cache.parameters.length).toBe(8);
  expect(cache.parameters[0].name).toBe(`aio_test`);
  expect(cache.parameters[0].range.start).toBe(26)
  expect(cache.parameters[7].name).toBe(`ai_fmtType`);
  expect(cache.parameters[7].range.start).toBe(33)

  const mainProcedure = cache.find(`main`);
  const procStart = mainProcedure.range.start;
  const procEnd = mainProcedure.range.end;
  expect(procEnd-procStart).toBe(4);

  const constants = cache.constants;
  expect(constants.length).toBe(4);

  expect(constants[0].name).toBe(`TYPE_VAR_CHAR`);
  expect(constants[0].range.start).toBe(9);
  expect(constants[0].range.end).toBe(10);
  expect(constants[0].keyword[`CONST`]).toBe(`x'8004'`);

  expect(constants[1].name).toBe(`p_tester`);
  expect(constants[1].range.start).toBe(35);
  expect(constants[1].range.end).toBe(35);
  expect(constants[1].keyword[`CONST`]).toBe(`7`);

  expect(constants[2].name).toBe(`p_system`);
  expect(constants[2].range.start).toBe(36);
  expect(constants[2].range.end).toBe(36);

  expect(constants[3].name).toBe(`p_fmtType`);
  expect(constants[3].range.start).toBe(37);
  expect(constants[3].range.end).toBe(37);
  expect(constants[3].keyword[`CONST`]).toBe(`9`);
});

// test('scoobydo', async () => {
//   const content = await getFileContent(path.join(__dirname, `..`, `rpgle`, `testing.rpgle`));
//   const lines = content.split(/\r?\n/);
//   const cache = await parser.getDocs(uri, content, { ignoreCache: true, withIncludes: false });

//   const kill = cache.findAll(`kill`);
  
//   const killPrototype = kill[0];
//   expect(killPrototype.name).toBe(`kill`);
//   expect(killPrototype.prototype).toBeTruthy();
//   expect(killPrototype.range.start).toBe(71);
//   expect(killPrototype.range.end).toBe(74);

//   const killProc = kill[1];
//   expect(killProc.name).toBe(`kill`);
//   expect(killProc.prototype).toBeFalsy();
//   expect(killProc.range.start).toBe(741);
//   expect(killProc.range.end).toBe(761);  
// });