
const assert = require(`assert`);
const path = require(`path`);

const { default: parserSetup } = require(`../parserSetup`);
const { default: Linter } = require(`../../server/src/language/linter`);

const uri = `source.rpgle`;

exports.test1 = async () => {
  const lines = [
    `**FREE`,
    `Dcl-s MyVariable CHAR(20);`
  ].join(`\n`);

  const parser = await parserSetup();
  const cache = await parser.getDocs(uri, lines);

  assert.strictEqual(cache.variables.length, 1, `Expect length of 1`);
  assert.strictEqual(cache.variables[0].position.line, 1, `Index of 1 expected`);
}

/**
   * Multiple variable definition test
   */
exports.test2 = async () => {
  const lines = [
    `**FREE`,
    `Dcl-s MyVariable CHAR(20);`,
    ``,
    `Dcl-s MyVariable2 CHAR(20);`
  ].join(`\n`);

  const parser = await parserSetup();
  const cache = await parser.getDocs(uri, lines);

  assert.strictEqual(cache.variables.length, 2, `Expect length of 2`);
  assert.strictEqual(cache.variables[0].position.line, 1, `Index of 1 expected`);
  assert.strictEqual(cache.variables[1].position.line, 3, `Index of 3 expected`);
}

/**
   * Variable definition and struct definition test
   */
exports.test3 = async () => {
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

  const parser = await parserSetup();
  const cache = await parser.getDocs(uri, lines);

  assert.strictEqual(cache.variables.length, 2, `Expect length of 2`);
  assert.strictEqual(cache.structs.length, 1, `Expect length of 1`);

  assert.strictEqual(cache.structs[0].subItems.length, 2, `Expect length of 2 subitems`);

  assert.strictEqual(cache.variables[0].position.line, 1, `Index of 1 expected`);
  assert.strictEqual(cache.variables[1].position.line, 6, `Index of 6 expected`);
  assert.strictEqual(cache.structs[0].position.line, 2, `Index of 2 expected`);

  assert.deepStrictEqual(cache.structs[0].range, {
    start: 2,
    end: 5
  });
}

/**
   * Variable and subroutine definition test
   * */
exports.test4 = async () => {
  const lines = [
    `**FREE`,
    `Dcl-s MyVariable2 CHAR(20);`,
    ``,
    ``,
    `Begsr theSubroutine;`,
    `  MyVariable2 = 'Hello world';`,
    `Endsr;`,
  ].join(`\n`);

  const parser = await parserSetup();
  const cache = await parser.getDocs(uri, lines);

  assert.strictEqual(cache.variables.length, 1, `Expect length of 1`);
  assert.strictEqual(cache.subroutines.length, 1, `Expect length of 1`);

  assert.strictEqual(cache.variables[0].position.line, 1, `Index of 1 expected`);
  assert.strictEqual(cache.subroutines[0].range.start, 4, `Index of 4 expected`);
  assert.strictEqual(cache.subroutines[0].range.end, 6);
}

/**
   * Variable and procedure definition test
   */
exports.test5 = async () => {
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

  const parser = await parserSetup();
  const cache = await parser.getDocs(uri, lines);

  assert.strictEqual(cache.variables.length, 1, `Expect length of 1`);
  assert.strictEqual(cache.procedures.length, 2, `Expect length of 2`);

  assert.strictEqual(cache.variables[0].position.line, 2, `Index of 2 expected`);
  assert.strictEqual(cache.procedures[0].position.line, 4, `Index of 4 expected`);
  assert.strictEqual(cache.procedures[1].position.line, 8, `Index of 8 expected`);

  assert.strictEqual(cache.procedures[0].subItems.length, 0, `Expect length of 0`);
  assert.strictEqual(cache.procedures[1].subItems.length, 1, `Expect length of 1`);
}

exports.test6 = async () => {
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

  const parser = await parserSetup();
  const cache = await parser.getDocs(uri, lines);

  assert.strictEqual(cache.variables.length, 1, `Expect length of 1`);
  assert.strictEqual(cache.procedures.length, 1, `Expect length of 1`);
}

/**
   * Test procedure length.
   * When Procedure is defined, the prototype is overridden.
   */
exports.test7 = async () => {
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

  const parser = await parserSetup();
  const cache = await parser.getDocs(uri, lines);

  assert.strictEqual(cache.variables.length, 1, `Expect length of 1`);
  assert.strictEqual(cache.procedures.length, 1, `Expect length of 1`);
  assert.strictEqual(cache.procedures[0].subItems.length, 1, `Expect length of 1`);
}

/**
   * Test procedure length.
   * When Procedure is defined, the prototype is overridden.
   */
exports.test7_fixed = async () => {
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

  const parser = await parserSetup();
  const cache = await parser.getDocs(uri, lines);

  assert.strictEqual(cache.procedures.length, 1, `Expect length of 1`);
  assert.strictEqual(cache.procedures[0].subItems.length, 1, `Expect length of 1`);
}

/**
   * Constant definition test
   * */
exports.test8 = async () => {
  const lines = [
    `**FREE`,
    `Dcl-s MyVariable2 Char(20);`,
    ``,
    `Dcl-C theConstant 'Hello world';`,
  ].join(`\n`);

  const parser = await parserSetup();
  const cache = await parser.getDocs(uri, lines);

  assert.strictEqual(cache.variables.length, 1, `Expect length of 1`);
  assert.strictEqual(cache.constants.length, 1, `Expect length of 1`);
}

/**
   * Check that local variables are not in global scope
   */
exports.test9 = async () => {
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

  const parser = await parserSetup();
  const cache = await parser.getDocs(uri, lines);

  assert.strictEqual(cache.variables.length, 1, `Expect length of 1`);
  assert.strictEqual(cache.constants.length, 1, `Expect length of 1`);
  assert.strictEqual(cache.procedures.length, 1, `Expect length of 1`);
  assert.strictEqual(cache.procedures[0].subItems.length, 1, `Expect length of 1`);
}

/**
   * Test copybook
   * */
exports.test10 = async () => {
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

  const parser = await parserSetup();
  const cache = await parser.getDocs(uri, lines);

  assert.strictEqual(Object.keys(cache.keyword).length, 1);
  assert.strictEqual(cache.keyword[`DFTACTGRP`], `*NO`);
  assert.strictEqual(cache.includes.length, 1);
  assert.strictEqual(cache.variables.length, 1, `Expect length of 1`);
  assert.strictEqual(cache.constants.length, 1, `Expect length of 1`);
  assert.strictEqual(cache.procedures.length, 1, `Expect length of 1`);
  assert.strictEqual(cache.procedures[0].subItems.length, 1, `Expect length of 1`);

  const baseNameInclude = path.basename(cache.procedures[0].position.path);
  assert.strictEqual(baseNameInclude, `copy1.rpgle`, `Path is incorrect`);
  assert.strictEqual(cache.procedures[0].position.line, 2, `Index of 3 expected`);
}

exports.test10_local_fixedcopy = async () => {
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

  const parser = await parserSetup();
  const cache = await parser.getDocs(uri, lines);

  assert.strictEqual(cache.includes.length, 1);
  assert.strictEqual(cache.variables.length, 1, `Expect length of 1`);
  assert.strictEqual(cache.constants.length, 1, `Expect length of 1`);
  assert.strictEqual(cache.procedures.length, 1, `Expect length of 1`);

  const uppercase = cache.find(`UPPERCASE`);

  const baseNameInclude = path.basename(uppercase.position.path);
  assert.strictEqual(baseNameInclude, `eof4.rpgle`, `Path is incorrect`);
}

/**
   * Test many copybooks
   * */
exports.test11 = async () => {
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

  const parser = await parserSetup();
  const cache = await parser.getDocs(uri, lines);

  assert.strictEqual(cache.includes.length, 2);
  assert.strictEqual(cache.variables.length, 1, `Expect length of 1`);
  assert.strictEqual(cache.constants.length, 2, `Expect length of 2`);
  assert.strictEqual(cache.structs.length, 1, `Expect length of 1`);
  assert.strictEqual(cache.structs[0].subItems.length, 1, `Expect length of 1`);
  assert.strictEqual(cache.procedures.length, 1, `Expect length of 1`);
  assert.strictEqual(cache.procedures[0].subItems.length, 1, `Expect length of 1`);
}

exports.test12 = async () => {
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

  const parser = await parserSetup();
  const cache = await parser.getDocs(uri, lines);

  assert.strictEqual(cache.includes.length, 1);

  assert.strictEqual(cache.variables.length, 1, `Expect length of 1`);
  assert.strictEqual(cache.constants.length, 1, `Expect length of 1`);

  // One prototype and one declared
  assert.strictEqual(cache.procedures.length, 2, `Expect length of 2`);

  // Valid names
  assert.strictEqual(cache.procedures[0].name, `theExtProcedure`, `Expect valid name`);
  assert.strictEqual(cache.procedures[1].name, `theLocalProc`, `Expect valid name`);

  const theLocalProc = cache.find(`theLocalProc`);

  assert.deepStrictEqual(theLocalProc.range, {
    start: 16,
    end: 23
  });

  // Has a parameter
  assert.strictEqual(theLocalProc.subItems.length, 1, `Expect length of 1`);

  // Has a local scope
  assert.strictEqual(theLocalProc.scope !== undefined, true, `Should have a scope`);

  // Should have a local variable
  assert.strictEqual(theLocalProc.scope.variables.length, 1, `Expect length of 1`);
};

exports.indicators1 = async () => {
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

  const parser = await parserSetup();
  const cache = await parser.getDocs(uri, lines);

  Linter.getErrors({ uri, content: lines }, {
    CollectReferences: true,
  }, cache);

  const in10 = cache.find(`IN10`);

  assert.strictEqual(in10.references.length, 2);
};


exports.subds1 = async () => {
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

  const parser = await parserSetup();
  const cache = await parser.getDocs(uri, lines);

  assert.strictEqual(cache.structs.length, 1);

  const DsChangingNodeRole = cache.find(`DsChangingNodeRole`);
  assert.strictEqual(DsChangingNodeRole.name, `DsChangingNodeRole`);
  assert.strictEqual(DsChangingNodeRole.position.line, 2);

  assert.strictEqual(DsChangingNodeRole.subItems.length, 13);
  assert.strictEqual(DsChangingNodeRole.subItems[12].name, `Role`);

  assert.deepStrictEqual(DsChangingNodeRole.range, {
    start: 2,
    end: 19
  });
};

exports.range1 = async () => {
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

  const parser = await parserSetup();
  const cache = await parser.getDocs(uri, lines);

  assert.strictEqual(cache.procedures.length, 2);

  const json_getDelims = cache.find(`json_getDelims`);
  assert.deepStrictEqual(json_getDelims.range, {
    start: 11,
    end: 11
  });

  const json_getDelimiters = cache.find(`json_setDelimiters`);
  assert.strictEqual(json_getDelimiters.subItems.length, 1);
  assert.deepStrictEqual(json_getDelimiters.range, {
    start: 21,
    end: 23
  });
};

exports.range2 = async () => {
  const lines = [
    `**free`,
    `Dcl-S  FullCmd      Char(32);`,
    `Dcl-DS ABCD  LikeDS(BBOOP);`,
    ``,
    ``,
    `Dcl-S Eod          Ind;`,
  ].join(`\n`);

  const parser = await parserSetup();
  const cache = await parser.getDocs(uri, lines);

  assert.strictEqual(cache.variables.length, 2);
  assert.strictEqual(cache.structs.length, 1);

  const ABCD = cache.find(`ABCD`);
  assert.strictEqual(ABCD.keyword[`LIKEDS`], `BBOOP`);
  assert.deepStrictEqual(ABCD.range, {
    start: 2,
    end: 2
  });
}

exports.inline_end_pi = async () => {
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

  const parser = await parserSetup();
  const cache = await parser.getDocs(uri, lines);

  const getHandle = cache.find(`getHandle`);

  assert.strictEqual(getHandle.keyword[`LIKE`], `HANDLE_T`);
  assert.strictEqual(getHandle.keyword[`END-PI`], undefined);
}

exports.issue_168 = async () => {
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

  const parser = await parserSetup();
  const cache = await parser.getDocs(uri, lines);
}

exports.issues_168a = async () => {
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

  const parser = await parserSetup();
  const cache = await parser.getDocs(uri, lines);
}

exports.issues_170b = async () => {
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

  const parser = await parserSetup();
  const cache = await parser.getDocs(uri, lines);

  const WkStnInd = cache.find(`WkStnInd`);
  assert.strictEqual(WkStnInd.name, `WkStnInd`);
  assert.strictEqual(WkStnInd.subItems.length, 11);

  const error = cache.find(`Error`);
  assert.strictEqual(error.name, `Error`);
  assert.strictEqual(error.keyword[`IND`], true);
  assert.strictEqual(error.keyword[`POS`], `25`);
}

exports.issues_dcl_subf = async () => {
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

  const parser = await parserSetup();
  const cache = await parser.getDocs(uri, lines);

  const inputsYo = cache.find(`inputsYo`);
  assert.strictEqual(inputsYo.subItems.length, 7);

  const boop_Addr1 = inputsYo.subItems[0];
  assert.strictEqual(boop_Addr1.name, `boop_Addr1`);
}

exports.issue_195a = async () => {
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

  const parser = parserSetup();
  const cache = await parser.getDocs(uri, lines);

  cache.clearReferences();
}

exports.issue_195b = async () => {
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

  const parser = parserSetup();
  const cache = await parser.getDocs(uri, lines);

  assert.strictEqual(cache.procedures.length, 1);

  const DoProfileStuff = cache.find(`DoProfileStuff`);
  assert.strictEqual(DoProfileStuff.scope.procedures.length, 2);
}

exports.exec_1 = async () => {
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

  const parser = parserSetup();
  const cache = await parser.getDocs(uri, lines);

  assert.strictEqual(cache.sqlReferences.length, 1);
  assert.strictEqual(cache.sqlReferences[0].name, `sysdummy1`);
  assert.strictEqual(cache.sqlReferences[0].description, `sysibm`);
}

exports.exec_2 = async () => {
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

  const parser = parserSetup();
  const cache = await parser.getDocs(uri, lines);

  assert.strictEqual(cache.sqlReferences.length, 2);
  assert.strictEqual(cache.sqlReferences[0].name, `Employee`);
  assert.strictEqual(cache.sqlReferences[0].description, ``);

  assert.strictEqual(cache.sqlReferences[1].name, `Employee`);
  assert.strictEqual(cache.sqlReferences[1].description, `sample`);
}

exports.exec_3 = async () => {
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

  const parser = parserSetup();
  const cache = await parser.getDocs(uri, lines);

  assert.strictEqual(cache.sqlReferences.length, 0);
}

exports.exec_4 = async () => {
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

  const parser = parserSetup();
  const cache = await parser.getDocs(uri, lines);

  assert.strictEqual(cache.sqlReferences.length, 1);
  assert.strictEqual(cache.sqlReferences[0].name, `employee`);
  assert.strictEqual(cache.sqlReferences[0].description, `sample`);
}

exports.exec_5 = async () => {
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

  const parser = parserSetup();
  const cache = await parser.getDocs(uri, lines);

  assert.strictEqual(cache.sqlReferences.length, 2);

  assert.strictEqual(cache.sqlReferences[0].name, `mytable`);
  assert.strictEqual(cache.sqlReferences[0].description, `sample`);

  assert.strictEqual(cache.sqlReferences[1].name, `othertable`);
  assert.strictEqual(cache.sqlReferences[1].description, ``);
}

exports.exec_6 = async () => {
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

  const parser = parserSetup();
  const cache = await parser.getDocs(uri, lines);

  assert.strictEqual(cache.sqlReferences.length, 2);

  assert.strictEqual(cache.sqlReferences[0].name, `mytable`);
  assert.strictEqual(cache.sqlReferences[0].description, `sample`);

  assert.strictEqual(cache.sqlReferences[1].name, `othertable`);
  assert.strictEqual(cache.sqlReferences[1].description, ``);
}

exports.exec_7 = async () => {
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

  const parser = parserSetup();
  const cache = await parser.getDocs(uri, lines);

  assert.strictEqual(cache.sqlReferences.length, 2);

  assert.strictEqual(cache.sqlReferences[0].name, `thetable`);
  assert.strictEqual(cache.sqlReferences[0].description, `sample`);

  assert.strictEqual(cache.sqlReferences[1].name, `cooltable`);
  assert.strictEqual(cache.sqlReferences[1].description, ``);
}

exports.exec_8 = async () => {
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

  const parser = parserSetup();
  const cache = await parser.getDocs(uri, lines);

  assert.strictEqual(cache.sqlReferences.length, 2);

  assert.strictEqual(cache.sqlReferences[0].name, `thetable`);
  assert.strictEqual(cache.sqlReferences[0].description, `sample`);

  assert.strictEqual(cache.sqlReferences[1].name, `wooptable`);
  assert.strictEqual(cache.sqlReferences[1].description, ``);
}

exports.exec_9 = async () => {
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

  const parser = parserSetup();
  const cache = await parser.getDocs(uri, lines);

  assert.strictEqual(cache.sqlReferences.length, 2);

  assert.strictEqual(cache.sqlReferences[0].name, `MyRandomProc`);
  assert.strictEqual(cache.sqlReferences[0].description, `sample`);

  assert.strictEqual(cache.sqlReferences[1].name, `OtherCoolProc`);
  assert.strictEqual(cache.sqlReferences[1].description, ``);
}