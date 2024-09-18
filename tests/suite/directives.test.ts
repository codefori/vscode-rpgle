import path from "path";
import setupParser from "../parserSetup";
import Linter from "../../language/linter";
import { test, expect } from "vitest";

const parser = setupParser();
const uri = `source.rpgle`;

test('skip1', async () => {
  const lines = [
    `**free`,
    ``,
    `/copy myds.ds`,
    `end-ds;`,
    ``,
    `dsply thingy;`,
    ``,
    `return`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, { withIncludes: true, ignoreCache: true });
  const { indentErrors } = Linter.getErrors({ uri, content: lines }, {
    indent: 2
  }, cache);

  expect(cache.includes.length).toBe(0); // Because it's not found.
  expect(indentErrors.length > 0).toBe(true);
})

test('skip2', async () => {
  const lines = [
    `**free`,
    ``,
    `/copy myds.ds`,
    `// @rpglint-skip`,
    `end-ds;`,
    ``,
    `dsply thingy;`,
    ``,
    `return`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, { withIncludes: true, ignoreCache: true });
  const { indentErrors } = Linter.getErrors({ uri, content: lines }, {
    indent: 2
  }, cache);

  expect(cache.includes.length).toBe(0); // Because it's not found.
  expect(indentErrors.length).toBe(0);
})

test('skip2_issue91_1', async () => {
  const lines = [
    `**free`,
    ``,
    `/copy myds.ds`,
    `// @rpglint-skip-indent`,
    `end-ds;`,
    ``,
    `dsply thingy;`,
    ``,
    `return`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, { withIncludes: true, ignoreCache: true });
  const { indentErrors } = Linter.getErrors({ uri, content: lines }, {
    indent: 2
  }, cache);

  expect(cache.includes.length).toBe(0); // Because it's not found.
  expect(indentErrors.length).toBe(0);
})

test('skip2_issue91_2', async () => {
  const lines = [
    `**free`,
    ``,
    `/copy myds.ds`,
    `// @rpglint-skip-rules`,
    `end-ds;`,
    ``,
    `dsply thingy;`,
    ``,
    `return`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, { withIncludes: true, ignoreCache: true });
  const { indentErrors } = Linter.getErrors({ uri, content: lines }, {
    indent: 2
  }, cache);

  expect(cache.includes.length).toBe(0); // Because it's not found.
  expect(indentErrors.length).toBe(3);
})

test('skip2_issue91', async () => {
  const lines = [
    `**FREE`,
    ``,
    `/IF DEFINED(ABCEEF)`,
    `/eof`,
    `/EndIf`,
    `/DEFINE ABCEEF`,

    `// @rpglint-skip-rules`,
    `CallP THEPROCEDURE2;`,
    ``,
    `Dcl-Proc theProcedure2;`,
    `  Dcl-S mylocal char(20);`,
    `  MyVariable2 = 'Hello world';`,
    `  mylocal = Myvariable2;`,
    `End-Proc;`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, { withIncludes: true, ignoreCache: true });
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    IncorrectVariableCase: true
  }, cache);

  expect(cache.procedures.length).toBe(1);
  const theProcedure2 = cache.find(`theProcedure2`);
  expect(theProcedure2.name).toBe(`theProcedure2`);

  expect(errors.length).toBe(0);
})


test('skip3', async () => {
  const lines = [
    `**free`,
    `dcl-s xxField1 char(1);`,
    ``,
    `// @rpglint-skip`,
    `/copy myds.ds`,
    ``,
    `dsply xxfield1;`,
    ``,
    `return`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, { withIncludes: true, ignoreCache: true });
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    IncorrectVariableCase: true
  }, cache);

  expect(cache.includes.length).toBe(0); // Because it's not found.
  expect(errors.length).toBe(1);
})

test('eof1', async () => {
  const lines = [
    `     D UPPERCASE       PR          4096    Varying`,
    `     D   String                    4096    Const Varying`,
    `     D   Escaped                       n   Const Options(*NoPass)`,
    `      /EoF`,
    `            Converts all of the letters in String to their`,
    `            UPPER CASE equivalents.  Non-alphabetic characters`,
    `            remain unchanged.`,
    ``,
    `            Escaped = *ON = converts characters that would crash iPDF and`,
    `                            HTML to approximately equivalent characters.`,
    `                            For example, translate " and ' to \` .`,
    `                            (Default)`,
    `                      *OFF= Do not convert any characters other than A-Z.`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, { withIncludes: true, ignoreCache: true });

  const uppercase = cache.find(`UPPERCASE`);
  expect(uppercase.name).toBe(`UPPERCASE`);
  expect(uppercase.position.line).toBe(0);
  expect(uppercase.subItems.length).toBe(2);
})

test('eof2', async () => {
  const lines = [
    `     D UPPERCASE       PR          4096    Varying`,
    `     D   String                    4096    Const Varying`,
    `     D   Escaped                       n   Const Options(*NoPass)`,
    `      /EoF`,
    ``,
    `     D LOWERCASE       PR          4096    Varying`,
    `     D   String                    4096    Const Varying`,
    `     D   Escaped                       n   Const Options(*NoPass)`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, { withIncludes: true, ignoreCache: true });

  expect(cache.procedures.length).toBe(1);

  const uppercase = cache.find(`UPPERCASE`);
  expect(uppercase.name).toBe(`UPPERCASE`);
  expect(uppercase.position.line).toBe(0);
  expect(uppercase.subItems.length).toBe(2);
})

/**
 * Similar to linter18 test
 */
test('eof3', async () => {
  const lines = [
    `**FREE`,
    `Dcl-s MyVariable2 Char(20);`,
    ``,
    `theProcedure();`,
    `Dsply MyVariable2;`,
    ``,
    `Dcl-Proc theProcedure;`,
    `  Dcl-S mylocal char(20);`,
    `  MyVariable2 = 'Hello world';`,
    `  mylocal = Myvariable2;`,
    `End-Proc;`,
    ``,
    `/eof`,
    ``,
    `Dcl-Proc theProcedure2;`,
    `  Dcl-S mylocal char(20);`,
    `  MyVariable2 = 'Hello world';`,
    `  mylocal = Myvariable2;`,
    `End-Proc;`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, { withIncludes: true, ignoreCache: true });
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    NoGlobalsInProcedures: true
  }, cache);

  expect(cache.procedures.length).toBe(1);
  expect(errors.length).toBe(2);
})

test('eof4', async () => {
  const lines = [
    `**FREE`,
    ``,
    `Ctl-Opt DftActGrp(*No);`,
    ``,
    `/copy './tests/rpgle/eof4.rpgle'`,
    ``,
    `Dcl-s MyVariable2 Char(20);`,
    ``,
    `CallP UPPERCASE(myVariable:*on);`,
    ``,
    `Return;`
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, { withIncludes: true, ignoreCache: true });

  expect(cache.includes.length).toBe(1);
  expect(cache.includes[0].line).toBe(4);

  expect(cache.variables.length).toBe(1);
  expect(cache.procedures.length).toBe(1);

  const uppercase = cache.find(`UPPERCASE`);

  expect(uppercase.subItems.length).toBe(2);

  const baseNameInclude = path.basename(uppercase.position.path);
  expect(baseNameInclude).toBe(`eof4.rpgle`);
  expect(uppercase.position.line).toBe(0);
})


/**
 * EOF inside of IF directive
 */
test('eof5_issue181', async () => {
  const lines = [
    `**FREE`,
    ``,
    `/IF DEFINED(ABCEEF)`,
    `/eof`,
    `/EndIf`,
    `/DEFINE ABCEEF`,

    `CallP THEPROCEDURE2;`,
    ``,
    `Dcl-Proc theProcedure2;`,
    `  Dcl-S mylocal char(20);`,
    `  MyVariable2 = 'Hello world';`,
    `  mylocal = Myvariable2;`,
    `End-Proc;`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, { withIncludes: true, ignoreCache: true });
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    IncorrectVariableCase: true
  }, cache);

  expect(cache.procedures.length).toBe(1);
  const theProcedure2 = cache.find(`theProcedure2`);
  expect(theProcedure2.name).toBe(`theProcedure2`);

  expect(errors.length).toBe(1);
})

test('incorrectEnd1', async () => {
  const lines = [
    `Dcl-S Text Char(52);`,
    ``,
    `Text = 'Hello world';`,
    ``,
    `End-Proc;`
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, { withIncludes: true, ignoreCache: true });

  const { errors } = Linter.getErrors({ uri, content: lines }, {
    PrettyComments: true
  }, cache);

  expect(errors[0]).toEqual({
    offset: { position: 45, end: 53 }, type: `UnexpectedEnd`
  });

  expect(lines.substring(errors[0].offset.position, errors[0].offset.end)).toBe(`End-Proc`);
})

test('incorrectEnd2', async () => {
  const lines = [
    `**free`,
    ``,
    `dcl-proc *inzsr;`,
    `  dsply 'hello world';`,
    `endsr;`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, { withIncludes: true, ignoreCache: true });

  const { errors } = Linter.getErrors({ uri, content: lines }, {
    PrettyComments: true
  }, cache);

  expect(errors[0]).toEqual({
    offset: { position: 48, end: 53 }, type: `UnexpectedEnd`
  });

  expect(lines.substring(errors[0].offset.position, errors[0].offset.end)).toBe(`endsr`);
})

test('incorrectEnd3', async () => {
  const lines = [
    `**free`,
    ``,
    `begsr hello;`,
    `  dsply 'hello world';`,
    `end-proc;`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, { withIncludes: true, ignoreCache: true });

  const { errors } = Linter.getErrors({ uri, content: lines }, {
    PrettyComments: true
  }, cache);

  expect(errors[0]).toEqual({
    offset: { position: 44, end: 52 }, type: `UnexpectedEnd`
  });

  expect(lines.substring(errors[0].offset.position, errors[0].offset.end)).toBe(`end-proc`);
})

test('incorrectEnd4', async () => {
  const lines = [
    `**FREE`,
    `Dcl-s MyVariable2 Char(20);`,
    ``,
    `theProcedure();`,
    `Dsply MyVariable2;`,
    ``,
    `Dcl-Proc theProcedure;`,
    `  Exsr theSubroutine;`,
    `  Begsr theSubroutine;`,
    `    MyVariable2 = 'Hello world';`,
    `    // Endsr;`,
    `End-Proc;`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, { withIncludes: true, ignoreCache: true });

  const { errors } = Linter.getErrors({ uri, content: lines }, {
    PrettyComments: true
  }, cache);

  expect(errors[0]).toEqual({
    offset: { position: 187, end: 195 }, type: `UnexpectedEnd`
  });
})

test('if1', async () => {
  const lines = [
    `**FREE`,
    `// Function Return Param Definitions`,
    `Dcl-Ds Prp00a Qualified`,
    `/IF DEFINED(PRP00A_TEMPLATE_ALL_DS)`,
    ` Template`,
    `/ENDIF`,
    `;`,
    `  Address Char(220);`,
    `  Emp Packed(6);`,
    `  Empname Char(60);`,
    `  Phone_w_errm Char(95);`,
    `  Phone Char(15) Overlay(Phone_w_errm :1);`,
    `  Zipcode_w_errm Char(90);`,
    `  Zipcode Char(10) Overlay(Zipcode_w_errm :1);`,
    `End-Ds;`,
    ``,
    `Dcl-Ds Tmplt_EmpFmtAddress Qualified Template;`,
    `  Name Char(60);`,
    `  Addr1 Char(40);`,
    `  Addr2 Char(40);`,
    `  Addr3 Char(40);`,
    `  Addr4 Char(40);`,
    `End-Ds;`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, { withIncludes: true, ignoreCache: true });

  expect(cache.structs.length).toBe(2);

  const Prp00a = cache.find(`Prp00a`);
  expect(Prp00a.subItems.length).toBe(7);
  expect(Prp00a.keyword[`QUALIFIED`]).toBe(true);
  expect(Prp00a.keyword[`TEMPLATE`]).toBeUndefined();
})

test('if2', async () => {
  const lines = [
    `     D ObjNam          s             10a`,
    `     d someDs          ds`,
    `       /IF DEFINED(RPGBNV)`,
    `     d                                     based(somepointer)`,
    `       /ENDIF`,
    `     d  xxxxxx                       10i 0`,
    `     d  xxxxxxxx                     10i 0`,
    `     d  xxxxxx                       20i 0`,
    `     d  xxx                          10i 0`,
    `     d  xxxxx                        10i 0`,
    `     d  yyyyy                        10i 0`,
    `     d  zzzzz                        10i 0`,
    `     d  fffffff                        N`,
    `     d  jjjjj                          N`,
    `     d  jjjjjjj                        N`,
    `     d  mmmmm                        10`,
    `     d  cccccc                        3`,
    `     d  bbbbbbd                      10i 0`,
    `     d  dddd                         10i 0`,
    `     d  ddddd                        10i 0`,
    `     d  bbbbb                        10i 0`,
    `     d  ccc                          10i 0`,
    `     d  bbbwee                        7`,
    `     d  fffbb                        10i 0`,
    `     d  ff                         1024a`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, { withIncludes: true, ignoreCache: true });

  expect(cache.structs.length).toBe(1);

  const someDs = cache.find(`someDs`);
  expect(someDs.keyword[`BASED`]).toBeUndefined();
})

test('variable_case1', async () => {
  const lines = [
    `**FREE`,
    `Ctl-Opt DftActGrp(*No);`,
    `/copy './tests/rpgle/copy3.rpgle'`,
    `Dcl-S MyCustomerName1 like(customername_t);`,
    `Dcl-S MyCustomerName2 like(CustomerName_t);`,
    `Dcl-S MyCustomerName3 like(CUSTOMERNAME_t);`,
    `Dcl-S MyCustomerName4 like(CUSTOMERNAME_T);`,
    `MyCustomerName1 = 'John Smith';`,
    `dsply MyCustomerName1;`,
    `Return;`
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, { withIncludes: true, ignoreCache: true });
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    IncorrectVariableCase: true
  }, cache);

  expect(errors.length).toBe(3);

  expect(errors[0]).toEqual({
    offset: { position: 92, end: 106 },
    type: `IncorrectVariableCase`,
    newValue: `CustomerName_t`
  });

  expect(errors[1]).toEqual({
    offset: { position: 180, end: 194 },
    type: `IncorrectVariableCase`,
    newValue: `CustomerName_t`
  });

  expect(errors[2]).toEqual({
    offset: { position: 224, end: 238 },
    type: `IncorrectVariableCase`,
    newValue: `CustomerName_t`
  });
});

test('variable_case1 commented out', async () => {
  const lines = [
    `**FREE`,
    `Ctl-Opt DftActGrp(*No);`,
    `// /copy './tests/rpgle/copy3.rpgle'`,
    `Dcl-S MyCustomerName1 like(customername_t);`,
    `Dcl-S MyCustomerName2 like(CustomerName_t);`,
    `Dcl-S MyCustomerName3 like(CUSTOMERNAME_t);`,
    `Dcl-S MyCustomerName4 like(CUSTOMERNAME_T);`,
    `MyCustomerName1 = 'John Smith';`,
    `dsply MyCustomerName1;`,
    `Return;`
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, { withIncludes: true, ignoreCache: true });
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    IncorrectVariableCase: true
  }, cache);

  expect(errors.length).toBe(0);
});

test('uppercase1', async () => {
  const lines = [
    `**FREE`,
    `Ctl-Opt DftActGrp(*No);`,
    `/copy './tests/rpgle/copy1.rpgle'`,
    `/Copy './tests/rpgle/copy2.rpgle'`,
    `/COPY './tests/rpgle/copy3.rpgle'`,
    `Dcl-S MyCustomerName1 like(CustomerName_t);`,
    `MyCustomerName1 = 'John Smith';`,
    `dsply MyCustomerName1;`,
    `Return;`
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, { withIncludes: true, ignoreCache: true });
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    DirectiveCase: `upper`
  }, cache);

  expect(errors.length).toBe(2);

  expect(errors[0]).toEqual({
    offset: { position: 31, end: 36 },
    type: `DirectiveCase`,
    newValue: `/COPY`
  });

  expect(errors[1]).toEqual({
    offset: { position: 65, end: 70 },
    type: `DirectiveCase`,
    newValue: `/COPY`
  });
})

test('lowercase1', async () => {
  const lines = [
    `**FREE`,
    `Ctl-Opt DftActGrp(*No);`,
    `/copy './tests/rpgle/copy1.rpgle'`,
    `/Copy './tests/rpgle/copy2.rpgle'`,
    `/COPY './tests/rpgle/copy3.rpgle'`,
    `Dcl-S MyCustomerName1 like(CustomerName_t);`,
    `MyCustomerName1 = 'John Smith';`,
    `dsply MyCustomerName1;`,
    `Return;`
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, { withIncludes: true, ignoreCache: true });
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    DirectiveCase: `lower`
  }, cache);

  expect(errors.length).toBe(2);

  expect(errors[0]).toEqual({
    offset: { position: 65, end: 70 },
    type: `DirectiveCase`,
    newValue: `/copy`
  });

  expect(errors[1]).toEqual({
    offset: { position: 99, end: 104 },
    type: `DirectiveCase`,
    newValue: `/copy`
  });
})