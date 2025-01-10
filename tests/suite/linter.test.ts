
import path from "path";
import setupParser from "../parserSetup";
import Linter from "../../language/linter";
import { test, expect } from "vitest";
import { tokenise } from "../../language/tokens";

const parser = setupParser();
const uri = `source.rpgle`;
const includeUri = `source.rpgleinc`;
const memberIncludeUri = `/LIB/SRC/MEMBER.RPGLEINC?readonly`;

test("linter_indent_multi_1", async () => {
  const lines = [
    `**FREE`,
    `Begsr shouldBeGood;`,
    `  If a`,
    `  and b`,
    `      or c;`,
    `    clear x;`,
    `  Endif;`,
    `  If a // comment`,
    `  and b // comment`,
    `      or c; // comment`,
    `    clear x;`,
    `  Elseif x`,
    `  and y`,
    `      or z;`,
    `    Dou not x`,
    `    and not y`,
    `    and not z;`,
    `      clear z;`,
    `    Enddo;`,
    `  Endif;`,
    `Endsr;`,
    `Return;`
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true});
  const { indentErrors } = Linter.getErrors({ uri, content: lines }, {
    indent: 2
  }, cache);

  expect(indentErrors.length).toBe(0);
});

test("linter_indent_multi_2", async () => {
  const lines = [
    `**FREE`,
    `Begsr shouldError;`,
    `  If a`,
    ` and b`,
    `      or c;`,
    `     clear x;`,
    `  Endif;`,
    `Endsr;`,
    `Return;`
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
  const { indentErrors } = Linter.getErrors({ uri, content: lines }, {
    indent: 2
  }, cache);

  expect(indentErrors.length).toBe(1);
  expect(indentErrors[0].line).toBe(5);
  expect(indentErrors[0].currentIndent).toBe(5);
  expect(indentErrors[0].expectedIndent).toBe(4);
});

test("linter_invalid_statement", async () => {
  const lines = [
    `**FREE`,
    `Dcl-s abcd char(5); Dsply 'This is bad but shouldn't error';`,
    `Begsr shouldError;`,
    `Endsr;`,
    `Return;`
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
  const { indentErrors } = Linter.getErrors({ uri, content: lines }, {
    indent: 2
  }, cache);

  expect(indentErrors.length).toBe(0);
});

test("linter1_indent", async () => {
  const lines = [
    `**FREE`,
    ``,
    `Ctl-Opt DFTACTGRP(*No);`,
    ``,
    `// My variable`,
    `Dcl-s MyVariable2 Char(20);`,
    ``,
    `myVariable2 = *blank;`,
    ``,
    `If myVariable2 = *blank;`,
    `  // Inside if`,
    `MyVariable2 = 'Hello world';`,
    `Endif;`,
    `Return;`
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
  const { indentErrors } = Linter.getErrors({ uri, content: lines }, {
    indent: 2
  }, cache);

  expect(indentErrors.length).toBe(1);
  expect(indentErrors[0].line).toBe(11);
  expect(indentErrors[0].currentIndent).toBe(0);
  expect(indentErrors[0].expectedIndent).toBe(2);
});

/**
  * Testing spaces before the EOL
  */
test("linter1_1_indent", async () => {
  const lines = [
   `**FREE`,
   ``,
   `Ctl-Opt DFTACTGRP(*No);`,
   ``,
   `// Prototype`,
   `Dcl-Pr printf Int(10) ExtProc('printf');`,
   `  format Pointer Value Options(*String); `, //This space at the end was causing an indent error on the next line
   `END-PR;`,
   ``,
   `Dcl-s MyVariable2 Char(20);`,
   ``,
   `myVariable2 = *blank;`,
   ``,
   `If myVariable2 = *blank;`,
   `  // Inside if`,
   `  MyVariable2 = 'Hello world';`,
   `Endif;`,
   `Return;`
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
  const { indentErrors } = Linter.getErrors({ uri, content: lines }, {
   indent: 2
  }, cache);

  expect(indentErrors.length).toBe(0);
});

test("linter2_indent", async () => {
  const lines = [
   `**FREE`,
   ``,
   `Ctl-Opt DFTACTGRP(*No);`,
   ``,
   `Dcl-s MyVariable2 Char(20);`,
   ``,
   `myVariable2 = *blank;`,
   ``,
   `If myVariable2 = *blank;`,
   `  // Inside if`,
   `MyVariable2 = 'Hello world';`,
   `  Select;`,
   `    When myVariable2 = *blank;`,
   `      // First when`,
   `      MyVariable2 = 'Still blank?';`,
   `    When myVariable2 = 'YOYOYO';`,
   `      // Second when`,
   `        MyVariable2 = 'YOYOYO';`,
   `  Endsl;`,
   `Endif;`,
   `Return;`
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
  const { indentErrors } = Linter.getErrors({ uri, content: lines }, {
   indent: 2
  }, cache);

  expect(indentErrors.length).toBe(2);

  expect(indentErrors[0].line).toBe(10);
  expect(indentErrors[0].currentIndent).toBe(0);
  expect(indentErrors[0].expectedIndent).toBe(2);

  expect(indentErrors[1].line).toBe(17);
  expect(indentErrors[1].currentIndent).toBe(8);
  expect(indentErrors[1].expectedIndent).toBe(6);
});

test('linter2_indent_other', async () => {
  const lines = [
    `**FREE`,
    ``,
    `Ctl-Opt DFTACTGRP(*No);`,
    ``,
    `Dcl-s MyVariable2 Char(20);`,
    ``,
    `myVariable2 = *blank;`,
    ``,
    `If myVariable2 = *blank;`,
    `  // Inside if`,
    `MyVariable2 = 'Hello world';`,
    `  Select;`,
    `    When myVariable2 = *blank;`,
    `      // First when`,
    `      MyVariable2 = 'Still blank?';`,
    `    When myVariable2 = 'YOYOYO';`,
    `      // Second when`,
    `        MyVariable2 = 'YOYOYO';`,
    `    Other;`,
    `      // Second when`,
    `      MyVariable2 = 'YOYOYO';`,
    `  Endsl;`,
    `Endif;`,
    `Return;`
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
  const { indentErrors } = Linter.getErrors({ uri, content: lines }, {
    indent: 2
  }, cache);

  expect(indentErrors.length).to.equal(2);
  expect(indentErrors[0].line).to.equal(10);
  expect(indentErrors[0].currentIndent).to.equal(0);
  expect(indentErrors[0].expectedIndent).to.equal(2);
  expect(indentErrors[1].line).to.equal(17);
  expect(indentErrors[1].currentIndent).to.equal(8);
  expect(indentErrors[1].expectedIndent).to.equal(6);
});

test('linter3_indent', async () => {
  const lines = [
    `**FREE`,
    ``,
    `Ctl-Opt DFTACTGRP(*No);`,
    ``,
    `Dcl-s MyVariable2 Char(20);`,
    ``,
    `myVariable2 = *blank;`,
    ``,
    `If myVariable2 = *blank;`,
    `MyVariable2 = 'Hello world';`,
    `  Select;`,
    `    When myVariable2 = *blank;`,
    `      MyVariable2 = 'Still blank?';`,
    `    When myVariable2 = 'YOYOYO';`,
    `        MyVariable2 = 'YOYOYO';`,
    `  Endsl;`,
    `Endif;`,
    ``,
    `  MyVariable2 = 'YOYOYO';`,
    `Return;`
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
  const { indentErrors } = Linter.getErrors({ uri, content: lines }, {
    indent: 2
  }, cache);

  expect(indentErrors.length).to.equal(3);
  expect(indentErrors[0].line).to.equal(9);
  expect(indentErrors[0].currentIndent).to.equal(0);
  expect(indentErrors[0].expectedIndent).to.equal(2);
  expect(indentErrors[1].line).to.equal(14);
  expect(indentErrors[1].currentIndent).to.equal(8);
  expect(indentErrors[1].expectedIndent).to.equal(6);
  expect(indentErrors[2].line).to.equal(18);
  expect(indentErrors[2].currentIndent).to.equal(2);
  expect(indentErrors[2].expectedIndent).to.equal(0);
});

test('linter4', async () => {
  const lines = [
    `**FREE`,
    ``,
    `Ctl-Opt DFTACTGRP(*No);`,
    ``,
    `Dcl-s MyVariable2 Char(20);`,
    ``,
    `myVariable2 = '';`,
    ``,
    `If myVariable2 = '';`,
    `  MyVariable2 = 'Hello world';`,
    `Endif;`,
    `Return;`
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    RequireBlankSpecial: true
  }, cache);

  expect(errors.length).to.equal(2);
  expect(errors[0].type).to.equal('RequireBlankSpecial');
  expect(errors[0].offset.start).to.equal(76);
  expect(errors[0].offset.end).to.equal(78);
  expect(errors[0].newValue).to.equal('*BLANK');
  expect(errors[1].type).to.equal('RequireBlankSpecial');
  expect(errors[1].offset.start).to.equal(98);
  expect(errors[1].offset.end).to.equal(100);
  expect(errors[1].newValue).to.equal('*BLANK');
});

test('linter5', async () => {
  const lines = [
    `**FREE`,
    ``,
    `Ctl-Opt DFTACTGRP(*No);`,
    ``,
    `Dcl-s MyVariable2 Char(20);`,
    ``,
    `myVariable2 = '';`,
    ``,
    `If MyVariable2 = '';`,
    `  If *on = *on;`,
    `    Myvariable2 = 'Hello world';`,
    `  Endif;`,
    `Endif;`,
    `Return;`
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    IncorrectVariableCase: true
  }, cache);

  expect(errors.length).to.equal(2);
  expect(errors[0].type).to.equal('IncorrectVariableCase');
  expect(errors[0].offset.start).to.equal(62);
  expect(errors[0].offset.end).to.equal(73);
  expect(errors[0].newValue).to.equal('MyVariable2');
  expect(errors[1].type).to.equal('IncorrectVariableCase');
  expect(errors[1].offset.start).to.equal(122);
  expect(errors[1].offset.end).to.equal(133);
  expect(errors[1].newValue).to.equal('MyVariable2');
});

test('linter6', async () => {
  const lines = [
    `**FREE`,
    ``,
    `Ctl-Opt DFTACTGRP(*No);`,
    ``,
    `Dcl-s MyVariable2 Char(20);`,
    ``,
    `myVariable2 = *blank;`,
    ``,
    `If myVariable2 = *blank;`,
    `MyVariable2 = 'Hello world';`,
    `  Select;`,
    `    When myVariable2 = *blank;`,
    `      MyVariable2 = 'Still blank?';`,
    `    When myVariable2 = 'YOYOYO';`,
    `        MyVariable2 = 'YOYOYO';`,
    `  Endsl;`,
    `Endif;`,
    `Return;`
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    StringLiteralDupe: true
  }, cache);

  expect(errors.length).to.equal(2);
  expect(errors[0].type).to.equal('StringLiteralDupe');
  expect(errors[0].offset.start).to.equal(239);
  expect(errors[0].offset.end).to.equal(247);
  expect(errors[1].type).to.equal('StringLiteralDupe');
  expect(errors[1].offset.start).to.equal(271);
  expect(errors[1].offset.end).to.equal(279);
});

test('linter6_lf', async () => {
  const lines = [
    `**FREE`,
    ``,
    `Dcl-S Myotherthing char(10);`,
    ``,
    `dsply 'hello friends'`,
    `      + 'hello friends' + ''`,
    `    + myotherthing;`,
    `    `
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    StringLiteralDupe: true,
    IncorrectVariableCase: true
  }, cache);

  expect(errors.length).to.equal(3);

  expect(errors[0]).toMatchObject({
    offset: { start: 95, end: 107 },
    type: 'IncorrectVariableCase',
    newValue: 'Myotherthing'
  });

  expect(errors[1]).toMatchObject({
    offset: { start: 44, end: 59 },
    type: 'StringLiteralDupe',
    newValue: undefined
  });

  expect(errors[2]).toMatchObject({
    offset: { start: 68, end: 83 },
    type: 'StringLiteralDupe',
    newValue: undefined
  });
});

test('linter6_crlf', async () => {
  const lines = [
    `**FREE`,
    ``,
    `Dcl-S Myotherthing char(10);`,
    ``,
    `dsply 'hello friends'`,
    `      + 'hello friends' + ''`,
    `    + myotherthing;`,
    `    `
  ].join(`\r\n`);

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    StringLiteralDupe: true,
    IncorrectVariableCase: true
  }, cache);

  expect(errors.length).to.equal(3);

  expect(errors[0]).toMatchObject({
    offset: { start: 101, end: 113 },
    type: 'IncorrectVariableCase',
    newValue: 'Myotherthing'
  });

  expect(errors[1]).toMatchObject({
    offset: { start: 48, end: 63 },
    type: 'StringLiteralDupe',
    newValue: undefined
  });

  expect(errors[2]).toMatchObject({
    offset: { start: 73, end: 88 },
    type: 'StringLiteralDupe',
    newValue: undefined
  });
});

test('linter7_casing1', async () => {
  const lines = [
    `**FREE`,
    ``,
    `Ctl-Opt DFTACTGRP(*No);`,
    ``,
    `Dcl-s MyVariable2 Char(20);`,
    ``,
    `myVariable2 = *blank;`,
    ``,
    `If myVariable2 = *blank;`,
    `MyVariable2 = 'Hello world';`,
    `  Select;`,
    `    When myVariable2 = *blank;`,
    `      MyVariable2 = 'Still blank?';`,
    `    When myVariable2 = 'YOYOYO';`,
    `        MyVariable2 = 'YOYOYO';`,
    `  Endsl;`,
    `Endif;`,
    `Return;`
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    SpecificCasing: [
      { operation: 'if', expected: 'If' },
      { operation: 'endif', expected: 'Endif' },
      { operation: 'select', expected: 'SELECT' },
    ]
  }, cache);

  expect(errors.length).to.equal(1);

  expect(errors[0]).toMatchObject({
    offset: { start: 141, end: 147 },
    type: 'SpecificCasing',
    newValue: 'SELECT'
  });
});

test("linter7_casing2", async () => {
  const lines = [
    `**FREE`,
    ``,
    `Ctl-Opt DFTACTGRP(*No);`,
    ``,
    `Dcl-s MyVariable2 Char(20);`,
    ``,
    `myVariable2 = *blank;`,
    ``,
    `If myVariable2 = *blank;`,
    `MyVariable2 = 'Hello world';`,
    `  Select;`,
    `    When myVariable2 = *blank;`,
    `      MyVariable2 = 'Still blank?';`,
    `    When myVariable2 = 'YOYOYO';`,
    `        MyVariable2 = 'YOYOYO';`,
    `  Endsl;`,
    `Endif;`,
    `Return;`
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    SpecificCasing: [
      { operation: `ctl-opt`, expected: `Ctl-OPT` },
    ]
  }, cache);

  expect(errors.length).toBe(1);
  expect(errors[0]).toMatchObject({
    offset: { start: 8, end: 15 },
    type: `SpecificCasing`,
    newValue: `Ctl-OPT`
  });
});

test("linter7_casing3", async () => {
  const lines = [
    `**FREE`,
    ``,
    `Ctl-Opt DFTACTGRP(*No);`,
    ``,
    `Dcl-s MyVariable2 Char(20);`,
    ``,
    `myVariable2 = *blank;`,
    ``,
    `If myVariable2 = *blank;`,
    `MyVariable2 = 'Hello world';`,
    `  Select;`,
    `    When myVariable2 = *blank;`,
    `      MyVariable2 = 'Still blank?';`,
    `    When myVariable2 = 'YOYOYO';`,
    `        MyVariable2 = 'YOYOYO';`,
    `  Endsl;`,
    `Endif;`,
    `Return;`
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    SpecificCasing: [
      { operation: `dcl-s`, expected: `DCL-S` },
    ]
  }, cache);

  expect(errors.length).toBe(1);
  expect(errors[0]).toMatchObject({
    offset: { start: 33, end: 38 },
    type: `SpecificCasing`,
    newValue: `DCL-S`
  });
});

test("linter7_casing4", async () => {
  const lines = [
    `**FREE`,
    ``,
    `Ctl-Opt DFTACTGRP(*No);`,
    ``,
    `Dcl-S MyVariable1 Varchar(20);`,
    `Dcl-S MyVariable2 Char(20);`,
    ``,
    `myVariable2 = 'Hello world';`,
    ``,
    `If myVariable1 = *blank;`,
    `  MyVariable1 = %Trim(myVariable2);`,
    `Endif;`,
    `Return;`
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    SpecificCasing: [
      { operation: `%trim`, expected: `%trim` },
    ]
  }, cache);

  expect(errors.length).toBe(1);
  expect(errors[0]).toMatchObject({
    offset: { start: 164, end: 169 },
    type: `SpecificCasing`,
    newValue: `%trim`
  });
});

test("linter7_casing5", async () => {
  const lines = [
    `**FREE`,
    ``,
    `Ctl-Opt DFTACTGRP(*No);`,
    ``,
    `Dcl-S MyVariable2 Char(20);`,
    ``,
    `myVariable2 = *blank;`,
    ``,
    `If myVariable2 = *blank;`,
    `MyVariable2 = 'Hello world';`,
    `  Select;`,
    `    When myVariable2 = *blank;`,
    `      MyVariable2 = 'Still blank?';`,
    `    When myVariable2 = 'YOYOYO';`,
    `        MyVariable2 = 'YOYOYO';`,
    `  Endsl;`,
    `Endif;`,
    `Return;`
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    SpecificCasing: [
      { operation: `dcl-s`, expected: `Dcl-S` },
      { operation: `ctl-opt`, expected: `Ctl-Opt` },
    ]
  }, cache);

  expect(errors.length).toBe(0);
});

test("linter7_casing6", async () => {
  const lines = [
    `**FREE`,
    ``,
    `Ctl-Opt DFTACTGRP(*No);`,
    ``,
    `Dcl-S MyVariable1 Varchar(20);`,
    `Dcl-S MyVariable2 Char(20);`,
    ``,
    `myVariable2 = 'Hello world';`,
    ``,
    `If myVariable1 = *blank;`,
    `  MyVariable1 = %Trim(myVariable2);`,
    `Endif;`,
    `Return;`
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    SpecificCasing: [
      { operation: `*declare`, expected: `*upper` },
    ]
  }, cache);

  expect(errors.length).toBe(3);
  expect(errors[0].newValue).toBe(`CTL-OPT`);
  expect(errors[1].newValue).toBe(`DCL-S`);
  expect(errors[2].newValue).toBe(`DCL-S`);
});

test("linter7_casing7", async () => {
  const lines = [
    `**FREE`,
    ``,
    `Ctl-Opt DFTACTGRP(*No);`,
    ``,
    `Dcl-S MyVariable1 Varchar(20);`,
    `Dcl-S MyVariable2 Char(20);`,
    ``,
    `myVariable2 = 'Hello world';`,
    ``,
    `If myVariable1 = *blank;`,
    `  MyVariable1 = %Trim(myVariable2);`,
    `Endif;`,
    `Return;`
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    SpecificCasing: [
      { operation: `ctl-opt`, expected: `Ctl-Opt` },
      { operation: `*declare`, expected: `*upper` },
    ]
  }, cache);

  expect(errors.length).toBe(2);
  expect(errors[0].newValue).toBe(`DCL-S`);
  expect(errors[1].newValue).toBe(`DCL-S`);
});

test("linter7_casing8", async () => {
  const lines = [
    `**FREE`,
    ``,
    `Ctl-Opt DFTACTGRP(*No);`,
    ``,
    `Dcl-S MyVariable1 Varchar(20);`,
    `Dcl-S MyVariable2 Char(20);`,
    ``,
    `myVariable2 = 'Hello world';`,
    ``,
    `If myVariable1 = *blank;`,
    `  MyVariable1 = %Trim(myVariable2);`,
    `Endif;`,
    `Return;`
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    SpecificCasing: [
      { operation: `ctl-opt`, expected: `Ctl-Opt` },
      { operation: `*declare`, expected: `*LOWER` },
    ]
  }, cache);

  expect(errors.length).toBe(2);
  expect(errors[0].newValue).toBe(`dcl-s`);
  expect(errors[1].newValue).toBe(`dcl-s`);
});

test("linter7_casing9", async () => {
  const lines = [
    `**FREE`,
    ``,
    `Ctl-Opt DFTACTGRP(*No);`,
    ``,
    `Dcl-S MyVariable1 Varchar(20);`,
    `Dcl-S MyVariable2 Char(20);`,
    ``,
    `myVariable2 = 'Hello world';`,
    ``,
    `If myVariable1 = *blank;`,
    `  MyVariable1 = %Trim(myVariable2);`,
    `Endif;`,
    `Return;`
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    SpecificCasing: [
      { operation: `*bif`, expected: `*lower` },
    ]
  }, cache);

  expect(errors.length).toBe(1);
  expect(errors[0].newValue).toBe(`%trim`);
});

test('linter7_casing10', async () => {
  const lines = [
    `**free`,
    ``,
    `dcl-s sFirstName char(10);`,
    `dcl-s sEmpNo char(6);`,
    ``,
    `Exec Sql`,
    `  // Break The Parser`,
    `  Select`,
    `    empno`,
    `  Into`,
    `    :sEmpNo`,
    `  From`,
    `    sample.employee`,
    `  Where`,
    `    firstnme = :SFIRSTNAME;`,
    ``,
    `return;`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    IncorrectVariableCase: true
  }, cache);

  expect(errors.length).to.equal(1);
  expect(errors[0].offset.start).to.equal(178);
  expect(errors[0].offset.end).to.equal(188);
  expect(errors[0].newValue).to.equal('sFirstName');
});

test('linter7_casing11', async () => {
  const lines = [
    `**free`,
    ``,
    `dcl-s sFirstName char(10);`,
    `dcl-s sEmpNo char(6);`,
    ``,
    `Exec Sql`,
    `  // Break The Parser`,
    `  Select`,
    `    sempno`,
    `  Into`,
    `    :sempno`,
    `  From`,
    `    sample.employee`,
    `  Where`,
    `    firstnme = :SFIRSTNAME;`,
    ``,
    `return;`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    IncorrectVariableCase: true
  }, cache);

  expect(errors.length).to.equal(2);

  expect(errors[0]).toMatchObject({
    offset: { start: 121, end: 127 },
    type: 'IncorrectVariableCase',
    newValue: 'sEmpNo'
  });

  expect(errors[1]).toMatchObject({
    offset: { start: 179, end: 189 },
    type: 'IncorrectVariableCase',
    newValue: 'sFirstName'
  });
});

test('linter7_casing12', async () => {
  const lines = [
    `**free`,
    ``,
    `dcl-s NULL pointer inz(*NULL);`,
    `dcl-s amount1 packed(7:2);`,
    `dcl-s amount2 packed(7:2);`,
    `dcl-s amount3 packed(7:2);`,
    `dcl-s amount4 packed(7:2);`,
    `dcl-s amount5 packed(7:2);`,
    ``,
    `// Watch null move left`,
    `Exec Sql`,
    `  select`,
    `    max(case when bonus < 900 then bonus else null end),`,
    ``,
    `    max(case when bonus < 800 then bonus else null end),`,
    ``,
    `    max(case when bonus < 700 then bonus else null end),`,
    ``,
    `    max(case when bonus < 600 then bonus else null end),`,
    ``,
    `    max(case when bonus < 500 then bonus else null end)`,
    `  into`,
    `    :amount1,:amount2,:amount3,:amount4,:amount5`,
    `  from`,
    `    sample.employee;`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    IncorrectVariableCase: true
  }, cache);

  expect(errors.length).to.equal(0);
});

test('linter8', async () => {
  const lines = [
    `**FREE`,
    `ctl-opt debug nomain option(*nodebugio: *srcstmt) ;`,
    `dcl-proc BASE36ADD export ;`,
    `  dcl-pi BASE36ADD varchar(50);`,
    `    PI_Value varchar(50) const; // Input value`,
    `  end-pi BASE36ADD;`,
    `  dcl-s a char(1);`,
    `  if a ='/';`,
    `    a=' ';`,
    `    a= BASE36ADD;`,
    `  endif;`,
    `  return a;`,
    `end-proc BASE36ADD;`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    RequiresParameter: true
  }, cache);

  expect(errors.length).to.equal(1);
  expect(errors[0]).toMatchObject({
    offset: { start: 236, end: 245 },
    type: 'RequiresParameter'
  });
});

test('linter_Do_Not_Require_Parameters_For_Control_Options', async () => {
  const lines = [
    `**FREE`,
    `ctl-opt main(main) ;`,
    `dcl-proc main ;`,
    `  return ;`,
    `end-proc main ;`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    RequiresParameter: true
  }, cache);

  expect(errors.length).to.equal(0);
});

test('linter_Do_Not_Require_Parameters_For_Compile_Directives', async () => {
  const lines = [
    `**FREE`,
    `/if defined(MYPROCEDURE);`,
    `/eof;`,
    `/endif;`,
    `/define ;`,
    `dcl-pr MYPROCEDURE;`,
    `end-pr;`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    RequiresParameter: true
  }, cache);

  expect(errors.length).to.equal(0);
});

test('linter9', async () => {
  const lines = [
    `**FREE`,
    ``,
    `Ctl-Opt DFTACTGRP(*No);`,
    ``,
    `Dcl-s MyVariable2 Char(20);`,
    ``,
    `Dcl-C theConstant 'Hello world';`,
    ``,
    `Dcl-Proc theProcedure;`,
    `  Dcl-Pi *N;`,
    `    newValue Char(20);`,
    `  End-Pi;`,
    `  Dcl-S localVar Char(20);`,
    `  localvar = newValue;`,
    `  Myvariable2 = localvar;`,
    `End-Proc;`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    IncorrectVariableCase: true
  }, cache);

  expect(cache.variables.length).to.equal(1);
  expect(cache.constants.length).to.equal(1);
  expect(cache.procedures.length).to.equal(1);
  expect(cache.procedures[0].subItems.length).to.equal(1);

  expect(errors.length).to.equal(3);

  expect(errors[0]).toMatchObject({
    offset: { start: 194, end: 202 },
    type: 'IncorrectVariableCase',
    newValue: 'localVar'
  });

  expect(errors[1]).toMatchObject({
    offset: { start: 217, end: 228 },
    type: 'IncorrectVariableCase',
    newValue: 'MyVariable2'
  });

  expect(errors[2]).toMatchObject({
    offset: { start: 231, end: 239 },
    type: 'IncorrectVariableCase',
    newValue: 'localVar'
  });
});

test('linter10', async () => {
  const lines = [
    `**FREE`,
    `ctl-opt debug option(*nodebugio: *srcstmt);`,
    `dcl-ds mything DIM(8) PERRCD(3) CTDATA;`,
    `end-ds;`,
    ``,
    `Dcl-s MyVariable2 Char(20);`,
    ``,
    `myVariable2 = *blank;`,
    ``,
    `If myVariable2 = *blank;`,
    `MyVariable2 = 'Hello world';`,
    `Endif;`,
    `Return;`,
    ``,
    `**CTDATA ARC`,
    `Toronto        12:15:00Winnipeg       13:23:00Calgary        15:44:00`,
    `Sydney         17:24:30Edmonton       21:33:00Saskatoon      08:40:00`,
    `Regina         12:33:00Vancouver      13:20:00`
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    NoCTDATA: true
  }, cache);

  expect(errors.length).to.equal(2);

  expect(errors[0]).toMatchObject({
    type: 'NoCTDATA',
    offset: { start: 51, end: 89 }
  });

  expect(errors[1]).toMatchObject({
    offset: { start: 222, end: 230 },
    type: 'NoCTDATA'
  });
});

test('linter11', async () => {
  const lines = [
    `**FREE`,
    ``,
    `Dcl-c HELLO 'hello friends';`,
    `Dcl-S Myotherthing char(10);`,
    ``,
    `dsply 'hello friends'`,
    `      + 'hello friends' + ''`,
    `    + 'oioi';`,
    `    `
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    StringLiteralDupe: true
  }, cache);

  expect(errors.length).to.equal(2);

  expect(errors[0]).toMatchObject({
    offset: { start: 73, end: 88 },
    type: 'StringLiteralDupe',
    newValue: 'HELLO'
  });

  expect(errors[1]).toMatchObject({
    offset: { start: 97, end: 112 },
    type: 'StringLiteralDupe',
    newValue: 'HELLO'
  });
});

test('linter12', async () => {
  const lines = [
    `**free`,
    `If 2 = 2;`,
    `  localvar = 'Hello' + `,
    `    'World';`,
    `Else;`,
    `  buyralg_setbu_aprordr(buyralg_getbu_aprordr + `,
    `    arrayapr(wkind));  // 10-26-2016`,
    `Endif;`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
  const { indentErrors } = Linter.getErrors({ uri, content: lines }, {
    indent: 2
  }, cache);

  expect(indentErrors.length).to.equal(0);
});

test('linter13', async () => {
  const lines = [
    `**FREE`,
    ``,
    `Ctl-Opt DFTACTGRP(*No);`,
    ``,
    `Dcl-s MyVariable2 Char(20);`,
    ``,
    `// my constant`,
    `// second line`,
    `Dcl-C theConstant 'Hello world';`,
    `  // comment with bad indent`,
    ``,
    `Dcl-Proc theProcedure;`,
    `  Dcl-Pi *N;`,
    `    newValue Char(20);`,
    `  End-Pi;`,
    `// comment with wrong indent`,
    `  Dcl-S localVar Char(20);`,
    `  localvar = newValue;`,
    `  // but valid indent`,
    `  // with another line`,
    `      // too many spaces`,
    `  Myvariable2 = localvar;`,
    `End-Proc;`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
  const { indentErrors } = Linter.getErrors({ uri, content: lines }, {
    indent: 2
  }, cache);

  expect(indentErrors.length).to.equal(0);
});

test('linter13_commentIndent', async () => {
  const lines = [
    `**FREE`,
    ``,
    `Ctl-Opt DFTACTGRP(*No);`,
    ``,
    `Dcl-s MyVariable2 Char(20);`,
    ``,
    `// my constant`,
    `// second line`,
    `Dcl-C theConstant 'Hello world';`,
    `  // comment with bad indent`,
    ``,
    `Dcl-Proc theProcedure;`,
    `  Dcl-Pi *N;`,
    `    newValue Char(20);`,
    `  End-Pi;`,
    `// comment with wrong indent`,
    `  Dcl-S localVar Char(20);`,
    `  localvar = newValue;`,
    `  // but valid indent`,
    `  // with another line`,
    `      // too many spaces`,
    `  Myvariable2 = localvar;`,
    `End-Proc;`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
  const { indentErrors } = Linter.getErrors({ uri, content: lines }, {
    PrettyComments: true,
    indent: 2
  }, cache);

  expect(indentErrors.length).to.equal(3);

  expect(indentErrors[0]).toMatchObject({
    currentIndent: 2,
    expectedIndent: 0,
    line: 9
  });

  expect(indentErrors[1]).toMatchObject({
    currentIndent: 0,
    expectedIndent: 2,
    line: 15
  });

  expect(indentErrors[2]).toMatchObject({
    currentIndent: 6,
    expectedIndent: 2,
    line: 20
  });
});

test('linter14', async () => {
  const lines = [
    `**free`,
    `If 2 = 2;`,
    `  localvar = 'Hello' + `,
    `    'World';`,
    `Else;`,
    `  buyralg_setbu_aprordr(buyralg_getbu_aprordr + `,
    `    arrayapr(wkind));  // 10-26-2016`,
    `Endif;`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
  const { indentErrors } = Linter.getErrors({ uri, content: lines }, {
    indent: 2
  }, cache);

  expect(indentErrors.length).to.equal(0);
});

test('linter15', async () => {
  const lines = [
    `**FREE`,
    ``,
    `Ctl-Opt DFTACTGRP(*No);`,
    ``,
    `//`,
    `//Append a single quote. This procedure exists to make other code more readable.`,
    `//`,
    ``,
    `// my variable`,
    `Dcl-S MyVariable2 Char(20);`,
    ``,
    `MyVariable2 = 'Hello world';`,
    ``,
    `If 2 = 2;`,
    `  //Change thw value of my variable`,
    `  MyVariable2 = 'Hello friends';`,
    `  //`,
    `Endif;`,
    ``,
    `Dsply MyVariable2;`,
    ``,
    `     // Append a single quote. This procedure exists to make other code more readable.`,
    ``,
    `return;`
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    PrettyComments: true
  }, cache);

  expect(errors.length).to.equal(2);

  expect(errors[0]).toMatchObject({
    offset: { start: 36, end: 38 },
    type: 'PrettyComments',
    newValue: '// '
  });

  expect(errors[1]).toMatchObject({
    offset: { start: 207, end: 209 },
    type: 'PrettyComments',
    newValue: '// '
  });
});

/**
 * Subroutine check test
 */
test('linter16', async () => {
  const lines = [
    `**FREE`,
    `Dcl-s MyVariable2 Char(20);`,
    ``,
    `Exsr theSubroutine;`,
    `Dsply MyVariable2;`,
    ``,
    `Begsr theSubroutine;`,
    `  MyVariable2 = 'Hello world';`,
    `Endsr;`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    NoGlobalSubroutines: true
  }, cache);

  expect(errors.length).to.equal(3);
  expect(errors[0]).toMatchObject({
    type: `NoGlobalSubroutines`,
    offset: { start: 36, end: 54 }
  });
  expect(errors[1]).toMatchObject({
    offset: { start: 76, end: 81 },
    type: `NoGlobalSubroutines`
  });
  expect(errors[2]).toMatchObject({
    offset: { start: 128, end: 133 },
    type: `NoGlobalSubroutines`
  });
});

test('linter16_with_leavesr', async () => {
  const lines = [
    `**FREE`,
    `Dcl-s SomeNum int(5);`,
    `Dcl-s MyVariable2 Char(20);`,
    ``,
    `SomeNum = 5;`,
    `Exsr theSubroutine;`,
    `Dsply MyVariable2;`,
    ``,
    `Begsr theSubroutine;`,
    `  If (SomeNum = 5);`,
    `    Leavesr;`,
    `  Endif;`,
    `  MyVariable2 = 'Hello world';`,
    `Endsr;`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    NoGlobalSubroutines: true
  }, cache);

  expect(errors.length).to.equal(4);
  expect(errors[0]).toMatchObject({
    type: `NoGlobalSubroutines`,
    offset: { start: 71, end: 89 }
  });
  expect(errors[1]).toMatchObject({
    offset: { start: 111, end: 116 },
    type: `NoGlobalSubroutines`
  });
  expect(errors[2]).toMatchObject({
    type: `NoGlobalSubroutines`,
    offset: { start: 156, end: 163 }
  });
  expect(errors[3]).toMatchObject({
    offset: { start: 205, end: 210 },
    type: `NoGlobalSubroutines`
  });
});

/**
 * Subroutine in procedure check test
 */
test('linter17', async () => {
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
    `  Endsr;`,
    `End-Proc;`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    NoLocalSubroutines: true
  }, cache);

  expect(errors.length).to.equal(1);
  expect(errors[0]).toMatchObject({
    type: `NoLocalSubroutines`, offset: { start: 119, end: 138 }
  });
});

test('linter18', async () => {
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
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    NoGlobalsInProcedures: true
  }, cache);

  expect(errors.length).to.equal(2);
  expect(errors[0]).toMatchObject({
    offset: { start: 123, end: 134 },
    type: `NoGlobalsInProcedures`
  });
  expect(errors[1]).toMatchObject({
    offset: { start: 164, end: 175 },
    type: `NoGlobalsInProcedures`
  });
});

test('linter19', async () => {
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
    `localVarYes = 'Y';`,
    `procYes();`,
    ``,
    `subfa = 'Yes!';`,
    `structYesAlso = 'Really yes';`,
    ``,
    `qualStructYes.qualsubA = 5;`,
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
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    NoUnreferenced: true
  }, cache);

  expect(errors.length).to.equal(11);
  expect(errors[0]).toMatchObject({
    type: `NoUnreferenced`, offset: { start: 66, end: 86 }
  });
  expect(errors[1]).toMatchObject({
    type: `NoUnreferenced`, offset: { start: 1089, end: 1104 } 
  });
  expect(errors[2]).toMatchObject({
    type: `NoUnreferenced`, offset: { start: 160, end: 176 } 
  });
  expect(errors[3]).toMatchObject({
    type: `NoUnreferenced`, offset: { start: 139, end: 154 }
  });
  expect(errors[4]).toMatchObject({
    type: `NoUnreferenced`, offset: { start: 337, end: 354 }
  });
  expect(errors[5]).toMatchObject({
    type: `NoUnreferenced`, offset: { start: 302, end: 331 }
  });
  expect(errors[6]).toMatchObject({
    type: `NoUnreferenced`, offset: { start: 539, end: 566 }
  });
  expect(errors[7]).toMatchObject({
    type: `NoUnreferenced`, offset: { start: 735, end: 749 }
  });
  expect(errors[8]).toMatchObject({
    type: `NoUnreferenced`, offset: { start: 705, end: 725 }
  });
  expect(errors[9]).toMatchObject({
    type: `NoUnreferenced`, offset: { start: 893, end: 910 }
  });
  expect(errors[10]).toMatchObject({
    type: `NoUnreferenced`, offset: { start: 849, end: 883 }
  });
});

test('linter20', async () => {
  const lines = [
    `**FREE`,
    ``,
    `Dcl-s MyVariable2 Char(20);  `,
    ``,
    `EXEC SQL`,
    `    FETCH NEXT FROM empCur       `,
    `    INTO :myvariable2;`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    IncorrectVariableCase: true
  }, cache);

  expect(errors.length).to.equal(1);
  expect(errors[0].type).to.equal(`IncorrectVariableCase`);
  expect(errors[0].offset.start).to.equal(92);
  expect(errors[0].offset.end).to.equal(103);
  expect(errors[0].newValue).to.equal(`MyVariable2`);
});

test('linter21', async () => {
  const lines = [
    `**FREE`,
    ``,
    `Ctl-Opt DFTACTGRP(*No);`,
    ``,
    `Dcl-s MyVariable2 Char(20);  `,
    ``,
    `Yesproc(MyVariable2);`,
    `AlsoYesProc();`,
    `Return;`,
    ``,
    `Dcl-Proc Yesproc;`,
    `  Dcl-Pi *N;`,
    `    parmA Char(20);`,
    `  End-Pi;`,
    ``,
    `  parmA = 'Goodbye world';`,
    `End-Proc;`,
    ``,
    `Dcl-Proc AlsoYesProc;`,
    `  Dcl-Pi *n Int(10);`,
    `    parmB Int(10);`,
    `  End-pi;`,
    ``,
    `  return 2 * 2;`,
    `End-Proc;`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true, collectReferences: true});
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    NoUnreferenced: true
  }, cache);

  expect(errors.length).to.equal(1);
  expect(errors[0]).toMatchObject({
    type: `NoUnreferenced`, offset: { start: 257, end: 270 }
  });
});

test('linter22', async () => {
  const lines = [
    `**FREE`,
    ``,
    `Dcl-Pr TheProcedure;`,
    `  parmA CHAR(20);`,
    `End-Pr`,
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

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    PrototypeCheck: true
  }, cache);

  expect(errors.length).to.equal(1);
  expect(errors[0]).toMatchObject({
    type: `PrototypeCheck`, offset: { start: 8, end: 27 }
  });
});

test('linter22_b', async () => {
  const lines = [
    `**FREE`,
    ``,
    `Dcl-Pr TheProcedure EXTPROC;`,
    `  parmA CHAR(20);`,
    `End-Pr`,
    ``,
    `Dcl-S theVar CHAR(20);`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    PrototypeCheck: true
  }, cache);

  expect(errors.length).to.equal(0);
});

test("linter23", async () => {
  const lines = [
    `**free`,
    ``,
    `Dcl-Proc SQL_ToUpper Export;`,
    `  Dcl-Pi *n char(256);`,
    `    stringIn char(20);`,
    `  end-Pi;`,
    ``,
    `  EXEC SQL SET :stringIn = UPPER(:stringIn);`,
    `End-Proc;`,
    ``,
    `Dcl-Proc SQL_ToLower Export;`,
    `  Dcl-Pi *n char(256);`,
    `    stringIn char(20);`,
    `  end-Pi;`,
    ``,
    `  EXEC SQL SET :stringIn = LOWER(:stringIn);`,
    `End-Proc;`
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true, collectReferences: true});
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    NoUnreferenced: true
  }, cache);

  expect(errors.length).toBe(0);
});

test("linter24", async () => {
  const lines = [
    `**FREE`,
    ``,
    `Dcl-Pi AUTH;`,
    `  pUserID   Char(10);`,
    `  pPassword Char(32);`,
    `  Result    Char(1);`,
    `End-Pi;`,
    ``,
    `Dcl-PR GetProfile  ExtPgm('QSYGETPH');`,
    `  UserID         Char(10)   const;`,
    `  Password       Char(32767) const options(*varsize);`,
    `  Handle         Char(12);`,
    `  ErrorCode      Char(256)  Options(*Varsize : *NoPass);`,
    `  PswLength      Int(10)    const Options(*NoPass);`,
    `  CCSIDCode      Int(10)    const Options(*NoPass);`,
    `End-PR;`,
    ``,
    `Dcl-Pr CloseProfile ExtPgm('QSYRLSPH');`,
    `  Handle         Char(12);`,
    `End-Pr;`,
    ``,
    `Dcl-S ResultHandle Char(12);`,
    ``,
    `Dcl-S errorOut Char(256);`,
    `Dcl-S pwLength Int(3);`,
    ``,
    `pwLength = %Len(%Trim(pPassword));`,
    ``,
    `//pPassword = %Trim(pPassword);`,
    `ResultHandle = '';`,
    `Result = *Off;`,
    ``,
    `GetProfile(pUserID:pPassword:ResultHandle:errorOut:pwLength:37);`,
    ``,
    `//Indicates is incorrect`,
    `If ResultHandle <> x'000000000000000000000000';`,
    `  Result = *On;`,
    `  //We don't want to keep handles open`,
    `  `,
    `  CloseProfile(ResultHandle);`,
    `Endif;`,
    ``,
    `*InLR = *On;`,
    `Return;`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    NoExternalTo: [
      `QSYGETPH`
    ]
  }, cache);

  expect(errors.length).toBe(1);
  expect(errors[0]).toMatchObject({
    type: `NoExternalTo`, offset: { start: 95, end: 132 }
  });

  expect(lines.substring(errors[0].offset.start, errors[0].offset.end)).toBe(`Dcl-PR GetProfile  ExtPgm('QSYGETPH')`);
});

test("linter25", async () => {
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

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    NoExternalTo: [
      `QSYGETPH`,
      `QSYRLSPH`
    ]
  }, cache);

  expect(errors.length).toBe(2);

  expect(errors[0]).toMatchObject({
    type: `NoExternalTo`, offset: { start: 163, end: 200 }
  });

  expect(lines.substring(errors[0].offset.start, errors[0].offset.end)).toBe(`Dcl-PR GetProfile  ExtPgm('QSYGETPH')`);

  expect(errors[1]).toMatchObject({
    type: `NoExternalTo`, offset: { start: 506, end: 544 }
  });

  expect(lines.substring(errors[1].offset.start, errors[1].offset.end)).toBe(`Dcl-Pr CloseProfile ExtPgm('QSYRLSPH')`);
});

test("linter26", async () => {
  const lines = [
    `**FREE`,
    ``,
    `Dcl-Pr CloseProfile ExtPgm('QSYRLSPH');`,
    `  Handle         Char(12);`,
    `End-Pr;`,
    ``,
    `Dcl-S ResultHandle Char(12);`,
    ``,
    `CloseProfile(ResultHandle);`,
    ``,
    `*InLR = *On;`,
    `Return;`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true, collectReferences: true});
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    NoUnreferenced: true
  }, cache);

  expect(errors.length).toBe(0);
});

test("linter27", async () => {
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

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    NoExecuteImmediate: true
  }, cache);

  expect(errors.length).toBe(1);
  expect(errors[0]).toMatchObject({
    type: `NoExecuteImmediate`, offset: { start: 105, end: 148 }
  });
});

test("linter28", async () => {
  const lines = [
    `**free`,
    `Dcl-Pr APGM extpgm(myvarNotused);`,
    `End-Pr;`,
    `Dcl-Pr OTHERPGM extproc(*cwiden:myvarNotused);`,
    `End-Pr;`,
    `Dcl-Pr OTHERPGM extpgm('realString');`,
    `End-Pr;`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    NoExtProgramVariable: true
  }, cache);

  expect(errors.length).toBe(2);

  expect(errors[0]).toMatchObject({
    offset: { start: 26, end: 38 }, type: `NoExtProgramVariable`
  });

  expect(errors[1]).toMatchObject({
    offset: { start: 81, end: 93 }, type: `NoExtProgramVariable`
  });
});

test("linter29", async () => {
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

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    IncludeMustBeRelative: true
  }, cache);

  expect(cache.includes.length).toBe(1);
  expect(cache.includes[0].line).toBe(4);

  expect(cache.variables.length).toBe(1);
  expect(cache.constants.length).toBe(1);
  expect(cache.procedures.length).toBe(1);
  expect(cache.procedures[0].subItems.length).toBe(1);

  const baseNameInclude = path.basename(cache.procedures[0].position.path);
  expect(baseNameInclude).toBe("copy1.rpgle");
  expect(cache.procedures[0].position.range.line).toBe(2);

  expect(errors.length).toBe(0);
});

test("linter30", async () => {
  const lines = [
    `**FREE`,
    ``,
    `Ctl-Opt DftActGrp(*No);`,
    ``,
    `/copy copy1`,
    ``,
    `Dcl-s MyVariable2 Char(20);`,
    ``,
    `Dcl-C theConstant 'Hello world';`,
    ``,
    `CallP theExtProcedure(myVariable);`,
    ``,
    `Return;`
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    IncludeMustBeRelative: true
  }, cache);

  expect(errors.length).toBe(1);

  expect(errors[0]).toMatchObject({
    offset: { start: 39, end: 49 },
    type: `IncludeMustBeRelative`,
    newValue: undefined
  });
});

test("linter31_a", async () => {
  const lines = [
    `**FREE`,
    ``,
    `Ctl-Opt DftActGrp(*No);`,
    ``,
    `/copy rpgle,copy1`,
    ``,
    `Dcl-s MyVariable2 Char(20);`,
    ``,
    `Dcl-C theConstant 'Hello world';`,
    ``,
    `CallP theExtProcedure(myVariable);`,
    ``,
    `Return;`
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    IncludeMustBeRelative: true
  }, cache);

  expect(cache.includes.length).toBe(1);
  expect(cache.includes[0].line).toBe(4);

  expect(errors.length).toBe(0);
});

test("linter31_b", async () => {
  const lines = [
    `**FREE`,
    ``,
    `Ctl-Opt DftActGrp(*No);`,
    ``,
    `/copy rpgle,copy1`,
    ``,
    `Dcl-s MyVariable2 Char(20);`,
    ``,
    `Dcl-C theConstant 'Hello world';`,
    ``,
    `CallP theExtProcedure(myVariable);`,
    ``,
    `Return;`
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
  const { errors } = Linter.getErrors({
    uri,
    content: lines,
    availableIncludes: [`rpgle/copy1.rpgle`]
  }, {
    IncludeMustBeRelative: true,
  }, cache);

  expect(cache.includes.length).toBe(1);
  expect(cache.includes[0].line).toBe(4);

  expect(errors.length).toBe(1);

  expect(errors[0]).toMatchObject({
    offset: { start: 39, end: 50 },
    type: `IncludeMustBeRelative`,
    newValue: `'rpgle/copy1.rpgle'`
  });
});

test("linter32", async () => {
  const lines = [
    `**FREE`,
    ``,
    `Ctl-Opt DftActGrp(*No);`,
    ``,
    `/copy 'rpgle/copy1.rpgle'`,
    ``,
    `Dcl-s MyVariable2 Char(20);`,
    ``,
    `Dcl-C theConstant 'Hello world';`,
    ``,
    `CallP theExtProcedure(myVariable);`,
    ``,
    `Return;`
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    IncludeMustBeRelative: true
  }, cache);

  expect(cache.includes.length).toBe(1);
  expect(cache.includes[0].line).toBe(4);

  expect(errors.length).toBe(0);
});

test("linter32_b", async () => {
  const lines = [
    `**FREE`,
    ``,
    `Ctl-Opt DftActGrp(*No);`,
    ``,
    `/copy 'copy1.rpgle'`,
    ``,
    `Dcl-s MyVariable2 Char(20);`,
    ``,
    `Dcl-C theConstant 'Hello world';`,
    ``,
    `CallP theExtProcedure(myVariable);`,
    ``,
    `Return;`
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
  const { errors } = Linter.getErrors({
    uri,
    content: lines,
    availableIncludes: [`rpgle/copy1.rpgle`]
  }, {
    IncludeMustBeRelative: true
  }, cache);

  expect(cache.includes.length).toBe(1);
  expect(cache.includes[0].line).toBe(4);

  expect(errors.length).toBe(1);

  expect(errors[0]).toMatchObject({
    offset: { start: 39, end: 52 },
    type: `IncludeMustBeRelative`,
    newValue: `'rpgle/copy1.rpgle'`
  });
});

test("linter33", async () => {
  const lines = [
    `**FREE`,
    ``,
    `Ctl-Opt DftActGrp(*No);`,
    ``,
    `/copy '/rpgle/copy1.rpgle'`,
    ``,
    `Dcl-s MyVariable2 Char(20);`,
    ``,
    `Dcl-C theConstant 'Hello world';`,
    ``,
    `CallP theExtProcedure(myVariable);`,
    ``,
    `Return;`
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    IncludeMustBeRelative: true
  }, cache);

  expect(cache.includes.length).toBe(1);
  expect(cache.includes[0].line).toBe(4);

  expect(errors.length).toBe(1);

  expect(errors[0]).toMatchObject({
    offset: { start: 39, end: 59 }, type: `IncludeMustBeRelative`
  });
});

test("linter34", async () => {
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
    `    FROM Employee`,
    `    WHERE WORKDEPT = :deptNum;`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    SQLHostVarCheck: true
  }, cache);

  expect(errors.length).toBe(1);

  expect(errors[0]).toMatchObject({
    offset: { start: 183, end: 190 },
    type: `SQLHostVarCheck`,
    newValue: `:Deptnum`
  });
});

test("linter35", async () => {
  const lines = [
    `**FREE`,
    ``,
    `Ctl-Opt DFTACTGRP(*No);`,
    ``,
    `// My variable`,
    `Dcl-s MyVariable2 Char(20);`,
    `Dcl-s abcd ind;`,
    ``,
    `myVariable2 = *blank;`,
    ``,
    `// a comment`,
    `if (abcd = *off);`,
    `  // Inside if`,
    `  MyVariable2 = 'Hello world';`,
    `Endif;`,
    `Return;`
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    ForceOptionalParens: true
  }, cache);

  expect(errors.length).toBe(0);
});

test("linter36", async () => {
  const lines = [
    `**FREE`,
    ``,
    `Ctl-Opt DFTACTGRP(*No);`,
    ``,
    `// My variable`,
    `Dcl-s MyVariable2 Char(20);`,
    `Dcl-s abcd ind;`,
    ``,
    `myVariable2 = *blank;           // Why comment here`,
    ``,
    `// a comment                    // Why comment here`,
    `if (abcd = *off);               // Why comment here`,
    `  // Inside if`,
    `  MyVariable2 = 'Hello world';  // Why comment here`,
    `Endif;                          // Why comment here`,
    `Return;`
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    ForceOptionalParens: true
  }, cache);

  expect(errors.length).toBe(0);
});

test("linter37", async () => {
  const lines = [
    `**FREE`,
    ``,
    `Ctl-Opt DftActGrp(*No);`,
    ``,
    `/copy 'rpgle/copy1.rpgle'`,
    ``,
    `Dcl-s MyVariable2 Char(20);`,
    ``,
    `Dcl-C theConstant 'Hello world';`,
    ``,
    `CallP theExtProcedure(myVariable);`,
    `CallP(e) theExtProcedure(myVariable);`,
    ``,
    `Return;`
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    UselessOperationCheck: true
  }, cache);

  expect(cache.includes.length).toBe(1);
  expect(cache.includes[0].line).toBe(4);

  expect(errors.length).toBe(1);

  expect(errors[0]).toMatchObject({
    offset: { start: 123, end: 129 },
    type: `UselessOperationCheck`
  });
});

test("linter39", async () => {
  const lines = [
    `**FREE`,
    `ctl-opt debug nomain option(*nodebugio: *srcstmt) ;`,
    `dcl-proc BASE36ADD export ;`,
    `  dcl-pi BASE36ADD varchar(50);`,
    `    PI_Value varchar(50) const; // Input value`,
    `  end-pi BASE36ADD;`,
    `  dcl-s a char(1);`,
    `  if a ='/';`,
    `    a=' ';`,
    `    a= BASE36ADD;`,
    `  endif;`,
    `  return a;`,
    `end-proc BASE36ADD;`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    RequiresProcedureDescription: true
  }, cache);

  expect(errors.length).toBe(1);
  expect(errors[0]).toMatchObject({
    type: `RequiresProcedureDescription`,
    offset: { start: 59, end: 84 }
  });
});

test("linter40", async () => {
  const lines = [
    `**FREE`,
    `ctl-opt debug nomain option(*nodebugio: *srcstmt) ;`,
    `///`,
    `// BASE36ADD`,
    `// Does a thing!`,
    `///`,
    `dcl-proc BASE36ADD export ;`,
    `  dcl-pi BASE36ADD varchar(50);`,
    `    PI_Value varchar(50) const; // Input value`,
    `  end-pi BASE36ADD;`,
    `  dcl-s a char(1);`,
    `  if a ='/';`,
    `    a=' ';`,
    `    a= BASE36ADD;`,
    `  endif;`,
    `  return a;`,
    `end-proc BASE36ADD;`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    RequiresProcedureDescription: true
  }, cache);

  expect(errors.length).toEqual(0);
});

test("linter41", async () => {
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

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    RequireBlankSpecial: true,
    StringLiteralDupe: true
  }, cache);

  expect(errors.length).toEqual(3);

  expect(errors[0]).toMatchObject({
    offset: { start: 52, end: 54 },
    type: `RequireBlankSpecial`,
    newValue: `*BLANK`
  });

  expect(errors[1]).toMatchObject({
    offset: { start: 39, end: 44 },
    type: `StringLiteralDupe`,
    newValue: undefined
  });

  expect(errors[2]).toMatchObject({
    offset: { start: 62, end: 67 },
    type: `StringLiteralDupe`,
    newValue: undefined
  });
});

test("linter42", async () => {
  const lines = [
    `**FREE`,
    ``,
    `Ctl-Opt DFTACTGRP(*No);`,
    ``,
    `Dcl-s MyVariable2 Char(20);`,
    ``,
    `myVariable2 = *blank;`,
    ``,
    `If myVariable2 = *blank;`,
    `  // Inside if`,
    `  MyVariable2 = 'Hello world';`,
    `  Select;`,
    `    When myVariable2 = *blank;`,
    `      // First when`,
    `      MyVariable2 = 'Still blank?';`,
    `    When myVariable2 = 'YOYOYO';`,
    `      // Second when`,
    `      MyVariable2 = 'YOYOYO';`,
    `  Endsl;`,
    `Endif;`,
    `Return;`
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    RequireOtherBlock: true
  }, cache);

  expect(errors.length).toEqual(1);
  expect(errors[0]).toMatchObject({
    type: `RequireOtherBlock`, offset: { start: 339, end: 344 }
  });
});

test("linter43", async () => {
  const lines = [
    `**FREE`,
    ``,
    `Ctl-Opt DFTACTGRP(*No);`,
    ``,
    `Dcl-s MyVariable2 Char(20);`,
    ``,
    `myVariable2 = *blank;`,
    ``,
    `If myVariable2 = *blank;`,
    `  // Inside if`,
    `  MyVariable2 = 'Hello world';`,
    `  Select;`,
    `    When myVariable2 = *blank;`,
    `      // First when`,
    `      MyVariable2 = 'Still blank?';`,
    `    When myVariable2 = 'YOYOYO';`,
    `      // Second when`,
    `      MyVariable2 = 'YOYOYO';`,
    `    other;`,
    `      // Other block`,
    `      MyVariable2 = 'other';`,
    `  Endsl;`,
    `Endif;`,
    `Return;`
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    RequireOtherBlock: true
  }, cache);

  expect(errors.length).toEqual(0);
});

test("linter44", async () => {
  const lines = [
    `**FREE`,
    ``,
    `Ctl-Opt DFTACTGRP(*No);`,
    ``,
    `Dcl-s MyVariable2 Char(20);`,
    ``,
    `myVariable2 = *blank;`,
    ``,
    `If myVariable2 = *blank;`,
    `  // Inside if`,
    `  MyVariable2 = 'Hello world';`,
    `  Select;`,
    `    When myVariable2 = *blank;`,
    `      // First when`,
    `      MyVariable2 = 'Still blank?';`,
    `    When myVariable2 = 'YOYOYO';`,
    `      // Second when`,
    `      MyVariable2 = 'YOYOYO';`,
    `      Select;`,
    `        When myVariable2 = *blank;`,
    `          // First when`,
    `          MyVariable2 = 'Still blank?';`,
    `        When myVariable2 = 'YOYOYO';`,
    `          // Second when`,
    `          MyVariable2 = 'YOYOYO';`,
    `      Endsl;`,
    `  Endsl;`,
    `Endif;`,
    `Return;`
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    RequireOtherBlock: true
  }, cache);

  expect(errors.length).toBe(2);
  expect(errors[0]).toMatchObject({
    type: `RequireOtherBlock`, offset: { start: 552, end: 557 }
  });
  expect(errors[1]).toMatchObject({
    type: `RequireOtherBlock`, offset: { start: 561, end: 566 }
  });
});

test("issue_170", async () => {
  const lines = [
    `**FREE`,
    ``,
    `Dcl-Ds Myds;`,
    `  MyDsField Char(1);`,
    `End-Ds;`,
    ``,
    `dcl-s Date8 packed(8);`,
    `dcl-s Msgpgm char(10);`,
    ``,
    `Stn = Stn;`,
    `Bat = Bat;`,
    `Wedat = Wedat;`,
    ``,
    `MyDsField = 'A';`,
    ``,
    `*INLR = *ON;`,
    `Return;`,
    ``,
    `//----------------------------------------------------------------`,
    `//Initialization routine`,
    `//----------------------------------------------------------------`,
    `Begsr *INZSR;`,
    `  Date8 = *DATE;`,
    `  Msgpgm = Pgmnam;`,
    `Endsr;`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true, collectReferences: true});
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    NoUnreferenced: true
  }, cache);

  expect(errors.length).toBe(0);
});

test("issue_170a", async () => {
  const lines = [
    `**FREE`,
    ``,
    `Dcl-Ds SBM_DS;`,
    `  Move1  Char(128)  Inz('WSBMJOB JOB(CRTBATC*) REPLACE(xxx) -`,
    `  CMD(CALL PRP04A PARM(''SSS''  ''DDDDDDDD'')) -`,
    `  MSGQ(*NONE)')`,
    `  ;`,
    `End-Ds;`,
    ``,
    `return;`,
  ].join(`\n`);


  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true, collectReferences: true});
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    NoUnreferenced: true
  }, cache);

  expect(cache.structs.length).toBe(1);

  const SBM_DS = cache.find(`SBM_DS`);
  expect(SBM_DS.name).toBe(`SBM_DS`);
  expect(SBM_DS.subItems.length).toBe(1);
  expect(SBM_DS.position.range.line).toBe(2);
  expect(SBM_DS.references.length).toBe(1);

  const Move1 = SBM_DS.subItems[0];
  expect(Move1.name).toBe(`Move1`);
  expect(Object.keys(Move1.keyword).length).toBe(2);
  expect(Move1.position.range.line).toBe(3);
  expect(Move1.references.length).toBe(1);
  
  expect(errors.length).toBe(2);
});

test("linter40_keywordrefs", async () => {
  const lines = [
    `**free`,
    `Dcl-C  RANDOMLEN    286;`,
    `Dcl-s  somevar      Int(10) inz(randomLen);`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true, collectReferences: true});
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    IncorrectVariableCase: true
  }, cache);

  const RANDOMLEN = cache.find(`RANDOMLEN`);

  expect(RANDOMLEN.references.length).toBe(2);
  expect(RANDOMLEN.references[1]).toMatchObject({
    offset: { start: 64, end: 73, line: 2 },
    uri: uri,
  });

  expect(errors.length).toBe(1);
  expect(errors[0]).toMatchObject({
    offset: { start: 64, end: 73 },
    type: `IncorrectVariableCase`,
    newValue: `RANDOMLEN`
  });
});

test("linter_casing_on_error_not_a_variable", async () => {
  const lines = [
    `**free`,
    `dcl-c  ERROR  -1;`,
    `monitor;`,
    `  callSomething();`,
    `on-error;`,
    `  result = ERROR;`,
    `endmon;`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true, collectReferences: true});
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    IncorrectVariableCase: true
  }, cache);

  expect(errors.length).toBe(0);
});

test("issue_175", async () => {
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
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    NoUnreferenced: true
  }, cache);

  expect(errors.length).toBe(0);
});

test("issue180", async () => {
  const lines = [
    `**free`,
    `Begsr Checkemp;`,
    ``,
    `  In91 = *OFF;`,
    `  In92 = *OFF;`,
    ``,
    `  S0Issues = Rs.Wiiss_Seq;`,
    `  If S0Issues = 1;`,
    `    S0Err = Rs.Wiissmsg;`,
    `    If S0Err = *BLANKS;`,
    `      Clear S0Issues;`,
    `    Endif;`,
    `    If Rs.Wirvsts = 'Y';`,
    `      %SUBST( S0Err :( %SIZE( S0Err ) -2) :3) = '(R)';`,
    `    Endif;`,
    `  Elseif S0Issues > 1;`,
    `    S0Err = 'Multiple Issues exist (' +%CHAR( S0Issues ) +')';`,
    `  Else;`,
    `    S0Err = ' ';`,
    `  Endif;`,
    `  S0Isssvty = Rs.Wiisssvty;`,
    `  // Mark as error for all issue that need correction`,
    `  If S0Isssvty >= 31;`,
    `    In92 = *ON;`,
    `    Emperrs = *ON;`,
    `  Endif;`,
    ``,
    `Endsr;`,
  ].join(`\n`);
  
  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true, collectReferences: true});

  Linter.getErrors({ uri, content: lines }, {
  }, cache);
});

test("issue_234_a", async () => {
  const lines = [
    `**free`,
    `DCL-DS MagicDS QUALIFIED;`,
    `    Char      CHAR(5000) POS(1);`,
    `END-DS;`,
    ``,
    `DCL-S Pos         INT(5);`
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    IncorrectVariableCase: true
  }, cache);

  expect(errors.length).toBe(0);
});


test("issue_234_b", async () => {
  const lines = [
    `**free`,
    `ctl-opt debug  option(*nodebugio: *srcstmt) dftactgrp(*no) actgrp(*caller)`,
    `main(Main);`,
    `dcl-proc Main;`,
    `    dsply %CHAR(CalcDiscount(10000));`,
    `    dsply %char(CalcDiscount(1000));`,
    `    x = %TIMESTAMP(y);`,
    `    y = %TimeStamp(x);`,
    `    return;`,
    `end-proc;`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    IncorrectVariableCase: true
  }, cache);

  expect(errors.length).toBe(0);
});

test("issue_238", async () => {
  const lines = [
    `**FREE`,
    `/Copy Qcpysrc,Hspecle`,
    `// Prototypes`,
    `/Copy Qcpysrc,Copybook1`,
    `/Copy Qcpysrc,Copybook2`,
    `/Copy Qcpysrc,Copybook3`,
    `/Copy Qcpysrc,Copybook4`,
    `/Copy Qcpysrc,Copybook5`,
    `Dcl-S MyParm1 Char(1) Const;`,
    `Dcl-S MyParm2 Char(1) Const;`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
  const { indentErrors, errors } = Linter.getErrors({ uri, content: lines }, {
    indent: 2
  }, cache);

  expect(indentErrors.length).toBe(0);
});

test("issue_240", async () => {
  const lines = [
    `**FREE`,
    ``,
    `///`,
    `// -> ~~~~ <- This is where the error is reported`,
    `// Comment line 2`,
    `// Comment line 3`,
    `// Comment line 4`,
    `// Comment line 5`,
    `// Comment line 6`,
    `// Comment line 7`,
    `// Comment line 8`,
    `// Comment line 9`,
    `// Comment line 10`,
    `// Comment line 11`,
    `// Comment line 12`,
    `// Comment line 13`,
    `// Comment line 14`,
    `// Comment line 15`,
    `// Comment line 16`,
    `///`,
    ``,
    `dcl-pr QCMDEXC  extpgm('QCMDEXC');`,
    `  cmd            char(32702)  options(*VARSIZE) const;`,
    `  cmdLen         packed(15:5) const;`,
    `  igc            char(3)      options(*NOPASS) const;`,
    `end-pr;`,
    ``,
    `*inLR = *ON;`,
    `return;`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    "NoExternalTo": [`QCMD`, `QP2TERM`, `QSH`, `SYSTEM`, `QCMDEXC`]
  }, cache);

  expect(errors.length).toBe(1);

  expect(errors[0]).toMatchObject({
    type: `NoExternalTo`,
    offset: {
      start: 344,
      end: 377,
    }
  });

  expect(lines.substring(errors[0].offset.start, errors[0].offset.end)).toBe(`dcl-pr QCMDEXC  extpgm('QCMDEXC')`);
});

test("issue_239", async () => {
  const lines = [
    `**FREE`,
    `ctl-opt dftactgrp(*NO);`,
    ``,
    `// If RequiresParameter is enabled, the following line will be flagged with "Procedure calls require brackets.".`,
    `dcl-s myValue like(getSomeValue);`,
    ``,
    `// !! The following is not valid syntax and will not compile:`,
    `// !! dcl-s myValue like(getSomeValue()); // Invalid syntax; will not compile`,
    ``,
    `myValue = getSomeValue();`,
    `*inLR = *ON;`,
    `return;`,
    ``,
    `dcl-proc getSomeValue;`,
    `  dcl-pi *N varchar(10);`,
    `  end-pi;`,
    `  return %char(%date(): *ISO);`,
    `end-proc;`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    RequiresParameter: true
  }, cache);

  expect(errors.length).toBe(0);
});

test("issue_251", async () => {
  const lines = [
    `**free`,
    `ctl-opt debug  option(*nodebugio: *srcstmt) dftactgrp(*no) actgrp(*caller)`,
    `main(Main);`,
    `dcl-s STDDSC  packed(5:2) inz(9.0); //from Item Master External  DS`,
    `dcl-c THE_DEFAULT_DISCOUNT 9;`,
    `dcl-proc Main;`,
    `    dsply %char(CalcDiscount(1000));`,
    `    return;`,
    `end-proc;`,
    `dcl-proc CalcDiscount ;`,
    `    dcl-pi CalcDiscount packed(7:2);`,
    `        iCost packed(7:2) value;`,
    `    end-pi;`,
    `    // @rpglint-skip`,
    `    STDDSC = The_Default_Discount;`,
    `    if (wkChar = *blanks);`,
    `    endif;`,
    `    if iCost >= 10000;`,
    `        wkDisc = (iCost*BigDisc)/100;`,
    `    Else;`,
    `        wkDisc = (iCost*STDDSC)/100;  // << Error here <<`,
    `    endif;`,
    `    Return wkDisc;`,
    `end-proc;`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    ForceOptionalParens: true
  }, cache);

  expect(errors.length).toBe(1);
  expect(errors[0].type).toBe(`ForceOptionalParens`);
  expect(lines.substring(errors[0].offset.start, errors[0].offset.end)).toBe(`iCost >= 10000`);
});

test('paddr_issue_250', async () => {
  const lines = [
    `**FREE`,
    `ctl-opt dftactgrp(*NO);`,
    `dcl-s callback  pointer(*PROC);`,
    `callback = %paddr('SOMEOTHERFUNCTION');`,
    ``,
    `.///`,
    `// The following line will incorrectly be flagged in error.`,
    `///`,
    `callback = %paddr(someFunction);`,
    ``,
    ``,
    `*inLR = *ON;`,
    `return;`,
    `dcl-proc someFunction;`,
    `  dcl-pi *N int(10);`,
    `    value1                    char(10) const;`,
    `    value2                    char(10) const;`,
    `  end-pi;`,
    `  if value1 < value2;`,
    `    return -1;`,
    `  endif;`,
    `  if value1 > value2;`,
    `    return 1;`,
    `  endif;`,
    `  return 0;`,
    `end-proc;`,
    `dcl-proc someOtherFunction;`,
    `  dcl-pi *N int(10);`,
    `    value1                    int(10) const;`,
    `    value2                    int(10) const;`,
    `  end-pi;`,
    `  if value1 < value2;`,
    `    return -1;`,
    `  endif;`,
    `  if value1 > value2;`,
    `    return 1;`,
    `  endif;`,
    `  return 0;`,
    `end-proc;`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    RequiresParameter: true
  }, cache);

  expect(errors.length).toBe(0);
});

test('new_select_1', async () => {
  const lines = [
    `**free`,
    `SELECT a.b(i).c(j);`,
    `WHEN-IS 0;`,
    `   // Handle a.b(i).c(j) = 0`,
    `WHEN-IN %RANGE(5 : 20);`,
    `   // Handle a.b(i).c(j) between 5 and 100`,
    `WHEN-IN %LIST(2 : 3 : N : M + 1);`,
    `   // Handle a.b(i).c(j) = 5, 10, N, or (M + 1)`,
    `WHEN-IS N + 1;`,
    `   // Handle a.b(i).c(j) = N + 1`,
    `OTHER;`,
    `   // Handle any other values for a.b(i).c(j)`,
    `ENDSL;`,
  ].join(`\r\n`);

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true});
  const { indentErrors } = Linter.getErrors({ uri, content: lines }, {
    indent: 2
  }, cache);

  expect(indentErrors.length).toBe(5);

  // Expect all expected indent to be 2
  expect(indentErrors.some(e => e.expectedIndent !== 2)).toBe(false);
});

test('new_select_2', async () => {
  const lines = [
    `**free`,
    `SELECT a.b(i).c(j);`,
    `  WHEN-IS 0;`,
    `   // Handle a.b(i).c(j) = 0`,
    `  WHEN-IN %RANGE(5 : 20);`,
    `   // Handle a.b(i).c(j) between 5 and 100`,
    `  WHEN-IN %LIST(2 : 3 : N : M + 1);`,
    `   // Handle a.b(i).c(j) = 5, 10, N, or (M + 1)`,
    `  WHEN-IS N + 1;`,
    `   // Handle a.b(i).c(j) = N + 1`,
    `  OTHER;`,
    `   // Handle any other values for a.b(i).c(j)`,
    `ENDSL;`,
  ].join(`\r\n`);

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true});
  const { indentErrors } = Linter.getErrors({ uri, content: lines }, {
    indent: 2
  }, cache);

  expect(indentErrors.length).toBe(0);
});

test('on_excp_1', async () => {
  const lines = [
    `**free`,
    `DCL-F badfile DISK(10) USROPN;`,
    `DCL-S status PACKED(5);`,
    ``,
    `MONITOR;`,
    `  OPEN badfile;`,
    `ON-EXCP 'CPF4101';`,
    `  status = %status();`,
    `  DSPLY ('Message CPF4101, status ' + %char(status)); //  1 `,
    `ON-EXCP 'RNX1217';`,
    `  DSPLY 'Message RNX1217';`,
    `ON-ERROR 1217;`,
    `  DSPLY 'Status 1217';`,
    `ENDMON;`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true});
  const { indentErrors } = Linter.getErrors({ uri, content: lines }, {
    indent: 2
  }, cache);

  expect(indentErrors.length).toBe(0);
});

test('on_excp_2', async () => {
  const lines = [
    `**FREE`,
    `monitor; `,
    `on-error *all; `,
    `dsply 'on error'; `,
    `on-excp CPF0000;`,
    `endmon;`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true});
  const { indentErrors } = Linter.getErrors({ uri, content: lines }, {
    indent: 2
  }, cache);
  
  expect(indentErrors.length).toBe(1);
  expect(indentErrors[0]).toMatchObject({
    line: 3,
    expectedIndent: 2,
    currentIndent: 0
  });
});

test('sqlRunner1_1', async () => {
  const lines = [
    `**free`,
    `EXEC SQL`,
    `  DECLARE CUSCUR CURSOR FOR`,
    `    SELECT CUSNO FROM CUSTOMER;`,
    ``,
    `EXEC SQL`,
    `  OPEN CUSCUR;`,
    ``,
    `EXEC SQL`,
    `  FETCH NEXT FROM CUSCUR INTO :cust.CUSNO;`,
    ``,
    `EXEC SQL`,
    `  CLOSE CUSCUR;`,
    ``,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    SQLRunner: true
  }, cache);

  expect(errors.length).toBe(1);
  expect(errors[0]).toMatchObject({
    type: 'SQLRunner',
    offset: { start: 7, end: 74 },
    newValue: 'EXEC SQL\n  DECLARE CUSCUR CURSOR FOR\n    SELECT CUSNO FROM CUSTOMER'
  });
});

test('sqlRunner1_b', async () => {
  const lines = [
    `**free`,
    `EXEC SQL DECLARE CUSCUR CURSOR FOR`,
    `    SELECT CUSNO FROM CUSTOMER;`,
    ``,
    `EXEC SQL`,
    `  OPEN CUSCUR;`,
    ``,
    `EXEC SQL`,
    `  FETCH NEXT FROM CUSCUR INTO :cust.CUSNO;`,
    ``,
    `EXEC SQL`,
    `  CLOSE CUSCUR;`,
    ``,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    SQLRunner: true
  }, cache);

  expect(errors.length).toBe(1);
  expect(errors[0]).toMatchObject({
    type: 'SQLRunner',
    offset: { start: 7, end: 72 },
    newValue: 'EXEC SQL DECLARE CUSCUR CURSOR FOR\n    SELECT CUSNO FROM CUSTOMER'
  });
});

test(`snd-msg casing #309`, async () => {
  const lines = [
    `**FREE`,
    `Dcl-S Msg Varchar(64);`,
    `Msg = 'My message to the joblog';`,
    `SND-MSG *INFO %MSG( 'MSG9997' : 'WFIMSGF' :Msg ) %TARGET( *SELF );`,
    `*INLR = *ON;`,
    `Return;`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    IncorrectVariableCase: true
  }, cache);

  expect(errors.length).toBe(0);
});

test(`define and undefine directives #310`, async () => {
  const lines = [
    `**free`,
    `/define #stuff`,
    `// do exciting things here`,
    `/undefine #stuff`,
    `dcl-ds gPSDS psds qualified;`,
    `  pgmName *proc;`,
    `end-ds;`,
    `*inlr = *on;`,
    `return;`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, { ignoreCache: true, withIncludes: true });
  const { indentErrors } = Linter.getErrors({ uri, content: lines }, {
    indent: 2
  }, cache);

  expect(indentErrors.length).toBe(0);
});

test('linter with non-free copybook', async () => {
  const lines = [
    `**Free`,
    `Ctl-Opt Main(Engage_Usage_Report);`,
    `Ctl-Opt Debug Option(*SrcStmt);`,
    `Ctl-Opt ActGrp(*Caller);`,
    `Ctl-opt Bnddir('PCRPROCS');`,
    `Ctl-opt ExtBinInt(*Yes);`,
    ` `,
    `/copy './rpgle/fixed1.rpgleinc'`,
    ` `,
    `Dcl-Proc Engage_Usage_Report;`,
    ` `,
    `  Dcl-Pi Engage_Usage_Report;`,
    `    p_mode Char(3) Options(*NoPass);`,
    `  End-Pi;`,
    ``,
    `  dcl-s abnormal_exit ind;`,
    ` `,
    `  If %parms() > 0 and %Addr(p_mode) <> *Null;`,
    ` `,
    `  Else;`,
    ` `,
    `  Endif;`,
    ` `,
    `  Clean_Up();`,
    ` `,
    `  On-Exit abnormal_exit;`,
    ``,
    `End-Proc Engage_Usage_Report;`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, { ignoreCache: true, withIncludes: true, collectReferences: true });

  expect(cache.includes.length).toBe(1);

  const { errors } = Linter.getErrors({ uri, content: lines }, {
    NoUnreferenced: true
  }, cache);

  expect(errors.length).toBe(0);
});

test('constant replace picking up wrong variable #330', async () => {
  const lines = [
    `**Free`,
    `Ctl-Opt Main(Proc_Name);`,
    `Ctl-Opt Debug Option(*SrcStmt:*NoDebugIO);`,
    `Ctl-Opt ActGrp(*Caller);`,
    `Ctl-opt ExtBinInt(*Yes); `,
    ``,
    `Dcl-Proc Proc_1; `,
    ``,
    `  Dcl-Pi Proc_1 Char(20);`,
``,
    `  End-Pi;`,
    `  `,
    `  Dcl-s altError Char(20);`,
``,
    `  Dcl-c basicError 'Invalid credentials';`,
``,
    `  altError = 'Invalid credentials';`,
``,
    `  Return basicError;`,
``,
    `On-Exit;`,
    `End-Proc Proc_1;`,
``,
    `Dcl-Proc Proc_2;`,
``,
    `  Dcl-Pi Proc_2 Char(20);`,
``,
    `  End-Pi;`,
``,
    `  Return 'Invalid credentials';`,
``,
    `On-Exit;`,
    `End-Proc Proc_2;`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, { ignoreCache: true, withIncludes: true });
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    StringLiteralDupe: true
  }, cache);

  expect(errors.length).toBe(2);
  
  expect(errors[0]).toMatchObject({
    offset: { start: 270, end: 291 },
    type: 'StringLiteralDupe',
    newValue: 'basicError'
  });

  expect(errors[1]).toMatchObject({
    offset: { start: 408, end: 429 },
    type: 'StringLiteralDupe',
    newValue: undefined
  });
});

test('Linter running on rpgleinc', async () => {
  const lines = [
    `**free`,
    `Dcl-S CustomerName_t varchar(40) template;`,
  ].join(`\n`);

  const cache = await parser.getDocs(includeUri, lines, { ignoreCache: true, withIncludes: true, collectReferences: true });
  const { errors } = Linter.getErrors({ uri: includeUri, content: lines }, {
    IncorrectVariableCase: true,
    NoUnreferenced: true,
    SpecificCasing: [{operation: "dcl-s", expected: `DCL-S`}]
  }, cache);

  expect(errors.length).toBe(1);
  expect(errors[0]).toMatchObject({
    offset: { start: 7, end: 12 },
    type: 'SpecificCasing',
    newValue: 'DCL-S'
  });
});

test('Linter running on member rpgleinc', async () => {
  const lines = [
    `**free`,
    `Dcl-S CustomerName_t varchar(40) template;`,
  ].join(`\n`);

  const cache = await parser.getDocs(memberIncludeUri, lines, { ignoreCache: true, withIncludes: true, collectReferences: true });
  const { errors } = Linter.getErrors({ uri: memberIncludeUri, content: lines }, {
    IncorrectVariableCase: true,
    NoUnreferenced: true,
    SpecificCasing: [{operation: "dcl-s", expected: `DCL-S`}]
  }, cache);

  expect(errors.length).toBe(1);
  expect(errors[0]).toMatchObject({
    offset: { start: 7, end: 12 },
    type: 'SpecificCasing',
    newValue: 'DCL-S'
  });
});

test('issue_353_indent_1', async () => {
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

  const { indentErrors, errors } = Linter.getErrors({ uri, content: lines }, {
    indent: 2
  }, cache);

  expect(errors.length).toBe(0);
  expect(indentErrors.length).toBe(0);
});

test('issue_353_indent_2', async () => {
  const lines = [
    `**free`,
    `dcl-ds HEDINF                     based(p1@);`,
    `  HRLEN                 Int(10:0);                            // Record length`,
    `   HCRRN                 Int(10:0);                            // Cursor's RRN`,
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

  const { indentErrors, errors } = Linter.getErrors({ uri, content: lines }, {
    indent: 2
  }, cache);

  expect(indentErrors.length).toBe(1);
  expect(indentErrors[0]).toMatchObject({
    line: 3,
    expectedIndent: 2,
    currentIndent: 3
  });
});

test('issue_353_indent_3', async () => {
  const lines = [
    `**free`,
    `begsr displayHelp;`,
    `  // Do something with this program's`,
    `  //  name and library ... assume the program is running`,
    `  //  from the same library as contains the source.`,
    `  fileName = 'QRPGLESRC';`,
    `  library = pgSts.lib;`,
    `endsr;`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, { ignoreCache: true, withIncludes: false });
  const { indentErrors, errors } = Linter.getErrors({ uri, content: lines }, {
    indent: 2
  }, cache);

  expect(errors.length).toBe(0);
  expect(indentErrors.length).toBe(0);
});

test('issue_353_indent_4', async () => {
  const lines = [
    `**FREE`,
    `dcl-c @QUOTE                   '''';`,
    ``,
    `dcl-c @SOMETHING_IN_BETWEEN 'whatever';`,
    ``,
    `dcl-c @SEUCOMMENT_FREE  '//&& ';`,
    `// ********************************************************************`,
    ``,
    `dcl-ds copybookinfo_t           qualified template inz;`,
    `  qualObj;`,
    `  file                  Char(10)    overlay(qualObj: 1);`,
    `  lib                   Char(10)    overlay(qualObj: *NEXT);`,
    `  mbr                   Char(10);`,
    `  found                 Ind         inz(*OFF);`,
    `end-ds;`,
    `// ********************************************************************`,
  ].join(`\r\n`);

  const cache = await parser.getDocs(uri, lines, { ignoreCache: true, withIncludes: false });

  const atQuote = cache.find(`@QUOTE`);
  expect(atQuote).toBeDefined();
  expect(atQuote).toBeDefined();
  expect(atQuote.keyword[`CONST`]).toBe(`''`);

  const atSomethingInBetween = cache.find(`@SOMETHING_IN_BETWEEN`);
  expect(atSomethingInBetween).toBeDefined();
  expect(atSomethingInBetween.keyword[`CONST`]).toBe(`'whatever'`);

  const atSeuCommentFree = cache.find(`@SEUCOMMENT_FREE`);
  expect(atSeuCommentFree).toBeDefined();
  expect(atSeuCommentFree.keyword[`CONST`]).toBe(`'//&& '`);

  const copybookinfo_t = cache.find(`copybookinfo_t`);
  expect(copybookinfo_t).toBeDefined();
  expect(copybookinfo_t.subItems.length).toBe(5);
  expect(copybookinfo_t.keyword[`QUALIFIED`]).toBe(true);
  expect(copybookinfo_t.keyword[`TEMPLATE`]).toBe(true);
  expect(copybookinfo_t.keyword[`INZ`]).toBe(true);

  const { indentErrors, errors } = Linter.getErrors({ uri, content: lines }, {
    indent: 2
  }, cache);

  expect(errors.length).toBe(0);
  console.log(indentErrors);
  expect(indentErrors.length).toBe(0);
});

test(`issue_353_indent_5`, async () => {
  const lines = [
    `**FREE`,
    `*in01 = 'yy' in %list('zz': ' ''');`,
    `*inLR = *ON;`,
    `return;`,
    `dcl-proc test;`,
    `  // If the line ends with a known end-of line character, then`,
    `  //  assume it is free format.  Make sure to strip any free-format`,
    `  //  comment first.  The stripping is not precise and will cause`,
    `  //  a free form line that has a '//' literal to not be detected.`,
    `  eol = ';';`,
    `end-proc;`
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, { ignoreCache: true, withIncludes: false });

  const { indentErrors, errors } = Linter.getErrors({ uri, content: lines }, {
    indent: 2
  }, cache);

  expect(errors.length).toBe(0);
  expect(indentErrors.length).toBe(0);
});

test(`issue_353_indent_6`, async () => {
  const lines = [
    `**FREE`,
    `*in01 = 'yy' in %list('zz': ' ''');`,
    `*inLR = *ON;`,
    `return;`,
    ``,
    `dcl-proc test;`,
    `  if specType = 'C' and not likelyComment;`,
    `    if not (%subst(srcDta: 7: 2) in`,
    `        %list('  ': 'L0': 'L1': 'L2': 'L3': 'L4': 'L5'`,
    `        : 'L6': 'L7': 'L8': 'L9': 'LR': 'SR': 'AN': 'OR'));`,
    `      return *ON;`,
    `    endif;`,
    `  endif;`,
    ``,
    `  // If the line ends with a known end-of line character, then`,
    `  //  assume it is free format.  Make sure to strip any free-format`,
    `  //  comment first.  The stripping is not precise and will cause`,
    `  //  a free form line that has a '//' literal to not be detected.`,
    `  eol = ';';`,
    `  posStr = %scan('//': srcDta);`,
    `  if posStr > 1;`,
    `    evalr eol = %trim(%subst(srcDta: 1: posStr - 1));`,
    `  endif;`,
    `  if (eol in %list(';': ':': '+': '-': '/': '*': '=': '_'));`,
    `    return *ON;`,
    `  endif;`,
    ``,
    `  // Despite our best guesses, we cannot confirm this to be a`,
    `  //  free format line, so indicate that it is _not_ free format.`,
    `  return *OFF;`,
    `end-proc;`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, { ignoreCache: true, withIncludes: false });

  const { indentErrors, errors } = Linter.getErrors({ uri, content: lines }, {
    indent: 2
  }, cache);

  expect(indentErrors.length).toBe(0);
});

test('issue_358_no_reference', async () => {
  const lines = [
    `**Free`,
    ``,
    `Ctl-Opt Main(Lint_Test);`,
    `Ctl-Opt ActGrp(*Caller);`,
    ``,
    `Dcl-s errorCPF Char(7) Import('_EXCP_MSGID');`,
    ``,
    `Dcl-Proc Lint_Test;`,
    ``,
    `  Dcl-Pi Lint_Test;`,
    ``,
    `  End-Pi;`,
    ``,
    `  Dcl-s error_msg like(t_error_msg);`,
    ``,
    `  exsr set_error_msg;`,
    ``,
    `  Monitor;`,
    ``,
    `  On-Error;`,
    `    common_dblog(pgm_sts.thisPgm:'Initialize':%Trim(errorCPF) + ' - Error occurred.');`,
    `  EndMon;`,
    ``,
    `  Return;`,
    ``,
    `  Begsr set_error_msg;`,
    ``,
    `    error_msg = 'Error message';`,
    ``,
    `  Endsr;`,
    ``,
    `  On-Exit;`,
    ``,
    `End-Proc Lint_Test;`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, { ignoreCache: true, collectReferences: true });

  const { errors } = Linter.getErrors({ uri, content: lines }, {
    NoUnreferenced: true
  }, cache);

  expect(errors.length).toBe(0);
});

test('issue_358_no_reference_2', async () => {
  // This test case is built from rpgle-repl: REPL_VARS.SQLRPGLE

  const lines = [
    `**free`,
    `dcl-proc copyDataStructureDefinitions export;`,
    `  dcl-pi *n;`,
    `    variable likeds(t_variable) const;`,
    `  end-pi;`,
    ``,
    `  dcl-s copyDataStructure like(t_variable.name);`,
    ``,
    `  copyDataStructure = toUpperCase(variable.definition);`,
    `  copyDataStructure = %scanrpl('LIKEDS':'':copyDataStructure);`,
    `  copyDataStructure = %scanrpl('(':'':copyDataStructure);`,
    `  copyDataStructure = %scanrpl(')':'':copyDataStructure);`,
    `  copyDataStructure = %trim(toUpperCase(copyDataStructure));`,
    ``,
    `  // see if it exists in the current scope`,
    `  exec sql`,
    `    INSERT INTO replvars`,
    `      (variable_name, variable_type, is_qualified, array_size,`,
    `       definition, variable_scope, is_template,`,
    `       defined)`,
    `    SELECT :variable.name, 'datastruct',`,
    `           /* DS's declared LIKEDS are qualified for free */`,
    `           'Y', NULLIF(:variable.arraySize, 0),`,
    `           NULLIF(:variable.definition, ''), :variable.scope,`,
    `           :variable.template, COALESCE(defined, 'N')`,
    `      FROM replvars`,
    `     WHERE session_id = (QSYS2.JOB_NAME)`,
    `           AND UPPER(variable_name) = UPPER(:copyDataStructure)`,
    `           AND variable_scope = :variable.scope;`,
    ``,
    ``,
    ``,
    `end-proc;`,
    ``,
    `dcl-proc fetchStoredVariable export;`,
    `  dcl-pi *n likeds(t_variable);`,
    `    variableName like(t_variable.name) const;`,
    `    scope like(t_variable.scope) const;`,
    `  end-pi;`,
    ``,
    `  dcl-ds variable likeds(t_variable);`,
    ``,
    `  variable.id = 0;`,
    `  variable.name = variableName;`,
    `  variable.arraySize = 0;`,
    `  variable.parentName = *blanks;`,
    `  variable.parentArraySize = 0;`,
    ``,
    `  if %scan('*IN': toUpperCase(%trim(variableName))) = 1;`,
    `    variable.type = '*indicator';`,
    `    return variable;`,
    `  endif;`,
    ``,
    `  // check the exact name first`,
    `  exec sql`,
    `    SELECT variable_name,`,
    `            CAST(variable_type AS CHAR(10)),`,
    `            COALESCE(array_size, 0),`,
    `            COALESCE(parent_data_structure, ''),`,
    ``,
    `            0,`,
    `            variable_scope,`,
    `            COALESCE(definition, ''),`,
    `            isTemplate,`,
    `            defined,`,
    `            variable_id,`,
    `            is_used`,
    `       INTO :variable`,
    `       FROM replvars`,
    `      WHERE session_id = (QSYS2.JOB_NAME)`,
    `           AND UPPER(variable_name) = UPPER(:variableName)`,
    `           AND variable_scope = :scope`,
    `           AND parent_data_structure IS NULL`,
    `      ORDER BY parent_data_structure`,
    `      LIMIT 1;`,
    ``,
    `  if sqlstt = '00000';`,
    `    return variable;`,
    `  endif;`,
    `end-process;`,
  ].join(`\r\n`);

  const cache = await parser.getDocs(uri, lines, { ignoreCache: true, collectReferences: true });

  const procA = cache.find(`copyDataStructureDefinitions`);

  for (const vari of procA.scope.variables) {
    for (const ref of vari.references) {
      expect(vari.name.toUpperCase()).toBe(lines.substring(ref.offset.start, ref.offset.end).toUpperCase());
    }
  }

  const procB = cache.find(`fetchStoredVariable`);

  for (const parm of procB.scope.parameters) {
    for (const ref of parm.references) {
      expect(parm.name.toUpperCase()).toBe(lines.substring(ref.offset.start, ref.offset.end).toUpperCase());
    }
  }
});