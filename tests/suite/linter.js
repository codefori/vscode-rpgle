
const assert = require(`assert`);

const { default: parserSetup } = require(`../parserSetup`);
const { default: Linter } = require(`../../server/src/language/linter`);
const path = require(`path`);
const { Range, Position } = require(`../../server/src/language/models/DataPoints`);

const uri = `source.rpgle`;

exports.linter_indent_multi_1 = async () => {
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

  const parser = parserSetup();
  const cache = await parser.getDocs(uri, lines);
  const { indentErrors } = Linter.getErrors({ uri, content: lines }, {
    indent: 2
  }, cache);

  assert.strictEqual(indentErrors.length, 0, `There should be no errors`);
};

exports.linter_indent_multi_2 = async () => {
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

  const parser = parserSetup();
  const cache = await parser.getDocs(uri, lines);
  const { indentErrors } = Linter.getErrors({ uri, content: lines }, {
    indent: 2
  }, cache);

  assert.strictEqual(indentErrors.length, 2, `There should be 2 errors`);

  assert.strictEqual(indentErrors[0].line, 3, `First error should be index 3`);
  assert.strictEqual(indentErrors[0].currentIndent, 1, `Actual indent should be 1`);
  assert.strictEqual(indentErrors[0].expectedIndent, 2, `Expected indent should be 2`);

  assert.strictEqual(indentErrors[1].line, 5, `Second error should be index 5`);
  assert.strictEqual(indentErrors[1].currentIndent, 5, `Actual indent should be 5`);
  assert.strictEqual(indentErrors[1].expectedIndent, 4, `Expected indent should be 4`);
};

exports.linter1_indent = async () => {
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

  const parser = parserSetup();
  const cache = await parser.getDocs(uri, lines);
  const { indentErrors } = Linter.getErrors({ uri, content: lines }, {
    indent: 2
  }, cache);

  assert.strictEqual(indentErrors.length, 1, `Expect length of 1`);
  assert.strictEqual(indentErrors[0].line, 11, `Index of 9 expected`);
  assert.strictEqual(indentErrors[0].currentIndent, 0, `Value of 0 expected`);
  assert.strictEqual(indentErrors[0].expectedIndent, 2, `Value of 2 expected`);
};

/**
   * Testing spaces before the EOL
   */
exports.linter1_1_indent = async () => {
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

  const parser = parserSetup();
  const cache = await parser.getDocs(uri, lines);
  const { indentErrors } = Linter.getErrors({ uri, content: lines }, {
    indent: 2
  }, cache);

  assert.strictEqual(indentErrors.length, 0, `Expect length of 0`);
};

exports.linter2_indent = async () => {
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

  const parser = parserSetup();
  const cache = await parser.getDocs(uri, lines);
  const { indentErrors } = Linter.getErrors({ uri, content: lines }, {
    indent: 2
  }, cache);

  assert.strictEqual(indentErrors.length, 2, `Expect length of 2`);

  assert.strictEqual(indentErrors[0].line, 10, `Index of 9 expected`);
  assert.strictEqual(indentErrors[0].currentIndent, 0, `Value of 0 expected`);
  assert.strictEqual(indentErrors[0].expectedIndent, 2, `Value of 2 expected`);

  assert.strictEqual(indentErrors[1].line, 17, `Index of 14 expected`);
  assert.strictEqual(indentErrors[1].currentIndent, 8, `Value of 8 expected`);
  assert.strictEqual(indentErrors[1].expectedIndent, 6, `Value of 6 expected`);
};

exports.linter2_indent_other = async () => {
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

  const parser = parserSetup();
  const cache = await parser.getDocs(uri, lines);
  const { indentErrors } = Linter.getErrors({ uri, content: lines }, {
    indent: 2
  }, cache);

  assert.strictEqual(indentErrors.length, 2, `Expect length of 2`);

  assert.strictEqual(indentErrors[0].line, 10, `Index of 9 expected`);
  assert.strictEqual(indentErrors[0].currentIndent, 0, `Value of 0 expected`);
  assert.strictEqual(indentErrors[0].expectedIndent, 2, `Value of 2 expected`);

  assert.strictEqual(indentErrors[1].line, 17, `Index of 14 expected`);
  assert.strictEqual(indentErrors[1].currentIndent, 8, `Value of 8 expected`);
  assert.strictEqual(indentErrors[1].expectedIndent, 6, `Value of 6 expected`);
};

exports.linter3_indent = async () => {
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

  const parser = parserSetup();
  const cache = await parser.getDocs(uri, lines);
  const { indentErrors } = Linter.getErrors({ uri, content: lines }, {
    indent: 2
  }, cache);

  assert.strictEqual(indentErrors.length, 3, `Expect length of 3`);

  assert.strictEqual(indentErrors[0].line, 9, `Index of 9 expected`);
  assert.strictEqual(indentErrors[0].currentIndent, 0, `Value of 0 expected`);
  assert.strictEqual(indentErrors[0].expectedIndent, 2, `Value of 2 expected`);

  assert.strictEqual(indentErrors[1].line, 14, `Index of 14 expected`);
  assert.strictEqual(indentErrors[1].currentIndent, 8, `Value of 8 expected`);
  assert.strictEqual(indentErrors[1].expectedIndent, 6, `Value of 6 expected`);

  assert.strictEqual(indentErrors[2].line, 18, `Index of 18 expected`);
  assert.strictEqual(indentErrors[2].currentIndent, 2, `Value of 2 expected`);
  assert.strictEqual(indentErrors[2].expectedIndent, 0, `Value of 0 expected`);
};

exports.linter4 = async () => {
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

  const parser = parserSetup();
  const cache = await parser.getDocs(uri, lines);
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    RequireBlankSpecial: true
  }, cache);

  assert.strictEqual(errors.length, 2, `Expect length of 2`);

  assert.strictEqual(errors[0].type, `RequireBlankSpecial`, `Expect RequireBlankSpecial`);
  assert.strictEqual(errors[0].range.start.line, 6, `Index of 6 expected`);
  assert.strictEqual(errors[0].offset.position, 14, `Index of 14 expected`);
  assert.strictEqual(errors[0].offset.end, 16, `Index of 16 expected`);
  assert.strictEqual(errors[0].newValue, `*BLANK`, `Value of *BLANK expected`);

  assert.strictEqual(errors[1].type, `RequireBlankSpecial`, `Expect RequireBlankSpecial`);
  assert.strictEqual(errors[1].range.start.line, 8, `Index of 8 expected`);
  assert.strictEqual(errors[1].offset.position, 17, `Index of 17 expected`);
  assert.strictEqual(errors[1].offset.end, 19, `Index of 19 expected`);
  assert.strictEqual(errors[1].newValue, `*BLANK`, `Value of *BLANK expected`);
};

exports.linter5 = async () => {
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

  const parser = parserSetup();
  const cache = await parser.getDocs(uri, lines);
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    IncorrectVariableCase: true
  }, cache);

  assert.strictEqual(errors.length, 2, `Expect length of 2`);

  assert.strictEqual(errors[0].type, `IncorrectVariableCase`, `Expect IncorrectVariableCase`);
  assert.strictEqual(errors[0].range.start.line, 6, `Index of 6 expected`);
  assert.strictEqual(errors[0].range.start.character, 0, `Index of 0 expected`);
  assert.strictEqual(errors[0].range.end.line, errors[0].range.start.line, `Should be on same line`);
  assert.strictEqual(errors[0].offset.position, 0, `Index of 0 expected`);
  assert.strictEqual(errors[0].offset.end, 11, `Should be index of 11`);
  assert.strictEqual(errors[0].newValue, `MyVariable2`, `Value of MyVariable2 expected`);

  assert.strictEqual(errors[1].type, `IncorrectVariableCase`, `Expect IncorrectVariableCase`);
  assert.strictEqual(errors[1].range.start.line, 10, `Index of 10 expected`);
  assert.strictEqual(errors[1].range.start.character, 4, `Index of 0 expected`);
  assert.strictEqual(errors[1].range.end.line, errors[1].range.start.line, `Should be on same line`);
  assert.strictEqual(errors[1].offset.position, 0, `Index of 0 expected`);
  assert.strictEqual(errors[1].offset.end, 11, `Should be index of 11`);
  assert.strictEqual(errors[1].newValue, `MyVariable2`, `Value of MyVariable2 expected`);
};

exports.linter6 = async () => {
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

  const parser = parserSetup();
  const cache = await parser.getDocs(uri, lines);
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    StringLiteralDupe: true
  }, cache);

  assert.strictEqual(errors.length, 2, `Expect length of 2`);

  assert.strictEqual(errors[0].type, `StringLiteralDupe`, `Expect StringLiteralDupe`);
  assert.strictEqual(errors[0].range.start.line, 13, `Index of 13 expected`);
  assert.strictEqual(errors[0].range.start.character, 4, `Index of 4 expected`);
  assert.strictEqual(errors[0].offset.position, 19, `Index of 19 expected`);
  assert.strictEqual(errors[0].offset.end, 27, `Index of 27 expected`);

  assert.strictEqual(errors[1].type, `StringLiteralDupe`, `Expect StringLiteralDupe`);
  assert.strictEqual(errors[1].range.start.line, 14, `Index of 14 expected`);
  assert.strictEqual(errors[1].range.start.character, 8, `Index of 8 expected`);
  assert.strictEqual(errors[1].offset.position, 14, `Index of 19 expected`);
  assert.strictEqual(errors[1].offset.end, 22, `Index of 22 expected`);
};

exports.linter6_lf = async () => {
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

  const parser = parserSetup();
  const cache = await parser.getDocs(uri, lines);
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    StringLiteralDupe: true,
    IncorrectVariableCase: true
  }, cache);

  assert.strictEqual(errors.length, 3, `Expect length of 3`);

  const line = new Range(new Position(4, 0), new Position(6, 18));

  assert.deepStrictEqual(errors[0], {
    range: line,
    offset: { position: 57, end: 69 },
    type: `IncorrectVariableCase`,
    newValue: `Myotherthing`,
  }, `Error not as expected`);

  assert.deepStrictEqual(errors[1], {
    range: line,
    offset: { position: 6, end: 21 },
    type: `StringLiteralDupe`,
    newValue: undefined,
  }, `Error not as expected`);

  assert.deepStrictEqual(errors[2], {
    range: line,
    offset: { position: 30, end: 45 },
    type: `StringLiteralDupe`,
    newValue: undefined,
  }, `Error not as expected`);
};

exports.linter6_crlf = async () => {
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

  //const lines = `**FREE\r\n\r\nDcl-S Myotherthing char(10);\r\n\r\ndsply 'hello friends'\r\n      + 'hello friends' + ''\r\n    + myotherthing;`;

  const parser = parserSetup();
  const cache = await parser.getDocs(uri, lines);
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    StringLiteralDupe: true,
    IncorrectVariableCase: true
  }, cache);

  assert.strictEqual(errors.length, 3, `Expect length of 3`);

  const line = new Range(new Position(4, 0), new Position(6, 18));

  assert.deepStrictEqual(errors[0], {
    range: line,
    offset: { position: 59, end: 71 },
    type: `IncorrectVariableCase`,
    newValue: `Myotherthing`,
  }, `Error not as expected`);

  assert.deepStrictEqual(errors[1], {
    range: line,
    offset: { position: 6, end: 21 },
    type: `StringLiteralDupe`,
    newValue: undefined,
  }, `Error not as expected`);

  assert.deepStrictEqual(errors[2], {
    range: line,
    offset: { position: 31, end: 46 },
    type: `StringLiteralDupe`,
    newValue: undefined,
  }, `Error not as expected`);
};

exports.linter7_casing1 = async () => {
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

  const parser = parserSetup();
  const cache = await parser.getDocs(uri, lines);
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    SpecificCasing: [
      { operation: `if`, expected: `If` },
      { operation: `endif`, expected: `Endif` },
      { operation: `select`, expected: `SELECT` },
    ]
  }, cache);

  assert.strictEqual(errors.length, 1, `Expect length of 1`);

  assert.deepStrictEqual(errors[0], {
    range: new Range(
      new Position(10, 2),
      new Position(10, 8),
    ),
    offset: { position: 0, end: 6 },
    type: `SpecificCasing`,
    newValue: `SELECT`
  }, `Error not as expected`);
};

exports.linter7_casing2 = async () => {
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

  const parser = parserSetup();
  const cache = await parser.getDocs(uri, lines);
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    SpecificCasing: [
      { operation: `ctl-opt`, expected: `Ctl-OPT` },
    ]
  }, cache);

  assert.strictEqual(errors.length, 1, `Expect length of 1`);

  assert.deepStrictEqual(errors[0], {
    range: new Range(
      new Position(2, 0),
      new Position(2, 22),
    ),
    offset: { position: 0, end: 7 },
    type: `SpecificCasing`,
    newValue: `Ctl-OPT`
  }, `Error not as expected`);
};

exports.linter7_casing3 = async () => {
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

  const parser = parserSetup();
  const cache = await parser.getDocs(uri, lines);
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    SpecificCasing: [
      { operation: `dcl-s`, expected: `DCL-S` },
    ]
  }, cache);

  assert.strictEqual(errors.length, 1, `Expect length of 1`);

  assert.deepStrictEqual(errors[0], {
    range: new Range(
      new Position(4, 0),
      new Position(4, 26),
    ),
    offset: { position: 0, end: 5 },
    type: `SpecificCasing`,
    newValue: `DCL-S`
  }, `Error not as expected`);
};

exports.linter7_casing4 = async () => {
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

  const parser = parserSetup();
  const cache = await parser.getDocs(uri, lines);
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    SpecificCasing: [
      { operation: `%trim`, expected: `%trim` },
    ]
  }, cache);

  assert.strictEqual(errors.length, 1, `Expect length of 1`);

  assert.deepStrictEqual(errors[0], {
    range: new Range(
      new Position(10, 2),
      new Position(10, 34),
    ),
    offset: { position: 14, end: 19 },
    type: `SpecificCasing`,
    newValue: `%trim`
  }, `Error not as expected`);
};

exports.linter7_casing5 = async () => {
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

  const parser = parserSetup();
  const cache = await parser.getDocs(uri, lines);
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    SpecificCasing: [
      { operation: `dcl-s`, expected: `Dcl-S` },
      { operation: `ctl-opt`, expected: `Ctl-Opt` },
    ]
  }, cache);

  assert.strictEqual(errors.length, 0, `Expect length of 0`);
};

exports.linter7_casing6 = async () => {
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

  const parser = parserSetup();
  const cache = await parser.getDocs(uri, lines);
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    SpecificCasing: [
      { operation: `*declare`, expected: `*upper` },
    ]
  }, cache);

  assert.strictEqual(errors.length, 3, `Expect length of 3`);
  assert.strictEqual(errors[0].newValue, `CTL-OPT`);
  assert.strictEqual(errors[1].newValue, `DCL-S`);
  assert.strictEqual(errors[2].newValue, `DCL-S`);
};

exports.linter7_casing7 = async () => {
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

  const parser = parserSetup();
  const cache = await parser.getDocs(uri, lines);
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    SpecificCasing: [
      { operation: `ctl-opt`, expected: `Ctl-Opt` },
      { operation: `*declare`, expected: `*upper` },
    ]
  }, cache);

  assert.strictEqual(errors.length, 2, `Expect length of 2`);
  assert.strictEqual(errors[0].newValue, `DCL-S`);
  assert.strictEqual(errors[1].newValue, `DCL-S`);
};

exports.linter7_casing8 = async () => {
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

  const parser = parserSetup();
  const cache = await parser.getDocs(uri, lines);
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    SpecificCasing: [
      { operation: `ctl-opt`, expected: `Ctl-Opt` },
      { operation: `*declare`, expected: `*LOWER` },
    ]
  }, cache);

  assert.strictEqual(errors.length, 2, `Expect length of 2`);
  assert.strictEqual(errors[0].newValue, `dcl-s`);
  assert.strictEqual(errors[1].newValue, `dcl-s`);
};

exports.linter7_casing9 = async () => {
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

  const parser = parserSetup();
  const cache = await parser.getDocs(uri, lines);
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    SpecificCasing: [
      { operation: `*bif`, expected: `*lower` },
    ]
  }, cache);

  assert.strictEqual(errors.length, 1, `Expect length of 1`);
  assert.strictEqual(errors[0].newValue, `%trim`);
};

exports.linter7_casing10 = async () => {
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

  const parser = parserSetup();
  const cache = await parser.getDocs(uri, lines);
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    IncorrectVariableCase: true
  }, cache);

  assert.strictEqual(errors.length, 1, `Expect length of 1`);
  assert.strictEqual(errors[0].range.start.line, 5);
  assert.strictEqual(errors[0].range.end.line, 14);
  assert.strictEqual(errors[0].offset.position, 120);
  assert.strictEqual(errors[0].offset.end, 130);
  assert.strictEqual(errors[0].newValue, `sFirstName`)
};

exports.linter7_casing11 = async () => {
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

  const parser = parserSetup();
  const cache = await parser.getDocs(uri, lines);
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    IncorrectVariableCase: true
  }, cache);

  assert.strictEqual(errors.length, 2);

  assert.deepStrictEqual(errors[0], {
    type: `IncorrectVariableCase`,
    range: new Range(
      new Position(5, 0),
      new Position(14, 26),
    ),
    offset: {
      position: 63,
      end: 69
    },
    newValue: `sEmpNo`
  });

  assert.deepStrictEqual(errors[1], {
    type: `IncorrectVariableCase`,
    range: new Range(
      new Position(5, 0),
      new Position(14, 26),
    ),
    offset: {
      position: 121,
      end: 131
    },
    newValue: `sFirstName`
  });
};

exports.linter7_casing12 = async () => {
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

  const parser = parserSetup();
  const cache = await parser.getDocs(uri, lines);
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    IncorrectVariableCase: true
  }, cache);

  assert.strictEqual(errors.length, 0);
}

exports.linter8 = async () => {
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

  const parser = parserSetup();
  const cache = await parser.getDocs(uri, lines);
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    RequiresParameter: true
  }, cache);

  assert.strictEqual(errors.length, 1, `Expect length of 1`);
  assert.deepStrictEqual(errors[0], {
    range: new Range(
      new Position(9, 4),
      new Position(9, 16),
    ),
    offset: { position: 3, end: 12 },
    type: `RequiresParameter`,
  }, `Error not as expected`);
};

exports.linter_Do_Not_Require_Parameters_For_Control_Options = async () => {
  const lines = [
    `**FREE`,
    `ctl-opt main(main) ;`,
    `dcl-proc main ;`,
    `  return ;`,
    `end-proc main ;`,
  ].join(`\n`);

  const parser = parserSetup();
  const cache = await parser.getDocs(uri, lines);
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    RequiresParameter: true
  }, cache);

  assert.strictEqual(errors.length, 0, `Unexpected RequiresParamters error`);
};

exports.linter_Do_Not_Require_Parameters_For_Compile_Directives = async () => {
  const lines = [
    `**FREE`,
    `/if defined(MYPROCEDURE);`,
    `/eof;`,
    `/endif;`,
    `/define ;`,
    `dcl-pr MYPROCEDURE;`,
    `end-pr;`,
  ].join(`\n`);

  const parser = parserSetup();
  const cache = await parser.getDocs(uri, lines);
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    RequiresParameter: true
  }, cache);

  assert.strictEqual(errors.length, 0, `Unexpected RequiresParamters error`);
};

/**
   * Check that local variables are not in global scope
   */
exports.linter9 = async () => {
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

  const parser = parserSetup();
  const cache = await parser.getDocs(uri, lines);
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    IncorrectVariableCase: true
  }, cache);

  assert.strictEqual(cache.variables.length, 1, `Expect length of 1`);
  assert.strictEqual(cache.constants.length, 1, `Expect length of 1`);
  assert.strictEqual(cache.procedures.length, 1, `Expect length of 1`);
  assert.strictEqual(cache.procedures[0].subItems.length, 1, `Expect length of 1`);

  assert.strictEqual(errors.length, 3, `Expect length of 3`);

  assert.deepStrictEqual(errors[0], {
    range: new Range(
      new Position(13, 2),
      new Position(13, 21),
    ),
    offset: { position: 0, end: 8 },
    type: `IncorrectVariableCase`,
    newValue: `localVar`
  }, `Error not as expected`);

  assert.deepStrictEqual(errors[1], {
    range: new Range(
      new Position(14, 2),
      new Position(14, 24),
    ),
    offset: { position: 0, end: 11 },
    type: `IncorrectVariableCase`,
    newValue: `MyVariable2`
  }, `Error not as expected`);

  assert.deepStrictEqual(errors[2], {
    range: new Range(
      new Position(14, 2),
      new Position(14, 24),
    ),
    offset: { position: 14, end: 22 },
    type: `IncorrectVariableCase`,
    newValue: `localVar`
  }, `Error not as expected`);
};

exports.linter10 = async () => {
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

  const parser = parserSetup();
  const cache = await parser.getDocs(uri, lines);
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    NoCTDATA: true
  }, cache);

  assert.strictEqual(errors.length, 2, `Expect length of 2`);

  assert.deepStrictEqual(errors[0], {
    range: new Range(
      new Position(2, 0),
      new Position(2, 38),
    ),
    type: `NoCTDATA`,
  }, `Error not as expected`);

  assert.deepStrictEqual(errors[1], {
    range: new Range(
      new Position(14, 0),
      new Position(14, 12),
    ),
    offset: { position: 0, end: 8 },
    type: `NoCTDATA`,
  }, `Error not as expected`);
};

exports.linter11 = async () => {
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

  const parser = parserSetup();
  const cache = await parser.getDocs(uri, lines);
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    StringLiteralDupe: true
  }, cache);

  assert.strictEqual(errors.length, 2, `Expect length of 2`);

  const line = new Range(new Position(5, 0), new Position(7, 12));

  assert.deepStrictEqual(errors[0], {
    range: line,
    offset: { position: 6, end: 21 },
    type: `StringLiteralDupe`,
    newValue: `HELLO`,
  }, `Error not as expected`);

  assert.deepStrictEqual(errors[1], {
    range: line,
    offset: { position: 30, end: 45 },
    type: `StringLiteralDupe`,
    newValue: `HELLO`,
  }, `Error not as expected`);
};

exports.linter12 = async () => {
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

  const parser = parserSetup();
  const cache = await parser.getDocs(uri, lines);
  const { indentErrors } = Linter.getErrors({ uri, content: lines }, {
    indent: 2
  }, cache);

  assert.strictEqual(indentErrors.length, 0, `Expect length of 0`);
};

exports.linter13 = async () => {
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

  const parser = parserSetup();
  const cache = await parser.getDocs(uri, lines);
  const { indentErrors } = Linter.getErrors({ uri, content: lines }, {
    indent: 2
  }, cache);

  assert.strictEqual(indentErrors.length, 0, `Expect length of 0`);
};

exports.linter13_commentIndent = async () => {
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

  const parser = parserSetup();
  const cache = await parser.getDocs(uri, lines);
  const { indentErrors } = Linter.getErrors({ uri, content: lines }, {
    PrettyComments: true,
    indent: 2
  }, cache);

  assert.strictEqual(indentErrors.length, 3, `Expect length of 3`);

  assert.deepStrictEqual(indentErrors[0], {
    currentIndent: 2,
    expectedIndent: 0,
    line: 9
  });

  assert.deepStrictEqual(indentErrors[1], {
    currentIndent: 0,
    expectedIndent: 2,
    line: 15
  });

  assert.deepStrictEqual(indentErrors[2], {
    currentIndent: 6,
    expectedIndent: 2,
    line: 20
  });
};

exports.linter14 = async () => {
  const lines = [
    `**FREE`,
    `//--------------------------------------------------------------------------------------------------`,
    `// Append a single quote. This procedure exists to make other code more readable.`,
    `//--------------------------------------------------------------------------------------------------`,
    `DCL-PROC Q;`,
    `  DCL-PI Q VARCHAR(2048);`,
    `    in_str VARCHAR(2048) CONST OPTIONS(*TRIM);`,
    `  END-PI;`,
    `  `,
    `  DCL-C C_QUOTE '''';`,
    `  DCL-S is_abend IND INZ(*OFF);`,
    `  DCL-S return_str LIKE(in_str);`,
    `  `,
    `  return_str = %TRIM(C_QUOTE + in_str + C_QUOTE);`,
    `  RETURN return_str;`,
    `  // End of procedure`,
    `  `,
    `  ON-EXIT is_abend;`,
    `    // Exit handler`,
    `    IF is_abend;`,
    `      return_str = 'This is a string';`,
    `    ENDIF;`,
    `END-PROC Q;`,
    ``,
    `// New procedure`,
    `Dcl-Proc theProcedure;`,
    `  Dcl-Pi *N;`,
    `    newValue Char(20);`,
    `  End-Pi;`,
    `  // comment with right indent`,
    `  Dcl-S localVar Char(20);`,
    `  localvar = newValue;`,
    `  // but valid indent`,
    `  // with another line`,
    `  Myvariable2 = localvar;`,
    `End-Proc;`,
  ].join(`\n`);

  const parser = parserSetup();
  const cache = await parser.getDocs(uri, lines);
  const { indentErrors } = Linter.getErrors({ uri, content: lines }, {
    indent: 2
  }, cache);

  assert.strictEqual(indentErrors.length, 0, `Expect length of 0`);
};

exports.linter15 = async () => {
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

  const parser = parserSetup();
  const cache = await parser.getDocs(uri, lines);
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    PrettyComments: true
  }, cache);

  assert.strictEqual(errors.length, 5, `Expect length of 5`);

  assert.deepStrictEqual(errors[0], {
    type: `PrettyComments`,
    newValue: ``,
    range: new Range(
      new Position(4, 0),
      new Position(4, 2),
    ),
  });

  assert.deepStrictEqual(errors[1], {
    type: `PrettyComments`,
    newValue: `// `,
    range: new Range(
      new Position(5, 0),
      new Position(5, 2),
    ),
  });

  assert.deepStrictEqual(errors[2], {
    type: `PrettyComments`,
    newValue: ``,
    range: new Range(
      new Position(6, 0),
      new Position(6, 2),
    ),
  });

  assert.deepStrictEqual(errors[3], {
    type: `PrettyComments`,
    newValue: `// `,
    range: new Range(
      new Position(14, 2),
      new Position(14, 4),
    ),
  });

  assert.deepStrictEqual(errors[4], {
    type: `PrettyComments`,
    newValue: ``,
    range: new Range(
      new Position(16, 2),
      new Position(16, 4),
    ),
  });
};

/**
   * Subroutine check test
   * */
exports.linter16 = async () => {
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

  const parser = parserSetup();
  const cache = await parser.getDocs(uri, lines);
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    NoGlobalSubroutines: true
  }, cache);

  assert.strictEqual(errors.length, 3, `Expect length of 3`);

  assert.deepStrictEqual(errors[0], {
    type: `NoGlobalSubroutines`,
    newValue: `theSubroutine()`,
    range: new Range(
      new Position(3, 0),
      new Position(3, 18),
    ),
  });

  assert.deepStrictEqual(errors[1], {
    type: `NoGlobalSubroutines`,
    newValue: `Dcl-Proc`,
    range: new Range(
      new Position(6, 0),
      new Position(6, 19),
    ),
    offset: {
      position: 0,
      end: 5
    }
  });

  assert.deepStrictEqual(errors[2], {
    type: `NoGlobalSubroutines`,
    newValue: `End-Proc`,
    range: new Range(
      new Position(8, 0),
      new Position(8, 5),
    ),
    offset: {
      position: 0,
      end: 5
    }
  });
};

exports.linter16_with_leavesr = async () => {
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

  const parser = parserSetup();
  const cache = await parser.getDocs(uri, lines);
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    NoGlobalSubroutines: true
  }, cache);

  assert.strictEqual(errors.length, 4, `Expect length of 3`);

  assert.deepStrictEqual(errors[0], {
    type: `NoGlobalSubroutines`,
    newValue: `theSubroutine()`,
    range: new Range(
      new Position(5, 0),
      new Position(5, 18),
    ),
  });

  assert.deepStrictEqual(errors[1], {
    type: `NoGlobalSubroutines`,
    newValue: `Dcl-Proc`,
    range: new Range(
      new Position(8, 0),
      new Position(8, 19),
    ),
    offset: {
      position: 0,
      end: 5
    }
  });

  assert.deepStrictEqual(errors[2], {
    type: `NoGlobalSubroutines`,
    newValue: `return`,
    range: new Range(
      new Position(10, 4),
      new Position(10, 11),
    ),
  });

  assert.deepStrictEqual(errors[3], {
    type: `NoGlobalSubroutines`,
    newValue: `End-Proc`,
    range: new Range(
      new Position(13, 0),
      new Position(13, 5),
    ),
    offset: {
      position: 0,
      end: 5
    }
  });
};

/**
   * Subroutine in procedure check test
   * */
exports.linter17 = async () => {
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

  const parser = parserSetup();
  const cache = await parser.getDocs(uri, lines);
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    NoLocalSubroutines: true
  }, cache);

  assert.strictEqual(errors.length, 1, `Expect length of 1`);

  assert.deepStrictEqual(errors[0], {
    type: `NoLocalSubroutines`,
    range: new Range(
      new Position(8, 2),
      new Position(8, 21),
    ),
  });
};

exports.linter18 = async () => {
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

  const parser = parserSetup();
  const cache = await parser.getDocs(uri, lines);
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    NoGlobalsInProcedures: true
  }, cache);

  assert.strictEqual(errors.length, 2, `Expect length of 2`);

  assert.deepStrictEqual(errors[0], {
    type: `NoGlobalsInProcedures`,
    range: new Range(
      new Position(8, 2),
      new Position(8, 29),
    ),
    offset: {
      position: 0,
      end: 11
    },
  });

  assert.deepStrictEqual(errors[1], {
    type: `NoGlobalsInProcedures`,
    range: new Range(
      new Position(9, 2),
      new Position(9, 23),
    ),
    offset: {
      position: 10,
      end: 21
    },
  });
}

exports.linter19 = async () => {
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

  const parser = parserSetup();
  const cache = await parser.getDocs(uri, lines);
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    NoUnreferenced: true
  }, cache);

  assert.strictEqual(errors.length, 11);

  assert.deepStrictEqual(errors[0], {
    type: `NoUnreferenced`,
    range: new Range(
      new Position(4, 0),
      new Position(4, 100),
    ),
  });

  assert.deepStrictEqual(errors[1], {
    type: `NoUnreferenced`,
    range: new Range(
      new Position(68, 0),
      new Position(68, 100),
    ),
  });

  assert.deepStrictEqual(errors[2], {
    type: `NoUnreferenced`,
    range: new Range(
      new Position(11, 0),
      new Position(11, 100),
    ),
  });

  assert.deepStrictEqual(errors[3], {
    type: `NoUnreferenced`,
    range: new Range(
      new Position(10, 0),
      new Position(10, 100),
    ),
  });

  assert.deepStrictEqual(errors[4], {
    type: `NoUnreferenced`,
    range: new Range(
      new Position(23, 0),
      new Position(23, 100),
    ),
  });

  assert.deepStrictEqual(errors[5], {
    type: `NoUnreferenced`,
    range: new Range(
      new Position(22, 0),
      new Position(22, 100),
    ),
  });

  assert.deepStrictEqual(errors[6], {
    type: `NoUnreferenced`,
    range: new Range(
      new Position(38, 0),
      new Position(38, 100),
    ),
  });

  assert.deepStrictEqual(errors[7], {
    type: `NoUnreferenced`,
    range: new Range(
      new Position(49, 0),
      new Position(49, 100),
    ),
  });

  assert.deepStrictEqual(errors[8], {
    type: `NoUnreferenced`,
    range: new Range(
      new Position(48, 0),
      new Position(48, 100),
    ),
  });

  assert.deepStrictEqual(errors[9], {
    type: `NoUnreferenced`,
    range: new Range(
      new Position(57, 0),
      new Position(57, 100),
    ),
  });

  assert.deepStrictEqual(errors[10], {
    type: `NoUnreferenced`,
    range: new Range(
      new Position(56, 0),
      new Position(56, 100),
    ),
  });
}

exports.linter20 = async () => {
  const lines = [
    `**FREE`,
    ``,
    `Dcl-s MyVariable2 Char(20);  `,
    ``,
    `EXEC SQL`,
    `    FETCH NEXT FROM empCur       `,
    `    INTO :myvariable2;`,
  ].join(`\n`);

  const parser = parserSetup();
  const cache = await parser.getDocs(uri, lines);
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    IncorrectVariableCase: true
  }, cache);

  assert.strictEqual(errors.length, 1);

  assert.strictEqual(errors[0].type, `IncorrectVariableCase`, `Expect IncorrectVariableCase`);
  assert.strictEqual(errors[0].range.start.line, 4);
  assert.strictEqual(errors[0].range.end.line, 6);
  assert.strictEqual(errors[0].range.start.character, 0);
  assert.strictEqual(errors[0].offset.position, 53);
  assert.strictEqual(errors[0].offset.end, 64);
  assert.strictEqual(errors[0].newValue, `MyVariable2`, `Value of MyVariable2 expected`);
};

exports.linter21 = async () => {
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

  const parser = parserSetup();
  const cache = await parser.getDocs(uri, lines);
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    NoUnreferenced: true
  }, cache);

  assert.strictEqual(errors.length, 1);
  assert.deepStrictEqual(errors[0], {
    type: `NoUnreferenced`,
    range: new Range(
      new Position(20, 0),
      new Position(20, 100),
    ),
  });
}

exports.linter22 = async () => {
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

  const parser = parserSetup();
  const cache = await parser.getDocs(uri, lines);
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    PrototypeCheck: true
  }, cache);

  assert.strictEqual(errors.length, 1);
  assert.deepStrictEqual(errors[0], {
    type: `PrototypeCheck`,
    range: new Range(
      new Position(2, 0),
      new Position(2, 19),
    ),
  });
}

exports.linter22_b = async () => {
  const lines = [
    `**FREE`,
    ``,
    `Dcl-Pr TheProcedure EXTPROC;`,
    `  parmA CHAR(20);`,
    `End-Pr`,
    ``,
    `Dcl-S theVar CHAR(20);`,
  ].join(`\n`);

  const parser = parserSetup();
  const cache = await parser.getDocs(uri, lines);
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    PrototypeCheck: true
  }, cache);

  assert.strictEqual(errors.length, 0);
}

exports.linter23 = async () => {
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

  const parser = parserSetup();
  const cache = await parser.getDocs(uri, lines);
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    NoUnreferenced: true
  }, cache);

  assert.strictEqual(errors.length, 0);
}

exports.linter24 = async () => {
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

  const parser = parserSetup();
  const cache = await parser.getDocs(uri, lines);
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    NoExternalTo: [
      `QSYGETPH`
    ]
  }, cache);

  assert.strictEqual(errors.length, 1);
  assert.deepStrictEqual(errors[0], {
    type: `NoExternalTo`,
    range: new Range(
      new Position(8, 0),
      new Position(8, 100),
    ),
  });
}

exports.linter25 = async () => {
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
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    NoExternalTo: [
      `QSYGETPH`,
      `QSYRLSPH`
    ]
  }, cache);

  assert.strictEqual(errors.length, 2);

  assert.deepStrictEqual(errors[0], {
    type: `NoExternalTo`,
    range: new Range(
      new Position(14, 0),
      new Position(14, 100),
    ),
  });

  assert.deepStrictEqual(errors[1], {
    type: `NoExternalTo`,
    range: new Range(
      new Position(23, 0),
      new Position(23, 100),
    ),
  });
}

exports.linter26 = async () => {
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

  const parser = parserSetup();
  const cache = await parser.getDocs(uri, lines);
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    NoUnreferenced: true
  }, cache);

  assert.strictEqual(errors.length, 0);
}

exports.linter27 = async () => {
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
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    NoExecuteImmediate: true
  }, cache);

  assert.strictEqual(errors.length, 1);
  assert.deepStrictEqual(errors[0], {
    type: `NoExecuteImmediate`,
    range: new Range(
      new Position(7, 0),
      new Position(8, 34),
    ),
  });
}

exports.linter28 = async () => {
  const lines = [
    `**free`,
    `Dcl-Pr APGM extpgm(myvarNotused);`,
    `End-Pr;`,
    `Dcl-Pr OTHERPGM extproc(*cwiden:myvarNotused);`,
    `End-Pr;`,
    `Dcl-Pr OTHERPGM extpgm('realString');`,
    `End-Pr;`,
  ].join(`\n`);

  const parser = parserSetup();
  const cache = await parser.getDocs(uri, lines);
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    NoExtProgramVariable: true
  }, cache);

  assert.strictEqual(errors.length, 2);

  assert.deepStrictEqual(errors[0], {
    type: `NoExtProgramVariable`,
    range: new Range(
      new Position(1, 0),
      new Position(1, 32),
    ),
    offset: {
      position: 19,
      end: 31
    }
  });

  assert.deepStrictEqual(errors[1], {
    type: `NoExtProgramVariable`,
    range: new Range(
      new Position(3, 0),
      new Position(3, 45),
    ),
    offset: {
      position: 32,
      end: 44
    }
  });
}

exports.linter29 = async () => {
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

  const parser = parserSetup();
  const cache = await parser.getDocs(uri, lines);
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    IncludeMustBeRelative: true
  }, cache);

  assert.strictEqual(cache.includes.length, 1);
  assert.strictEqual(cache.includes[0].line, 4);

  assert.strictEqual(cache.variables.length, 1, `Expect length of 1`);
  assert.strictEqual(cache.constants.length, 1, `Expect length of 1`);
  assert.strictEqual(cache.procedures.length, 1, `Expect length of 1`);
  assert.strictEqual(cache.procedures[0].subItems.length, 1, `Expect length of 1`);

  const baseNameInclude = path.basename(cache.procedures[0].position.path);
  assert.strictEqual(baseNameInclude, `copy1.rpgle`, `Path is incorrect`);
  assert.strictEqual(cache.procedures[0].position.line, 2, `Index of 3 expected`);

  assert.strictEqual(errors.length, 0);
}

exports.linter30 = async () => {
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

  const parser = parserSetup();
  const cache = await parser.getDocs(uri, lines);
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    IncludeMustBeRelative: true
  }, cache);

  assert.strictEqual(errors.length, 1);

  assert.deepStrictEqual(errors[0], {
    type: `IncludeMustBeRelative`,
    range: new Range(
      new Position(4, 0),
      new Position(4, 11),
    ),
    offset: {
      position: 6,
      end: 11
    },
    newValue: undefined
  });
}

exports.linter31 = async () => {
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

  const parser = parserSetup();
  const cache = await parser.getDocs(uri, lines);
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    IncludeMustBeRelative: true
  }, cache);

  assert.strictEqual(cache.includes.length, 1);
  assert.strictEqual(cache.includes[0].line, 4);

  assert.strictEqual(errors.length, 1);

  assert.deepStrictEqual(errors[0], {
    type: `IncludeMustBeRelative`,
    range: new Range(
      new Position(4, 0),
      new Position(4, 17),
    )
  });
}

exports.linter32 = async () => {
  const lines = [
    `**FREE`,
    ``,
    `Ctl-Opt DftActGrp(*No);`,
    ``,
    `/copy 'tests/rpgle/copy1.rpgle'`,
    ``,
    `Dcl-s MyVariable2 Char(20);`,
    ``,
    `Dcl-C theConstant 'Hello world';`,
    ``,
    `CallP theExtProcedure(myVariable);`,
    ``,
    `Return;`
  ].join(`\n`);

  const parser = parserSetup();
  const cache = await parser.getDocs(uri, lines);
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    IncludeMustBeRelative: true
  }, cache);

  assert.strictEqual(cache.includes.length, 1);
  assert.strictEqual(cache.includes[0].line, 4);

  assert.strictEqual(errors.length, 0);
}

exports.linter32_b = async () => {
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

  const parser = parserSetup();
  const cache = await parser.getDocs(uri, lines);
  const { errors } = Linter.getErrors({
    uri,
    content: lines,
    availableIncludes: [`tests/rpgle/copy1.rpgle`]
  }, {
    IncludeMustBeRelative: true
  }, cache);

  assert.strictEqual(cache.includes.length, 1);
  assert.strictEqual(cache.includes[0].line, 4);

  assert.strictEqual(errors.length, 1);

  assert.deepStrictEqual(errors[0], {
    type: `IncludeMustBeRelative`,
    range: new Range(
      new Position(4, 0),
      new Position(4, 19),
    ),
    offset: {
      position: 6,
      end: 19
    },
    newValue: `'tests/rpgle/copy1.rpgle'`
  });
}

exports.linter33 = async () => {
  const lines = [
    `**FREE`,
    ``,
    `Ctl-Opt DftActGrp(*No);`,
    ``,
    `/copy '/tests/rpgle/copy1.rpgle'`,
    ``,
    `Dcl-s MyVariable2 Char(20);`,
    ``,
    `Dcl-C theConstant 'Hello world';`,
    ``,
    `CallP theExtProcedure(myVariable);`,
    ``,
    `Return;`
  ].join(`\n`);

  const parser = parserSetup();
  const cache = await parser.getDocs(uri, lines);
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    IncludeMustBeRelative: true
  }, cache);

  assert.strictEqual(cache.includes.length, 1);
  assert.strictEqual(cache.includes[0].line, 4);

  assert.strictEqual(errors.length, 1);

  assert.deepStrictEqual(errors[0], {
    type: `IncludeMustBeRelative`,
    range: new Range(
      new Position(4, 0),
      new Position(4, 32),
    ),
    offset: {
      position: 6,
      end: 32
    }
  });
}

exports.linter34 = async () => {
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

  const parser = parserSetup();
  const cache = await parser.getDocs(uri, lines);
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    SQLHostVarCheck: true
  }, cache);

  assert.strictEqual(errors.length, 1);

  assert.deepStrictEqual(errors[0], {
    type: `SQLHostVarCheck`,
    range: new Range(
      new Position(7, 0),
      new Position(10, 28),
    ),
    offset: {
      position: 117,
      end: 124
    },
    newValue: `:Deptnum`
  });
}

exports.linter35 = async () => {
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

  const parser = parserSetup();
  const cache = await parser.getDocs(uri, lines);
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    ForceOptionalParens: true
  }, cache);

  assert.strictEqual(errors.length, 0);
}

exports.linter36 = async () => {
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

  const parser = parserSetup();
  const cache = await parser.getDocs(uri, lines);
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    ForceOptionalParens: true
  }, cache);

  assert.strictEqual(errors.length, 0);
}

exports.linter37 = async () => {
  const lines = [
    `**FREE`,
    ``,
    `Ctl-Opt DftActGrp(*No);`,
    ``,
    `/copy 'tests/rpgle/copy1.rpgle'`,
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

  const parser = parserSetup();
  const cache = await parser.getDocs(uri, lines);
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    UselessOperationCheck: true
  }, cache);

  assert.strictEqual(cache.includes.length, 1);
  assert.strictEqual(cache.includes[0].line, 4);

  assert.strictEqual(errors.length, 1);

  assert.deepStrictEqual(errors[0], {
    type: `UselessOperationCheck`,
    range: new Range(
      new Position(10, 0),
      new Position(10, 33),
    ),
    offset: {
      position: 0,
      end: 6
    }
  });
}

exports.linter38_subrefs = async () => {
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

  const parser = parserSetup();
  const cache = await parser.getDocs(uri, lines);
  Linter.getErrors({ uri, content: lines }, {
    CollectReferences: true,
  }, cache);

  const subfa = cache.find(`subfa`);
  assert.strictEqual(subfa.references.length, 1);
  assert.deepStrictEqual(subfa.references[0], {
    range: Range.create(33, 0, 33, 14),
    offset: {
      position: 0,
      end: 5
    }
  });

  const structYesAlso = cache.find(`structYesAlso`);
  assert.strictEqual(structYesAlso.references.length, 1);
  assert.deepStrictEqual(structYesAlso.references[0], {
    range: Range.create(34, 0, 34, 28),
    offset: {
      position: 0,
      end: 13
    }
  });

  const subfc = structYesAlso.subItems[0];
  assert.strictEqual(subfc.name, `subfc`);
  assert.strictEqual(subfc.references.length, 0);

  const qualStructYes = cache.find(`qualStructYes`);
  assert.strictEqual(qualStructYes.references.length, 1);
  assert.deepStrictEqual(qualStructYes.references[0], {
    range: Range.create(36, 0, 36, 26),
    offset: {
      position: 0,
      end: 13
    }
  });

  const qualsubA = qualStructYes.subItems[0];
  assert.strictEqual(qualsubA.name, `qualsubA`);
  assert.strictEqual(qualsubA.references.length, 1);
  assert.deepStrictEqual(qualsubA.references[0], {
    range: Range.create(36, 0, 36, 26),
    offset: {
      position: 14,
      end: 22
    }
  });

  const procYes = cache.find(`procYes`);
  const subProc = procYes.scope;

  const localStructYes = subProc.find(`localStructYes`);
  assert.strictEqual(localStructYes.references.length, 1);
  assert.deepStrictEqual(localStructYes.references[0], {
    range: Range.create(69, 4, 69, 33),
    offset: {
      position: 0,
      end: 14
    }
  });

  const localStructAlsoYes = subProc.find(`localStructAlsoYes`);
  assert.strictEqual(localStructAlsoYes.references.length, 0);

  const subfe = localStructAlsoYes.subItems[0];
  assert.strictEqual(subfe.name, `subfe`);
  assert.strictEqual(subfe.references.length, 1);
  assert.deepStrictEqual(subfe.references[0], {
    range: Range.create(70, 4, 70, 24),
    offset: {
      position: 0,
      end: 5
    }
  });

  const qualDimStructYup = cache.find(`qualDimStructYup`);
  assert.strictEqual(qualDimStructYup.references.length, 3)

  assert.deepStrictEqual(qualDimStructYup.references[0], {
    range: Range.create(38, 0, 38, 31),
    offset: {
      position: 0,
      end: 16
    }
  });

  assert.deepStrictEqual(qualDimStructYup.references[1], {
    range: Range.create(39, 0, 39, 45),
    offset: {
      position: 0,
      end: 16
    }
  });

  assert.deepStrictEqual(qualDimStructYup.references[2], {
    range: Range.create(40, 0, 40, 49),
    offset: {
      position: 0,
      end: 16
    }
  });

  const boopABC = qualDimStructYup.subItems[0];
  assert.strictEqual(boopABC.name, `boopABC`);
  assert.strictEqual(boopABC.references.length, 3);

  assert.deepStrictEqual(boopABC.references[0], {
    range: Range.create(38, 0, 38, 31),
    offset: {
      position: 20,
      end: 27
    }
  });

  assert.deepStrictEqual(boopABC.references[1], {
    range: Range.create(39, 0, 39, 45),
    offset: {
      position: 34,
      end: 41
    }
  });

  assert.deepStrictEqual(boopABC.references[2], {
    range: Range.create(40, 0, 40, 49),
    offset: {
      position: 38,
      end: 45
    }
  });
}

exports.linter39 = async () => {
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

  const parser = parserSetup();
  const cache = await parser.getDocs(uri, lines);
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    RequiresProcedureDescription: true
  }, cache);

  assert.strictEqual(errors.length, 1);
  assert.deepStrictEqual(errors[0], {
    range: Range.create(2, 0, 2, 26),
    type: `RequiresProcedureDescription`
  });
};

exports.linter40 = async () => {
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

  const parser = parserSetup();
  const cache = await parser.getDocs(uri, lines);
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    RequiresProcedureDescription: true
  }, cache);

  assert.strictEqual(errors.length, 0);
};

exports.linter40_return = async () => {
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

  const parser = parserSetup();
  const cache = await parser.getDocs(uri, lines);
  Linter.getErrors({ uri, content: lines }, {
    CollectReferences: true,
  }, cache);

  const procedure = cache.find(`InputIsValid`);
  const validationResult = procedure.scope.find(`validationResult`);

  assert.strictEqual(validationResult.references.length, 6);
}

exports.linter41 = async () => {
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
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    RequireBlankSpecial: true,
    StringLiteralDupe: true
  }, cache);

  assert.strictEqual(errors.length, 3);

  assert.deepStrictEqual(errors[0], {
    range: Range.create(5, 0, 5, 8),
    offset: {
      position: 6,
      end: 8
    },
    type: `RequireBlankSpecial`,
    newValue: `*BLANK`
  });

  assert.deepStrictEqual(errors[1], {
    range: Range.create(4, 0, 4, 11),
    offset: {
      position: 6,
      end: 11
    },
    type: `StringLiteralDupe`,
    newValue: undefined
  });

  assert.deepStrictEqual(errors[2], {
    range: Range.create(6, 0, 6, 11),
    offset: {
      position: 6,
      end: 11
    },
    type: `StringLiteralDupe`,
    newValue: undefined
  });
}

exports.linter42 = async () => {
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

  const parser = parserSetup();
  const cache = await parser.getDocs(uri, lines);
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    RequireOtherBlock: true
  }, cache);

  assert.strictEqual(errors.length, 1);
  assert.deepStrictEqual(errors[0], {
    range: Range.create(11, 2, 11, 8),
    type: `RequireOtherBlock`
  });
};

exports.linter43 = async () => {
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

  const parser = parserSetup();
  const cache = await parser.getDocs(uri, lines);
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    RequireOtherBlock: true
  }, cache);

  assert.strictEqual(errors.length, 0);
};

exports.linter44 = async () => {
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

  const parser = parserSetup();
  const cache = await parser.getDocs(uri, lines);
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    RequireOtherBlock: true
  }, cache);

  assert.strictEqual(errors.length, 2);
  assert.deepStrictEqual(errors[0], {
    range: Range.create(18, 6, 18, 12),
    type: `RequireOtherBlock`
  });
  assert.deepStrictEqual(errors[1], {
    range: Range.create(11, 2, 11, 8),
    type: `RequireOtherBlock`
  });
};

exports.issue_170 = async () => {
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

  const parser = parserSetup();
  const cache = await parser.getDocs(uri, lines);
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    NoUnreferenced: true
  }, cache);

  assert.deepStrictEqual(errors.length, 0);
};

exports.issue_170a = async () => {
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


  const parser = parserSetup();
  const cache = await parser.getDocs(uri, lines);
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    NoUnreferenced: true
  }, cache);

  assert.strictEqual(cache.structs.length, 1);

  const SBM_DS = cache.find(`SBM_DS`);
  assert.strictEqual(SBM_DS.name, `SBM_DS`);
  assert.strictEqual(SBM_DS.subItems.length, 1);
  assert.strictEqual(SBM_DS.position.line, 2);
  assert.strictEqual(SBM_DS.references.length, 0);

  const Move1 = SBM_DS.subItems[0];
  assert.strictEqual(Move1.name, `Move1`);
  assert.strictEqual(Object.keys(Move1.keyword).length, 2);
  assert.strictEqual(Move1.position.line, 3);
  assert.strictEqual(Move1.references.length, 0);
  
  assert.deepStrictEqual(errors.length, 2);
}

exports.linter40_keywordrefs = async () => {
  const lines = [
    `**free`,
    `Dcl-C  RANDOMLEN    286;`,
    `Dcl-s  somevar      Int(10) inz(randomLen);`,
  ].join(`\n`);

  const parser = parserSetup();
  const cache = await parser.getDocs(uri, lines);
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    CollectReferences: true,
    IncorrectVariableCase: true
  }, cache);

  const RANDOMLEN = cache.find(`RANDOMLEN`);

  assert.strictEqual(RANDOMLEN.references.length, 1);
  assert.deepStrictEqual(RANDOMLEN.references[0], {
    range: Range.create(2, 0, 2, 42),
    offset: { position: 32, end: 41 },
  });

  assert.strictEqual(errors.length, 1);
  assert.deepStrictEqual(errors[0], {
    range: Range.create(2, 0, 2, 42),
    offset: { position: 32, end: 41 },
    type: `IncorrectVariableCase`,
    newValue: `RANDOMLEN`,
  });
}

exports.issue_175 = async () => {
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

  const parser = parserSetup();
  const cache = await parser.getDocs(uri, lines);
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    NoUnreferenced: true
  }, cache);

  assert.deepStrictEqual(errors.length, 0);
};

exports.issue180 = async () => {
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
  
  const parser = await parserSetup();
  const cache = await parser.getDocs(uri, lines);

  Linter.getErrors({ uri, content: lines }, {
    CollectReferences: true,
  }, cache);
}

exports.dcl_subf_issue184 = async () => {
  const lines = [
    `**FREE`,
    ``,
    `Ctl-Opt DftActGrp(*No);`,
    ``,
    `DCL-DS *N;`,
    `   DCL-SUBF select CHAR(10);`,
    `   name CHAR(10);`,
    `   DCL-SUBF address CHAR(25);`,
    `END-DS;`,
    ``,
    `Return;`
  ].join(`\n`);

  const parser = parserSetup();
  const cache = await parser.getDocs(uri, lines);
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    UselessOperationCheck: true
  }, cache);

  assert.strictEqual(errors.length, 1);

  assert.deepStrictEqual(errors[0], {
    type: `UselessOperationCheck`,
    range: new Range(
      new Position(7, 3),
      new Position(7, 28),
    ),
    offset: {
      position: 0,
      end: 8
    }
  });

  const selectVar = cache.find(`select`);
  assert.strictEqual(selectVar.name, `select`);

  const nameVar = cache.find(`name`);
  assert.strictEqual(nameVar.name, `name`);

  const addressVar = cache.find(`address`);
  assert.strictEqual(addressVar.name, `address`);
}

exports.dcl_parm_issue184 = async () => {
  const lines = [
    `**FREE`,
    ``,
    `Ctl-Opt DftActGrp(*No);`,
    ``,
    `DCL-PR myProc;`,
    `   DCL-PARM select CHAR(10);`,
    `   name CHAR(10);`,
    `   DCL-PARM address CHAR(25);`,
    `END-PR;`,
    ``,
    `Return;`
  ].join(`\n`);

  const parser = parserSetup();
  const cache = await parser.getDocs(uri, lines);
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    UselessOperationCheck: true
  }, cache);

  assert.strictEqual(errors.length, 1);

  assert.deepStrictEqual(errors[0], {
    type: `UselessOperationCheck`,
    range: new Range(
      new Position(7, 3),
      new Position(7, 28),
    ),
    offset: {
      position: 0,
      end: 8
    }
  });

  const myProc = cache.find(`myProc`);
  assert.strictEqual(myProc.name, `myProc`);

  const parms = myProc.subItems;
  assert.strictEqual(parms[0].name, `select`);
  assert.strictEqual(parms[1].name, `name`);
  assert.strictEqual(parms[2].name, `address`);
}