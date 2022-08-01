
const vscode = require(`vscode`);
const assert = require(`assert`);

const Parser = require(`../../src/language/parser`);
const Linter = require(`../../src/language/linter`);

const uri = vscode.Uri.parse(`source.rpgle`);

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
 
  const parser = new Parser();
  const cache = await parser.getDocs(uri, lines);
  const { indentErrors } = Linter.getErrors({uri, content: lines}, {
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
 
  const parser = new Parser();
  const cache = await parser.getDocs(uri, lines);
  const { indentErrors } = Linter.getErrors({uri, content: lines}, {
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

  const parser = new Parser();
  const cache = await parser.getDocs(uri, lines);
  const { indentErrors } = Linter.getErrors({uri, content: lines}, {
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
exports.linter1_1_indent =  async () => {
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

  const parser = new Parser();
  const cache = await parser.getDocs(uri, lines);
  const { indentErrors } = Linter.getErrors({uri, content: lines}, {
    indent: 2
  }, cache);

  assert.strictEqual(indentErrors.length, 0, `Expect length of 0`);
};

exports.linter2_indent =  async () => {
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

  const parser = new Parser();
  const cache = await parser.getDocs(uri, lines);
  const { indentErrors } = Linter.getErrors({uri, content: lines}, {
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

exports.linter3_indent =  async () => {
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

  const parser = new Parser();
  const cache = await parser.getDocs(uri, lines);
  const { indentErrors } = Linter.getErrors({uri, content: lines}, {
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

exports.linter4 =  async () => {
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

  const parser = new Parser();
  const cache = await parser.getDocs(uri, lines);
  const { errors } = Linter.getErrors({uri, content: lines}, {
    RequireBlankSpecial: true
  }, cache);

  assert.strictEqual(errors.length, 2, `Expect length of 2`);

  assert.strictEqual(errors[0].type, `RequireBlankSpecial`, `Expect RequireBlankSpecial`);
  assert.strictEqual(errors[0].range.start.line, 6, `Index of 6 expected`);
  assert.strictEqual(errors[0].offset.position, 14, `Index of 14 expected`);
  assert.strictEqual(errors[0].offset.length, 16, `Index of 16 expected`);
  assert.strictEqual(errors[0].newValue, `*BLANK`, `Value of *BLANK expected`);

  assert.strictEqual(errors[1].type, `RequireBlankSpecial`, `Expect RequireBlankSpecial`);
  assert.strictEqual(errors[1].range.start.line, 8, `Index of 8 expected`);
  assert.strictEqual(errors[1].offset.position, 17, `Index of 17 expected`);
  assert.strictEqual(errors[1].offset.length, 19, `Index of 19 expected`);
  assert.strictEqual(errors[1].newValue, `*BLANK`, `Value of *BLANK expected`);
};

exports.linter5 =  async () => {
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

  const parser = new Parser();
  const cache = await parser.getDocs(uri, lines);
  const { errors } = Linter.getErrors({uri, content: lines}, {
    IncorrectVariableCase: true
  }, cache);

  assert.strictEqual(errors.length, 2, `Expect length of 2`);
    
  assert.strictEqual(errors[0].type, `IncorrectVariableCase`, `Expect IncorrectVariableCase`);
  assert.strictEqual(errors[0].range.start.line, 6, `Index of 6 expected`);
  assert.strictEqual(errors[0].range.start.character, 0, `Index of 0 expected`);
  assert.strictEqual(errors[0].range.end.line, errors[0].range.start.line, `Should be on same line`);
  assert.strictEqual(errors[0].offset.position, 0, `Index of 0 expected`);
  assert.strictEqual(errors[0].offset.length, 11, `Should be index of 11`);
  assert.strictEqual(errors[0].newValue, `MyVariable2`, `Value of MyVariable2 expected`);

  assert.strictEqual(errors[1].type, `IncorrectVariableCase`, `Expect IncorrectVariableCase`);
  assert.strictEqual(errors[1].range.start.line, 10, `Index of 10 expected`);
  assert.strictEqual(errors[1].range.start.character, 4, `Index of 0 expected`);
  assert.strictEqual(errors[1].range.end.line, errors[1].range.start.line, `Should be on same line`);
  assert.strictEqual(errors[1].offset.position, 0, `Index of 0 expected`);
  assert.strictEqual(errors[1].offset.length, 11, `Should be index of 11`);
  assert.strictEqual(errors[1].newValue, `MyVariable2`, `Value of MyVariable2 expected`);
};

exports.linter6 =  async () => {
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

  const parser = new Parser();
  const cache = await parser.getDocs(uri, lines);
  const { errors } = Linter.getErrors({uri, content: lines}, {
    StringLiteralDupe: true
  }, cache);

  assert.strictEqual(errors.length, 2, `Expect length of 2`);

  assert.strictEqual(errors[0].type, `StringLiteralDupe`, `Expect StringLiteralDupe`);
  assert.strictEqual(errors[0].range.start.line, 13, `Index of 13 expected`);
  assert.strictEqual(errors[0].range.start.character, 4, `Index of 4 expected`);
  assert.strictEqual(errors[0].offset.position, 19, `Index of 19 expected`);
  assert.strictEqual(errors[0].offset.length, 27, `Index of 27 expected`);

  assert.strictEqual(errors[1].type, `StringLiteralDupe`, `Expect StringLiteralDupe`);
  assert.strictEqual(errors[1].range.start.line, 14, `Index of 14 expected`);
  assert.strictEqual(errors[1].range.start.character, 8, `Index of 8 expected`);
  assert.strictEqual(errors[1].offset.position, 14, `Index of 19 expected`);
  assert.strictEqual(errors[1].offset.length, 22, `Index of 22 expected`);
};

exports.linter6_lf =  async () => {
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

  const parser = new Parser();
  const cache = await parser.getDocs(uri, lines);
  const { errors } = Linter.getErrors({uri, content: lines}, {
    StringLiteralDupe: true,
    IncorrectVariableCase: true
  }, cache);

  assert.strictEqual(errors.length, 3, `Expect length of 3`);

  const line = new vscode.Range(new vscode.Position(4, 0), new vscode.Position(6, 18));

  assert.deepStrictEqual(errors[0], {
    range: line,
    offset: { position: 57, length: 69 },
    type: `IncorrectVariableCase`,
    newValue: `Myotherthing`,
  }, `Error not as expected`);

  assert.deepStrictEqual(errors[1], {
    range: line,
    offset: { position: 6, length: 21 },
    type: `StringLiteralDupe`,
    newValue: undefined,
  }, `Error not as expected`);

  assert.deepStrictEqual(errors[2], {
    range: line,
    offset: { position: 30, length: 45 },
    type: `StringLiteralDupe`,
    newValue: undefined,
  }, `Error not as expected`);
};

exports.linter6_crlf =  async () => {
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

  const parser = new Parser();
  const cache = await parser.getDocs(uri, lines);
  const { errors } = Linter.getErrors({uri, content: lines}, {
    StringLiteralDupe: true,
    IncorrectVariableCase: true
  }, cache);

  assert.strictEqual(errors.length, 3, `Expect length of 3`);

  const line = new vscode.Range(new vscode.Position(4, 0), new vscode.Position(6, 18));

  assert.deepStrictEqual(errors[0], {
    range: line,
    offset: { position: 59, length: 71 },
    type: `IncorrectVariableCase`,
    newValue: `Myotherthing`,
  }, `Error not as expected`);

  assert.deepStrictEqual(errors[1], {
    range: line,
    offset: { position: 6, length: 21 },
    type: `StringLiteralDupe`, 
    newValue: undefined,
  }, `Error not as expected`);

  assert.deepStrictEqual(errors[2], {
    range: line,
    offset: { position: 31, length: 46 },
    type: `StringLiteralDupe`,
    newValue: undefined,
  }, `Error not as expected`);
};

exports.linter7_casing1 =  async () => {
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

  const parser = new Parser();
  const cache = await parser.getDocs(uri, lines);
  const { errors } = Linter.getErrors({uri, content: lines}, {
    SpecificCasing: [
      { operation: `if`, expected: `If` },
      { operation: `endif`, expected: `Endif` },
      { operation: `select`, expected: `SELECT` },
    ]
  }, cache);

  assert.strictEqual(errors.length, 1, `Expect length of 1`);

  assert.deepStrictEqual(errors[0], {
    range: new vscode.Range(
      new vscode.Position(10, 2),
      new vscode.Position(10, 8),
    ),
    offset: { position: 0, length: 6 },
    type: `SpecificCasing`,
    newValue: `SELECT`
  }, `Error not as expected`);
};

exports.linter7_casing2 =  async () => {
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

  const parser = new Parser();
  const cache = await parser.getDocs(uri, lines);
  const { errors } = Linter.getErrors({uri, content: lines}, {
    SpecificCasing: [
      { operation: `ctl-opt`, expected: `Ctl-OPT` },
    ]
  }, cache);

  assert.strictEqual(errors.length, 1, `Expect length of 1`);

  assert.deepStrictEqual(errors[0], {
    range: new vscode.Range(
      new vscode.Position(2, 0),
      new vscode.Position(2, 22),
    ),
    offset: { position: 0, length: 7 },
    type: `SpecificCasing`,
    newValue: `Ctl-OPT`
  }, `Error not as expected`);
};

exports.linter7_casing3 =  async () => {
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

  const parser = new Parser();
  const cache = await parser.getDocs(uri, lines);
  const { errors } = Linter.getErrors({uri, content: lines}, {
    SpecificCasing: [
      { operation: `dcl-s`, expected: `DCL-S` },
    ]
  }, cache);

  assert.strictEqual(errors.length, 1, `Expect length of 1`);

  assert.deepStrictEqual(errors[0], {
    range: new vscode.Range(
      new vscode.Position(4, 0),
      new vscode.Position(4, 26),
    ),
    offset: { position: 0, length: 5 },
    type: `SpecificCasing`,
    newValue: `DCL-S`
  }, `Error not as expected`);
};

exports.linter7_casing4 =  async () => {
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

  const parser = new Parser();
  const cache = await parser.getDocs(uri, lines);
  const { errors } = Linter.getErrors({uri, content: lines}, {
    SpecificCasing: [
      { operation: `%trim`, expected: `%trim` },
    ]
  }, cache);

  assert.strictEqual(errors.length, 1, `Expect length of 1`);

  assert.deepStrictEqual(errors[0], {
    range: new vscode.Range(
      new vscode.Position(10, 2),
      new vscode.Position(10, 34),
    ),
    offset: { position: 14, length: 19 },
    type: `SpecificCasing`,
    newValue: `%trim`
  }, `Error not as expected`);
};

exports.linter7_casing5 =  async () => {
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

  const parser = new Parser();
  const cache = await parser.getDocs(uri, lines);
  const { errors } = Linter.getErrors({uri, content: lines}, {
    SpecificCasing: [
      { operation: `dcl-s`, expected: `Dcl-S` },
      { operation: `ctl-opt`, expected: `Ctl-Opt` },
    ]
  }, cache);

  assert.strictEqual(errors.length, 0, `Expect length of 0`);
};

exports.linter7_casing6 =  async () => {
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

  const parser = new Parser();
  const cache = await parser.getDocs(uri, lines);
  const { errors } = Linter.getErrors({uri, content: lines}, {
    SpecificCasing: [
      { operation: `*declare`, expected: `*upper` },
    ]
  }, cache);

  assert.strictEqual(errors.length, 3, `Expect length of 3`);
  assert.strictEqual(errors[0].newValue, `CTL-OPT`);
  assert.strictEqual(errors[1].newValue, `DCL-S`);
  assert.strictEqual(errors[2].newValue, `DCL-S`);
};

exports.linter7_casing7 =  async () => {
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

  const parser = new Parser();
  const cache = await parser.getDocs(uri, lines);
  const { errors } = Linter.getErrors({uri, content: lines}, {
    SpecificCasing: [
      { operation: `ctl-opt`, expected: `Ctl-Opt` },
      { operation: `*declare`, expected: `*upper` },
    ]
  }, cache);

  assert.strictEqual(errors.length, 2, `Expect length of 2`);
  assert.strictEqual(errors[0].newValue, `DCL-S`);
  assert.strictEqual(errors[1].newValue, `DCL-S`);
};

exports.linter7_casing8 =  async () => {
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

  const parser = new Parser();
  const cache = await parser.getDocs(uri, lines);
  const { errors } = Linter.getErrors({uri, content: lines}, {
    SpecificCasing: [
      { operation: `ctl-opt`, expected: `Ctl-Opt` },
      { operation: `*declare`, expected: `*LOWER` },
    ]
  }, cache);

  assert.strictEqual(errors.length, 2, `Expect length of 2`);
  assert.strictEqual(errors[0].newValue, `dcl-s`);
  assert.strictEqual(errors[1].newValue, `dcl-s`);
};

exports.linter7_casing9 =  async () => {
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

  const parser = new Parser();
  const cache = await parser.getDocs(uri, lines);
  const { errors } = Linter.getErrors({uri, content: lines}, {
    SpecificCasing: [
      { operation: `*bif`, expected: `*lower` },
    ]
  }, cache);

  assert.strictEqual(errors.length, 1, `Expect length of 1`);
  assert.strictEqual(errors[0].newValue, `%trim`);
};

exports.linter7_casing10 =  async () => {
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

  const parser = new Parser();
  const cache = await parser.getDocs(uri, lines);
  const { errors } = Linter.getErrors({uri, content: lines}, {
    IncorrectVariableCase: true
  }, cache);

  assert.strictEqual(errors.length, 1, `Expect length of 1`);
  assert.strictEqual(errors[0].range.start.line, 5);
  assert.strictEqual(errors[0].range.end.line, 14);
  assert.strictEqual(errors[0].offset.position, 120);
  assert.strictEqual(errors[0].offset.length, 130);
  assert.strictEqual(errors[0].newValue, `sFirstName`)
};

exports.linter8 =  async () => {
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

  const parser = new Parser();
  const cache = await parser.getDocs(uri, lines);
  const { errors } = Linter.getErrors({uri, content: lines}, {
    RequiresParameter: true
  }, cache);

  assert.strictEqual(errors.length, 1, `Expect length of 1`);
  assert.deepStrictEqual(errors[0], {
    range: new vscode.Range(
      new vscode.Position(9, 4),
      new vscode.Position(9, 16),
    ),
    offset: { position: 3, length: 12 },
    type: `RequiresParameter`,
  }, `Error not as expected`);
};

/**
   * Check that local variables are not in global scope
   */
exports.linter9 =  async () => {
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
  
  const parser = new Parser();
  const cache = await parser.getDocs(uri, lines);
  const { errors } = Linter.getErrors({uri, content: lines}, {
    IncorrectVariableCase: true
  }, cache);
  
  assert.strictEqual(cache.variables.length, 1, `Expect length of 1`);
  assert.strictEqual(cache.constants.length, 1, `Expect length of 1`);
  assert.strictEqual(cache.procedures.length, 1, `Expect length of 1`);
  assert.strictEqual(cache.procedures[0].subItems.length, 1, `Expect length of 1`);

  assert.strictEqual(errors.length, 3, `Expect length of 3`);

  assert.deepStrictEqual(errors[0], {
    range: new vscode.Range(
      new vscode.Position(13, 2),
      new vscode.Position(13, 21),
    ),
    offset: { position: 0, length: 8 },
    type: `IncorrectVariableCase`,
    newValue: `localVar`
  }, `Error not as expected`);

  assert.deepStrictEqual(errors[1], {
    range: new vscode.Range(
      new vscode.Position(14, 2),
      new vscode.Position(14, 24),
    ),
    offset: { position: 0, length: 11 },
    type: `IncorrectVariableCase`,
    newValue: `MyVariable2`
  }, `Error not as expected`);

  assert.deepStrictEqual(errors[2], {
    range: new vscode.Range(
      new vscode.Position(14, 2),
      new vscode.Position(14, 24),
    ),
    offset: { position: 14, length: 22 },
    type: `IncorrectVariableCase`,
    newValue: `localVar`
  }, `Error not as expected`);
};

exports.linter10 =  async () => {
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

  const parser = new Parser();
  const cache = await parser.getDocs(uri, lines);
  const { errors } = Linter.getErrors({uri, content: lines}, {
    NoCTDATA: true
  }, cache);

  assert.strictEqual(errors.length, 2, `Expect length of 2`);

  assert.deepStrictEqual(errors[0], {
    range: new vscode.Range(
      new vscode.Position(2, 0),
      new vscode.Position(2, 38),
    ),
    type: `NoCTDATA`,
  }, `Error not as expected`);

  assert.deepStrictEqual(errors[1], {
    range: new vscode.Range(
      new vscode.Position(14, 0),
      new vscode.Position(14, 12),
    ),
    offset: { position: 0, length: 8 },
    type: `NoCTDATA`,
  }, `Error not as expected`);
};

exports.linter11 =  async () => {
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
  
  const parser = new Parser();
  const cache = await parser.getDocs(uri, lines);
  const { errors } = Linter.getErrors({uri, content: lines}, {
    StringLiteralDupe: true
  }, cache);
  
  assert.strictEqual(errors.length, 2, `Expect length of 2`);
  
  const line = new vscode.Range(new vscode.Position(5, 0), new vscode.Position(7, 12));
  
  assert.deepStrictEqual(errors[0], {
    range: line,
    offset: { position: 6, length: 21 },
    type: `StringLiteralDupe`,
    newValue: `HELLO`,
  }, `Error not as expected`);
  
  assert.deepStrictEqual(errors[1], {
    range: line,
    offset: { position: 30, length: 45 },
    type: `StringLiteralDupe`,
    newValue: `HELLO`,
  }, `Error not as expected`);
};

exports.linter12 =  async () => {
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

  const parser = new Parser();
  const cache = await parser.getDocs(uri, lines);
  const { indentErrors } = Linter.getErrors({uri, content: lines}, {
    indent: 2
  }, cache);

  assert.strictEqual(indentErrors.length, 0, `Expect length of 0`);
};

exports.linter13 =  async () => {
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

  const parser = new Parser();
  const cache = await parser.getDocs(uri, lines);
  const { indentErrors } = Linter.getErrors({uri, content: lines}, {
    indent: 2
  }, cache);

  assert.strictEqual(indentErrors.length, 0, `Expect length of 0`);
};

exports.linter13_commentIndent =  async () => {
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

  const parser = new Parser();
  const cache = await parser.getDocs(uri, lines);
  const { indentErrors } = Linter.getErrors({uri, content: lines}, {
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

exports.linter14 =  async () => {
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

  const parser = new Parser();
  const cache = await parser.getDocs(uri, lines);
  const { indentErrors } = Linter.getErrors({uri, content: lines}, {
    indent: 2
  }, cache);

  assert.strictEqual(indentErrors.length, 0, `Expect length of 0`);
};

exports.linter15 =  async () => {
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
    `return;`
  ].join(`\n`);

  const parser = new Parser();
  const cache = await parser.getDocs(uri, lines);
  const { errors } = Linter.getErrors({uri, content: lines}, {
    PrettyComments: true
  }, cache);

  assert.strictEqual(errors.length, 5, `Expect length of 5`);

  assert.deepStrictEqual(errors[0], {
    type: `PrettyComments`,
    newValue: ``,
    range: new vscode.Range(
      new vscode.Position(4, 0),
      new vscode.Position(4, 2),
    ),
  });

  assert.deepStrictEqual(errors[1], {
    type: `PrettyComments`,
    newValue: `// `,
    range: new vscode.Range(
      new vscode.Position(5, 0),
      new vscode.Position(5, 2),
    ),
  });

  assert.deepStrictEqual(errors[2], {
    type: `PrettyComments`,
    newValue: ``,
    range: new vscode.Range(
      new vscode.Position(6, 0),
      new vscode.Position(6, 2),
    ),
  });

  assert.deepStrictEqual(errors[3], {
    type: `PrettyComments`,
    newValue: `// `,
    range: new vscode.Range(
      new vscode.Position(14, 2),
      new vscode.Position(14, 4),
    ),
  });

  assert.deepStrictEqual(errors[4], {
    type: `PrettyComments`,
    newValue: ``,
    range: new vscode.Range(
      new vscode.Position(16, 2),
      new vscode.Position(16, 4),
    ),
  });
};

/**
   * Subroutine check test
   * */
exports.linter16 =  async () => {
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

  const parser = new Parser();
  const cache = await parser.getDocs(uri, lines);
  const { errors } = Linter.getErrors({uri, content: lines}, {
    NoGlobalSubroutines: true
  }, cache);

  assert.strictEqual(errors.length, 3, `Expect length of 3`);

  assert.deepStrictEqual(errors[0], {
    type: `NoGlobalSubroutines`,
    newValue: `theSubroutine()`,
    range: new vscode.Range(
      new vscode.Position(3, 0),
      new vscode.Position(3, 18),
    ),
  });

  assert.deepStrictEqual(errors[1], {
    type: `NoGlobalSubroutines`,
    newValue: `Dcl-Proc`,
    range: new vscode.Range(
      new vscode.Position(6, 0),
      new vscode.Position(6, 19),
    ),
    offset: {
      position: 0,
      length: 5
    }
  });

  assert.deepStrictEqual(errors[2], {
    type: `NoGlobalSubroutines`,
    newValue: `End-Proc`,
    range: new vscode.Range(
      new vscode.Position(8, 0),
      new vscode.Position(8, 5),
    ),
    offset: {
      position: 0,
      length: 5
    }
  });
};

exports.linter16_with_leavesr =  async () => {
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

  const parser = new Parser();
  const cache = await parser.getDocs(uri, lines);
  const { errors } = Linter.getErrors({uri, content: lines}, {
    NoGlobalSubroutines: true
  }, cache);

  assert.strictEqual(errors.length, 4, `Expect length of 3`);

  assert.deepStrictEqual(errors[0], {
    type: `NoGlobalSubroutines`,
    newValue: `theSubroutine()`,
    range: new vscode.Range(
      new vscode.Position(5, 0),
      new vscode.Position(5, 18),
    ),
  });

  assert.deepStrictEqual(errors[1], {
    type: `NoGlobalSubroutines`,
    newValue: `Dcl-Proc`,
    range: new vscode.Range(
      new vscode.Position(8, 0),
      new vscode.Position(8, 19),
    ),
    offset: {
      position: 0,
      length: 5
    }
  });

  assert.deepStrictEqual(errors[2], {
    type: `NoGlobalSubroutines`,
    newValue: `return`,
    range: new vscode.Range(
      new vscode.Position(10, 4),
      new vscode.Position(10, 11),
    ),
  });

  assert.deepStrictEqual(errors[3], {
    type: `NoGlobalSubroutines`,
    newValue: `End-Proc`,
    range: new vscode.Range(
      new vscode.Position(13, 0),
      new vscode.Position(13, 5),
    ),
    offset: {
      position: 0,
      length: 5
    }
  });
};
  
/**
   * Subroutine in procedure check test
   * */
exports.linter17 =  async () => {
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
  
  const parser = new Parser();
  const cache = await parser.getDocs(uri, lines);
  const { errors } = Linter.getErrors({uri, content: lines}, {
    NoLocalSubroutines: true
  }, cache);
  
  assert.strictEqual(errors.length, 1, `Expect length of 1`);
  
  assert.deepStrictEqual(errors[0], {
    type: `NoLocalSubroutines`,
    range: new vscode.Range(
      new vscode.Position(8, 2),
      new vscode.Position(8, 21),
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
  
  const parser = new Parser();
  const cache = await parser.getDocs(uri, lines);
  const { errors } = Linter.getErrors({uri, content: lines}, {
    NoGlobalsInProcedures: true
  }, cache);
  
  assert.strictEqual(errors.length, 2, `Expect length of 2`);

  assert.deepStrictEqual(errors[0], {
    type: `NoGlobalsInProcedures`,
    range: new vscode.Range(
      new vscode.Position(8, 2),
      new vscode.Position(8, 29),
    ),
    offset: {
      position: 0,
      length: 11
    },
  });

  assert.deepStrictEqual(errors[1], {
    type: `NoGlobalsInProcedures`,
    range: new vscode.Range(
      new vscode.Position(9, 2),
      new vscode.Position(9, 23),
    ),
    offset: {
      position: 10,
      length: 21
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

  const parser = new Parser();
  const cache = await parser.getDocs(uri, lines);
  const { errors } = Linter.getErrors({uri, content: lines}, {
    NoUnreferenced: true
  }, cache);

  assert.strictEqual(errors.length, 11);
  
  assert.deepStrictEqual(errors[0], {
    type: `NoUnreferenced`,
    range: new vscode.Range(
      new vscode.Position(4, 0),
      new vscode.Position(4, 100),
    ),
  });

  assert.deepStrictEqual(errors[1], {
    type: `NoUnreferenced`,
    range: new vscode.Range(
      new vscode.Position(68, 0),
      new vscode.Position(68, 100),
    ),
  });

  assert.deepStrictEqual(errors[2], {
    type: `NoUnreferenced`,
    range: new vscode.Range(
      new vscode.Position(11, 0),
      new vscode.Position(11, 100),
    ),
  });

  assert.deepStrictEqual(errors[3], {
    type: `NoUnreferenced`,
    range: new vscode.Range(
      new vscode.Position(10, 0),
      new vscode.Position(10, 100),
    ),
  });

  assert.deepStrictEqual(errors[4], {
    type: `NoUnreferenced`,
    range: new vscode.Range(
      new vscode.Position(23, 0),
      new vscode.Position(23, 100),
    ),
  });

  assert.deepStrictEqual(errors[5], {
    type: `NoUnreferenced`,
    range: new vscode.Range(
      new vscode.Position(22, 0),
      new vscode.Position(22, 100),
    ),
  });

  assert.deepStrictEqual(errors[6], {
    type: `NoUnreferenced`,
    range: new vscode.Range(
      new vscode.Position(38, 0),
      new vscode.Position(38, 100),
    ),
  });

  assert.deepStrictEqual(errors[7], {
    type: `NoUnreferenced`,
    range: new vscode.Range(
      new vscode.Position(49, 0),
      new vscode.Position(49, 100),
    ),
  });

  assert.deepStrictEqual(errors[8], {
    type: `NoUnreferenced`,
    range: new vscode.Range(
      new vscode.Position(48, 0),
      new vscode.Position(48, 100),
    ),
  });

  assert.deepStrictEqual(errors[9], {
    type: `NoUnreferenced`,
    range: new vscode.Range(
      new vscode.Position(57, 0),
      new vscode.Position(57, 100),
    ),
  });

  assert.deepStrictEqual(errors[10], {
    type: `NoUnreferenced`,
    range: new vscode.Range(
      new vscode.Position(56, 0),
      new vscode.Position(56, 100),
    ),
  });
}

exports.linter20 =  async () => {
  const lines = [
    `**FREE`,
    ``,
    `Dcl-s MyVariable2 Char(20);  `,
    ``,
    `EXEC SQL`,
    `    FETCH NEXT FROM empCur       `,
    `    INTO :myvariable2;`,
  ].join(`\n`);

  const parser = new Parser();
  const cache = await parser.getDocs(uri, lines);
  const { errors } = Linter.getErrors({uri, content: lines}, {
    IncorrectVariableCase: true
  }, cache);

  assert.strictEqual(errors.length, 1);
    
  assert.strictEqual(errors[0].type, `IncorrectVariableCase`, `Expect IncorrectVariableCase`);
  assert.strictEqual(errors[0].range.start.line, 4);
  assert.strictEqual(errors[0].range.end.line, 6);
  assert.strictEqual(errors[0].range.start.character, 0);
  assert.strictEqual(errors[0].offset.position, 53);
  assert.strictEqual(errors[0].offset.length, 64);
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

  const parser = new Parser();
  const cache = await parser.getDocs(uri, lines);
  const { errors } = Linter.getErrors({uri, content: lines}, {
    NoUnreferenced: true
  }, cache);

  assert.strictEqual(errors.length, 1);
  assert.deepStrictEqual(errors[0], {
    type: `NoUnreferenced`,
    range: new vscode.Range(
      new vscode.Position(20, 0),
      new vscode.Position(20, 100),
    ),
  });
}

/**
   * Test procedure length.
   * When Procedure is defined, the prototype is overridden.
   */
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

  const parser = new Parser();
  const cache = await parser.getDocs(uri, lines);
  const { errors } = Linter.getErrors({uri, content: lines}, {
    PrototypeCheck: true
  }, cache);

  assert.strictEqual(errors.length, 1);
  assert.deepStrictEqual(errors[0], {
    type: `PrototypeCheck`,
    range: new vscode.Range(
      new vscode.Position(2, 0),
      new vscode.Position(2, 19),
    ),
  });
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

  const parser = new Parser();
  const cache = await parser.getDocs(uri, lines);
  const { errors } = Linter.getErrors({uri, content: lines}, {
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

  const parser = new Parser();
  const cache = await parser.getDocs(uri, lines);
  const { errors } = Linter.getErrors({uri, content: lines}, {
    NoExternalTo: [
      `QSYGETPH`
    ]
  }, cache);

  assert.strictEqual(errors.length, 1);
  assert.deepStrictEqual(errors[0], {
    type: `NoExternalTo`,
    range: new vscode.Range(
      new vscode.Position(8, 0),
      new vscode.Position(8, 100),
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

  const parser = new Parser();
  const cache = await parser.getDocs(uri, lines);
  const { errors } = Linter.getErrors({uri, content: lines}, {
    NoExternalTo: [
      `QSYGETPH`,
      `QSYRLSPH`
    ]
  }, cache);

  assert.strictEqual(errors.length, 2);

  assert.deepStrictEqual(errors[0], {
    type: `NoExternalTo`,
    range: new vscode.Range(
      new vscode.Position(14, 0),
      new vscode.Position(14, 100),
    ),
  });

  assert.deepStrictEqual(errors[1], {
    type: `NoExternalTo`,
    range: new vscode.Range(
      new vscode.Position(23, 0),
      new vscode.Position(23, 100),
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

  const parser = new Parser();
  const cache = await parser.getDocs(uri, lines);
  const { errors } = Linter.getErrors({uri, content: lines}, {
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

  const parser = new Parser();
  const cache = await parser.getDocs(uri, lines);
  const { errors } = Linter.getErrors({uri, content: lines}, {
    NoExecuteImmediate: true
  }, cache);
  
  assert.strictEqual(errors.length, 1);
  assert.deepStrictEqual(errors[0], {
    type: `NoExecuteImmediate`,
    range: new vscode.Range(
      new vscode.Position(7, 0),
      new vscode.Position(8, 34),
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

  const parser = new Parser();
  const cache = await parser.getDocs(uri, lines);
  const { errors } = Linter.getErrors({uri, content: lines}, {
    NoExtProgramVariable: true
  }, cache);
  
  assert.strictEqual(errors.length, 2);

  assert.deepStrictEqual(errors[0], {
    type: `NoExtProgramVariable`,
    range: new vscode.Range(
      new vscode.Position(1, 0),
      new vscode.Position(1, 32),
    ),
    offset: {
      position: 19,
      length: 31
    }
  });

  assert.deepStrictEqual(errors[1], {
    type: `NoExtProgramVariable`,
    range: new vscode.Range(
      new vscode.Position(3, 0),
      new vscode.Position(3, 45),
    ),
    offset: {
      position: 32,
      length: 44
    }
  });
}