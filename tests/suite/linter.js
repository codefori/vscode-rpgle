
const assert = require(`assert`);

const { default: parserSetup } = require(`../parserSetup`);
const { default: Linter } = require(`../../language/linter`);
const path = require(`path`);
const { Range, Position } = require(`../../language/models/DataPoints`);

const parser = parserSetup();
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

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true});
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

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
  const { indentErrors } = Linter.getErrors({ uri, content: lines }, {
    indent: 2
  }, cache);

  assert.strictEqual(indentErrors.length, 1, `There should be 1 error`);

  // TODO: we aren't count this yet
  // assert.strictEqual(indentErrors[0].line, 3, `First error should be index 3`);
  // assert.strictEqual(indentErrors[0].currentIndent, 1, `Actual indent should be 1`);
  // assert.strictEqual(indentErrors[0].expectedIndent, 2, `Expected indent should be 2`);

  assert.strictEqual(indentErrors[0].line, 5, `Second error should be index 5`);
  assert.strictEqual(indentErrors[0].currentIndent, 5, `Actual indent should be 5`);
  
  assert.strictEqual(indentErrors[0].expectedIndent, 4, `Expected indent should be 4`);
};

exports.linter_invalid_statement = async () => {
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

  assert.strictEqual(indentErrors.length, 0, `There should be 1 error`);
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

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
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

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
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

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
  const { indentErrors } = Linter.getErrors({ uri, content: lines }, {
    indent: 2
  }, cache);

  assert.strictEqual(indentErrors.length, 2, `Expect length of 2`);

  assert.strictEqual(indentErrors[0].line, 10, `Index of 9 expected`);
  assert.strictEqual(indentErrors[0].currentIndent, 0, `Value of 0 expected`);
  assert.strictEqual(indentErrors[0].expectedIndent, 2, `Value of 2 expected`);

  assert.strictEqual(indentErrors[1].line, 17, `Index of 17 expected`);
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

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
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

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
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

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    RequireBlankSpecial: true
  }, cache);

  assert.strictEqual(errors.length, 2, `Expect length of 2`);

  assert.strictEqual(errors[0].type, `RequireBlankSpecial`, `Expect RequireBlankSpecial`);
  assert.strictEqual(errors[0].offset.position, 76);
  assert.strictEqual(errors[0].offset.end, 78);
  assert.strictEqual(errors[0].newValue, `*BLANK`, `Value of *BLANK expected`);

  assert.strictEqual(errors[1].type, `RequireBlankSpecial`, `Expect RequireBlankSpecial`);
  assert.strictEqual(errors[1].offset.position, 98, `Index of 17 expected`);
  assert.strictEqual(errors[1].offset.end, 100, `Index of 19 expected`);
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

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    IncorrectVariableCase: true
  }, cache);

  assert.strictEqual(errors.length, 2, `Expect length of 2`);

  assert.strictEqual(errors[0].type, `IncorrectVariableCase`, `Expect IncorrectVariableCase`);
  assert.strictEqual(errors[0].offset.position, 62);
  assert.strictEqual(errors[0].offset.end, 73);
  assert.strictEqual(errors[0].newValue, `MyVariable2`, `Value of MyVariable2 expected`);

  assert.strictEqual(errors[1].type, `IncorrectVariableCase`, `Expect IncorrectVariableCase`);
  assert.strictEqual(errors[1].offset.position, 122);
  assert.strictEqual(errors[1].offset.end, 133);
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

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    StringLiteralDupe: true
  }, cache);

  assert.strictEqual(errors.length, 2, `Expect length of 2`);

  assert.strictEqual(errors[0].type, `StringLiteralDupe`, `Expect StringLiteralDupe`);
  assert.strictEqual(errors[0].offset.position, 239);
  assert.strictEqual(errors[0].offset.end, 247);

  assert.strictEqual(errors[1].type, `StringLiteralDupe`, `Expect StringLiteralDupe`);
  assert.strictEqual(errors[1].offset.position, 271);
  assert.strictEqual(errors[1].offset.end, 279);
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

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    StringLiteralDupe: true,
    IncorrectVariableCase: true
  }, cache);

  assert.strictEqual(errors.length, 3, `Expect length of 3`);

  const line = new Range(new Position(4, 0), new Position(6, 18));

  assert.deepStrictEqual(errors[0], {
    offset: { position: 95, end: 107 },
    type: `IncorrectVariableCase`,
    newValue: `Myotherthing`
  });

  assert.deepStrictEqual(errors[1], {
    offset: { position: 44, end: 59 },
    type: `StringLiteralDupe`,
    newValue: undefined
  });

  assert.deepStrictEqual(errors[2], {
    offset: { position: 68, end: 83 },
    type: `StringLiteralDupe`,
    newValue: undefined
  });
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

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    StringLiteralDupe: true,
    IncorrectVariableCase: true
  }, cache);

  assert.strictEqual(errors.length, 3, `Expect length of 3`);

  const line = new Range(new Position(4, 0), new Position(6, 18));

  assert.deepStrictEqual(errors[0], {
    offset: { position: 101, end: 113 },
    type: `IncorrectVariableCase`,
    newValue: `Myotherthing`
  });

  assert.deepStrictEqual(errors[1], {
    offset: { position: 48, end: 63 },
    type: `StringLiteralDupe`,
    newValue: undefined
  });

  assert.deepStrictEqual(errors[2], {
    offset: { position: 73, end: 88 },
    type: `StringLiteralDupe`,
    newValue: undefined
  });
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

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    SpecificCasing: [
      { operation: `if`, expected: `If` },
      { operation: `endif`, expected: `Endif` },
      { operation: `select`, expected: `SELECT` },
    ]
  }, cache);

  assert.strictEqual(errors.length, 1, `Expect length of 1`);

  assert.deepStrictEqual(errors[0], {
    offset: { position: 141, end: 147 },
    type: `SpecificCasing`,
    newValue: `SELECT`
  });
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

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    SpecificCasing: [
      { operation: `ctl-opt`, expected: `Ctl-OPT` },
    ]
  }, cache);

  assert.strictEqual(errors.length, 1, `Expect length of 1`);

  assert.deepStrictEqual(errors[0], {
    offset: { position: 8, end: 15 },
    type: `SpecificCasing`,
    newValue: `Ctl-OPT`
  });
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

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    SpecificCasing: [
      { operation: `dcl-s`, expected: `DCL-S` },
    ]
  }, cache);

  assert.strictEqual(errors.length, 1, `Expect length of 1`);

  assert.deepStrictEqual(errors[0], {
    offset: { position: 33, end: 38 },
    type: `SpecificCasing`,
    newValue: `DCL-S`
  });
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

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    SpecificCasing: [
      { operation: `%trim`, expected: `%trim` },
    ]
  }, cache);

  assert.strictEqual(errors.length, 1, `Expect length of 1`);

  assert.deepStrictEqual(errors[0], {
    offset: { position: 164, end: 169 },
    type: `SpecificCasing`,
    newValue: `%trim`
  });
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

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
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

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
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

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
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

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
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

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
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

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    IncorrectVariableCase: true
  }, cache);

  assert.strictEqual(errors.length, 1, `Expect length of 1`);
  assert.strictEqual(errors[0].offset.position, 178);
  assert.strictEqual(errors[0].offset.end, 188);
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

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    IncorrectVariableCase: true
  }, cache);

  assert.strictEqual(errors.length, 2);

  assert.deepStrictEqual(errors[0], {
    offset: { position: 121, end: 127 },
    type: `IncorrectVariableCase`,
    newValue: `sEmpNo`
  });

  assert.deepStrictEqual(errors[1], {
    offset: { position: 179, end: 189 },
    type: `IncorrectVariableCase`,
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

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
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

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    RequiresParameter: true
  }, cache);

  assert.strictEqual(errors.length, 1, `Expect length of 1`);
  assert.deepStrictEqual(errors[0], {
    offset: { position: 236, end: 245 }, type: `RequiresParameter`
  });
};

exports.linter_Do_Not_Require_Parameters_For_Control_Options = async () => {
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

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
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

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    IncorrectVariableCase: true
  }, cache);

  assert.strictEqual(cache.variables.length, 1, `Expect length of 1`);
  assert.strictEqual(cache.constants.length, 1, `Expect length of 1`);
  assert.strictEqual(cache.procedures.length, 1, `Expect length of 1`);
  assert.strictEqual(cache.procedures[0].subItems.length, 1, `Expect length of 1`);

  assert.strictEqual(errors.length, 3, `Expect length of 3`);

  assert.deepStrictEqual(errors[0], {
    offset: { position: 194, end: 202 },
    type: `IncorrectVariableCase`,
    newValue: `localVar`
  });

  assert.deepStrictEqual(errors[1], {
    offset: { position: 217, end: 228 },
    type: `IncorrectVariableCase`,
    newValue: `MyVariable2`
  });

  assert.deepStrictEqual(errors[2], {
    offset: { position: 231, end: 239 },
    type: `IncorrectVariableCase`,
    newValue: `localVar`
  });
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

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    NoCTDATA: true
  }, cache);

  assert.strictEqual(errors.length, 2, `Expect length of 2`);

  assert.deepStrictEqual(errors[0], {
    type: `NoCTDATA`, offset: { position: 51, end: 89 }
  });

  assert.deepStrictEqual(errors[1], {
    offset: { position: 222, end: 230 }, type: `NoCTDATA`
  });
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

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    StringLiteralDupe: true
  }, cache);

  assert.strictEqual(errors.length, 2, `Expect length of 2`);

  const line = new Range(new Position(5, 0), new Position(7, 12));

  assert.deepStrictEqual(errors[0], {
    offset: { position: 73, end: 88 },
    type: `StringLiteralDupe`,
    newValue: `HELLO`
  });

  assert.deepStrictEqual(errors[1], {
    offset: { position: 97, end: 112 },
    type: `StringLiteralDupe`,
    newValue: `HELLO`
  });
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

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
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

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
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

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
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

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
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

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    PrettyComments: true
  }, cache);

  assert.strictEqual(errors.length, 2, `Expect length of 5`);

  assert.deepStrictEqual(errors[0], {
    offset: { position: 36, end: 38 },
    type: 'PrettyComments',
    newValue: '// '
  });

  assert.deepStrictEqual(errors[1], {
    offset: { position: 207, end: 209 },
    type: 'PrettyComments',
    newValue: '// '
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

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    NoGlobalSubroutines: true
  }, cache);

  assert.strictEqual(errors.length, 3, `Expect length of 3`);

  assert.deepStrictEqual(errors[0], {
    type: `NoGlobalSubroutines`,
    offset: { position: 36, end: 54 },
    newValue: `theSubroutine()`
  });

  assert.deepStrictEqual(errors[1], {
    offset: { position: 76, end: 81 },
    type: `NoGlobalSubroutines`,
    newValue: `Dcl-Proc`
  });

  assert.deepStrictEqual(errors[2], {
    offset: { position: 128, end: 133 },
    type: `NoGlobalSubroutines`,
    newValue: `End-Proc`
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

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    NoGlobalSubroutines: true
  }, cache);

  assert.strictEqual(errors.length, 4, `Expect length of 3`);

  assert.deepStrictEqual(errors[0], {
    type: `NoGlobalSubroutines`,
    offset: { position: 71, end: 89 },
    newValue: `theSubroutine()`
  });

  assert.deepStrictEqual(errors[1], {
    offset: { position: 111, end: 116 },
    type: `NoGlobalSubroutines`,
    newValue: `Dcl-Proc`
  });

  assert.deepStrictEqual(errors[2], {
    type: `NoGlobalSubroutines`,
    offset: { position: 156, end: 163 },
    newValue: `return`
  });

  assert.deepStrictEqual(errors[3], {
    offset: { position: 205, end: 210 },
    type: `NoGlobalSubroutines`,
    newValue: `End-Proc`
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

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    NoLocalSubroutines: true
  }, cache);

  assert.strictEqual(errors.length, 1, `Expect length of 1`);

  assert.deepStrictEqual(errors[0], {
    type: `NoLocalSubroutines`, offset: { position: 119, end: 138 }
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

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    NoGlobalsInProcedures: true
  }, cache);

  assert.strictEqual(errors.length, 2, `Expect length of 2`);

  assert.deepStrictEqual(errors[0], {
    offset: { position: 123, end: 134 },
    type: `NoGlobalsInProcedures`
  });

  assert.deepStrictEqual(errors[1], {
    offset: { position: 164, end: 175 },
    type: `NoGlobalsInProcedures`
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

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    NoUnreferenced: true
  }, cache);

  assert.strictEqual(errors.length, 11);

  assert.deepStrictEqual(errors[0], {
    type: `NoUnreferenced`, offset: { position: 66, end: 86 }
  });

  assert.deepStrictEqual(errors[1], {
    type: `NoUnreferenced`, offset: { position: 1089, end: 1104 } 
  });

  assert.deepStrictEqual(errors[2], {
    type: `NoUnreferenced`, offset: { position: 160, end: 176 } 
  });

  assert.deepStrictEqual(errors[3], {
    type: `NoUnreferenced`, offset: { position: 139, end: 154 }
  });

  assert.deepStrictEqual(errors[4], {
    type: `NoUnreferenced`, offset: { position: 337, end: 354 }
  });

  assert.deepStrictEqual(errors[5], {
    type: `NoUnreferenced`, offset: { position: 302, end: 331 }
  });

  assert.deepStrictEqual(errors[6], {
    type: `NoUnreferenced`, offset: { position: 539, end: 566 }
  });

  assert.deepStrictEqual(errors[7], {
    type: `NoUnreferenced`, offset: { position: 735, end: 749 }
  });

  assert.deepStrictEqual(errors[8], {
    type: `NoUnreferenced`, offset: { position: 705, end: 725 }
  });

  assert.deepStrictEqual(errors[9], {
    type: `NoUnreferenced`, offset: { position: 893, end: 910 }
  });

  assert.deepStrictEqual(errors[10], {
    type: `NoUnreferenced`, offset: { position: 849, end: 883 }
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

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    IncorrectVariableCase: true
  }, cache);

  assert.strictEqual(errors.length, 1);

  assert.strictEqual(errors[0].type, `IncorrectVariableCase`, `Expect IncorrectVariableCase`);
  assert.strictEqual(errors[0].offset.position, 92);
  assert.strictEqual(errors[0].offset.end, 103);
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

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    NoUnreferenced: true
  }, cache);

  assert.strictEqual(errors.length, 1);
  assert.deepStrictEqual(errors[0], {
    type: `NoUnreferenced`, offset: { position: 257, end: 270 }
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

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    PrototypeCheck: true
  }, cache);

  assert.strictEqual(errors.length, 1);
  assert.deepStrictEqual(errors[0], {
    type: `PrototypeCheck`, offset: { position: 8, end: 27 }
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

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
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

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
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

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    NoExternalTo: [
      `QSYGETPH`
    ]
  }, cache);

  assert.strictEqual(errors.length, 1);
  assert.deepStrictEqual(errors[0], {
    type: `NoExternalTo`, offset: { position: 95, end: 132 }
  });

  assert.strictEqual(lines.substring(errors[0].offset.position, errors[0].offset.end), `Dcl-PR GetProfile  ExtPgm('QSYGETPH')`);
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

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    NoExternalTo: [
      `QSYGETPH`,
      `QSYRLSPH`
    ]
  }, cache);

  assert.strictEqual(errors.length, 2);

  assert.deepStrictEqual(errors[0], {
    type: `NoExternalTo`, offset: { position: 163, end: 200 }
  });

  assert.strictEqual(lines.substring(errors[0].offset.position, errors[0].offset.end), `Dcl-PR GetProfile  ExtPgm('QSYGETPH')`);

  assert.deepStrictEqual(errors[1], {
    type: `NoExternalTo`, offset: { position: 506, end: 544 }
  });

  assert.strictEqual(lines.substring(errors[1].offset.position, errors[1].offset.end), `Dcl-Pr CloseProfile ExtPgm('QSYRLSPH')`);
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

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
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

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    NoExecuteImmediate: true
  }, cache);

  assert.strictEqual(errors.length, 1);
  assert.deepStrictEqual(errors[0], {
    type: `NoExecuteImmediate`, offset: { position: 105, end: 148 }
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

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    NoExtProgramVariable: true
  }, cache);

  assert.strictEqual(errors.length, 2);

  assert.deepStrictEqual(errors[0], {
    offset: { position: 26, end: 38 }, type: `NoExtProgramVariable`
  });

  assert.deepStrictEqual(errors[1], {
    offset: { position: 81, end: 93 }, type: `NoExtProgramVariable`
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

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
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

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    IncludeMustBeRelative: true
  }, cache);

  assert.strictEqual(errors.length, 1);

  assert.deepStrictEqual(errors[0], {
    offset: { position: 39, end: 49 },
    type: `IncludeMustBeRelative`,
    newValue: undefined
  });
}

exports.linter31_a = async () => {
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

  assert.strictEqual(cache.includes.length, 1);
  assert.strictEqual(cache.includes[0].line, 4);

  assert.strictEqual(errors.length, 0);
}

exports.linter31_b = async () => {
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
    availableIncludes: [`tests/rpgle/copy1.rpgle`]
  }, {
    IncludeMustBeRelative: true,
  }, cache);

  assert.strictEqual(cache.includes.length, 1);
  assert.strictEqual(cache.includes[0].line, 4);

  assert.strictEqual(errors.length, 1);

  assert.deepStrictEqual(errors[0], {
    offset: { position: 39, end: 50 },
    type: `IncludeMustBeRelative`,
    newValue: `'tests/rpgle/copy1.rpgle'`
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

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
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

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
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
    offset: { position: 39, end: 52 },
    type: `IncludeMustBeRelative`,
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

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    IncludeMustBeRelative: true
  }, cache);

  assert.strictEqual(cache.includes.length, 1);
  assert.strictEqual(cache.includes[0].line, 4);

  assert.strictEqual(errors.length, 1);

  assert.deepStrictEqual(errors[0], {
    offset: { position: 39, end: 65 }, type: `IncludeMustBeRelative`
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

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    SQLHostVarCheck: true
  }, cache);

  assert.strictEqual(errors.length, 1);

  assert.deepStrictEqual(errors[0], {
    offset: { position: 183, end: 190 },
    type: `SQLHostVarCheck`,
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

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
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

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
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

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    UselessOperationCheck: true
  }, cache);

  assert.strictEqual(cache.includes.length, 1);
  assert.strictEqual(cache.includes[0].line, 4);

  assert.strictEqual(errors.length, 1);

  assert.deepStrictEqual(errors[0], {
    offset: { position: 129, end: 135 },
    type: `UselessOperationCheck`
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

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
  Linter.getErrors({ uri, content: lines }, {
    CollectReferences: true,
  }, cache);

  const subfa = cache.find(`subfa`);
  assert.strictEqual(subfa.references.length, 1);
  assert.deepStrictEqual(subfa.references[0], {
    offset: { position: 469, end: 474 }
  });

  const structYesAlso = cache.find(`structYesAlso`);
  assert.strictEqual(structYesAlso.references.length, 1);
  assert.deepStrictEqual(structYesAlso.references[0], {
    offset: { position: 485, end: 498 }
  });

  const subfc = structYesAlso.subItems[0];
  assert.strictEqual(subfc.name, `subfc`);
  assert.strictEqual(subfc.references.length, 0);

  const qualStructYes = cache.find(`qualStructYes`);
  assert.strictEqual(qualStructYes.references.length, 1);
  assert.deepStrictEqual(qualStructYes.references[0], {
    offset: { position: 516, end: 529 }
  });

  const qualsubA = qualStructYes.subItems[0];
  assert.strictEqual(qualsubA.name, `qualsubA`);
  assert.strictEqual(qualsubA.references.length, 1);
  assert.deepStrictEqual(qualsubA.references[0], {
    offset: { position: 530, end: 538 }
  });

  const procYes = cache.find(`procYes`);
  const subProc = procYes.scope;

  const localStructYes = subProc.find(`localStructYes`);
  assert.strictEqual(localStructYes.references.length, 1);
  assert.deepStrictEqual(localStructYes.references[0], {
    offset: { position: 1158, end: 1172 }
  });

  const localStructAlsoYes = subProc.find(`localStructAlsoYes`);
  assert.strictEqual(localStructAlsoYes.references.length, 0);

  const subfe = localStructAlsoYes.subItems[0];
  assert.strictEqual(subfe.name, `subfe`);
  assert.strictEqual(subfe.references.length, 1);
  assert.deepStrictEqual(subfe.references[0], {
    offset: { position: 1193, end: 1198 }
  });

  const qualDimStructYup = cache.find(`qualDimStructYup`);
  assert.strictEqual(qualDimStructYup.references.length, 3)

  assert.deepStrictEqual(qualDimStructYup.references[0], {
    offset: { position: 545, end: 561 }
  });

  assert.deepStrictEqual(qualDimStructYup.references[1], {
    offset: { position: 578, end: 594 }
  });

  assert.deepStrictEqual(qualDimStructYup.references[2], {
    offset: { position: 625, end: 641 }
  });

  const boopABC = qualDimStructYup.subItems[0];
  assert.strictEqual(boopABC.name, `boopABC`);
  assert.strictEqual(boopABC.references.length, 3);

  assert.deepStrictEqual(boopABC.references[0], {
    offset: { position: 565, end: 572 }
  });

  assert.deepStrictEqual(boopABC.references[1], {
    offset: { position: 612, end: 619 } 
  });

  assert.deepStrictEqual(boopABC.references[2], {
    offset: { position: 663, end: 670 }
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

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    RequiresProcedureDescription: true
  }, cache);

  assert.strictEqual(errors.length, 1);
  assert.deepStrictEqual(errors[0], {
    type: `RequiresProcedureDescription`,
    offset: { position: 59, end: 84 }
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

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
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

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
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

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    RequireBlankSpecial: true,
    StringLiteralDupe: true
  }, cache);

  assert.strictEqual(errors.length, 3);

  assert.deepStrictEqual(errors[0], {
    offset: { position: 52, end: 54 },
    type: `RequireBlankSpecial`,
    newValue: `*BLANK`
  });

  assert.deepStrictEqual(errors[1], {
    offset: { position: 39, end: 44 },
    type: `StringLiteralDupe`,
    newValue: undefined
  });

  assert.deepStrictEqual(errors[2], {
    offset: { position: 62, end: 67 },
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

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    RequireOtherBlock: true
  }, cache);

  assert.strictEqual(errors.length, 1);
  assert.deepStrictEqual(errors[0], {
    type: `RequireOtherBlock`, offset: { position: 339, end: 344 }
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

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
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

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    RequireOtherBlock: true
  }, cache);

  assert.strictEqual(errors.length, 2);
  assert.deepStrictEqual(errors[0], {
    type: `RequireOtherBlock`, offset: { position: 552, end: 557 }
  });
  assert.deepStrictEqual(errors[1], {
    type: `RequireOtherBlock`, offset: { position: 561, end: 566 }
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

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
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


  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
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

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    CollectReferences: true,
    IncorrectVariableCase: true
  }, cache);

  const RANDOMLEN = cache.find(`RANDOMLEN`);

  assert.strictEqual(RANDOMLEN.references.length, 1);
  assert.deepStrictEqual(RANDOMLEN.references[0], {
    offset: { position: 64, end: 73 }
  });

  assert.strictEqual(errors.length, 1);
  assert.deepStrictEqual(errors[0], {
    offset: { position: 64, end: 73 },
    type: `IncorrectVariableCase`,
    newValue: `RANDOMLEN`
  });
}

exports.linter_casing_on_error_not_a_variable = async () => {
  const lines = [
    `**free`,
    `dcl-c  ERROR  -1;`,
    `monitor;`,
    `  callSomething();`,
    `on-error;`,
    `  result = ERROR;`,
    `endmon;`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    CollectReferences: true,
    IncorrectVariableCase: true
  }, cache);

  assert.strictEqual(errors.length, 0, `on-error should not throw a variable casing error`);
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

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
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
  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});

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

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    UselessOperationCheck: true
  }, cache);

  assert.strictEqual(errors.length, 1);

  assert.deepStrictEqual(errors[0], {
    offset: { position: 94, end: 103 }, type: `UselessOperationCheck`
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

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    UselessOperationCheck: true
  }, cache);

  assert.strictEqual(errors.length, 1);

  assert.deepStrictEqual(errors[0], {
    offset: { position: 98, end: 107 }, type: `UselessOperationCheck`
  });

  const myProc = cache.find(`myProc`);
  assert.strictEqual(myProc.name, `myProc`);

  const parms = myProc.subItems;
  assert.strictEqual(parms[0].name, `select`);
  assert.strictEqual(parms[1].name, `name`);
  assert.strictEqual(parms[2].name, `address`);
}

exports.prettyCommentsChange = async () => {
  const lines = [
    `**FREE`,
    ``,
    `Ctl-Opt DFTACTGRP(*No);`,
    ``,
    `Dcl-s MyVariable2 Char(20);`,
    ``,
    `// my constant`,
    `//`,
    `// second line`,
    `Dcl-C theConstant 'Hello world';`,
    `//comment with bad indent`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
  const { errors } = Linter.getErrors({ uri, content: lines }, {
    PrettyComments: true
  }, cache);

  assert.strictEqual(errors.length, 1);

  assert.deepStrictEqual(errors[0], {
    offset: { position: 128, end: 130 },
    type: `PrettyComments`,
    newValue: `// `
  });
};

exports.issue_204 = async () => {
  const lines = [
    `**free`,
    ``,
    `ctl-opt dftactgrp(*no);`,
    ``,
    `///`,
    `// printf`,
    `// Print to standard out`,
    `// @param String value pointer`,
    `///`,
    `dcl-pr printf int(10) extproc('printf');`,
    `  format pointer value options(*string);`,
    `end-pr;`,
    ``,
    `dcl-ds person_t qualified template;`,
    `  name  int(10);`,
    `  age   char(50);`,
    `end-ds;`,
    ``,
    `dcl-ds myperson likeds(person_t);`,
    ``,
    `myperson = PERSON_New();`,
    `myperson.name = 'Liam Barry';`,
    `myperson.age = 25;`,
    `PERSON_printNice(myperson);`,
    ``,
    `return;`,
    ``,
    `dcl-proc PERSON_New;`,
    `  dcl-pi *n LikeDS(person_t) end-pi;`,
    `  // This is the constructor`,
    `  // Maybe parameters to set the defaults?`,
    ``,
    `  dcl-ds person likeds(person_t);`,
    ``,
    `  // Set defaults`,
    `  person.name = '';`,
    `  person.age = 0;`,
    ``,
    `  return person;`,
    `end-proc;`,
    ``,
    `dcl-proc PERSON_printNice;`,
    `  dcl-pi *n;`,
    `    person likeds(person_t);`,
    `  end-pi;`,
    ``,
    `  printf(%trim(person.name) + ' ' + %char(person.age));`,
    `end-proc;`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
  Linter.getErrors({ uri, content: lines }, {
    CollectReferences: true,
  }, cache);

  // Global checks

  const printf = cache.find(`printf`);
  assert.strictEqual(printf.references.length, 1);

  const person_t = cache.find(`person_t`);
  assert.strictEqual(person_t.references.length, 4);

  const myperson = cache.find(`myperson`);
  assert.strictEqual(myperson.references.length, 4);

  const global_person = cache.find(`person`);
  assert.strictEqual(global_person, null);

  // Proc A checks

  const PERSON_New = cache.find(`PERSON_New`);
  assert.strictEqual(PERSON_New.references.length, 1);
  const PERSON_New_person = PERSON_New.scope.find(`person`);
  assert.strictEqual(PERSON_New_person.references.length, 3);
  assert.strictEqual(PERSON_New_person.subItems.length, 2);

  // Proc B checks

  const PERSON_printNice = cache.find(`PERSON_printNice`);
  assert.strictEqual(PERSON_printNice.references.length, 1);
  const printNice_person = PERSON_printNice.scope.find(`person`);
  assert.strictEqual(printNice_person.references.length, 2);
  assert.strictEqual(printNice_person.subItems.length, 2);
}

exports.issue_237 = async () => {
  const lines = [
    `**FREE`,
    `If MyVar <> ThatValue //This is fine`,
    `;`,
    `  DoThisCode();`,
    `Else;`,
    `  CoThatCOde();`,
    `Endif;`,
    `*INLR = *ON;`,
    `Return;`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
  const { indentErrors } = Linter.getErrors({ uri, content: lines }, {
    indent: 2
  }, cache);

  assert.strictEqual(indentErrors.length, 0);
}

exports.issue_234_a = async () => {
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

  assert.strictEqual(errors.length, 0);
}


exports.issue_234_b = async () => {
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

  assert.strictEqual(errors.length, 0);
}

exports.issue_238 = async () => {
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

  assert.strictEqual(indentErrors.length, 0);
}

exports.issue_240 = async () => {
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

  assert.strictEqual(errors.length, 1);

  assert.deepStrictEqual(errors[0], {
    type: `NoExternalTo`,
    offset: {
      position: 344,
      end: 377,
    }
  });

  assert.strictEqual(lines.substring(errors[0].offset.position, errors[0].offset.end), `dcl-pr QCMDEXC  extpgm('QCMDEXC')`);
}

exports.issue_239 = async () => {
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

  assert.strictEqual(errors.length, 0);
}

exports.issue_251 = async () => {
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

  assert.strictEqual(errors.length, 1);
  assert.strictEqual(errors[0].type, `ForceOptionalParens`);
  assert.strictEqual(lines.substring(errors[0].offset.position, errors[0].offset.end), `iCost >= 10000`);
}

exports.paddr_issue_250 = async () => {
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

  assert.strictEqual(errors.length, 0);
}

exports.new_select_1 = async () => {
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

  assert.strictEqual(indentErrors.length, 5);
  // Expect all expected indent to be 2
  assert.strictEqual(indentErrors.some(e => e.expectedIndent !== 2), false)
}

exports.new_select_2 = async () => {
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

  assert.strictEqual(indentErrors.length, 0);
}