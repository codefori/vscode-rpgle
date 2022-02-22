
const vscode = require(`vscode`);
const assert = require(`assert`);

const Parser = require(`../../src/language/parser`);
const Linter = require(`../../src/language/linter`);

const URI = vscode.Uri.parse(`source.rpgle`);

module.exports = {
  /**
   * Variable definition test
   */
  test1: async () => {
    const lines = [
      `**FREE`,
      `Dcl-s MyVariable CHAR(20);`
    ].join(`\n`);

    const parser = new Parser();
    const cache = await parser.getDocs(URI, lines);

    assert.strictEqual(cache.variables.length, 1, `Expect length of 1`);
    assert.strictEqual(cache.variables[0].position.line, 1, `Index of 1 expected`);
  },

  /**
   * Multiple variable definition test
   */
  test2: async () => {
    const lines = [
      `**FREE`,
      `Dcl-s MyVariable CHAR(20);`,
      ``,
      `Dcl-s MyVariable2 CHAR(20);`
    ].join(`\n`);

    const parser = new Parser();
    const cache = await parser.getDocs(URI, lines);

    assert.strictEqual(cache.variables.length, 2, `Expect length of 2`);
    assert.strictEqual(cache.variables[0].position.line, 1, `Index of 1 expected`);
    assert.strictEqual(cache.variables[1].position.line, 3, `Index of 3 expected`);
  },

  /**
   * Variable definition and struct definition test
   */
  test3: async () => {
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

    const parser = new Parser();
    const cache = await parser.getDocs(URI, lines);

    assert.strictEqual(cache.variables.length, 2, `Expect length of 2`);
    assert.strictEqual(cache.structs.length, 1, `Expect length of 1`);
    
    assert.strictEqual(cache.structs[0].subItems.length, 2, `Expect length of 2 subitems`);

    assert.strictEqual(cache.variables[0].position.line, 1, `Index of 1 expected`);
    assert.strictEqual(cache.variables[1].position.line, 6, `Index of 6 expected`);
    assert.strictEqual(cache.structs[0].position.line, 2, `Index of 2 expected`);
  },

  /**
   * Variable and subroutine definition test
   * */
  test4: async () => {
    const lines = [
      `**FREE`,
      `Dcl-s MyVariable2 CHAR(20);`,
      ``,
      ``,
      `Begsr theSubroutine;`,
      `  MyVariable2 = 'Hello world';`,
      `Endsr;`,
    ].join(`\n`);

    const parser = new Parser();
    const cache = await parser.getDocs(URI, lines);

    assert.strictEqual(cache.variables.length, 1, `Expect length of 1`);
    assert.strictEqual(cache.subroutines.length, 1, `Expect length of 1`);

    assert.strictEqual(cache.variables[0].position.line, 1, `Index of 1 expected`);
    assert.strictEqual(cache.subroutines[0].range.start, 4, `Index of 4 expected`);
  },

  /**
   * Variable and procedure definition test
   */ 
  test5: async () => {
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

    const parser = new Parser();
    const cache = await parser.getDocs(URI, lines);

    assert.strictEqual(cache.variables.length, 1, `Expect length of 1`);
    assert.strictEqual(cache.procedures.length, 2, `Expect length of 2`);

    assert.strictEqual(cache.variables[0].position.line, 2, `Index of 2 expected`);
    assert.strictEqual(cache.procedures[0].position.line, 4, `Index of 4 expected`);
    assert.strictEqual(cache.procedures[1].position.line, 8, `Index of 8 expected`);

    assert.strictEqual(cache.procedures[0].subItems.length, 0, `Expect length of 0`);
    assert.strictEqual(cache.procedures[1].subItems.length, 1, `Expect length of 1`);
  },

  test6: async () => {
    const lines = [
      `**FREE`,
      `Dcl-s MyVariable2 CHAR(20);`,
      ``,
      `Dcl-Pr TheProcedure;`,
      `  parmA CHAR(20);`,
      `End-Pr`,
      ``,
      `MyVariable2 = 'Hello world';`,
    ].join(`\n`);

    const parser = new Parser();
    const cache = await parser.getDocs(URI, lines);

    assert.strictEqual(cache.variables.length, 1, `Expect length of 1`);
    assert.strictEqual(cache.procedures.length, 1, `Expect length of 1`);
  },

  /**
   * Test procedure length.
   * When Procedure is defined, the prototype is overridden.
   */
  test7: async () => {
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
    const cache = await parser.getDocs(URI, lines);

    assert.strictEqual(cache.variables.length, 1, `Expect length of 1`);
    assert.strictEqual(cache.procedures.length, 1, `Expect length of 1`);
    assert.strictEqual(cache.procedures[0].subItems.length, 1, `Expect length of 1`);
  },

  /**
   * Constant definition test
   * */
  test8: async () => {
    const lines = [
      `**FREE`,
      `Dcl-s MyVariable2 Char(20);`,
      ``,
      `Dcl-C theConstant 'Hello world';`,
    ].join(`\n`);

    const parser = new Parser();
    const cache = await parser.getDocs(URI, lines);

    assert.strictEqual(cache.variables.length, 1, `Expect length of 1`);
    assert.strictEqual(cache.constants.length, 1, `Expect length of 1`);
  },

  /**
   * Check that local variables are not in global scope
   */
  test9: async () => {
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

    const parser = new Parser();
    const cache = await parser.getDocs(URI, lines);

    assert.strictEqual(cache.variables.length, 1, `Expect length of 1`);
    assert.strictEqual(cache.constants.length, 1, `Expect length of 1`);
    assert.strictEqual(cache.procedures.length, 1, `Expect length of 1`);
    assert.strictEqual(cache.procedures[0].subItems.length, 1, `Expect length of 1`);
  },

  /**
   * Test copybook
   * */
  test10: async () => {
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

    const parser = new Parser();
    const cache = await parser.getDocs(URI, lines);

    assert.strictEqual(cache.variables.length, 1, `Expect length of 1`);
    assert.strictEqual(cache.constants.length, 1, `Expect length of 1`);
    assert.strictEqual(cache.procedures.length, 1, `Expect length of 1`);
    assert.strictEqual(cache.procedures[0].subItems.length, 1, `Expect length of 1`);

    assert.strictEqual(cache.procedures[0].position.path, `'./tests/rpgle/copy1.rpgle'`, `Path is incorrect`);
    assert.strictEqual(cache.procedures[0].position.line, 2, `Index of 3 expected`);
  },


  /**
   * Test many copybooks
   * */
  test11: async () => {
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

    const parser = new Parser();
    const cache = await parser.getDocs(URI, lines);

    assert.strictEqual(cache.variables.length, 1, `Expect length of 1`);
    assert.strictEqual(cache.constants.length, 2, `Expect length of 2`);
    assert.strictEqual(cache.structs.length, 1, `Expect length of 1`);
    assert.strictEqual(cache.structs[0].subItems.length, 1, `Expect length of 1`);
    assert.strictEqual(cache.procedures.length, 1, `Expect length of 1`);
    assert.strictEqual(cache.procedures[0].subItems.length, 1, `Expect length of 1`);
  },

  test12: async () => {
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

    const parser = new Parser();
    const cache = await parser.getDocs(URI, lines);

    assert.strictEqual(cache.variables.length, 1, `Expect length of 1`);
    assert.strictEqual(cache.constants.length, 1, `Expect length of 1`);

    // One prototype and one declared
    assert.strictEqual(cache.procedures.length, 2, `Expect length of 2`);

    // Valid names
    assert.strictEqual(cache.procedures[0].name, `theExtProcedure`, `Expect valid name`);
    assert.strictEqual(cache.procedures[1].name, `theLocalProc`, `Expect valid name`);

    const theLocalProc = cache.find(`theLocalProc`);

    // Has a parameter
    assert.strictEqual(theLocalProc.subItems.length, 1, `Expect length of 1`);

    // Has a local scope
    assert.strictEqual(theLocalProc.scope !== undefined, true, `Should have a scope`);

    // Should have a local variable
    assert.strictEqual(theLocalProc.scope.variables.length, 1, `Expect length of 1`);
  },

  linter1_indent: async () => {
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
    const cache = await parser.getDocs(URI, lines);
    const { indentErrors } = Linter.getErrors(lines, {
      indent: 2
    }, cache);

    assert.strictEqual(indentErrors.length, 1, `Expect length of 1`);
    assert.strictEqual(indentErrors[0].line, 11, `Index of 9 expected`);
    assert.strictEqual(indentErrors[0].currentIndent, 0, `Value of 0 expected`);
    assert.strictEqual(indentErrors[0].expectedIndent, 2, `Value of 2 expected`);
  },

  /**
   * Testing spaces before the EOL
   */
  linter1_1_indent: async () => {
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
    const cache = await parser.getDocs(URI, lines);
    const { indentErrors } = Linter.getErrors(lines, {
      indent: 2
    }, cache);

    assert.strictEqual(indentErrors.length, 0, `Expect length of 0`);
  },

  linter2_indent: async () => {
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
    const cache = await parser.getDocs(URI, lines);
    const { indentErrors } = Linter.getErrors(lines, {
      indent: 2
    }, cache);

    assert.strictEqual(indentErrors.length, 2, `Expect length of 2`);

    assert.strictEqual(indentErrors[0].line, 10, `Index of 9 expected`);
    assert.strictEqual(indentErrors[0].currentIndent, 0, `Value of 0 expected`);
    assert.strictEqual(indentErrors[0].expectedIndent, 2, `Value of 2 expected`);

    assert.strictEqual(indentErrors[1].line, 17, `Index of 14 expected`);
    assert.strictEqual(indentErrors[1].currentIndent, 8, `Value of 8 expected`);
    assert.strictEqual(indentErrors[1].expectedIndent, 6, `Value of 6 expected`);
  },

  linter3_indent: async () => {
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
    const cache = await parser.getDocs(URI, lines);
    const { indentErrors } = Linter.getErrors(lines, {
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
  },

  linter4: async () => {
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
    const cache = await parser.getDocs(URI, lines);
    const { errors } = Linter.getErrors(lines, {
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
  },

  linter5: async () => {
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
    const cache = await parser.getDocs(URI, lines);
    const { errors } = Linter.getErrors(lines, {
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
  },

  linter6: async () => {
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
    const cache = await parser.getDocs(URI, lines);
    const { errors } = Linter.getErrors(lines, {
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
  },

  linter6_lf: async () => {
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
    const cache = await parser.getDocs(URI, lines);
    const { errors } = Linter.getErrors(lines, {
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
  },

  linter6_crlf: async () => {
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
    const cache = await parser.getDocs(URI, lines);
    const { errors } = Linter.getErrors(lines, {
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
  },

  linter7: async () => {
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
    const cache = await parser.getDocs(URI, lines);
    const { errors } = Linter.getErrors(lines, {
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
  },

  linter8: async () => {
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
    const cache = await parser.getDocs(URI, lines);
    const { errors } = Linter.getErrors(lines, {
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
      newValue: undefined
    }, `Error not as expected`);
  },

  /**
   * Check that local variables are not in global scope
   */
  linter9: async () => {
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
    const cache = await parser.getDocs(URI, lines);
    const { errors } = Linter.getErrors(lines, {
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
  },

  linter10: async () => {
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
    const cache = await parser.getDocs(URI, lines);
    const { errors } = Linter.getErrors(lines, {
      NoCTDATA: true
    }, cache);

    assert.strictEqual(errors.length, 2, `Expect length of 2`);

    assert.deepStrictEqual(errors[0], {
      range: new vscode.Range(
        new vscode.Position(2, 0),
        new vscode.Position(2, 38),
      ),
      offset: undefined,
      type: `NoCTDATA`,
      newValue: undefined
    }, `Error not as expected`);

    assert.deepStrictEqual(errors[1], {
      range: new vscode.Range(
        new vscode.Position(14, 0),
        new vscode.Position(14, 12),
      ),
      offset: { position: 0, length: 8 },
      type: `NoCTDATA`,
      newValue: undefined
    }, `Error not as expected`);
  },

  linter11: async () => {
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
    const cache = await parser.getDocs(URI, lines);
    const { errors } = Linter.getErrors(lines, {
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
  },

  linter12: async () => {
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
    const cache = await parser.getDocs(URI, lines);
    const { indentErrors } = Linter.getErrors(lines, {
      indent: 2
    }, cache);

    assert.strictEqual(indentErrors.length, 0, `Expect length of 0`);
  },

  linter13: async () => {
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
    const cache = await parser.getDocs(URI, lines);
    const { indentErrors } = Linter.getErrors(lines, {
      indent: 2
    }, cache);

    assert.strictEqual(indentErrors.length, 0, `Expect length of 0`);
  },

  linter13_commentIndent: async () => {
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
    const cache = await parser.getDocs(URI, lines);
    const { indentErrors } = Linter.getErrors(lines, {
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
  },

  linter14: async () => {
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
    const cache = await parser.getDocs(URI, lines);
    const { indentErrors } = Linter.getErrors(lines, {
      indent: 2
    }, cache);

    assert.strictEqual(indentErrors.length, 0, `Expect length of 0`);
  },

  linter15: async () => {
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
    const cache = await parser.getDocs(URI, lines);
    const { errors } = Linter.getErrors(lines, {
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
      offset: undefined,
    });

    assert.deepStrictEqual(errors[1], {
      type: `PrettyComments`,
      newValue: `// `,
      range: new vscode.Range(
        new vscode.Position(5, 0),
        new vscode.Position(5, 2),
      ),
      offset: undefined,
    });

    assert.deepStrictEqual(errors[2], {
      type: `PrettyComments`,
      newValue: ``,
      range: new vscode.Range(
        new vscode.Position(6, 0),
        new vscode.Position(6, 2),
      ),
      offset: undefined,
    });

    assert.deepStrictEqual(errors[3], {
      type: `PrettyComments`,
      newValue: `// `,
      range: new vscode.Range(
        new vscode.Position(14, 2),
        new vscode.Position(14, 4),
      ),
      offset: undefined,
    });

    assert.deepStrictEqual(errors[4], {
      type: `PrettyComments`,
      newValue: ``,
      range: new vscode.Range(
        new vscode.Position(16, 2),
        new vscode.Position(16, 4),
      ),
      offset: undefined,
    });
  },

  /**
   * Subroutine check test
   * */
  linter16: async () => {
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
    const cache = await parser.getDocs(URI, lines);
    const { errors } = Linter.getErrors(lines, {
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
      offset: undefined,
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
  },
  
  /**
   * Subroutine in procedure check test
   * */
  linter17: async () => {
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
    const cache = await parser.getDocs(URI, lines);
    const { errors } = Linter.getErrors(lines, {
      NoLocalSubroutines: true
    }, cache);
  
    assert.strictEqual(errors.length, 1, `Expect length of 1`);
  
    assert.deepStrictEqual(errors[0], {
      type: `NoLocalSubroutines`,
      range: new vscode.Range(
        new vscode.Position(8, 2),
        new vscode.Position(8, 21),
      ),
      offset: undefined,
      newValue: undefined,
    });
  },

  linter18: async () => {
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
    const cache = await parser.getDocs(URI, lines);
    const { errors } = Linter.getErrors(lines, {
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
      newValue: undefined,
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
      newValue: undefined,
    });
  },

  qualified1: async () => {
    const lines = [
      `**FREE`,
      `Dcl-Ds Kx Likerec(TitXe :*Key);`,
      `Dcl-s MyVariable2 Char(20);`,
      ``,
      `Dsply MyVariable2;`,
      ``,
      `Return`,
    ].join(`\n`);
  
    const parser = new Parser();
    const cache = await parser.getDocs(URI, lines);
    const { errors } = Linter.getErrors(lines, {
      QualifiedCheck: true,
    }, cache);
  
    assert.strictEqual(errors.length, 0, `Expect length of 0`);
  },

  ctdata1: async () => {
    const lines = [
      `**free`,
      `dcl-s myarray char(100) dim(10) ctdata;`,
      `dcl-s xxField1 char(1);`,
      `dcl-ds master qualified inz;`,
      ``,
      `  dcl-ds a inz;`,
      `    fielda1 Like(xxFiel1);`,
      `    fielda2 packed(2);`,
      `  End-ds;`,
      ``,
      `  dcl-ds b inz;`,
      `    fieldb1 like(xxField1);`,
      `    fieldb2 packed(9);`,
      `  End-ds;`,
      `End-ds;`,
      ``,
      ``,
      `eval master.a.fielda1 = 'a1';`,
      `eval master.a.f;`,
      `eval master.b.fieldb1 = 'b1';`,
      `//eval myds2.p.field1 = 'p';`,
      `//eval myds2.o.`,
      ``,
      `*INLR = *ON;`,
      `**ctdata myarray`,
      `select RMSDESC ,RMACRONYM ,RMLPID ,RMCBAPLAN ,LTTYPE ,LTID ,LTATTREA`,
      `,digits( RHHRTYPE ) as RHHRTYPE ,varchar( PWDES ,30 )`,
      ` ,EOEMP as EMP ,min( RHEFFDT ) as EFFDATE`,
      ` ,dec( 0.0 ,7,2 ) as Hours`,
      ` ,dec( 0.0 ,10,5 ) as Earned`,
      ` ,dec( 0.0 ,7,2 ) as Taken`,
      ` ,dec( ifnull( PTHRS ,0 ) ,7,2 ) as Due`,
      ` ,dec( 0.0 ,7,2 ) as Prior`,
      ` ,'N' as SysGen`,
      `from PRPEMPV0 V0`,
      `cross join PRPLPMTB RM`,
      `inner join PRPLPTTB LO on LTLPID = RMLPID`,
      `inner join PRPLPHTB HT on RHLTID = LTID`,
      `inner join PRPPHRTP on PWHTP = RHHRTYPE`,
      `left  join PRPHWLTB PT on EOEMP = PTEMP and PTLPID = LTLPID and PTTID = LTID`,
      `       and ( PTDTEOW between date( xEARNED_LEAVE_TO_x ) -7 days`,
      `        and date( xEARNED_LEAVE_TO_x ) -1 days )`,
      `where EOEFFDT = ( select EOEFFDT from PRPEOCPF where EOEMP = V0.EOEMP`,
      `            anD EOEFFDT <=xEARNED_LEAVE_TO_8x order by EOEFFDT desc fetch first row only )`,
      `and   EHHDT = ( select EHHDT from PRPEHTPF where EHEMP = V0.EOEMP`,
      `            and EHHDT <=xEARNED_LEAVE_TO_8x order by EHHDT desc fetch first row only )`,
      `and   ETEFFDT= ( select ETEFFDT from PRPETXPF where ETEMP = V0.EOEMP`,
      `            and ETEFFDT <=xEARNED_LEAVE_TO_8x order by ETEFFDT desc fetch first row only )`,
      `and RMACRONYM = 'CBA'`,
      `and EOEMP = xEMP_USEx`,
      `and LTEFFDT = ( select LTEFFDT from PRPLPTTB LI where LO.LTLPID = LI.LTLPID`,
      `                and LO.LTTYPE = LI.LTTYPE`,
      `                and LI.LTEFFDT <= xEARNED_LEAVE_TO_x`,
      `                order by LTEFFDT desc fetch first row only ) and LTSTS = 'A'`,
      `and RHEFFDT = ( select RHEFFDT from PRPLPHTB I where I.RHLTID = HT.RHLTID`,
      `                and I.RHEFFDT <= xEARNED_LEAVE_TO_x`,
      `                order by RHEFFDT desc fetch first row only ) and RHHTSTS = 'A'`,
      `group by RMSDESC ,RMACRONYM ,RMLPID ,RMCBAPLAN ,LTTYPE ,LTID ,LTATTREA`,
      ` ,RHHRTYPE ,PWDES ,EOEMP ,PTHRS`,
      `order by RMLPID ,LTID ,EFFDATE`,
    ].join(`\n`);

    const parser = new Parser();
    const cache = await parser.getDocs(URI, lines);
    const { indentErrors } = Linter.getErrors(lines, {
      indent: 2
    }, cache);

    assert.strictEqual(indentErrors.length, 0, `Expect length of 0`);
  },

  skip1: async () => {
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

    const parser = new Parser();
    const cache = await parser.getDocs(URI, lines);
    const { indentErrors } = Linter.getErrors(lines, {
      indent: 2
    }, cache);

    assert.strictEqual(indentErrors.length > 0, true, `Expect indent errors`);
  },

  skip2: async () => {
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

    const parser = new Parser();
    const cache = await parser.getDocs(URI, lines);
    const { indentErrors } = Linter.getErrors(lines, {
      indent: 2
    }, cache);

    assert.strictEqual(indentErrors.length, 0, `Expect no indent errors`);
  },  
  
  skip3: async () => {
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

    const parser = new Parser();
    const cache = await parser.getDocs(URI, lines);
    const { errors } = Linter.getErrors(lines, {
      IncorrectVariableCase: true
    }, cache);

    assert.strictEqual(errors.length, 1, `Expect one errors`);
  },

  fixed1: async () => {
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

    const parser = new Parser();
    const cache = await parser.getDocs(URI, lines);

    assert.strictEqual(cache.variables.length, 2, `Expect length of 2`);

    const wkCorp = cache.variables[0];
    assert.strictEqual(wkCorp.name, `wkCorp`);
    assert.strictEqual(wkCorp.position.line, 3);
    assert.strictEqual(wkCorp.keywords[0], `CHAR(10)`);
    assert.strictEqual(wkCorp.keywords[1], `INZ('100')`);

    const wkInvoice = cache.variables[1];
    assert.strictEqual(wkInvoice.name, `wkInvoice`);
    assert.strictEqual(wkInvoice.position.line, 4);
    assert.strictEqual(wkInvoice.keywords[0], `CHAR(15)`);
  },

  fixed2: async () => {
    const lines = [
      ``,
      `      *`,
      `      *  Field Definitions.`,
      `      *`,
      `     d Count           s              4  0`,
      `     d Format          s              8`,
      `     d GenLen          s              8`,
      `     d InLibrary       s             10`,
      `     d InType          s             10`,
      `     d ObjectLib       s             20`,
      `     d SpaceVal        s              1    inz(*BLANKS)`,
      `     d SpaceAuth       s             10    inz('*CHANGE')`,
      `     d SpaceText       s             50    inz(*BLANKS)`,
      `     d SpaceRepl       s             10    inz('*YES')`,
      `     d SpaceAttr       s             10    inz(*BLANKS)`,
      `     d UserSpaceOut    s             20`,
      `     d Worktype        s             10    inz('*OUTQ')`,
      ``,
      `     `,
    ].join(`\n`);

    const parser = new Parser();
    const cache = await parser.getDocs(URI, lines);

    assert.strictEqual(cache.variables.length, 13, `Expect length of 13`);

    const CHARFields = cache.variables.filter(v => v.keywords[0].startsWith(`CHAR`));
    assert.strictEqual(CHARFields.length, 12, `Expect length of 12`);

    const countVar = cache.variables.find(v => v.name === `Count`);
    assert.strictEqual(countVar.keywords[0], `PACKED(4:0)`);
  },

  fixed3: async () => {
    const lines = [
      `     d Worktype        s             10    INZ('*OUTQ')`,
      ``,
      `      *`,
      `     d                 DS`,
      `     d  StartPosit             1      4B 0`,
      `     d  StartLen               5      8B 0`,
      `     d  SpaceLen               9     12B 0`,
      `     d  ReceiveLen            13     16B 0`,
      `     d  MessageKey            17     20B 0`,
      `     d  MsgDtaLen             21     24B 0`,
      `     d  MsgQueNbr             25     28B 0`,
    ].join(`\n`);

    const parser = new Parser();
    const cache = await parser.getDocs(URI, lines);

    assert.strictEqual(cache.variables.length, 1, `Expect length of 1`);
    assert.strictEqual(cache.structs.length, 1, `Expect length of 1`);
    
    const Worktype = cache.variables[0];
    assert.strictEqual(Worktype.name, `Worktype`);
    assert.strictEqual(Worktype.position.line, 0);
    assert.strictEqual(Worktype.keywords[0], `CHAR(10)`);
    assert.strictEqual(Worktype.keywords[1], `INZ('*OUTQ')`);

    const DS = cache.structs[0];
    assert.strictEqual(DS.name, `*N`);
    assert.strictEqual(DS.position.line, 3);
    assert.strictEqual(DS.subItems.length, 7);
    assert.strictEqual(DS.subItems.find(i => !i.keywords[0].startsWith(`BINDEC`)), undefined);
  },

  fixed4: async () => {
    const lines = [
      ``,
      `     d InType          s             10`,
      ``,
      `      *`,
      `      * Date structure for retriving userspace info`,
      `      *`,
      `     d InputDs         DS`,
      `     d  UserSpace              1     20`,
      `     d  SpaceName              1     10`,
      `     d  SpaceLib              11     20`,
      `     d  InpFileLib            29     48`,
      `     d  InpFFilNam            29     38`,
      `     d  InpFFilLib            39     48`,
      `     d  InpRcdFmt             49     58`,
      `     d Worktype        s             10    inz('*OUTQ')`,
      ``,
    ].join(`\n`);

    const parser = new Parser();
    const cache = await parser.getDocs(URI, lines);

    assert.strictEqual(cache.variables.length, 2, `Expect length of 2`);
    assert.strictEqual(cache.structs.length, 1, `Expect length of 1`);

    const InType = cache.find(`InType`);
    assert.strictEqual(InType.name, `InType`);
    assert.strictEqual(InType.position.line, 1);

    const Worktype = cache.variables[1];
    assert.strictEqual(Worktype.name, `Worktype`);
    assert.strictEqual(Worktype.position.line, 14);

    const InputDs = cache.structs[0];
    assert.strictEqual(InputDs.name, `InputDs`);
    assert.strictEqual(InputDs.position.line, 6);
    assert.strictEqual(InputDs.subItems.length, 7);
  },

  fixed5: async () => {
    const lines = [
      ``,
      `      *`,
      `      *  Field Definitions.`,
      `      *`,
      `     d UserSpaceOut    s             20`,
      `     d Worktype        s             10    inz('*OUTQ')`,
      ``,
      `      *`,
      `     d                 DS`,
      `     d  StartPosit             1      4B 0`,
      `     d  StartLen               5      8B 0`,
      `     d  SpaceLen               9     12B 0`,
      `     d  ReceiveLen            13     16B 0`,
      `     d  MessageKey            17     20B 0`,
      `     d  MsgDtaLen             21     24B 0`,
      `     d  MsgQueNbr             25     28B 0`,
      ``,
      `      *-- Retrieve object description:  -------------------------------`,
      `     d RtvObjD         Pr                  ExtPgm( 'QUSROBJD' )`,
      `     d  RoRcvVar                  32767a         Options( *VarSize )`,
      `     d  RoRcvVarLen                  10i 0 Const`,
      `     d  RoFmtNam                      8a   Const`,
      `     d  RoObjNamQ                    20a   Const`,
      `     d  RoObjTyp                     10a   Const`,
      `     d  RoError                   32767a         Options( *VarSize )`,
      ``,
      `      *`,
      `      * Date structure for retriving userspace info`,
      `      *`,
      `     d InputDs         DS`,
      `     d  UserSpace              1     20`,
      `     d  SpaceName              1     10`,
      `     d  SpaceLib              11     20`,
      `     d  InpFileLib            29     48`,
      `     d  InpFFilNam            29     38`,
      `     d  InpFFilLib            39     48`,
      `     d  InpRcdFmt             49     58`,
    ].join(`\n`);

    const parser = new Parser();
    const cache = await parser.getDocs(URI, lines);

    assert.strictEqual(cache.variables.length, 2, `Expect length of 2`);
    assert.strictEqual(cache.structs.length, 2, `Expect length of 2`);
    assert.strictEqual(cache.procedures.length, 1, `Expect length of 1`);

    const RtvObjD = cache.procedures[0];
    assert.strictEqual(RtvObjD.name, `RtvObjD`);
    assert.strictEqual(RtvObjD.position.line, 18);
    assert.strictEqual(RtvObjD.keywords.join(` `).trim(), `EXTPGM( 'QUSROBJD' )`);
    assert.strictEqual(RtvObjD.subItems.length, 6);
  },

  fixed6: async () => {
    const lines = [
      ``,
      `0.00 DDATE0            S               D                                             130124`,
      `2.00 DDATE1            S               D                                             130129`,
      `0.00 DDATE2            S               D   DATFMT(*JIS)                              130129`,
      `4.00 DDATE3            S               D   INZ(D'2001-01-12')                        130129`,
      `5.00 DDATE3_CHAR       S             10                                              130129`,
      `0.00 D len             S              5I 0                                           130130`,
      `6.00 DTIME0            S               T   INZ(T'10.12.15')                          130129`,
      `0.00 DTIME0_CHAR       S              8                                              130129`,
      ``,
    ].join(`\n`);

    const parser = new Parser();
    const cache = await parser.getDocs(URI, lines);

    assert.strictEqual(cache.variables.length, 8, `Expect length of 8`);

    const lenVar = cache.find(`len`);
    assert.strictEqual(lenVar.name, `len`);
    assert.strictEqual(lenVar.position.line, 6);
    assert.strictEqual(lenVar.keywords[0], `INT(5)`);

    const date2Var = cache.find(`DATE2`);
    assert.strictEqual(date2Var.name, `DATE2`);
    assert.strictEqual(date2Var.position.line, 3);
    assert.strictEqual(date2Var.keywords[0], `DATE`);
    assert.strictEqual(date2Var.keywords[1], `DATFMT(*JIS)`);

    const time0Var = cache.find(`TIME0`);
    assert.strictEqual(time0Var.name, `TIME0`);
    assert.strictEqual(time0Var.position.line, 7);
    assert.strictEqual(time0Var.keywords[0], `TIME`);
    assert.strictEqual(time0Var.keywords[1], `INZ(T'10.12.15')`);
  },

  fixed7: async () => {
    const lines = [
      ``,
      `       // -----------------------`,
      ``,
      `     P Obj_Next        B                   Export`,
      `     D Obj_Next        PI                  LikeDS(ObjectDs)`,
      ``,
      `      /Free`,
      `          $UserSpace( Userspace : StartPosit : StartLen : ObjectDs);`,
      `          StartPosit += SizeEntry;`,
      ``,
      `          Return ObjectDs;`,
      `      /End-Free`,
      ``,
      `     P                 E`,
      ``,
    ].join(`\n`);

    const parser = new Parser();
    const cache = await parser.getDocs(URI, lines);

    assert.strictEqual(cache.procedures.length, 1, `Expect length of 1`);

    const Obj_Next = cache.find(`Obj_Next`);
    assert.strictEqual(Obj_Next.name, `Obj_Next`);
    assert.strictEqual(Obj_Next.position.line, 3);
    assert.strictEqual(Obj_Next.keywords.includes(`EXPORT`), true);
    assert.strictEqual(Obj_Next.keywords.includes(`LIKEDS(OBJECTDS)`), true);
    assert.strictEqual(Obj_Next.subItems.length, 0);
  },

  fixed8: async () => {
    const lines = [
      ``,
      `      **========================================================================`,
      `      ** $QUSCRTUS - API to create user space`,
      `      **========================================================================`,
      `     c     $QUSCRTUS     begsr`,
      `      **`,
      `      ** Delete old space`,
      `      **`,
      `             system('DLTOBJ OBJ(QTEMP/MEMBERS) OBJTYPE(*USRSPC)');`,
      `      **`,
      `      ** Create a user space named ListMember in QTEMP.`,
      `      **`,
      `     c                   Eval      BytesPrv = 116`,
      `     c                   Eval      SpaceName = 'MEMBERS'`,
      `     c                   Eval      SpaceLib = 'QTEMP'`,
      `      **`,
      `      ** Create the user space`,
      `      **`,
      `     c                   call(e)   'QUSCRTUS'`,
      `     c                   parm      UserSpace     UserSpaceOut`,
      `     c                   parm                    SpaceAttr`,
      `     c                   parm      4096          SpaceLen`,
      `     c                   parm                    SpaceVal`,
      `     c                   parm                    SpaceAuth`,
      `     c                   parm                    SpaceText`,
      `     c                   parm                    SpaceRepl`,
      `     c                   parm                    ErrorDs`,
      `      **`,
      `     c                   endsr`,
      ``,
      `      **========================================================================`,
      `      ** $QUSLMBR  - API List all members in a file`,
      `      **========================================================================`,
      `     c     $QUSLMBR      begsr`,
      `      **`,
      `     c                   eval      nBufLen = %size(MbrD0100)`,
      `      **`,
      `     c                   call(e)   'QUSLMBR'`,
      `     c                   parm                    UserSpaceOut`,
      `     c                   parm                    Format`,
      `     c                   parm                    FileLib`,
      `     c                   parm                    AllMembers`,
      `     c                   parm                    bOvr`,
      `     c                   parm                    ErrorDs`,
      `      *`,
      `     c                   endsr`,
      ``,
    ].join(`\n`);

    const parser = new Parser();
    const cache = await parser.getDocs(URI, lines);

    assert.strictEqual(cache.subroutines.length, 2);
    assert.strictEqual(cache.subroutines[0].name, `$QUSCRTUS`);
    assert.strictEqual(cache.subroutines[1].name, `$QUSLMBR`);
  },

  fixed9: async () => {
    const lines = [
      ``,
      `       // -----------------------`,
      `      /copy './tests/rpgle/copy1.rpgle'`,
      `       // -----------------------`,
      ``,
      `     P Obj_Next        B                   Export`,
      `     D Obj_Next        PI                  LikeDS(ObjectDs)`,
      ``,
      `      /Free`,
      `          $UserSpace( Userspace : StartPosit : StartLen : ObjectDs);`,
      `          StartPosit += SizeEntry;`,
      ``,
      `          Return ObjectDs;`,
      `      /End-Free`,
      ``,
      `     P                 E`,
      ``,
    ].join(`\n`);

    const parser = new Parser();
    const cache = await parser.getDocs(URI, lines);

    assert.strictEqual(cache.procedures.length, 2);

    const Obj_Next = cache.find(`Obj_Next`);
    assert.strictEqual(Obj_Next.name, `Obj_Next`);
    assert.strictEqual(Obj_Next.position.line, 5);
    assert.strictEqual(Obj_Next.keywords.includes(`EXPORT`), true);
    assert.strictEqual(Obj_Next.keywords.includes(`LIKEDS(OBJECTDS)`), true);
    assert.strictEqual(Obj_Next.subItems.length, 0);

    const theExtProcedure = cache.find(`theExtProcedure`);
    assert.strictEqual(theExtProcedure.name, `theExtProcedure`);
    assert.strictEqual(theExtProcedure.position.line, 2);
    assert.strictEqual(theExtProcedure.keywords.includes(`EXTPROC`), true);
    assert.strictEqual(theExtProcedure.subItems.length, 1);
  },

  fixedfree1: async () => {
    const lines = [
      `      *  Field Definitions.`,
      `      * ~~~~~~~~~~~~~~~~~~~~~~~~`,
      `     D ObjNam          s             10a`,
      `     D ObjLib          s             10a`,
      `     D ObjTyp          s             10a`,
      ``,
      `     P Obj_List        B                   Export`,
      `     D Obj_List        PI`,
      `     D    pLibrary                   10A   Const`,
      `     D    pObject                    10A   Const`,
      `     D    pType                      10A   Const`,
      `     D Result          s              5i 0`,
      ``,
      `      /Free`,
      ``,
      `          exsr $QUSCRTUS;`,
      `          ObjectLib =  pObject + pLibrary;`,
      `          WorkType = pType;`,
      ``,
      `          Format = 'OBJL0200';`,
      `          $ListObjects( Userspace : Format : ObjectLib : WorkType);`,
      `          //`,
      `          // Retrive header entry and process the user space`,
      `          //`,
      `          StartPosit = 125;`,
      `          StartLen   = 16;`,
      `          $UserSpace( Userspace : StartPosit : StartLen : GENDS);`,
      ``,
      `          StartPosit = OffsetHdr + 1;`,
      `          StartLen = %size(ObjectDS);`,
      ``,
      `          Return;`,
      ``,
      `          //--------------------------------------------------------`,
      `          // $QUSCRTUS - create userspace`,
      `          //--------------------------------------------------------`,
      `          begsr $QUSCRTUS;`,
      ``,
      `             system('DLTOBJ OBJ(QTEMP/LISTOUTQS) OBJTYPE(*USRSPC)');`,
      ``,
      `             BytesPrv = 116;`,
      `             Spacename = 'LISTOUTQS';`,
      `             SpaceLib = 'QTEMP';`,
      ``,
      `             // Create the user space`,
      `             $CreateSpace( Userspace : SpaceAttr : 4096 :`,
      `                           SpaceVal : SpaceAuth : SpaceText : SpaceRepl:`,
      `                           ErrorDs);`,
      `          endsr;`,
      `      /End-Free`,
      `     P                 E`,
      ``,
    ].join(`\n`);

    const parser = new Parser();
    const cache = await parser.getDocs(URI, lines);

    assert.strictEqual(cache.variables.length, 3);
    assert.strictEqual(cache.variables.find(i => !i.keywords.includes(`CHAR(10)`)), undefined);

    assert.strictEqual(cache.subroutines.length, 0);

    assert.strictEqual(cache.procedures.length, 1);
    
    const Obj_List = cache.find(`Obj_List`);
    assert.strictEqual(Obj_List.name, `Obj_List`);
    assert.strictEqual(Obj_List.position.line, 6);
    assert.strictEqual(Obj_List.keywords.includes(`EXPORT`), true);
    assert.strictEqual(Obj_List.subItems.length, 3);

    assert.strictEqual(Obj_List.subItems.find(i => !i.keywords.includes(`CHAR(10)`)), undefined);
    assert.strictEqual(Obj_List.subItems.find(i => !i.keywords.includes(`CONST`)), undefined);

    const scope = Obj_List.scope;
    assert.strictEqual(scope.subroutines.length, 1);
    assert.strictEqual(scope.variables.length, 1);
  },

  subds1: async () => {
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

    const parser = new Parser();
    const cache = await parser.getDocs(URI, lines);

    assert.strictEqual(cache.structs.length, 1);

    const DsChangingNodeRole = cache.find(`DsChangingNodeRole`);
    assert.strictEqual(DsChangingNodeRole.name, `DsChangingNodeRole`);
    assert.strictEqual(DsChangingNodeRole.position.line, 2);

    assert.strictEqual(DsChangingNodeRole.subItems.length, 13);
    assert.strictEqual(DsChangingNodeRole.subItems[12].name, `Role`);
  },

  likeds1: async () => {
    const lines = [
      `**FREE`,
      `Dcl-s MyVariable2 CHAR(20);`,
      `Dcl-Ds astructure qualified;`,
      `  Subitem1 CHAR(20);`,
      `  Subitem2 CHAR(20);`,
      `End-ds;`,
      `Dcl-s MyVariable CHAR(20);`,
      `Dcl-Ds MyOtherStruct LikeDS(Astructure);`,
      `//Yes`
    ].join(`\n`);

    const parser = new Parser();
    const cache = await parser.getDocs(URI, lines);

    assert.strictEqual(cache.variables.length, 2);
    assert.strictEqual(cache.structs.length, 2);

    const MyOtherStruct = cache.find(`MyOtherStruct`);
    assert.strictEqual(MyOtherStruct.name, `MyOtherStruct`);
    assert.strictEqual(MyOtherStruct.position.line, 7);
    assert.strictEqual(MyOtherStruct.subItems.length, 2);
  },

  likeds2: async () => {
    const lines = [
      `**FREE`,
      `Dcl-s MyVariable2 CHAR(20);`,
      `Dcl-Ds astructure qualified;`,
      `  Subitem1 CHAR(20);`,
      `  Subitem2 CHAR(20);`,
      `End-ds;`,
      `Dcl-s MyVariable CHAR(20);`,
      `Dsply MyVariable;`,
      `Return;`,
      `Dcl-Proc myprocedure;`,
      `  Dcl-Pi *N;`,
      `    inputDS Likeds(astructure);`,
      `  End-Pi;`,
      `  Dsply 'Inside';`,
      `  Return;`,
      `End-Proc;`
    ].join(`\n`);

    const parser = new Parser();
    const cache = await parser.getDocs(URI, lines);

    assert.strictEqual(cache.variables.length, 2);
    assert.strictEqual(cache.structs.length, 1);
    assert.strictEqual(cache.procedures.length, 1);

    const MyOtherStruct = cache.find(`myprocedure`);
    assert.strictEqual(MyOtherStruct.name, `myprocedure`);
    assert.strictEqual(MyOtherStruct.position.line, 9);
    assert.strictEqual(MyOtherStruct.subItems.length, 1);

    const parmInputDs = MyOtherStruct.subItems[0];
    assert.strictEqual(parmInputDs.name, `inputDS`);
    assert.strictEqual(parmInputDs.position.line, 11);
    assert.strictEqual(parmInputDs.subItems.length, 2);
  },

  eof1: async () => {
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

    const parser = new Parser();
    const cache = await parser.getDocs(URI, lines);

    const uppercase = cache.find(`UPPERCASE`);
    assert.strictEqual(uppercase.name, `UPPERCASE`);
    assert.strictEqual(uppercase.position.line, 0);
    assert.strictEqual(uppercase.subItems.length, 2);
  },

  eof2: async () => {
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

    const parser = new Parser();
    const cache = await parser.getDocs(URI, lines);

    assert.strictEqual(cache.procedures.length, 1);

    const uppercase = cache.find(`UPPERCASE`);
    assert.strictEqual(uppercase.name, `UPPERCASE`);
    assert.strictEqual(uppercase.position.line, 0);
    assert.strictEqual(uppercase.subItems.length, 2);
  },

  /**
   * Similar to linter18 test
   */
  eof3: async () => {
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
    
    const parser = new Parser();
    const cache = await parser.getDocs(URI, lines);
    const { errors } = Linter.getErrors(lines, {
      NoGlobalsInProcedures: true
    }, cache);

    assert.strictEqual(cache.procedures.length, 1);
    assert.strictEqual(errors.length, 2);
  },

  eof4: async () => {
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

    const parser = new Parser();
    const cache = await parser.getDocs(URI, lines);

    assert.strictEqual(cache.variables.length, 1, `Expect length of 1`);
    assert.strictEqual(cache.procedures.length, 1, `Expect length of 1`);

    const uppercase = cache.find(`UPPERCASE`);

    assert.strictEqual(uppercase.subItems.length, 2, `Expect length of 2`);

    assert.strictEqual(uppercase.position.path, `'./tests/rpgle/eof4.rpgle'`, `Path is incorrect`);
    assert.strictEqual(uppercase.position.line, 0, `Index of 0 expected`);
  },

  columnFix: async () => {
    const lines = [
      `       Dcl-pr abcd1         Extpgm('ABC049');`,
      `         ParentProductSearch           zoned(11);`,
      `         AllowSelect                   char(1)   Options(*nopass);`,
      `         ReturnItemNumber              zoned(7)  Options(*nopass);`,
      `       end-pr;`,
      `       dcl-pr abcd2    extpgm('ABC039');`,
      `         SelectFlag                  char(1);`,
      `         ReturnProduct               zoned(7);`,
      `         SupplierFilter              zoned(3) options(*nopass);`,
      `         DescriptionFilter           char(20) Options(*nopass);`,
      `       end-pr;`,
      `       dcl-pr abcd3      extpgm('ABC001');`,
      `         ProductZoned                  Zoned(7);`,
      `       end-pr;`,
    ].join(`\n`);

    const parser = new Parser();
    const cache = await parser.getDocs(URI, lines);

    assert.strictEqual(cache.procedures.length, 3);
  }
}