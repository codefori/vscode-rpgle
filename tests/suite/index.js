
const vscode = require(`vscode`);
const assert = require(`assert`);

const Parser = require(`../../src/parser`);
const Linter = require(`../../src/linter`);

const URI = vscode.Uri.parse(`source.rpgle`);

module.exports = {
  /**
   * Variable definition test
   */
  test1: async () => {
    const lines = [
      `**FREE`,
      `Dcl-s MyVariable Char(20);`
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
      `Dcl-s MyVariable Char(20);`,
      ``,
      `Dcl-s MyVariable2 Char(20);`
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
      `Dcl-s MyVariable2 Char(20);`,
      `Dcl-Ds astructure qualified;`,
      `  Subitem1 Char(20);`,
      `  Subitem2 Char(20);`,
      `End-ds;`,
      `Dcl-s MyVariable Char(20);`,
      `//Yes`
    ].join(`\n`);

    const parser = new Parser();
    const cache = await parser.getDocs(URI, lines);

    assert.strictEqual(cache.variables.length, 2, `Expect length of 2`);
    assert.strictEqual(cache.structs.length, 1, `Expect length of 1`);
    
    assert.strictEqual(cache.structs[0].subItems.length, 2, `Expect length of 2 subitems`);

    assert.strictEqual(cache.variables[0].position.line, 0, `Index of 0 expected`);
    assert.strictEqual(cache.variables[1].position.line, 5, `Index of 5 expected`);
    assert.strictEqual(cache.structs[0].position.line, 1, `Index of 1 expected`);
  },

  /**
   * Variable and subroutine definition test
   * */
  test4: async () => {
    const lines = [
      `Dcl-s MyVariable2 Char(20);`,
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

    assert.strictEqual(cache.variables[0].position.line, 0, `Index of 0 expected`);
    assert.strictEqual(cache.subroutines[0].position.line, 3, `Index of 3 expected`);
  },

  /**
   * Variable and procedure definition test
   */ 
  test5: async () => {
    const lines = [
      ``,
      `Dcl-s MyVariable2 Char(20);`,
      ``,
      `Dcl-Proc theProcedure;`,
      `  MyVariable2 = 'Hello world';`,
      `End-Proc;`,
      ``,
      `Dcl-Proc setValue;`,
      `  Dcl-Pi *N;`,
      `    newValue Char(20);`,
      `  End-Pi;`,
      `  MyVariable2 = newValue;`,
      `End-Proc;`,
    ].join(`\n`);

    const parser = new Parser();
    const cache = await parser.getDocs(URI, lines);

    assert.strictEqual(cache.variables.length, 1, `Expect length of 1`);
    assert.strictEqual(cache.procedures.length, 2, `Expect length of 2`);

    assert.strictEqual(cache.variables[0].position.line, 1, `Index of 1 expected`);
    assert.strictEqual(cache.procedures[0].position.line, 3, `Index of 3 expected`);
    assert.strictEqual(cache.procedures[1].position.line, 7, `Index of 7 expected`);

    assert.strictEqual(cache.procedures[0].subItems.length, 0, `Expect length of 0`);
    assert.strictEqual(cache.procedures[1].subItems.length, 1, `Expect length of 1`);
  },

  test6: async () => {
    const lines = [
      `Dcl-s MyVariable2 Char(20);`,
      ``,
      `Dcl-Pr TheProcedure;`,
      `  parmA char(20);`,
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
      ``,
      `Dcl-Pr TheProcedure;`,
      `  parmA char(20);`,
      `End-Pr`,
      ``,
      `Dcl-S theVar Char(20);`,
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
      `Dcl-s MyVariable2 Char(20);`,
      ``,
      `Dcl-C theConstant 'Hello world';`,
    ].join(`\n`);

    const parser = new Parser();
    const cache = await parser.getDocs(URI, lines);

    assert.strictEqual(cache.variables.length, 1, `Expect length of 1`);
    assert.strictEqual(cache.constants.length, 1, `Expect length of 1`);
  },

  linter1_indent: async () => {
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
      `Endif;`,
      `Return;`
    ].join(`\n`);

    const parser = new Parser();
    const cache = await parser.getDocs(URI, lines);
    const { indentErrors } = Linter.getErrors(lines, {
      indent: 2
    }, cache);

    assert.strictEqual(indentErrors.length, 1, `Expect length of 1`);
    assert.strictEqual(indentErrors[0].line, 9, `Index of 9 expected`);
    assert.strictEqual(indentErrors[0].currentIndent, 0, `Value of 0 expected`);
    assert.strictEqual(indentErrors[0].expectedIndent, 2, `Value of 2 expected`);
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
    const { indentErrors } = Linter.getErrors(lines, {
      indent: 2
    }, cache);

    assert.strictEqual(indentErrors.length, 2, `Expect length of 2`);

    assert.strictEqual(indentErrors[0].line, 9, `Index of 9 expected`);
    assert.strictEqual(indentErrors[0].currentIndent, 0, `Value of 0 expected`);
    assert.strictEqual(indentErrors[0].expectedIndent, 2, `Value of 2 expected`);

    assert.strictEqual(indentErrors[1].line, 14, `Index of 14 expected`);
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
  }
}