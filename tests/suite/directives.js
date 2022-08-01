
const vscode = require(`vscode`);
const assert = require(`assert`);

const Parser = require(`../../src/language/parser`);
const Linter = require(`../../src/language/linter`);

const uri = vscode.Uri.parse(`source.rpgle`);

module.exports = {
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
    const cache = await parser.getDocs(uri, lines);
    const { indentErrors } = Linter.getErrors({uri, content: lines}, {
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
    const cache = await parser.getDocs(uri, lines);
    const { indentErrors } = Linter.getErrors({uri, content: lines}, {
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
    const cache = await parser.getDocs(uri, lines);
    const { errors } = Linter.getErrors({uri, content: lines}, {
      IncorrectVariableCase: true
    }, cache);

    assert.strictEqual(errors.length, 1, `Expect one errors`);
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
    const cache = await parser.getDocs(uri, lines);

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
    const cache = await parser.getDocs(uri, lines);

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
    const cache = await parser.getDocs(uri, lines);
    const { errors } = Linter.getErrors({uri, content: lines}, {
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
    const cache = await parser.getDocs(uri, lines);

    assert.strictEqual(cache.variables.length, 1, `Expect length of 1`);
    assert.strictEqual(cache.procedures.length, 1, `Expect length of 1`);

    const uppercase = cache.find(`UPPERCASE`);

    assert.strictEqual(uppercase.subItems.length, 2, `Expect length of 2`);

    assert.strictEqual(uppercase.position.path, `'./tests/rpgle/eof4.rpgle'`, `Path is incorrect`);
    assert.strictEqual(uppercase.position.line, 0, `Index of 0 expected`);
  },

  incorrectEnd1: async () => {
    const lines = [
      `Dcl-S Text Char(52);`,
      ``,
      `Text = 'Hello world';`,
      ``,
      `End-Proc;`
    ].join(`\n`);

    const parser = new Parser();
    const cache = await parser.getDocs(uri, lines);

    const { errors } = Linter.getErrors({uri, content: lines}, {
      PrettyComments: true
    }, cache);

    assert.deepStrictEqual(errors[0], {
      type: `UnexpectedEnd`,
      range: new vscode.Range(
        new vscode.Position(4, 0),
        new vscode.Position(4, 8),
      ),
      offset: {
        position: 0,
        length: 8
      },
    });
  },
}