
const assert = require(`assert`);

const {default: parserSetup} = require(`../parserSetup`);
const {default: Linter} = require(`../../server/src/language/linter`);
const path = require(`path`);
const { Range, Position } = require(`../../server/src/language/models/DataPoints`);

const uri = `source.rpgle`;

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

    const parser = parserSetup();
    const cache = await parser.getDocs(uri, lines);
    const { indentErrors } = Linter.getErrors({uri, content: lines}, {
      indent: 2
    }, cache);

    assert.strictEqual(cache.includes.length, 0); // Because it's not found.
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

    const parser = parserSetup();
    const cache = await parser.getDocs(uri, lines);
    const { indentErrors } = Linter.getErrors({uri, content: lines}, {
      indent: 2
    }, cache);

    assert.strictEqual(cache.includes.length, 0); // Because it's not found.
    assert.strictEqual(indentErrors.length, 0, `Expect no indent errors`);
  },

  skip2_issue91_1: async () => {
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

    const parser = parserSetup();
    const cache = await parser.getDocs(uri, lines);
    const { indentErrors } = Linter.getErrors({uri, content: lines}, {
      indent: 2
    }, cache);

    assert.strictEqual(cache.includes.length, 0); // Because it's not found.
    assert.strictEqual(indentErrors.length, 0, `Expect no indent errors`);
  },

  skip2_issue91_2: async () => {
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

    const parser = parserSetup();
    const cache = await parser.getDocs(uri, lines);
    const { indentErrors } = Linter.getErrors({uri, content: lines}, {
      indent: 2
    }, cache);

    assert.strictEqual(cache.includes.length, 0); // Because it's not found.
    assert.strictEqual(indentErrors.length, 3, `Expect no indent errors`);
  },

  skip2_issue91: async () => {
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
    
    const parser = parserSetup();
    const cache = await parser.getDocs(uri, lines);
    const { errors } = Linter.getErrors({uri, content: lines}, {
      IncorrectVariableCase: true
    }, cache);

    assert.strictEqual(cache.procedures.length, 1);
    const theProcedure2 = cache.find(`theProcedure2`);
    assert.strictEqual(theProcedure2.name, `theProcedure2`);

    assert.strictEqual(errors.length, 0);
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

    const parser = parserSetup();
    const cache = await parser.getDocs(uri, lines);
    const { errors } = Linter.getErrors({uri, content: lines}, {
      IncorrectVariableCase: true
    }, cache);

    assert.strictEqual(cache.includes.length, 0); // Because it's not found.
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

    const parser = parserSetup();
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

    const parser = parserSetup();
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
    
    const parser = parserSetup();
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

    const parser = parserSetup();
    const cache = await parser.getDocs(uri, lines);

    assert.strictEqual(cache.includes.length, 1);
    assert.strictEqual(cache.includes[0].line, 4);
    
    assert.strictEqual(cache.variables.length, 1, `Expect length of 1`);
    assert.strictEqual(cache.procedures.length, 1, `Expect length of 1`);

    const uppercase = cache.find(`UPPERCASE`);

    assert.strictEqual(uppercase.subItems.length, 2, `Expect length of 2`);

    const baseNameInclude = path.basename(uppercase.position.path);
    assert.strictEqual(baseNameInclude, `eof4.rpgle`, `Path is incorrect`);
    assert.strictEqual(uppercase.position.line, 0, `Index of 0 expected`);
  },


  /**
   * EOF inside of IF directive
   */
  eof5_issue181: async () => {
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
    
    const parser = parserSetup();
    const cache = await parser.getDocs(uri, lines);
    const { errors } = Linter.getErrors({uri, content: lines}, {
      IncorrectVariableCase: true
    }, cache);

    assert.strictEqual(cache.procedures.length, 1);
    const theProcedure2 = cache.find(`theProcedure2`);
    assert.strictEqual(theProcedure2.name, `theProcedure2`);

    assert.strictEqual(errors.length, 1);
  },

  incorrectEnd1: async () => {
    const lines = [
      `Dcl-S Text Char(52);`,
      ``,
      `Text = 'Hello world';`,
      ``,
      `End-Proc;`
    ].join(`\n`);

    const parser = parserSetup();
    const cache = await parser.getDocs(uri, lines);

    const { errors } = Linter.getErrors({uri, content: lines}, {
      PrettyComments: true
    }, cache);

    assert.deepStrictEqual(errors[0], {
      type: `UnexpectedEnd`,
      range: new Range(
        new Position(4, 0),
        new Position(4, 8),
      ),
      offset: {
        position: 0,
        end: 8
      },
    });
  },

  incorrectEnd2: async () => {
    const lines = [
      `**free`,
      ``,
      `dcl-proc *inzsr;`,
      `  dsply 'hello world';`,
      `endsr;`,
    ].join(`\n`);

    const parser = parserSetup();
    const cache = await parser.getDocs(uri, lines);

    const { errors } = Linter.getErrors({uri, content: lines}, {
      PrettyComments: true
    }, cache);

    assert.deepStrictEqual(errors[0], {
      type: `UnexpectedEnd`,
      range: new Range(
        new Position(4, 0),
        new Position(4, 5),
      ),
      offset: {
        position: 0,
        end: 5
      },
    });
  },

  incorrectEnd3: async () => {
    const lines = [
      `**free`,
      ``,
      `begsr hello;`,
      `  dsply 'hello world';`,
      `end-proc;`,
    ].join(`\n`);

    const parser = parserSetup();
    const cache = await parser.getDocs(uri, lines);

    const { errors } = Linter.getErrors({uri, content: lines}, {
      PrettyComments: true
    }, cache);

    assert.deepStrictEqual(errors[0], {
      type: `UnexpectedEnd`,
      range: new Range(
        new Position(4, 0),
        new Position(4, 8),
      ),
      offset: {
        position: 0,
        end: 8
      },
    });
  },

  incorrectEnd4: async () => {
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

    const parser = parserSetup();
    const cache = await parser.getDocs(uri, lines);

    const { errors } = Linter.getErrors({uri, content: lines}, {
      PrettyComments: true
    }, cache);

    assert.deepStrictEqual(errors[0], {
      type: `UnexpectedEnd`,
      range: new Range(
        new Position(11, 0),
        new Position(11, 8),
      ),
      offset: {
        position: 0,
        end: 8
      },
    });
  },

  if1: async () => {
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

    const parser = parserSetup();
    const cache = await parser.getDocs(uri, lines);

    assert.strictEqual(cache.structs.length, 2);
    
    const Prp00a = cache.find(`Prp00a`);
    assert.strictEqual(Prp00a.subItems.length, 7);
    assert.strictEqual(Prp00a.keyword[`QUALIFIED`], true);
    assert.strictEqual(Prp00a.keyword[`TEMPLATE`], undefined);
  },

  if2: async () => {
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

    const parser = parserSetup();
    const cache = await parser.getDocs(uri, lines);

    assert.strictEqual(cache.structs.length, 1);
    
    const someDs = cache.find(`someDs`);
    assert.strictEqual(someDs.keyword[`BASED`], undefined);
  },

  exec_sql: async () => {
    const lines = [
      `**FREE`,
      ``,
      `EXEC SQL`,
      ` select count(*) into :NB_LIG`,
      ` from DEPARTMENT`,
      ` where DEPTNAME = 'A'`,
      `EXEC SQL`,
      `  DECLARE C1 CURSOR FOR`,
      `  select *`,
      `  from DEPARTMENT`,
      `  where DEPTNAME = 'A';`,
      `exec sql`,
      ` OPEN C1`,
      `exec sql`,
      `   Fetch C1 INTO :DEPARTMENT`,
      `exec sql`,
      ` CLOSE C1`,
    ].join(`\n`);
  
    const parser = parserSetup();
    const cache = await parser.getDocs(uri, lines);
  
    assert.strictEqual(cache.cursors.length, 1);
    assert.strictEqual(cache.cursors[0].name, `C1`);
  }
}