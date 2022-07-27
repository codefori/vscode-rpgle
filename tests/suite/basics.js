const vscode = require(`vscode`);
const assert = require(`assert`);

const Parser = require(`../../src/language/parser`);
const Linter = require(`../../src/language/linter`);

const URI = vscode.Uri.parse(`source.rpgle`);

exports.test1 = async () => {
  const lines = [
    `**FREE`,
    `Dcl-s MyVariable CHAR(20);`
  ].join(`\n`);

  const parser = new Parser();
  const cache = await parser.getDocs(URI, lines);

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

  const parser = new Parser();
  const cache = await parser.getDocs(URI, lines);

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

  const parser = new Parser();
  const cache = await parser.getDocs(URI, lines);

  assert.strictEqual(cache.variables.length, 2, `Expect length of 2`);
  assert.strictEqual(cache.structs.length, 1, `Expect length of 1`);
    
  assert.strictEqual(cache.structs[0].subItems.length, 2, `Expect length of 2 subitems`);

  assert.strictEqual(cache.variables[0].position.line, 1, `Index of 1 expected`);
  assert.strictEqual(cache.variables[1].position.line, 6, `Index of 6 expected`);
  assert.strictEqual(cache.structs[0].position.line, 2, `Index of 2 expected`);
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

  const parser = new Parser();
  const cache = await parser.getDocs(URI, lines);

  assert.strictEqual(cache.variables.length, 1, `Expect length of 1`);
  assert.strictEqual(cache.subroutines.length, 1, `Expect length of 1`);

  assert.strictEqual(cache.variables[0].position.line, 1, `Index of 1 expected`);
  assert.strictEqual(cache.subroutines[0].range.start, 4, `Index of 4 expected`);
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

  const parser = new Parser();
  const cache = await parser.getDocs(URI, lines);

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
    `End-Pr`,
    ``,
    `MyVariable2 = 'Hello world';`,
  ].join(`\n`);

  const parser = new Parser();
  const cache = await parser.getDocs(URI, lines);

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

  const parser = new Parser();
  const cache = await parser.getDocs(URI, lines);

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

  const parser = new Parser();
  const cache = await parser.getDocs(URI, lines);

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

  const parser = new Parser();
  const cache = await parser.getDocs(URI, lines);

  assert.strictEqual(cache.variables.length, 1, `Expect length of 1`);
  assert.strictEqual(cache.constants.length, 1, `Expect length of 1`);
  assert.strictEqual(cache.procedures.length, 1, `Expect length of 1`);
  assert.strictEqual(cache.procedures[0].subItems.length, 1, `Expect length of 1`);

  assert.strictEqual(cache.procedures[0].position.path, `'./tests/rpgle/copy1.rpgle'`, `Path is incorrect`);
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

  const parser = new Parser();
  const cache = await parser.getDocs(URI, lines);

  assert.strictEqual(cache.variables.length, 1, `Expect length of 1`);
  assert.strictEqual(cache.constants.length, 1, `Expect length of 1`);
  assert.strictEqual(cache.procedures.length, 1, `Expect length of 1`);

  const uppercase = cache.find(`UPPERCASE`);
  assert.strictEqual(uppercase.position.path, `tests,eof4`, `Path is incorrect`);
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

  const parser = new Parser();
  const cache = await parser.getDocs(URI, lines);

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

  const parser = new Parser();
  const cache = await parser.getDocs(URI, lines);

  Linter.getErrors(lines, {
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

  const parser = new Parser();
  const cache = await parser.getDocs(URI, lines);

  assert.strictEqual(cache.structs.length, 1);

  const DsChangingNodeRole = cache.find(`DsChangingNodeRole`);
  assert.strictEqual(DsChangingNodeRole.name, `DsChangingNodeRole`);
  assert.strictEqual(DsChangingNodeRole.position.line, 2);

  assert.strictEqual(DsChangingNodeRole.subItems.length, 13);
  assert.strictEqual(DsChangingNodeRole.subItems[12].name, `Role`);
};