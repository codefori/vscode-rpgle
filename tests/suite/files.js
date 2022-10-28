
const assert = require(`assert`);

const {default: parserSetup} = require(`../parserSetup`);

const uri = `source.rpgle`;
  
exports.simple_file = async () => {
  const lines = [
    `**free`,
    ``,
    `dcl-f employee disk usage(*input);`,
    ``,
    `dsply employee.workdept;`,
    ``,
    `return;`
  ].join(`\n`);

  const parser = parserSetup();
  const cache = await parser.getDocs(uri, lines);

  assert.strictEqual(cache.files.length, 1);
  assert.strictEqual(cache.structs.length, 0);

  const fileDef = cache.find(`employee`);
  assert.strictEqual(fileDef.name, `employee`);
  assert.strictEqual(fileDef.keyword[`DISK`], true);
  assert.strictEqual(fileDef.keyword[`USAGE`], `*INPUT`);

  // file record formats should be expanded into the subitems
  assert.strictEqual(fileDef.subItems.length, 1);

  const empRdcFmt = fileDef.subItems[0];

  assert.strictEqual(empRdcFmt.name, `EMPLOYEE`);

  // 14 fields inside of this record format
  assert.strictEqual(empRdcFmt.subItems.length, 14);
};

exports.many_formats = async () => {
  const lines = [
    `**free`,
    ``,
    `dcl-f emps workstn;`,
    ``,
    `write SFLDTA;`,
    ``,
    `return;`
  ].join(`\n`);

  const parser = parserSetup();
  const cache = await parser.getDocs(uri, lines);

  assert.strictEqual(cache.files.length, 1);

  const fileDef = cache.find(`emps`);
  assert.strictEqual(fileDef.name, `emps`);
  assert.strictEqual(fileDef.keyword[`WORKSTN`], true);

  // file record formats should be expanded into the subitems
  assert.strictEqual(fileDef.subItems.length, 2);

  const sfldta = fileDef.subItems[0];
  assert.strictEqual(sfldta.name, `SFLDTA`);
  assert.strictEqual(sfldta.subItems.length, 5);

  const sflctl = fileDef.subItems[1];
  assert.strictEqual(sflctl.name, `SFLCTL`);
  assert.strictEqual(sflctl.subItems.length, 1);
};

exports.ds_extname = async () => {
  const lines = [
    `**free`,
    ``,
    `Dcl-Ds Employee ExtName('EMPLOYEE') Qualified;`,
    `end-ds;`,
    ``,
    `Dsply Employee.empno;`,
    ``,
    `return;`
  ].join(`\n`);

  const parser = parserSetup();
  const cache = await parser.getDocs(uri, lines);

  assert.strictEqual(cache.files.length, 0);
  assert.strictEqual(cache.structs.length, 1);

  const structDef = cache.find(`employee`);
  assert.strictEqual(structDef.name, `Employee`);
  assert.strictEqual(structDef.subItems.length, 14);
};

exports.ds_extname_no_alias = async () => {
  const lines = [
    `**free`,
    ``,
    `Dcl-Ds dept ExtName('department') Qualified;`,
    `end-ds;`,
    ``,
    `Dsply dept.deptname;`,
    ``,
    `return;`
  ].join(`\n`);

  const parser = parserSetup();
  const cache = await parser.getDocs(uri, lines);

  assert.strictEqual(cache.files.length, 0);
  assert.strictEqual(cache.structs.length, 1);

  const dept = cache.find(`dept`);
  assert.strictEqual(dept.subItems.length, 5);

  assert.strictEqual(dept.subItems[0].name, `DEPTNO`);
  assert.strictEqual(dept.subItems[1].name, `DEPTNAME`);
}

exports.ds_extname_alias = async () => {
  const lines = [
    `**free`,
    ``,
    `Dcl-Ds dept ExtName('department') alias Qualified;`,
    `end-ds;`,
    ``,
    `Dsply dept.deptname;`,
    ``,
    `return;`
  ].join(`\n`);

  const parser = parserSetup();
  const cache = await parser.getDocs(uri, lines);

  assert.strictEqual(cache.files.length, 0);
  assert.strictEqual(cache.structs.length, 1);

  const dept = cache.find(`dept`);
  assert.strictEqual(dept.subItems.length, 5);

  assert.strictEqual(dept.subItems[0].name, `DEPTNO`);
  assert.strictEqual(dept.subItems[1].name, `DEPT_NAME`);
}

exports.file_prefix = async () => {
  const lines = [
    `**free`,
    ``,
    `Dcl-f display workstn usropn prefix(d);`,
    ``,
    `Exfmt display;`,
    ``,
    `return;`
  ].join(`\n`);

  const parser = parserSetup();
  const cache = await parser.getDocs(uri, lines);

  const disp = cache.find(`display`);
  assert.strictEqual(disp.subItems[0].subItems[0].name, `DE1_OPTION`);
}
