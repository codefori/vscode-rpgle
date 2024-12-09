
import path from "path";
import setupParser from "../parserSetup";
import { test, expect } from "vitest";

const parser = setupParser();
const uri = `source.rpgle`;
  
test("simple_file", async () => {
  const lines = [
    `**free`,
    ``,
    `dcl-f employee disk usage(*input);`,
    ``,
    `dsply employee.workdept;`,
    ``,
    `return;`
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {withIncludes: true, ignoreCache: true});

  expect(cache.files.length).toBe(1);
  expect(cache.structs.length).toBe(0);

  const fileDef = cache.find(`employee`);
  expect(fileDef.name).toBe(`employee`);
  expect(fileDef.keyword[`DISK`]).toBe(true);
  expect(fileDef.keyword[`USAGE`]).toBe(`*input`);

  // file record formats should be expanded into the subitems
  expect(fileDef.subItems.length).toBe(1);

  const empRdcFmt = fileDef.subItems[0];

  expect(empRdcFmt.name).toBe(`EMPLOYEE`);

  expect(empRdcFmt.subItems[1].keyword[`VARCHAR`]).toBe(`12`);																	   
  // 14 fields inside of this record format
  expect(empRdcFmt.subItems.length).toBe(14);
});

test("many_formats", async () => {
  const lines = [
    `**free`,
    ``,
    `dcl-f emps workstn;`,
    ``,
    `write SFLDTA;`,
    ``,
    `return;`
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {withIncludes: true, ignoreCache: true});

  expect(cache.files.length).toBe(1);

  const fileDef = cache.find(`emps`);
  expect(fileDef.name).toBe(`emps`);
  expect(fileDef.keyword[`WORKSTN`]).toBe(true);

  // file record formats should be expanded into the subitems
  expect(fileDef.subItems.length).toBe(2);

  const sfldta = fileDef.subItems[0];
  expect(sfldta.name).toBe(`SFLDTA`);
  expect(sfldta.subItems.length).toBe(5);

  const sflctl = fileDef.subItems[1];
  expect(sflctl.name).toBe(`SFLCTL`);
  expect(sflctl.subItems.length).toBe(1);
});

test("ds_extname", async () => {
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

  const cache = await parser.getDocs(uri, lines, {withIncludes: true, ignoreCache: true});

  expect(cache.files.length).toBe(0);
  expect(cache.structs.length).toBe(1);

  const structDef = cache.find(`employee`);
  expect(structDef.name).toBe(`Employee`);
  expect(structDef.subItems.length).toBe(14);
});

test("ds_extname_no_alias", async () => {
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

  const cache = await parser.getDocs(uri, lines, {withIncludes: true, ignoreCache: true});

  expect(cache.files.length).toBe(0);
  expect(cache.structs.length).toBe(1);

  const dept = cache.find(`dept`);
  expect(dept.subItems.length).toBe(5);

  expect(dept.subItems[0].name).toBe(`DEPTNO`);
  expect(dept.subItems[1].name).toBe(`DEPTNAME`);
});

test("ds_extname_alias", async () => {
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

  const cache = await parser.getDocs(uri, lines, {withIncludes: true, ignoreCache: true});

  expect(cache.files.length).toBe(0);
  expect(cache.structs.length).toBe(1);

  const dept = cache.find(`dept`);
  expect(dept.subItems.length).toBe(5);

  expect(dept.subItems[0].name).toBe(`DEPTNO`);
  expect(dept.subItems[1].name).toBe(`DEPTNAME`);
});

test("file_prefix", async () => {
  const lines = [
    `**free`,
    ``,
    `Dcl-f display workstn usropn prefix(d);`,
    ``,
    `Exfmt display;`,
    ``,
    `return;`
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {withIncludes: true, ignoreCache: true});

  const disp = cache.find(`display`);
  expect(disp.subItems[0].subItems[0].name).toBe(`DE1_OPTION`);
});

test('file DS in a copy book', async () => {
  const lines = [
    `**free`,
    `ctl-opt main(Main);`,
    `/copy './rpgle/file1.rpgleinc'`,
    ``,
    `dcl-proc Main;`,
    `dcl-pi *n;`,
    `end-pi;`,
    ``,
    `dcl-ds SomeStruct likeds(GlobalStruct) inz;`,
    ``,
    `end-proc;`,
  ].join(`\n`);


  const cache = await parser.getDocs(uri, lines, {withIncludes: true, ignoreCache: true});

  const globalStruct = cache.find(`GlobalStruct`);
  expect(globalStruct.subItems.length).toBeGreaterThan(0);

  const mainProc = cache.find(`Main`);

  expect(mainProc).toBeDefined();

  const someStruct = mainProc.scope.find(`SomeStruct`);
  expect(someStruct).toBeDefined();
  expect(someStruct.subItems.length).toBeGreaterThan(0);

  expect(someStruct.subItems.map(s => ({name: s.name, keyword: s.keyword}))).toMatchObject(globalStruct.subItems.map(s => ({name: s.name, keyword: s.keyword})));
})