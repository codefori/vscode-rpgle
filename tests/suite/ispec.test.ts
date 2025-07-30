import { expect, test } from "vitest";
import setupParser from "../parserSetup";
import exp from "constants";
import { parseISpec } from "../../language/models/fixed";

const parser = setupParser();
const uri = `source.rpgle`;

test('ispec rename 1', async () => {
  const lines =[
    `        dcl-f qrpglesrc prefix('RPG_');`,
    `        dcl-f qcbblesrc;`,
    ``,
    `      * Use the prefixed name if an I spec is needed`,
    `     Iqarpglesrc`,
    `     I                                          RPG_SRCSEQ              10`,
    `     I                                          RPG_SRCDTA              11`,
    `      * Renaming a prefixed field uses the external name. ALL_SRCDAT`,
    `      * is also one of the internal fields for QACBLLESRC`,
    `     I              SRCDAT                      ALL_SRCDAT              12`,
    ``,
    `      * Renaming I spec fields`,
    `      *             External name               Internal name`,
    `     Iqacbllesrc`,
    `     I              SRCSEQ                      CBL_SRCSEQ`,
    `     I              SRCDTA                      CBL_SRCDTA`,
    `     I              SRCDAT                      ALL_SRCDAT`,
  ].join('\n');

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true});

  const symbols = cache.symbols;
  expect(symbols.length).toBeGreaterThan(0);

  const files = cache.files;
  expect(files.length).toBe(2);

  const rpgSrc = files.find(f => f.name === `qrpglesrc`);
  expect(rpgSrc).toBeDefined();

  const rpgExpectedColumns = [`ALL_SRCDAT`, `RPG_SRCSEQ`, `RPG_SRCDTA`];
  expect(rpgSrc.subItems[0].subItems.map(s => s.name)).toEqual(rpgExpectedColumns);

  const cblSrc = files.find(f => f.name === `qcbblesrc`);
  expect(cblSrc).toBeDefined();
  expect(cblSrc.subItems[0].subItems.length).toBe(3);

  const cblExpectedColumns = [`ALL_SRCDAT`, `CBL_SRCSEQ`, `CBL_SRCDTA`];
  expect(cblSrc.subItems[0].subItems.map(s => s.name)).toEqual(cblExpectedColumns);

  // TODO: add check for input symbols in cache

  const inputs = cache.symbols.filter(s => s.type === `input`);

  expect(inputs.length).toBe(2);

  const qarpg = inputs.find(s => s.name === `qarpglesrc`);
  expect(qarpg).toBeDefined();
  expect(qarpg.subItems.length).toBe(3);
  const qaRpgNames = [`RPG_SRCSEQ`, `RPG_SRCDTA`, `ALL_SRCDAT`];
  expect(qarpg.subItems.map(s => s.name)).toEqual(qaRpgNames);

  const qacbl = inputs.find(s => s.name === `qacbllesrc`);
  expect(qacbl).toBeDefined();
  expect(qacbl.subItems.length).toBe(3);
  const qaCblNames = [`CBL_SRCSEQ`, `CBL_SRCDTA`, `ALL_SRCDAT`];
  expect(qacbl.subItems.map(s => s.name)).toEqual(qaCblNames);

  const allSrcDat = cache.find(`ALL_SRCDAT`);
  expect(allSrcDat).toBeDefined();

  const cblSrcSeq = cache.find(`CBL_SRCSEQ`);
  expect(cblSrcSeq).toBeDefined();
});

test('ispec range tests', async () => {
  const lines = [
    `     ISRCPF     NS`,
    `     I                                 13   90  WKLINE`
  ];

  const a = parseISpec(1, 0, lines[0]);
  expect(a).toBeDefined();

  console.log(a);
  console.log(lines[0].substring(a.name.range.start, a.name.range.end));

  expect(lines[0].substring(a.name.range.start, a.name.range.end)).toBe(`SRCPF`);

  const b = parseISpec(1, 0, lines[1]);
  expect(b).toBeDefined();
})

test('ispec file fields definitions', async () => {
  const lines = [
    ``,
    `         dcl-f file1 disk(100);`,
    ``,
    `         dcl-s B_S_DTYP_P5_0 packed(5:0);`,
    `         dcl-s B_P_DTYP_P5_0 packed(5:0);`,
    `         dcl-s B_B_DTYP_P9_0 packed(9:0);`,
    `         dcl-s B_I_DTYP_P10_0 packed(10:0);`,
    `         dcl-s B_U_DTYP_P3_0 packed(3:0);`,
    ``,
    `         dcl-s C_P_DTYP_P4_0 packed(4:0);`,
    `         dcl-s C_B_DTYP_P1_0 packed(1:0);`,
    ``,
    `         // Coding numeric fields in a DS with no type causes them`,
    `         // to default to zoned`,
    `         dcl-ds *n;`,
    `            D_S_DTYP_S5_0;`,
    `            D_P_DTYP_S5_0;`,
    // `            D_P_DTYP_S5_0;`,
    `            D_B_DTYP_S9_0;`,
    `            D_I_DTYP_S10_0;`,
    `            D_U_DTYP_S3_0;`,
    `         end-ds;`,
    ``,
    `         dcl-ds *n;`,
    `            E_S_DTYP_S5_0 zoned(5:0);`,
    `            E_P_DTYP_P5_0 packed(5:0);`,
    `            E_B_DTYP_B9_0 bindec(9:0);`,
    `            E_I_DTYP_I10_0 int(10);`,
    `            E_U_DTYP_U3_0 uns(3);`,
    `         end-ds;`,
    ``,
    ``,
    `     Ifile1     ns  01`,
    `      * A. These are not coded in any D specs. All packed`,
    `      * A. These are not coded in any D specs. All packed`,
    `     I                             S    1    5 0A_S_TYP_P5_0`,
    `     I                             P    1    3 0A_P_TYP_P5_0`,
    `     I                             B    1    4 0A_B_TYP_P9_0`,
    `     I                             I    1    4 0A_I_TYP_P10_0`,
    `     I                             U    1    1 0A_U_TYP_P3_0`,
    `      * B. These are also coded in D specs with same type as the defaul`,
    `     I                             S    1    5 0B_S_DTYP_P5_0`,
    `     I                             P    1    3 0B_P_DTYP_P5_0`,
    `     I                             B    1    4 0B_B_DTYP_P9_0`,
    `     I                             I    1    4 0B_I_DTYP_P10_0`,
    `     I                             U    1    1 0B_U_DTYP_P3_0`,
    `      * C. These are coded in D or C specs with a lower number of digit`,
    `     I                             P    1    3 0C_P_DTYP_P4_0`,
    `     I                             P    1    3 0C_P_CTYP_P4_0`,
    `     I                             B    1    2 0C_B_DTYP_P1_0`,
    `     I                             B    1    4 0C_B_CTYP_P5_0`,
    `      * D. These are coded in a data structure with no types. All zoned`,
    `     I                             S    1    5 0D_S_DTYP_S5_0`,
    `     I                             P    1    3 0D_P_DTYP_S5_0`,
    `     I                             B    1    4 0D_B_DTYP_S9_0`,
    `     I                             I    1    4 0D_I_DTYP_S10_0`,
    `     I                             U    1    1 0D_U_DTYP_S3_0`,
    `      * E. These are coded in a DS with the same types as the I specs`,
    `     I                             S    1    5 0E_S_DTYP_S5_0`,
    `     I                             P    1    3 0E_P_DTYP_P5_0`,
    `     I                             B    1    4 0E_B_DTYP_B9_0`,
    `     I                             I    1    4 0E_I_DTYP_I10_0`,
    `     I                             U    1    1 0E_U_DTYP_U3_0`,
    ``,
    `     C                   z-add     0             C_P_CTYP_P4_0     4 0`,
    `     C                   z-add     0             C_B_CTYP_P5_0     5 0`,
    ``,
    `     C                   return`,
  ];

  const cache = await parser.getDocs(uri, lines.join('\n'), {ignoreCache: true, collectReferences: true});

  const files = cache.files;
  expect(files.length).toBe(1);

  const inputs = cache.symbols.filter(s => s.type === `input`);
  expect(inputs.length).toBe(1);

  const structs = cache.structs;
  expect(structs.length).toBe(2);
  expect(structs.every(s => s.name.startsWith(`*n`))).toBeTruthy();

  const blankA = structs[0];
  expect(blankA.subItems.length).toBe(5);

  const blankATwo = blankA.subItems[1];
  expect(blankATwo.name).toBe(`D_P_DTYP_S5_0`);
  expect(blankATwo.keyword[`PACKED`]).toBe(`3:0`);

  const blankB = structs[1];
  expect(blankB.subItems.length).toBe(5);

  const file1input = inputs[0];
  expect(file1input.name).toBe(`file1`);
  expect(file1input.subItems.length).toBe(24);

  const multipleDefinitionVar = cache.findAll(`D_P_DTYP_S5_0`);
  expect(multipleDefinitionVar.length).toBe(1);

  const someVar = cache.find(`C_P_CTYP_P4_0`);
  expect(someVar).toBeDefined();
  expect(someVar.references.length).toBe(2);
});