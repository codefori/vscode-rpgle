import { expect, test } from "vitest";
import setupParser from "../parserSetup";

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

  // for (const file of [rpgSrc, cblSrc]) {
  //   console.log(`File: ${file.name}`);
  //   for (const subItem of file.subItems) {
  //     console.log(`  Record format: ${subItem.name}`);
  //     for (const subSubItem of subItem.subItems) {
  //       console.log(`    Field: ${subSubItem.name}`);
  //     }
  //   }
  // }
})