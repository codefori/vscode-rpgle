import { assert, expect, test } from 'vitest'
import { Targets } from '../src/targets'
import path from 'path';

export const cwd = path.join(`/`, `projects`);

export function createTargets(withDeps = false) {
  const targets = new Targets(cwd);

  const programA = targets.resolveObject(path.join(cwd, `qrpglesrc`, `programA.pgm.rpgle`));
  expect(programA.name).toBe(`PROGRAMA`);
  expect(programA.type).toBe(`PGM`);
  expect(programA.extension).toBe(`rpgle`);
  expect(programA.relativePath).toBe(path.join(`qrpglesrc`, `programA.pgm.rpgle`));

  const programB = targets.resolveObject(path.join(cwd, `qrpglesrc`, `programB.pgm.sqlrpgle`));
  expect(programB.name).toBe(`PROGRAMB`);
  expect(programB.type).toBe(`PGM`);
  expect(programB.extension).toBe(`sqlrpgle`);
  expect(programB.relativePath).toBe(path.join(`qrpglesrc`, `programB.pgm.sqlrpgle`));

  const moduleA = targets.resolveObject(path.join(cwd, `qrpglesrc`, `moduleA.rpgle`));
  expect(moduleA.name).toBe(`MODULEA`);
  expect(moduleA.type).toBe(`MODULE`);
  expect(moduleA.extension).toBe(`rpgle`);
  expect(moduleA.relativePath).toBe(path.join(`qrpglesrc`, `moduleA.rpgle`));
  
  const moduleB = targets.resolveObject(path.join(cwd, `qrpglesrc`, `moduleB.sqlrpgle`));
  expect(moduleB.name).toBe(`MODULEB`);
  expect(moduleB.type).toBe(`MODULE`);
  expect(moduleB.extension).toBe(`sqlrpgle`);
  expect(moduleB.relativePath).toBe(path.join(`qrpglesrc`, `moduleB.sqlrpgle`));
  
  const fileA = targets.resolveObject(path.join(cwd, `qddssrc`, `fileA.sql`));
  expect(fileA.name).toBe(`FILEA`);
  expect(fileA.type).toBe(`FILE`);
  expect(fileA.extension).toBe(`sql`);
  expect(fileA.relativePath).toBe(path.join(`qddssrc`, `fileA.sql`));
  
  const fileB = targets.resolveObject(path.join(cwd, `qddssrc`, `fileB.pf`));
  expect(fileB.name).toBe(`FILEB`);
  expect(fileB.type).toBe(`FILE`);
  expect(fileB.extension).toBe(`pf`);
  expect(fileB.relativePath).toBe(path.join(`qddssrc`, `fileB.pf`));

  if (withDeps) {
    targets.createOrAppend(programA, fileA);
    targets.createOrAppend(programA, programB);
    targets.createOrAppend(programB);
    targets.createOrAppend(moduleA, fileA);
    targets.createOrAppend(moduleA, fileB);
    targets.createOrAppend(moduleB, fileB);
  }

  return targets;
}