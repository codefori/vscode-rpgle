import { assert, expect, test } from 'vitest'
import { Targets } from '../src/targets'
import path from 'path';
import { createTargets } from './createTargets';

test('resolveObject', () => {
  createTargets();
});

test('createOrApend', () => {
  const targets = createTargets(true);

  const deps = targets.getDeps();

  const programA = deps.find(d => d.name === `PROGRAMA`);
  expect(programA).toBeDefined();
  expect(programA.deps.length).toBe(2);
  expect(programA.deps[0].name).toBe(`FILEA`);
  expect(programA.deps[1].name).toBe(`PROGRAMB`);

  const programB = deps.find(d => d.name === `PROGRAMB`);
  expect(programB).toBeDefined();
  expect(programB.deps.length).toBe(0);

  const moduleA = deps.find(d => d.name === `MODULEA`);
  expect(moduleA).toBeDefined();
  expect(moduleA.deps.length).toBe(2);
  expect(moduleA.deps[0].name).toBe(`FILEA`);
  expect(moduleA.deps[1].name).toBe(`FILEB`);

  const moduleB = deps.find(d => d.name === `MODULEB`);
  expect(moduleB).toBeDefined();
  expect(moduleB.deps.length).toBe(1);
  expect(moduleB.deps[0].name).toBe(`FILEB`);
});

test('resolveBinder', () => {
  const targets = createTargets(true);

  expect(targets.getDeps().length).toBe(4);
  expect(targets.binderRequired()).toBe(false);

  targets.resolveBinder();

  const deps = targets.getDeps();

  expect(deps.length).toBe(7);
  expect(targets.binderRequired()).toBe(true);

  const bnddir = deps.find(d => d.name === `$(BNDDIR)` && d.type === `BNDDIR`);
  expect(bnddir).toBeDefined();
  expect(bnddir.extension).toBeUndefined();
  expect(bnddir.relativePath).toBeUndefined();
  expect(bnddir.deps.length).toBe(2);

  for (const srvPgmDep of bnddir.deps) {
    // Ensure that the deps of the bnddir exist
    const srvPgm = deps.find(d => d.name === srvPgmDep.name && d.type === srvPgmDep.type);

    expect(srvPgm).toBeDefined();
    expect(srvPgm.deps.length).toBe(1);

    // By default, binder source is not required
    expect(srvPgm.relativePath).toBeUndefined();
    expect(srvPgm.extension).toBeUndefined();
  }

  // All programs should have the binder as a dep now
  const programs = targets.getParentObjects("PGM");

  expect(programs.length).toBeGreaterThan(0);

  for (const program of programs) {
    expect(program.deps.some(d => d.name === `$(BNDDIR)` && d.type === `BNDDIR`)).toBeTruthy();
  }
});