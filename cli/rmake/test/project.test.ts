import { assert, expect, test } from 'vitest'
import { Targets } from '../src/targets'
import path from 'path';
import { createTargets, cwd } from './createTargets';
import { Project } from '../src/project';

test('generateTargets (pre-resolve)', () => {
  const targets = createTargets(true);
  const project = new Project(cwd, targets);

  const targetContent = project.generateTargets();

  expect(targetContent.length).toBe(5);
  expect(targetContent).toEqual(
    [
      'all: $(PREPATH)/PROGRAMA.PGM $(PREPATH)/PROGRAMB.PGM',
      '',
      '$(PREPATH)/PROGRAMA.PGM: $(PREPATH)/FILEA.FILE $(PREPATH)/PROGRAMB.PGM',
      '$(PREPATH)/MODULEA.MODULE: $(PREPATH)/FILEA.FILE $(PREPATH)/FILEB.FILE',
      '$(PREPATH)/MODULEB.MODULE: $(PREPATH)/FILEB.FILE',
    ]
  );
});

test('generateTargets (post-resolve)', () => {
  const targets = createTargets(true);

  targets.resolveBinder();

  const project = new Project(cwd, targets);

  const targetContent = project.generateTargets();

  expect(targetContent.length).toBe(9);
  expect(targetContent).toEqual(
    [
      'all: $(PREPATH)/PROGRAMA.PGM $(PREPATH)/PROGRAMB.PGM',
      '',
      '$(PREPATH)/PROGRAMA.PGM: $(PREPATH)/FILEA.FILE $(PREPATH)/PROGRAMB.PGM $(PREPATH)/$(BNDDIR).BNDDIR',
      '$(PREPATH)/PROGRAMB.PGM: $(PREPATH)/$(BNDDIR).BNDDIR',
      '$(PREPATH)/MODULEA.MODULE: $(PREPATH)/FILEA.FILE $(PREPATH)/FILEB.FILE',
      '$(PREPATH)/MODULEB.MODULE: $(PREPATH)/FILEB.FILE',
      '$(PREPATH)/MODULEA.SRVPGM: $(PREPATH)/MODULEA.MODULE',
      '$(PREPATH)/$(BNDDIR).BNDDIR: $(PREPATH)/MODULEA.SRVPGM $(PREPATH)/MODULEB.SRVPGM',
      '$(PREPATH)/MODULEB.SRVPGM: $(PREPATH)/MODULEB.MODULE'
    ]
  );
});

test('applySettings (compiles)', () => {
  const targets = createTargets(true);

  targets.resolveBinder();

  const project = new Project(cwd, targets);

  const rulesContentA = project.generateGenericRules();

  const targetIndex = rulesContentA.findIndex(r => r === `$(PREPATH)/%.SRVPGM: `);
  const commandIndex = rulesContentA.findIndex(r => r === `\tsystem "CRTSRVPGM SRVPGM($(BIN_LIB)/$*) MODULE(*SRVPGM) EXPORT(*ALL) BNDDIR($(BNDDIR))"`);

  expect(targetIndex).toBeGreaterThanOrEqual(0);
  expect(commandIndex).toBeGreaterThanOrEqual(0);

  project.applySettings({
    compiles: {
      "srvpgm": {
        becomes: `SRVPGM`,
        dir: `qbndsrc`,
        command: `CRTSRVPGM SRVPGM($(BIN_LIB)/$*) MODULE(*SRVPGM) SRCSTMF('$<') BNDDIR($(BNDDIR))`,
      }
    }
  });

  const rulesContentB = project.generateGenericRules();

  expect(rulesContentB[targetIndex]).toBe(`$(PREPATH)/%.SRVPGM: qbndsrc/%.srvpgm`);
  expect(rulesContentB[commandIndex]).toBe(`\tsystem "CRTSRVPGM SRVPGM($(BIN_LIB)/$*) MODULE(*SRVPGM) SRCSTMF('$<') BNDDIR($(BNDDIR))"`);
});

test('generateHeader (binder changes)', () => {
  const targets = createTargets(true);

  const project = new Project(cwd, targets);

  const headerContentA = project.generateHeader();
  let bndDirIndex = headerContentA.findIndex(h => h.startsWith(`BNDDIR=`));

  expect(bndDirIndex).toBeGreaterThanOrEqual(0);
  expect(headerContentA[bndDirIndex]).toBe(`BNDDIR=*NONE`);

  targets.resolveBinder();

  const headerContentB = project.generateHeader();

  expect(headerContentB[bndDirIndex]).toBe(`BNDDIR=($(APP_BNDDIR))`);
});

test('applySettings (binder)', () => {
  const targets = createTargets(true);

  const project = new Project(cwd, targets);

  project.applySettings({
    binders: [`TESTING`]
  });

  const headerContentA = project.generateHeader();
  let bndDirIndex = headerContentA.findIndex(h => h.startsWith(`BNDDIR=`));

  expect(bndDirIndex).toBeGreaterThanOrEqual(0);
  expect(headerContentA[bndDirIndex]).toBe(`BNDDIR=(TESTING)`);

  targets.resolveBinder();

  const headerContentB = project.generateHeader();

  expect(headerContentB[bndDirIndex]).toBe(`BNDDIR=($(APP_BNDDIR)) (TESTING)`);
});