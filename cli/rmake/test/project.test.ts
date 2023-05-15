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