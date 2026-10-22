import { expect, test } from "vitest";
import setupParser from "../parserSetup";
import Cache from "../../language/models/cache";
import { assertCache, assertFound } from "../utils";
import {
  SINGLE_EXCEPT,
  MULTIPLE_EXCEPT,
  UNKNOWN_EXCEPT,
  FACTOR2,
  OSPEC_EXCEPT_NAME,
  EXCEPT_OPCODE,
  OSPEC_CONSTANT,
  FIELD_AND_CONSTANT,
  FILE_AND_RECORD,
  CONDITIONING_INDICATORS,
  OSPEC_FILE_NAME,
  OSPEC_FIELD_NAME,
  OSPEC_INDICATOR,
  CSPEC_RESULT_INDICATOR,
  DSPEC_NAME,
} from "./constants/ospecSources";

const parser = setupParser();
const uri = `source.rpgle`;

const parse = async (lines: string) => assertCache(await parser.getDocs(uri, lines, { ignoreCache: true, withIncludes: true, collectReferences: true }));

/** Simulates a click: resolves the symbol under the cursor at the given 0-based line and column */
const clickOn = (cache: Cache, lines: string, line: number, column: number) => {
  const lineStart = lines.split(`\n`).slice(0, line).reduce((total, l) => total + l.length + 1, 0);
  return Cache.referenceByOffset(uri, cache, lineStart + column);
};

/** Same as the definition provider: click, then return where VS Code would jump to */
const goToDefinition = (cache: Cache, lines: string, line: number, column: number) => {
  const def = clickOn(cache, lines, line, column);
  if (!def) return undefined;

  return { path: def.position.path, line: def.position.range.line };
};

/** Same as the find references provider: click, then list where the symbol is used */
const findReferences = (cache: Cache, lines: string, line: number, column: number) => {
  const def = clickOn(cache, lines, line, column);
  if (!def) return undefined;

  return def.references
    .map(ref => ({
      line: lines.substring(0, ref.offset.start).split(`\n`).length - 1,
      text: lines.substring(ref.offset.start, ref.offset.end),
    }))
    .sort((a, b) => a.line - b.line);
};

// ---------------------------------------------------------------------------
// Go to definition
// ---------------------------------------------------------------------------

test('Go to definition on EXCEPT FOO goes to the FOO exception in the O-specs', async () => {
  const cache = await parse(SINGLE_EXCEPT);

  // The jump lands on the O-spec record, not on the EXCEPT line
  expect(goToDefinition(cache, SINGLE_EXCEPT, 1, FACTOR2)).toEqual({ path: uri, line: 3 });

  // Underneath: one output declaration, which the click resolves to
  const foo = assertFound(cache.find(`FOO`, `output`), `FOO`);
  expect(cache.outputs.filter(o => o.name === `FOO`).length).toBe(1);
  expect(foo.position.path).toBe(uri);
  expect(foo.position.range.line).toBe(3);
  expect(clickOn(cache, SINGLE_EXCEPT, 1, FACTOR2)).toBe(foo);

  // Underneath: the references are the EXCEPT operand and the O-spec record, all pointing at the text FOO
  expect(foo.references.length).toBe(2);
  expect(foo.references.every(ref => SINGLE_EXCEPT.substring(ref.offset.start, ref.offset.end) === `FOO`)).toBe(true);
});

test('Go to definition on EXCEPT with several O-spec records of the same name goes to the first record', async () => {
  const cache = await parse(MULTIPLE_EXCEPT);

  // Both EXCEPT FOO calls land on the first FOO record, EXCEPT BAR lands on the BAR record
  expect(goToDefinition(cache, MULTIPLE_EXCEPT, 1, FACTOR2)).toEqual({ path: uri, line: 5 });
  expect(goToDefinition(cache, MULTIPLE_EXCEPT, 3, FACTOR2)).toEqual({ path: uri, line: 5 });
  expect(goToDefinition(cache, MULTIPLE_EXCEPT, 2, FACTOR2)).toEqual({ path: uri, line: 9 });

  // Underneath: the two FOO records collapse into one declaration and BAR has its own
  const foo = assertFound(cache.find(`FOO`, `output`), `FOO`);
  const bar = assertFound(cache.find(`BAR`, `output`), `BAR`);
  expect(cache.outputs.filter(o => o.name === `FOO`).length).toBe(1);
  expect(foo.position.range.line).toBe(5);
  expect(bar.position.range.line).toBe(9);
  expect(clickOn(cache, MULTIPLE_EXCEPT, 1, FACTOR2)).toBe(foo);
  expect(clickOn(cache, MULTIPLE_EXCEPT, 2, FACTOR2)).toBe(bar);
  expect(clickOn(cache, MULTIPLE_EXCEPT, 3, FACTOR2)).toBe(foo);
});

test('Go to definition on the first O-spec record of an exception stays on that record', async () => {
  const cache = await parse(MULTIPLE_EXCEPT);

  expect(goToDefinition(cache, MULTIPLE_EXCEPT, 5, OSPEC_EXCEPT_NAME)).toEqual({ path: uri, line: 5 });
});

test('Go to definition on the second O-spec record of an exception goes to the first record', async () => {
  const cache = await parse(MULTIPLE_EXCEPT);

  expect(goToDefinition(cache, MULTIPLE_EXCEPT, 7, OSPEC_EXCEPT_NAME)).toEqual({ path: uri, line: 5 });
});

test('Go to definition on an exception with a single O-spec record stays on that record', async () => {
  const cache = await parse(MULTIPLE_EXCEPT);

  expect(goToDefinition(cache, MULTIPLE_EXCEPT, 9, OSPEC_EXCEPT_NAME)).toEqual({ path: uri, line: 9 });
});

test('Go to definition on EXCEPT with an unknown name goes nowhere', async () => {
  const cache = await parse(UNKNOWN_EXCEPT);

  // EXCEPT NOPE: no O-spec exception with that name
  expect(goToDefinition(cache, UNKNOWN_EXCEPT, 1, FACTOR2)).toBeUndefined();
  // The EXCEPT opcode itself
  expect(goToDefinition(cache, UNKNOWN_EXCEPT, 1, EXCEPT_OPCODE)).toBeUndefined();
  // The constant on an O-spec output line
  expect(goToDefinition(cache, UNKNOWN_EXCEPT, 4, OSPEC_CONSTANT)).toBeUndefined();

  // Underneath: nothing was declared for NOPE
  expect(cache.find(`NOPE`, `output`)).toBeUndefined();
});

// ---------------------------------------------------------------------------
// Find references
// ---------------------------------------------------------------------------

test('Find references on an O-spec exception with two O-spec records lists every record and EXCEPT call', async () => {
  const cache = await parse(MULTIPLE_EXCEPT);

  // Cursor on FOO in the first O-spec record
  const refs = assertFound(findReferences(cache, MULTIPLE_EXCEPT, 5, OSPEC_EXCEPT_NAME), `FOO references`);

  expect(refs.map(r => r.line)).toEqual([1, 3, 5, 7]);
  expect(refs.every(r => r.text === `FOO`)).toBe(true);
});

test('Find references on the second O-spec record of an exception lists the same references as the first', async () => {
  const cache = await parse(MULTIPLE_EXCEPT);

  // Cursor on FOO in the second O-spec record
  const fromSecond = assertFound(findReferences(cache, MULTIPLE_EXCEPT, 7, OSPEC_EXCEPT_NAME), `FOO references`);
  const fromFirst = assertFound(findReferences(cache, MULTIPLE_EXCEPT, 5, OSPEC_EXCEPT_NAME), `FOO references`);

  expect(fromSecond).toEqual(fromFirst);
  expect(fromSecond.map(r => r.line)).toEqual([1, 3, 5, 7]);
});

test('Find references on a single O-spec exception lists its O-spec record and EXCEPT call', async () => {
  const cache = await parse(MULTIPLE_EXCEPT);

  // Cursor on BAR in its only O-spec record
  const refs = assertFound(findReferences(cache, MULTIPLE_EXCEPT, 9, OSPEC_EXCEPT_NAME), `BAR references`);

  expect(refs.map(r => r.line)).toEqual([2, 9]);
  expect(refs.every(r => r.text === `BAR`)).toBe(true);
});

test('Find references on a C-spec EXCEPT lists the O-spec records and every EXCEPT call', async () => {
  const cache = await parse(MULTIPLE_EXCEPT);

  // Cursor on the operand of each EXCEPT FOO
  for (const line of [1, 3]) {
    const refs = assertFound(findReferences(cache, MULTIPLE_EXCEPT, line, FACTOR2), `FOO references from line ${line}`);
    expect(refs.map(r => r.line)).toEqual([1, 3, 5, 7]);
    expect(refs.every(r => r.text === `FOO`)).toBe(true);
  }

  // Cursor on the operand of EXCEPT BAR
  const barRefs = assertFound(findReferences(cache, MULTIPLE_EXCEPT, 2, FACTOR2), `BAR references`);
  expect(barRefs.map(r => r.line)).toEqual([2, 9]);
});

test('Find references returns nothing when the cursor is not on an exception', async () => {
  const cache = await parse(UNKNOWN_EXCEPT);

  // EXCEPT NOPE: no O-spec exception with that name
  expect(findReferences(cache, UNKNOWN_EXCEPT, 1, FACTOR2)).toBeUndefined();
  // The EXCEPT opcode itself
  expect(findReferences(cache, UNKNOWN_EXCEPT, 1, EXCEPT_OPCODE)).toBeUndefined();
  // The constant on an O-spec output line
  expect(findReferences(cache, UNKNOWN_EXCEPT, 4, OSPEC_CONSTANT)).toBeUndefined();

  // FOO is only referenced by its own O-spec record, so it never picks up NOPE
  const fooRefs = assertFound(findReferences(cache, UNKNOWN_EXCEPT, 3, OSPEC_EXCEPT_NAME), `FOO references`);
  expect(fooRefs.map(r => r.line)).toEqual([3]);
});

// ---------------------------------------------------------------------------
// Field / constant references in field O-specs
// ---------------------------------------------------------------------------

/** Lines of the O-spec part of FIELD_AND_CONSTANT and FILE_AND_RECORD (everything from the first O-spec record on) */
const fromOSpecs = (refs: { line: number }[], firstOSpecLine: number) => refs.map(r => r.line).filter(line => line >= firstOSpecLine);

test('Go to definition on a field in a field O-spec goes to its D-spec', async () => {
  const cache = await parse(FIELD_AND_CONSTANT);

  // Both CUSTNO field lines (6 and 7) land on the D-spec at line 1
  expect(goToDefinition(cache, FIELD_AND_CONSTANT, 6, OSPEC_FIELD_NAME)).toEqual({ path: uri, line: 1 });
  expect(goToDefinition(cache, FIELD_AND_CONSTANT, 7, OSPEC_FIELD_NAME)).toEqual({ path: uri, line: 1 });

  // Underneath: the click resolves to the D-spec variable, not to the O-spec field line
  const custno = assertFound(cache.find(`CUSTNO`), `CUSTNO`);
  expect(custno.type).toBe(`variable`);
  expect(clickOn(cache, FIELD_AND_CONSTANT, 6, OSPEC_FIELD_NAME)).toBe(custno);
});

test('Go to definition on a constant in a field O-spec goes to its D-spec constant', async () => {
  const cache = await parse(FIELD_AND_CONSTANT);

  expect(goToDefinition(cache, FIELD_AND_CONSTANT, 8, OSPEC_FIELD_NAME)).toEqual({ path: uri, line: 2 });

  const maxlen = assertFound(cache.find(`MAXLEN`), `MAXLEN`);
  expect(maxlen.type).toBe(`constant`);
  expect(clickOn(cache, FIELD_AND_CONSTANT, 8, OSPEC_FIELD_NAME)).toBe(maxlen);
});

test('Go to definition on a field in a field O-spec that is not declared goes nowhere', async () => {
  const cache = await parse(FIELD_AND_CONSTANT);

  expect(goToDefinition(cache, FIELD_AND_CONSTANT, 9, OSPEC_FIELD_NAME)).toBeUndefined();
  expect(cache.find(`NOFIELD`)).toBeUndefined();
});

test('Find references on a D-spec field lists every field O-spec that uses it', async () => {
  const cache = await parse(FIELD_AND_CONSTANT);

  // Cursor on the D-spec, then on each of the field lines: all give the same answer
  for (const [line, column] of [[1, DSPEC_NAME], [6, OSPEC_FIELD_NAME], [7, OSPEC_FIELD_NAME]]) {
    const refs = assertFound(findReferences(cache, FIELD_AND_CONSTANT, line, column), `CUSTNO references from line ${line}`);
    expect(fromOSpecs(refs, 5)).toEqual([6, 7]);
    expect(refs.every(r => r.text === `CUSTNO`)).toBe(true);
  }
});

test('Find references on a D-spec constant lists the field O-spec that uses it', async () => {
  const cache = await parse(FIELD_AND_CONSTANT);

  for (const [line, column] of [[2, DSPEC_NAME], [8, OSPEC_FIELD_NAME]]) {
    const refs = assertFound(findReferences(cache, FIELD_AND_CONSTANT, line, column), `MAXLEN references from line ${line}`);
    expect(fromOSpecs(refs, 5)).toEqual([8]);
    expect(refs.every(r => r.text === `MAXLEN`)).toBe(true);
  }
});

// ---------------------------------------------------------------------------
// Record / file references in record O-specs
// ---------------------------------------------------------------------------

test('Go to definition on the file in a record O-spec goes to its F-spec', async () => {
  const cache = await parse(FILE_AND_RECORD);

  // Both records that name QPRINT (lines 4 and 8) land on the F-spec at line 0
  expect(goToDefinition(cache, FILE_AND_RECORD, 4, OSPEC_FILE_NAME)).toEqual({ path: uri, line: 0 });
  expect(goToDefinition(cache, FILE_AND_RECORD, 8, OSPEC_FILE_NAME)).toEqual({ path: uri, line: 0 });

  const qprint = assertFound(cache.find(`QPRINT`), `QPRINT`);
  expect(qprint.type).toBe(`file`);
  expect(clickOn(cache, FILE_AND_RECORD, 4, OSPEC_FILE_NAME)).toBe(qprint);
});

test('Go to definition on the file of a record O-spec with a blank file name goes nowhere', async () => {
  const cache = await parse(FILE_AND_RECORD);

  // The second HDR record (line 6) has no file name, so there is nothing under the cursor at column 7
  expect(goToDefinition(cache, FILE_AND_RECORD, 6, OSPEC_FILE_NAME)).toBeUndefined();
});

test('Find references on a file lists every record O-spec that names it', async () => {
  const cache = await parse(FILE_AND_RECORD);

  // Cursor on the F-spec, then on each record that names QPRINT: all give the same answer
  for (const [line, column] of [[0, OSPEC_FILE_NAME], [4, OSPEC_FILE_NAME], [8, OSPEC_FILE_NAME]]) {
    const refs = assertFound(findReferences(cache, FILE_AND_RECORD, line, column), `QPRINT references from line ${line}`);
    // Line 6 is the second HDR record, which has a blank file name
    expect(fromOSpecs(refs, 4)).toEqual([4, 8]);
    expect(refs.every(r => r.text === `QPRINT`)).toBe(true);
  }
});

test('Go to definition on a record O-spec goes to the first record of that exception', async () => {
  const cache = await parse(FILE_AND_RECORD);

  // EXCEPT HDR / EXCEPT DETAIL in the C-specs, and the second HDR record
  expect(goToDefinition(cache, FILE_AND_RECORD, 1, FACTOR2)).toEqual({ path: uri, line: 4 });
  expect(goToDefinition(cache, FILE_AND_RECORD, 2, FACTOR2)).toEqual({ path: uri, line: 8 });
  expect(goToDefinition(cache, FILE_AND_RECORD, 6, OSPEC_EXCEPT_NAME)).toEqual({ path: uri, line: 4 });
});

// ---------------------------------------------------------------------------
// Conditioning indicators in record and field O-specs
// ---------------------------------------------------------------------------

test('Find references on an indicator includes the record O-spec and field O-spec it conditions', async () => {
  const cache = await parse(CONDITIONING_INDICATORS);

  // SETON 10 in the C-specs (line 2), the record conditioned by 10 (line 6) and the field conditioned by 10 (line 8)
  for (const [line, column] of [[2, CSPEC_RESULT_INDICATOR], [6, OSPEC_INDICATOR], [8, OSPEC_INDICATOR]]) {
    const refs = assertFound(findReferences(cache, CONDITIONING_INDICATORS, line, column), `*IN10 references from line ${line}`);
    expect(refs.map(r => r.line)).toEqual([2, 6, 8]);
  }

  // Underneath: it is the *IN10 indicator
  const in10 = assertFound(cache.find(`IN10`, `indicator`), `IN10`);
  expect(clickOn(cache, CONDITIONING_INDICATORS, 6, OSPEC_INDICATOR)).toBe(in10);
  expect(clickOn(cache, CONDITIONING_INDICATORS, 8, OSPEC_INDICATOR)).toBe(in10);
});

test('Find references on an indicator that only conditions a record O-spec lists that record', async () => {
  const cache = await parse(CONDITIONING_INDICATORS);

  const refs = assertFound(findReferences(cache, CONDITIONING_INDICATORS, 9, OSPEC_INDICATOR), `*IN20 references`);
  expect(refs.map(r => r.line)).toEqual([9]);
});

test('Find references on an O-spec line without a conditioning indicator returns nothing at the indicator columns', async () => {
  const cache = await parse(CONDITIONING_INDICATORS);

  // Field line 7 has no indicator, so there is nothing under the cursor there
  expect(findReferences(cache, CONDITIONING_INDICATORS, 7, OSPEC_INDICATOR)).toBeUndefined();
});
