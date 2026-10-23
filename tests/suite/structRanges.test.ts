import { expect, test } from 'vitest';
import Parser from '../../language/ile/parser';
import Linter from '../../language/ile/linter';
import { assertCache } from '../utils';

const uri = `source.rpgle`;
const prefix = [`**free`, `dcl-pr time ind extproc('time');`, `end-pr;`, `dcl-s result ind;`];

test.each([
  {
    name: `same file`,
    body: [`dcl-ds data qualified;`, `time ind;`, `end-ds;`],
    includes: {},
    origin: uri, range: { start: 4, end: 6 },
    sourceRanges: [{ path: uri, start: 4, end: 6 }],
  },
  {
    name: `closed early in an include`,
    body: [`dcl-ds data qualified;`, `time ind;`, `/copy close`],
    includes: { close: [`**free`, `end-ds;`] },
    origin: uri, range: { start: 4, end: 6 },
    sourceRanges: [{ path: uri, start: 4, end: 6 }, { path: `close.rpgleinc`, start: 0, end: 1 }],
  },
  {
    name: `closed later in an include`,
    body: [`dcl-ds data qualified;`, `time ind;`, `/copy close`],
    includes: { close: [`**free`, ...Array(12).fill(`// padding`), `end-ds;`] },
    origin: uri, range: { start: 4, end: 6 },
    sourceRanges: [{ path: uri, start: 4, end: 6 }, { path: `close.rpgleinc`, start: 0, end: 13 }],
  },
  {
    name: `opened in an include`,
    body: [`/copy open`, `time ind;`, `end-ds;`],
    includes: { open: [`**free`, `dcl-ds data qualified;`] },
    origin: `open.rpgleinc`, range: { start: 1, end: 1 },
    sourceRanges: [{ path: `open.rpgleinc`, start: 1, end: 1 }, { path: uri, start: 4, end: 6 }],
  },
  {
    name: `continues after an include`,
    body: [`dcl-ds data qualified;`, `/copy fields`, `time ind;`, `end-ds;`],
    includes: { fields: [`**free`, `value ind;`] },
    origin: uri, range: { start: 4, end: 7 },
    sourceRanges: [{ path: uri, start: 4, end: 7 }, { path: `fields.rpgleinc`, start: 0, end: 1 }],
  },
  {
    name: `entirely within an include`,
    body: [`/copy complete`],
    includes: { complete: [`**free`, `dcl-ds data qualified;`, `time ind;`, `end-ds;`] },
    origin: `complete.rpgleinc`, range: { start: 1, end: 3 },
    sourceRanges: [{ path: `complete.rpgleinc`, start: 1, end: 3 }],
  },
  {
    name: `opened through nested includes`,
    body: [`/copy middle`, `time ind;`, `end-ds;`],
    includes: { middle: [`**free`, `/copy open`], open: [`**free`, `dcl-ds data qualified;`] },
    origin: `open.rpgleinc`, range: { start: 1, end: 1 },
    sourceRanges: [
      { path: `open.rpgleinc`, start: 1, end: 1 },
      { path: `middle.rpgleinc`, start: 1, end: 1 },
      { path: uri, start: 4, end: 6 },
    ],
  },
  {
    name: `closed through nested includes`,
    body: [`dcl-ds data qualified;`, `time ind;`, `/copy middle`],
    includes: { middle: [`**free`, `/copy close`], close: [`**free`, `end-ds;`] },
    origin: uri, range: { start: 4, end: 6 },
    sourceRanges: [
      { path: uri, start: 4, end: 6 },
      { path: `middle.rpgleinc`, start: 0, end: 1 },
      { path: `close.rpgleinc`, start: 0, end: 1 },
    ],
  },
])(`structure source ranges: $name`, async ({ body, includes, origin, range, sourceRanges }) => {
  const parser = new Parser();
  const files: Record<string, string[] | undefined> = includes;
  parser.setIncludeFileFetch(async (_base, name) => ({
    found: files[name] !== undefined,
    uri: `${name}.rpgleinc`,
    content: files[name]?.join(`\n`),
  }));
  const content = [...prefix, ...body, `result = time;`].join(`\n`);
  const cache = assertCache(await parser.getDocs(uri, content, { withIncludes: true }));
  expect(cache.structs).toHaveLength(1);
  const ds = cache.structs[0];

  // The legacy range remains usable by Outline in the declaration's own file.
  expect(ds.position.path).toBe(origin);
  expect(ds.position.range.line).toBe(range.start);
  expect(ds.range).toEqual(range);
  expect(ds.sourceRanges).toEqual(sourceRanges);
  for (const sourceRange of sourceRanges) {
    expect(ds.containsLine(sourceRange.path, sourceRange.start)).toBe(true);
    expect(ds.containsLine(sourceRange.path, sourceRange.end)).toBe(true);
    expect(ds.containsLine(sourceRange.path, sourceRange.start - 1)).toBe(false);
    expect(ds.containsLine(sourceRange.path, sourceRange.end + 1)).toBe(false);
  }
  expect(ds.containsLine(`unrelated.rpgle`, range.start)).toBe(false);

  const { errors } = Linter.getErrors({ uri, content }, { RequiresParameter: true }, cache);
  const callOffset = content.lastIndexOf(`time;`);
  expect(errors).toHaveLength(1);
  expect(errors[0]).toMatchObject({ type: `RequiresParameter`, offset: { start: callOffset, end: callOffset + 4 } });

  const clone = ds.clone();
  expect(clone.sourceRanges).toEqual(ds.sourceRanges);
  clone.addSourceRange(origin, range.end + 10);
  expect(ds.range).toEqual(range);
  expect(ds.sourceRanges).toEqual(sourceRanges);
});

test(`nested local structures keep separate ranges across an include`, async () => {
  const parser = new Parser();
  parser.setIncludeFileFetch(async () => ({ found: true, uri: `inner.rpgleinc`, content: `**free\ndcl-ds inner;\ntime ind;\nend-ds;` }));
  const content = [...prefix, `dcl-proc example;`, `dcl-ds data qualified;`, `/copy inner`, `time ind;`, `end-ds;`, `result = time;`, `end-proc;`].join(`\n`);
  const cache = assertCache(await parser.getDocs(uri, content, { withIncludes: true }));
  const scope = assertCache(cache.procedures.find(proc => proc.name === `example`)?.scope);
  const outer = scope.structs[0];
  const inner = outer.subItems.find(field => field.name === `inner`)!;
  expect(outer.range).toEqual({ start: 5, end: 8 });
  expect(outer.containsLine(`inner.rpgleinc`, 2)).toBe(true);
  expect(outer.containsLine(uri, 7)).toBe(true);
  expect(inner.range).toEqual({ start: 1, end: 3 });
  expect(inner.containsLine(uri, 7)).toBe(false);
  const { errors } = Linter.getErrors({ uri, content }, { RequiresParameter: true }, cache);
  expect(errors).toHaveLength(1);
  expect(errors[0].offset.start).toBe(content.lastIndexOf(`time;`));
});

test(`containsLine falls back to existing enum and inline-DS ranges`, async () => {
  const content = [`**free`, `dcl-enum values qualified;`, `time;`, `end-enum;`, `dcl-ds data qualified;`, `value ind;`, `end-ds;`, `dcl-ds copy likeds(data);`].join(`\n`);
  const cache = assertCache(await new Parser().getDocs(uri, content));
  const enumeration = cache.constants[0];
  const inline = cache.structs.find(ds => ds.name === `copy`)!;
  expect(enumeration.sourceRanges).toEqual([]);
  expect(enumeration.containsLine(uri, 2)).toBe(true);
  expect(enumeration.containsLine(uri, 4)).toBe(false);
  expect(inline.sourceRanges).toEqual([]);
  expect(inline.containsLine(uri, 7)).toBe(true);
  expect(inline.containsLine(uri, 8)).toBe(false);
});

test(`closing a structure in a copy does not suppress executable code in either file`, async () => {
  const parser = new Parser();
  const includeUri = `close.rpgleinc`;
  const include = [`**free`, `end-ds;`, `result = time;`].join(`\n`);
  parser.setIncludeFileFetch(async () => ({ found: true, uri: includeUri, content: include }));
  const content = [...prefix, `dcl-ds data qualified;`, `time ind;`, `/copy close`, `result = time;`].join(`\n`);
  const cache = assertCache(await parser.getDocs(uri, content, { withIncludes: true }));

  for (const [file, text] of [[uri, content], [includeUri, include]]) {
    const { errors } = Linter.getErrors({ uri: file, content: text }, { RequiresParameter: true }, cache);
    expect(errors).toHaveLength(1);
    expect(errors[0]).toMatchObject({
      type: `RequiresParameter`,
      offset: { start: text.lastIndexOf(`time;`), end: text.lastIndexOf(`time;`) + 4 },
    });
  }
  expect(cache.structs[0].containsLine(includeUri, 2)).toBe(false);
});
