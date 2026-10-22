import { test, expect } from "vitest";
import { builtInFunctionCompletionItems, getBuiltInCompletionItems, isInsideStringOrComment } from "../../extension/server/src/providers/builtInCompletion";

type TextEdit = { range: { start: { character: number }, end: { character: number } }, newText: string };

/** Applies the completion of `label` at the end of `line` and returns the resulting line. */
function complete(line: string, label: string): string {
  const items = getBuiltInCompletionItems(line, { line: 0, character: line.length });
  const item = items.find(i => i.label === label);
  expect(item).toBeDefined();

  if (item!.textEdit) {
    const edit = item!.textEdit as TextEdit;
    return line.substring(0, edit.range.start.character) + edit.newText + line.substring(edit.range.end.character);
  }

  // Without a text edit VS Code only replaces the current word, which does not include `%`
  const wordStart = line.search(/\w*$/);
  return line.substring(0, wordStart) + item!.insertText;
}

test("builtInCompletion: %tri completes to %trim, not %%trim", () => {
  expect(complete(`  value = %tri`, `%trim`)).toBe(`  value = %trim(\${1})`);
});

test("builtInCompletion: replaces only the typed built-in", () => {
  const line = `  value = %trim(name) + %sub`;
  const items = getBuiltInCompletionItems(line, { line: 3, character: line.length });
  const edit = items.find(i => i.label === `%subst`)!.textEdit as TextEdit;

  expect(edit.range).toEqual({ start: { line: 3, character: 24 }, end: { line: 3, character: 28 } });
  expect(complete(line, `%subst`)).toBe(`  value = %trim(name) + %subst(\${1})`);
});

test("builtInCompletion: lone % is replaced", () => {
  expect(complete(`  value = %`, `%trim`)).toBe(`  value = %trim(\${1})`);
});

test("builtInCompletion: no % typed returns the default items", () => {
  expect(getBuiltInCompletionItems(`  value = `, { line: 0, character: 10 })).toBe(builtInFunctionCompletionItems);
});

test.each([
  `  exec sql select * from t where name like 'A%te`,
  `  dsply 'Done 100%`,
  `  value = 1; // 100%`,
  `// %tri`,
])("builtInCompletion: no items inside string or comment: %s", (line) => {
  expect(getBuiltInCompletionItems(line, { line: 0, character: line.length })).toHaveLength(0);
});

test.each([
  [`x = 'it''s' + %tri`, false],
  [`x = 'a//b' + %tri`, false],
  [`x = 'abc%`, true],
  [`x = 1; // %`, true],
])("isInsideStringOrComment: %s", (line, expected) => {
  expect(isInsideStringOrComment(line, line.lastIndexOf(`%`))).toBe(expected);
});
