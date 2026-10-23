import { beforeEach, expect, test, vi } from "vitest";
import { CodeAction, CodeActionKind, Range } from "vscode-languageserver";
import { TextDocument } from "vscode-languageserver-textdocument";
import Linter from "../../../language/ile/linter";
import Parser from "../../../language/ile/parser";
import { Rules } from "../../../language/ile/parserTypes";
import { getLinterCodeActions } from "../src/providers/linter/codeActions";

const { refreshLinterDiagnostics, getActions } = vi.hoisted(() => ({
  refreshLinterDiagnostics: vi.fn(),
  getActions: vi.fn(),
}));

// Keep the language server connection out of these provider tests.
vi.mock(`../src/providers/linter`, () => ({ refreshLinterDiagnostics, getActions }));

let rules: Rules;

beforeEach(() => {
  rules = { indent: 2 };
  vi.resetAllMocks();
  refreshLinterDiagnostics.mockImplementation(async (document, docs) =>
    Linter.getErrors({ uri: document.uri, content: document.getText() }, rules, docs)
  );
  getActions.mockReturnValue([]);
});

async function getFixes(lines: string[], range: Range, eol = `\n`) {
  const document = TextDocument.create(`file:///indent.rpgle`, `rpgle`, 1, lines.join(eol));
  const docs = await new Parser().getDocs(document.uri, document.getText(), { collectReferences: true });
  if (!docs) throw new Error(`Expected parsed document`);
  const actions = await getLinterCodeActions(docs, document, range);
  return { document, actions: actions || [] };
}

function applyIndentFix(document: TextDocument, actions: CodeAction[]) {
  const action = actions.find(candidate => candidate.title === `Fix indentation`);
  expect(action?.kind).toBe(CodeActionKind.QuickFix);
  const edits = action?.edit?.changes?.[document.uri];
  expect(edits).toBeDefined();
  return TextDocument.applyEdits(document, edits!);
}

test.each([`\n`, `\r\n`])(`fixes an unindented field with %j line endings without changing another field`, async eol => {
  const lines = [`**free`, `dcl-ds data qualified;`, `first char(10); // Keep this comment`, `second char(10);`, `end-ds;`];
  // Cursor is in the code, beyond the diagnostic's empty whitespace range.
  const { document, actions } = await getFixes(lines, Range.create(2, 8, 2, 8), eol);
  expect(actions).toHaveLength(1);
  expect(applyIndentFix(document, actions)).toBe(
    [lines[0], lines[1], `  ${lines[2]}`, lines[3], lines[4]].join(eol)
  );
});

test(`removes excess indentation from a closing declaration`, async () => {
  const lines = [`**free`, `dcl-ds data qualified;`, `  field char(10);`, `    end-ds;`];
  const { document, actions } = await getFixes(lines, Range.create(3, 4, 3, 4));
  expect(applyIndentFix(document, actions)).toBe([...lines.slice(0, 3), `end-ds;`].join(`\n`));
});

test(`fixes selected nested declarations and excludes a selection ending at the next line's start`, async () => {
  const lines = [`**free`, `dcl-ds data qualified;`, `dcl-ds nested;`, `field char(10);`, `end-ds;`, `end-ds;`];
  const { document, actions } = await getFixes(lines, Range.create(2, 0, 4, 0));
  expect(actions).toHaveLength(1);
  expect(applyIndentFix(document, actions)).toBe(
    [lines[0], lines[1], `  ${lines[2]}`, `    ${lines[3]}`, lines[4], lines[5]].join(`\n`)
  );
});

test.each([`@rpglint-skip`, `@rpglint-skip-indent`])(`respects %s when fixing a selected block`, async directive => {
  const lines = [`**free`, `dcl-ds data qualified;`, `// ${directive}`, `first char(10);`, `second char(10);`, `end-ds;`];
  const { document, actions } = await getFixes(lines, Range.create(3, 0, 4, 10));
  expect(applyIndentFix(document, actions)).toBe(
    [...lines.slice(0, 4), `  ${lines[4]}`, lines[5]].join(`\n`)
  );
});

test(`still offers indentation fixes after a skip-rules directive`, async () => {
  const lines = [`**free`, `dcl-ds data qualified;`, `// @rpglint-skip-rules`, `field char(10);`, `end-ds;`];
  const { document, actions } = await getFixes(lines, Range.create(3, 0, 3, 0));
  expect(applyIndentFix(document, actions)).toBe(
    [...lines.slice(0, 3), `  ${lines[3]}`, lines[4]].join(`\n`)
  );
});

test(`does not offer a fix on a correctly indented line when another line has a warning`, async () => {
  const { actions } = await getFixes(
    [`**free`, `dcl-ds data qualified;`, `  first char(10);`, `second char(10);`, `end-ds;`],
    Range.create(2, 4, 2, 4)
  );
  expect(actions).toHaveLength(0);
});

test(`does not offer indentation fixes when the rule is disabled`, async () => {
  rules = {};
  const { actions } = await getFixes(
    [`**free`, `dcl-ds data qualified;`, `field char(10);`, `end-ds;`],
    Range.create(2, 0, 2, 0)
  );
  expect(actions).toHaveLength(0);
});

test(`does not offer an indentation fix for a negative indent caused by an unmatched closing statement`, async () => {
  const { actions } = await getFixes([`**free`, `  endif;`], Range.create(1, 2, 1, 2));
  expect(actions).toHaveLength(0);
});

test(`preserves other quick fixes without applying them as part of the indentation edit`, async () => {
  rules = { indent: 2, UselessOperationCheck: true };
  const otherAction = CodeAction.create(`Remove operation code`, CodeActionKind.QuickFix);
  getActions.mockReturnValue([otherAction]);
  const lines = [`**free`, `if *on;`, `eval value = 1;`, `endif;`];
  const { document, actions } = await getFixes(lines, Range.create(2, 0, 2, 0));
  expect(actions).toContain(otherAction);
  expect(getActions.mock.calls[0][1]).toEqual(expect.arrayContaining([
    expect.objectContaining({ type: `UselessOperationCheck` }),
  ]));
  expect(applyIndentFix(document, actions)).toBe(
    [lines[0], lines[1], `  ${lines[2]}`, lines[3]].join(`\n`)
  );
});
