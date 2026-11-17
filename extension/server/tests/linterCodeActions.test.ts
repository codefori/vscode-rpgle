import { beforeEach, expect, test, vi } from "vitest";
import { CodeAction, CodeActionKind, Range } from "vscode-languageserver";
import { TextDocument } from "vscode-languageserver-textdocument";
import Linter from "../../../language/ile/linter";
import Parser from "../../../language/ile/parser";
import { Rules } from "../../../language/ile/parserTypes";
import { getLinterCodeActions } from "../src/providers/linter/codeActions";
import genericCodeActionsProvider from "../src/providers/codeActions";

const { refreshLinterDiagnostics, getActions } = vi.hoisted(() => ({
	refreshLinterDiagnostics: vi.fn(),
	getActions: vi.fn(),
}));
const { documents, parser } = vi.hoisted(() => ({
	documents: { get: vi.fn() },
	parser: { getDocs: vi.fn() },
}));

// Keep the language server connection out of these provider tests.
vi.mock(`../src/providers/linter`, () => ({ refreshLinterDiagnostics, getActions }));
vi.mock(`../src/providers`, () => ({ documents, parser }));

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
	return { document, docs, actions: actions || [] };
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

test(`does not dedent correct code after the reviewer's stray endif`, async () => {
	const lines = [`**free`, `endif;`, `if *on;`, `  x = 1;`, `endif;`, `dcl-proc a;`, `  y = 1;`, `end-proc;`];
	const { actions } = await getFixes(lines, Range.create(0, 0, 7, 9));
	expect(actions).toHaveLength(0);
});

test(`only fixes reliable indentation before an unmatched closing statement`, async () => {
	const lines = [`**free`, `if *on;`, `x = 1;`, `endif;`, `endif;`, `if *on;`, `  y = 1;`, `endif;`];
	const { document, actions } = await getFixes(lines, Range.create(0, 0, 7, 6));
	expect(applyIndentFix(document, actions)).toBe(
		[lines[0], lines[1], `  ${lines[2]}`, ...lines.slice(3)].join(`\n`)
	);
});

test.each([`\t`, ` \t`, `\t `, ` \t `])(`replaces the full leading whitespace %j`, async whitespace => {
	const lines = [`**free`, `if *on;`, `${whitespace}x = 1; // Preserve the code and comment`, `endif;`];
	const { document, actions } = await getFixes(lines, Range.create(2, whitespace.length, 2, whitespace.length));
	expect(applyIndentFix(document, actions)).toBe(
		[lines[0], lines[1], `  x = 1; // Preserve the code and comment`, lines[3]].join(`\n`)
	);
});

test(`marks the indentation fix preferred and associates the exact indentation diagnostic`, async () => {
	const { actions } = await getFixes([`**free`, `if *on;`, `\tx = 1;`, `endif;`], Range.create(2, 1, 2, 1));
	const action = actions.find(candidate => candidate.title === `Fix indentation`);
	expect(action?.isPreferred).toBe(true);
	expect(action?.diagnostics).toEqual([{
		range: Range.create(2, 0, 2, 1),
		message: `Incorrect indentation. Expected 2 spaces; found tab characters.`,
		severity: 2,
	}]);
});

test.each([0, 5])(`keeps individual lint fixes out of block selections (end column %s)`, async endColumn => {
	rules = { indent: 2, UselessOperationCheck: true };
	const otherAction = CodeAction.create(`Remove operation code`, CodeActionKind.QuickFix);
	getActions.mockReturnValue([otherAction]);
	const lines = [`**free`, `if *on;`, `eval a = 1;`, `eval b = 2;`, `eval c = 3;`, `endif;`];
	const { document, actions } = await getFixes(lines, Range.create(2, 0, 4, endColumn));
	const selectedCount = endColumn === 0 ? 2 : 3;
	expect(getActions).not.toHaveBeenCalled();
	expect(actions).not.toContain(otherAction);
	const fixedLines = [...lines];
	for (let index = 2; index < 2 + selectedCount; index++) fixedLines[index] = `  ${lines[index]}`;
	expect(applyIndentFix(document, actions)).toBe(fixedLines.join(`\n`));
});

test.each([
	{ name: `tabbed closer`, lines: [`**free`, `if *on;`, `  x = 1;`, `\tendif;`, `y = 2;`], fixed: [`**free`, `if *on;`, `  x = 1;`, `endif;`, `y = 2;`] },
	{ name: `tabbed opener`, lines: [`**free`, `\tif *on;`, `x = 1;`, `endif;`, `y = 2;`], fixed: [`**free`, `if *on;`, `  x = 1;`, `endif;`, `y = 2;`] },
	{ name: `spaces plus tab`, lines: [`**free`, `if *on;`, `  \tx = 1;`, `endif;`], fixed: [`**free`, `if *on;`, `  x = 1;`, `endif;`] },
	{ name: `tab matching the expected character count`, lines: [`**free`, `if *on;`, ` \tx = 1;`, `endif;`], fixed: [`**free`, `if *on;`, `  x = 1;`, `endif;`] },
])(`tracks blocks and removes tabs: $name`, async ({ lines, fixed }) => {
	const { document, actions } = await getFixes(lines, Range.create(0, 0, lines.length - 1, 100));
	const output = applyIndentFix(document, actions);
	expect(output).toBe(fixed.join(`\n`));
	const after = await getFixes(output.split(`\n`), Range.create(0, 0, lines.length - 1, 100));
	expect(after.actions).toHaveLength(0);
	expect(Linter.getErrors({ uri: after.document.uri, content: output }, rules, after.docs).indentErrors).toEqual([]);
});

test.each([`@rpglint-skip`, `@rpglint-skip-indent`])(`tracks openers and closers despite %s`, async directive => {
	for (const lines of [
		[`**free`, `// ${directive}`, `if *on;`, `  x = 1;`, `endif;`, `y = 2;`],
		[`**free`, `if *on;`, `  x = 1;`, `// ${directive}`, `endif;`, `y = 2;`],
	]) {
		const { document, docs, actions } = await getFixes(lines, Range.create(0, 0, lines.length - 1, 100));
		expect(actions).toHaveLength(0);
		expect(Linter.getErrors({ uri: document.uri, content: document.getText() }, rules, docs).indentErrors).toEqual([]);
	}
});

test.each([
	{ name: `IF before a procedure`, lines: [`**free`, `if *on;`, `  x = 1;`, `y = 2;`, `dcl-proc a;`, `end-proc;`] },
	{ name: `procedure before another procedure`, lines: [`**free`, `dcl-proc a;`, `  x = 1;`, `dcl-proc b;`, `end-proc;`] },
	{ name: `IF at end of file`, lines: [`**free`, `if *on;`, `  x = 1;`] },
])(`does not offer unsafe edits for unclosed blocks: $name`, async ({ lines }) => {
	const { actions } = await getFixes(lines, Range.create(0, 0, lines.length - 1, 100));
	expect(actions).toHaveLength(0);
});

test(`does not rewrite a continuation line containing a second statement`, async () => {
	const lines = [`**free`, `x = foo(`, `  a); y = 1;`];
	const { document, docs, actions } = await getFixes(lines, Range.create(0, 0, 2, 100));
	expect(actions).toHaveLength(0);
	expect(Linter.getErrors({ uri: document.uri, content: document.getText() }, rules, docs).indentErrors).toEqual([]);
});

test(`keeps safe prefix fixes available before an unclosed block`, async () => {
	const lines = [`**free`, `if *on;`, `x = 1;`, `endif;`, `if *on;`, `  y = 2;`, `dcl-proc a;`, `end-proc;`];
	const { document, actions } = await getFixes(lines, Range.create(0, 0, 7, 100));
	expect(applyIndentFix(document, actions)).toBe([lines[0], lines[1], `  ${lines[2]}`, ...lines.slice(3)].join(`\n`));
});

test(`preserves a lint fix whose multiline diagnostic contains the selection`, async () => {
	rules = { indent: 2, ForceOptionalParens: true };
	const otherAction = CodeAction.create(`Add brackets around expression`, CodeActionKind.QuickFix);
	getActions.mockReturnValue([otherAction]);
	const { actions } = await getFixes([`**free`, `if a`, `  and b;`, `endif;`], Range.create(2, 0, 2, 8));
	expect(actions).toContain(otherAction);
});

test(`keeps fixes available for balanced local blocks after ordinary includes`, async () => {
	const lines = [`**free`, `/copy common`, `if *on;`, `x = 1;`, `endif;`];
	const { document, actions } = await getFixes(lines, Range.create(2, 0, 4, 6));
	expect(applyIndentFix(document, actions)).toBe([lines[0], lines[1], lines[2], `  ${lines[3]}`, lines[4]].join(`\n`));
});

test(`keeps other lint fixes available after indentation becomes unreliable`, async () => {
	rules = { indent: 2, UselessOperationCheck: true };
	const otherAction = CodeAction.create(`Remove operation code`, CodeActionKind.QuickFix);
	getActions.mockReturnValue([otherAction]);
	const { actions } = await getFixes([`**free`, `endif;`, `eval a = 1;`], Range.create(2, 0, 2, 0));
	expect(actions).toEqual([otherAction]);
});

test(`offers indentation and conversion together on a global BEGSR line`, async () => {
	const content = [`**free`, `  begsr work;`, `  x = 1;`, `endsr;`].join(`\n`);
	const document = TextDocument.create(`file:///subroutine.rpgle`, `rpgle`, 1, content);
	const docs = await new Parser().getDocs(document.uri, content, { collectReferences: true });
	if (!docs) throw new Error(`Expected parsed subroutine`);
	documents.get.mockReturnValue(document);
	parser.getDocs.mockResolvedValue(docs);
	const actions = await genericCodeActionsProvider({
		textDocument: { uri: document.uri },
		range: Range.create(1, 3, 1, 3),
		context: { diagnostics: [] },
	});
	expect(actions?.map(action => action.title)).toEqual(expect.arrayContaining([`Convert to procedure`, `Fix indentation`]));
	expect(applyIndentFix(document, actions!)).toBe([`**free`, `begsr work;`, `  x = 1;`, `endsr;`].join(`\n`));
});

test.each([
	{ name: `declaration guard without ELSE`, guard: [`/if defined(A)`, `dcl-s flag ind;`, `/endif`] },
	{ name: `balanced branches`, guard: [`/if defined(A)`, `if *on;`, `endif;`, `/else`, `if *off;`, `endif;`, `/endif`] },
	{ name: `nested guards`, guard: [`/if defined(A)`, `/if defined(B)`, `dcl-s flag ind;`, `/endif`, `/else`, `dcl-s other ind;`, `/endif`] },
	{ name: `ELSEIF branches`, guard: [`/if defined(A)`, `dcl-s a ind;`, `/elseif defined(B)`, `dcl-s b ind;`, `/else`, `dcl-s c ind;`, `/endif`] },
])(`resumes indentation fixes after compatible conditional branches: $name`, async ({ guard }) => {
	const lines = [`**free`, ...guard, `if *on;`, `x = 1;`, `endif;`];
	const { document, actions } = await getFixes(lines, Range.create(0, 0, lines.length - 1, 100));
	const fixed = [...lines];
	fixed[fixed.length - 2] = `  x = 1;`;
	expect(applyIndentFix(document, actions)).toBe(fixed.join(`\n`));
});

test(`reconciles blocks opened in either branch before fixing the shared body`, async () => {
	const lines = [`**free`, `/if defined(A)`, `if *on;`, `/else`, `if *off;`, `/endif`, `x = 1;`, `endif;`];
	const { document, actions } = await getFixes(lines, Range.create(0, 0, 7, 100));
	expect(applyIndentFix(document, actions)).toBe([...lines.slice(0, 6), `  x = 1;`, lines[7]].join(`\n`));
});

test.each([
	{ name: `unequal depths`, lines: [`**free`, `/if defined(A)`, `if *on;`, `/else`, `dcl-s flag ind;`, `/endif`, `x = 1;`, `endif;`] },
	{ name: `equal depths but different block types`, lines: [`**free`, `/if defined(A)`, `if *on;`, `/else`, `dow *on;`, `/endif`, `x = 1;`, `endif;`] },
	{ name: `implicit ELSE`, lines: [`**free`, `/if defined(A)`, `if *on;`, `/endif`, `x = 1;`, `endif;`] },
	{ name: `missing ENDIF directive`, lines: [`**free`, `/if defined(A)`, `if *on;`, `x = 1;`, `endif;`] },
])(`withholds edits after incompatible or incomplete conditional branches: $name`, async ({ lines }) => {
	const { actions } = await getFixes(lines, Range.create(0, 0, lines.length - 1, 100));
	expect(actions).toHaveLength(0);
});

test(`two tabs get a tab-specific diagnostic even when their character count equals the expected spaces`, async () => {
	const { actions } = await getFixes([`**free`, `if *on;`, `\t\tx = 1;`, `endif;`], Range.create(2, 2, 2, 2));
	const action = actions.find(candidate => candidate.title === `Fix indentation`);
	expect(action?.diagnostics?.[0].message).toBe(`Incorrect indentation. Expected 2 spaces; found tab characters.`);
});

test.each([`else;`, `on-error;`])(`does not dedent code after an unmatched branch statement: %s`, branch => {
	return getFixes([`**free`, branch, `  x = 1;`], Range.create(0, 0, 2, 100)).then(({ actions }) => {
		expect(actions).toHaveLength(0);
	});
});
