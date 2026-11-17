import { beforeEach, expect, test, vi } from 'vitest';
import { TextDocument } from 'vscode-languageserver-textdocument';
import { CodeAction, Range, TextEdit } from 'vscode-languageserver';
import Parser from '../../../language/ile/parser';
import Linter from '../../../language/ile/linter';
import type { Rules } from '../../../language/ile/parserTypes';
import documentFormattingProvider from '../src/providers/linter/documentFormatting';

const { documents, parser, getLintOptions, getActions } = vi.hoisted(() => ({
	documents: { get: vi.fn() }, parser: { getDocs: vi.fn() },
	getLintOptions: vi.fn(), getActions: vi.fn(),
}));
vi.mock(`../src/providers`, () => ({ documents, parser }));
vi.mock(`../src/providers/linter`, () => ({ getLintOptions, getActions }));

beforeEach(() => {
	vi.clearAllMocks();
	getLintOptions.mockResolvedValue({ indent: 2 });
	getActions.mockReturnValue([]);
});

async function format(lines: string[], rules: Rules = { indent: 2 }) {
	const document = TextDocument.create(`file:///format.rpgle`, `rpgle`, 1, lines.join(`\n`));
	const realParser = new Parser();
	const docs = await realParser.getDocs(document.uri, document.getText());
	if (!docs) throw new Error(`Expected parsed document`);
	documents.get.mockReturnValue(document);
	parser.getDocs.mockResolvedValue(docs);
	getLintOptions.mockResolvedValue(rules);
	const edits = await documentFormattingProvider({ textDocument: { uri: document.uri }, options: { tabSize: 2, insertSpaces: true } });
	return { output: TextDocument.applyEdits(document, edits ?? []), edits: edits ?? [] };
}

test.each([`\t`, `\t\t`, `  \t`])(`Format Document replaces the full leading tab whitespace %j`, async whitespace => {
	const { output } = await format([`**free`, `if *on;`, `${whitespace}x = 1;`, `\tendif;`, `y = 2;`]);
	expect(output).toBe([`**free`, `if *on;`, `  x = 1;`, `endif;`, `y = 2;`].join(`\n`));
});

test.each([
	[`**free`, `endif;`, `if *on;`, `  x = 1;`, `endif;`],
	[`**free`, `if *on;`, `  x = 1;`, `dcl-proc a;`, `end-proc;`],
	[`**free`, `/if defined(A)`, `if *on;`, `/endif`, `x = 1;`, `endif;`],
])(`Format Document withholds unsafe edits in malformed or ambiguous blocks: %j`, async (...lines) => {
	const { output, edits } = await format(lines);
	expect(edits).toEqual([]);
	expect(output).toBe(lines.join(`\n`));
});

test(`Format Document resumes after a balanced conditional guard`, async () => {
	const lines = [`**free`, `/if defined(A)`, `dcl-s flag ind;`, `/endif`, `if *on;`, `x = 1;`, `endif;`];
	const { output } = await format(lines);
	expect(output).toBe([...lines.slice(0, 5), `  x = 1;`, lines[6]].join(`\n`));
});

test.each([`@rpglint-skip`, `@rpglint-skip-indent`])(`Format Document respects skipped block boundaries: %s`, async directive => {
	const lines = [`**free`, `// ${directive}`, `if *on;`, `  x = 1;`, `// ${directive}`, `endif;`, `y = 2;`];
	expect((await format(lines)).output).toBe(lines.join(`\n`));
});

test(`Format Document leaves a continuation line containing a later statement alone`, async () => {
	const lines = [`**free`, `x = foo(`, `  a); y = 1;`];
	expect((await format(lines)).output).toBe(lines.join(`\n`));
});

test(`Format Document is idempotent after correcting tabs`, async () => {
	const first = await format([`**free`, `if *on;`, `\t\tx = 1;`, `endif;`]);
	const second = await format(first.output.split(`\n`));
	expect(second.edits).toEqual([]);
	expect(second.output).toBe(first.output);
	const docs = await new Parser().getDocs(`file:///format.rpgle`, first.output);
	expect(Linter.getErrors({ uri: `file:///format.rpgle`, content: first.output }, { indent: 2 }, docs).indentErrors).toEqual([]);
});

test(`Format Document preserves unrelated lint fixes alongside safe indentation edits`, async () => {
	const action = CodeAction.create(`Remove operation code`);
	action.edit = { changes: { [`file:///format.rpgle`]: [TextEdit.replace(Range.create(2, 1, 2, 6), ``)] } };
	getActions.mockReturnValue([action]);
	const { output } = await format([`**free`, `if *on;`, ` eval x = 1;`, `endif;`], { indent: 2, UselessOperationCheck: true });
	expect(output).toBe([`**free`, `if *on;`, `  x = 1;`, `endif;`].join(`\n`));
	expect(getActions.mock.calls[0][1]).toEqual(expect.arrayContaining([expect.objectContaining({ type: `UselessOperationCheck` })]));
});
