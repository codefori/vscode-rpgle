import { beforeEach, expect, test, vi } from 'vitest';
import { TextDocument } from 'vscode-languageserver-textdocument';
import Linter from '../../../language/ile/linter';
import Parser from '../../../language/ile/parser';
import { jsonCache, refreshLinterDiagnostics } from '../src/providers/linter';

const { connection, validateUri } = vi.hoisted(() => ({
	connection: { sendDiagnostics: vi.fn() },
	validateUri: vi.fn(async () => `file:///rpglint.json`),
}));
vi.mock(`../src/connection`, () => ({ connection, validateUri, watchedFilesChangeEvent: [] }));
vi.mock(`../src/providers`, () => ({ documents: {}, parser: {} }));
vi.mock(`../src/providers/project`, () => ({ isEnabled: false }));

beforeEach(() => {
	vi.restoreAllMocks();
	connection.sendDiagnostics.mockClear();
	jsonCache[`file:///rpglint.json`] = JSON.stringify({ indent: 2 });
});

test(`code actions reuse published lint results, invalidating versions, parsed scope and settings`, async () => {
	const content = [`**free`, `if *on;`, `x = 1;`, `endif;`].join(`\n`);
	const document = TextDocument.create(`file:///cached.rpgle`, `rpgle`, 1, content);
	const parser = new Parser();
	const docs = await parser.getDocs(document.uri, content);
	if (!docs) throw new Error(`Expected parsed document`);
	const lint = vi.spyOn(Linter, `getErrors`);

	const published = await refreshLinterDiagnostics(document, docs);
	expect(connection.sendDiagnostics).toHaveBeenCalledTimes(1);
	expect(await refreshLinterDiagnostics(document, docs, false)).toBe(published);
	expect(await refreshLinterDiagnostics(document, docs, false)).toBe(published);
	expect(lint).toHaveBeenCalledTimes(1);
	expect(connection.sendDiagnostics).toHaveBeenCalledTimes(1);

	TextDocument.update(document, [{ text: content.replace(`x = 1;`, `x = 2;`) }], 2);
	await refreshLinterDiagnostics(document, docs, false);
	expect(lint).toHaveBeenCalledTimes(2);

	const reparsed = await new Parser().getDocs(document.uri, document.getText());
	if (!reparsed) throw new Error(`Expected reparsed document`);
	await refreshLinterDiagnostics(document, reparsed, false);
	expect(lint).toHaveBeenCalledTimes(3);

	jsonCache[`file:///rpglint.json`] = JSON.stringify({ indent: 4 });
	await refreshLinterDiagnostics(document, reparsed, false);
	expect(lint).toHaveBeenCalledTimes(4);

	// Explicit diagnostic refreshes still recompute and publish, even at the same version.
	await refreshLinterDiagnostics(document, reparsed);
	expect(lint).toHaveBeenCalledTimes(5);
	expect(connection.sendDiagnostics).toHaveBeenCalledTimes(2);
});
