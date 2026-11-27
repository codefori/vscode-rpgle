import { TextDocument } from 'vscode-languageserver-textdocument';
import { afterEach, expect, test, vi } from 'vitest';
import { documents, parser } from '../src/providers';
import hoverProvider from '../src/providers/hover';

afterEach(() => {
	vi.restoreAllMocks();
});

test(`shows named and description-only ILEDoc parameters in procedure hover`, async () => {
	const uri = `file:///param-hover.rpgle`;
	const lines = [
		`**free`,
		`///`,
		`// @param   VAR1  The first parameter`,
		`// @param The second parameter`,
		`// @param countItems that failed`,
		`///`,
		`dcl-proc example;`,
		`  dcl-pi *n;`,
		`    var1 ind const;`,
		`    var2 ind const;`,
		`    count int(10) const;`,
		`  end-pi;`,
		`end-proc;`
	];
	const source = lines.join(`\n`);
	const document = TextDocument.create(uri, `rpgle`, 1, source);
	vi.spyOn(documents, `get`).mockReturnValue(document);
	await parser.getDocs(uri, source, { ignoreCache: true, collectReferences: true });

	const hover = await hoverProvider({
		textDocument: { uri },
		position: { line: 6, character: 10 }
	});

	expect(hover?.contents).toMatchObject({
		kind: `markdown`,
		value: expect.stringContaining([
			`*@param* \`var1\` The first parameter`,
			`*@param* \`var2\` The second parameter`,
			`*@param* \`count\` countItems that failed`
		].join(`\n\n`))
	});
});
