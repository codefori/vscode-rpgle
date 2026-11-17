import { afterEach, expect, test } from 'vitest';
import { mkdtempSync, mkdirSync, readFileSync, rmSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import * as path from 'node:path';
import { createRequire } from 'node:module';
import { spawnSync } from 'node:child_process';

const taskRequire = createRequire(__filename);
const tsx = taskRequire.resolve(`tsx/cli`);
const cli = path.resolve(__dirname, `../../cli/rpglint/index.ts`);
const directories: string[] = [];

afterEach(() => {
	for (const directory of directories.splice(0)) rmSync(directory, { recursive: true, force: true });
});

function runCli(lines: string[], args: string[] = []) {
	const directory = mkdtempSync(path.join(tmpdir(), `rpglint-605-`));
	directories.push(directory);
	mkdirSync(path.join(directory, `.vscode`));
	writeFileSync(path.join(directory, `.vscode/rpglint.json`), JSON.stringify({ indent: 2 }));
	const source = lines.join(`\n`);
	const sourcePath = path.join(directory, `program.rpgle`);
	writeFileSync(sourcePath, source);
	const result = spawnSync(process.execPath, [tsx, cli, ...args], { cwd: directory, encoding: `utf8`, timeout: 20_000 });
	expect(result.error).toBeUndefined();
	expect(result.stderr).toBe(``);
	expect(readFileSync(sourcePath, `utf8`)).toBe(source);
	return result;
}

test(`rpglint CLI uses a clear message for two tabs instead of Expected 2, got 2`, () => {
	const result = runCli([`**free`, `if *on;`, `\t\tx = 1;`, `endif;`]);
	expect(result.status).toBe(1);
	expect(result.stdout).toContain(`Expected 2 spaces; found tab characters.`);
	expect(result.stdout).not.toContain(`got 2`);
});

test(`rpglint CLI recognizes a tab-indented closer without drifting onto the next line`, () => {
	const result = runCli([`**free`, `if *on;`, `  x = 1;`, `\tendif;`, `y = 2;`]);
	expect(result.status).toBe(1);
	expect(result.stdout).toContain(`program.rpgle: 1 error.`);
	expect(result.stdout).toContain(`Line 4: Incorrect indentation. Expected 0 spaces; found tab characters.`);
	expect(result.stdout).not.toContain(`Line 5`);
});

test.each([`@rpglint-skip`, `@rpglint-skip-indent`])(`rpglint CLI maintains depth through skipped boundaries: %s`, directive => {
	const result = runCli([`**free`, `// ${directive}`, `if *on;`, `  x = 1;`, `// ${directive}`, `endif;`, `y = 2;`]);
	expect(result.status).toBe(0);
	expect(result.stdout).not.toContain(`error`);
});

test.each([
	{ name: `balanced conditional`, lines: [`**free`, `/if defined(A)`, `dcl-s flag ind;`, `/endif`, `if *on;`, `  x = 1;`, `endif;`] },
	{ name: `equivalent branch openers with a shared body`, lines: [`**free`, `/if defined(A)`, `if *on;`, `/else`, `if *off;`, `  /endif`, `  x = 1;`, `endif;`] },
	{ name: `continuation with a later statement`, lines: [`**free`, `x = foo(`, `  a); y = 1;`] },
])(`rpglint CLI does not report false indentation errors: $name`, ({ lines }) => {
	const result = runCli(lines);
	expect(result.status).toBe(0);
	expect(result.stdout).not.toContain(`error`);
});

test(`rpglint CLI retains the ordinary space-indentation message and failing exit status`, () => {
	const result = runCli([`**free`, `if *on;`, `x = 1;`, `endif;`]);
	expect(result.status).toBe(1);
	expect(result.stdout).toContain(`Line 3: expected indent of 2, got 0`);
});

test(`rpglint CLI includes the tab-specific message in file/line/column output`, () => {
	const result = runCli([`**free`, `if *on;`, `\t\tx = 1;`, `endif;`], [`--output`, `flc`]);
	expect(result.status).toBe(1);
	expect(result.stdout).toContain(`program.rpgle:3:2:Incorrect indentation. Expected 2 spaces; found tab characters.`);
});
