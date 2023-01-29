// rpglint CLI
// --files [glob]
// --cwd "[path]"

import glob from "glob";
import { readFileSync } from 'fs';

import Parser from '../server/src/language/parser';
import Linter from '../server/src/language/linter';
import { Rules } from '../server/src/language';

main();

async function main() {
	const parms = process.argv.slice(2);

	let cwd = process.cwd();
	let scanGlob = `**/*.{SQLRPGLE,sqlrpgle,RPGLE,rpgle}`;

	for (let i = 0; i < parms.length; i++) {
		switch (parms[0]) {
			case `-f`:
			case `--files`:
				scanGlob = parms[i + 1];
				i++;
				break;

			case `-d`:
			case `--cwd`:
				cwd = parms[i + 1];
				i++;
				break;
		}
	}

	let rules: Rules;
	let parser: Parser;
	let files: string[];

	try {
		rules = getLintConfig(cwd);
		parser = setupParser(cwd, scanGlob);
		files = getFiles(cwd, scanGlob);
	} catch (e) {
		console.log(e.message || e);
		process.exit();
	}

	const ruleCount = Object.keys(rules).length;

	if (ruleCount === 0) {
		console.log(`rpglint.json does not have any rules. Exiting.`);
		process.exit();
	}

	let totalFailures = 0;

	for (const filePath of files) {
		try {
			const content = readFileSync(filePath, { encoding: `utf-8` });

			if (content.length > 6 && content.substring(0, 6).toLowerCase() === `**free`) {
				const docs = await parser.getDocs(
					filePath,
					content,
					{
						withIncludes: true
					}
				);

				const lintResult = Linter.getErrors({
					uri: filePath,
					content
				}, rules, docs);

				const totalErrors = lintResult.errors.length + lintResult.indentErrors.length;

				if (totalErrors) {
					totalFailures += 1;

					console.log();
					console.log(`${filePath}: ${totalErrors} error${totalErrors > 1 ? `s` : ``}.`);
					if (lintResult.indentErrors.length) {
						lintResult.indentErrors.forEach(indentError => {
							console.log(`\t\tLine ${indentError.line + 1}: expected indent of ${indentError.expectedIndent}, got ${indentError.currentIndent}`);
						});
					}

					if (lintResult.errors.length) {
						lintResult.errors.forEach(error => {
							console.log(`\t\tLine ${error.range.start.line + 1}, column ${error.range.start.character}: ${Linter.getErrorText(error.type)}`);
						});
					}
				}
			}

		} catch (e) {
			console.log(`Failed to lint ${filePath}: ${e.message || e}`);
			console.log(`Report this issue to us with an example: github.com/halcyon-tech/vscode-rpgle/issues`);
		}
	}

	process.exit(totalFailures > 0 ? 1 : 0);
}

function getLintConfig(cwd: string): Rules {
	const files = glob.sync(`**/rpglint.json`, {
		cwd,
		absolute: true,
		nocase: true,
		dot: true
	});

	if (files.length >= 1) {
		const file = files[0];

		const content = readFileSync(file, { encoding: `utf-8` });
		const result = JSON.parse(content);
		return result;
	}

	throw new Error(`Unable to locate rpglint.json`);
}

function getFiles(cwd: string, globPath: string): string[] {
	return glob.sync(globPath, {
		cwd,
		absolute: true,
		nocase: true,
	});
}

function setupParser(cwd: string, globPath: string): Parser {
	const parser = new Parser();

	parser.setIncludeFileFetch(async (baseFile: string, includeFile: string) => {
		if (includeFile.startsWith(`'`) && includeFile.endsWith(`'`)) {
			includeFile = includeFile.substring(1, includeFile.length - 1);
		}

		if (includeFile.includes(`,`)) {
			includeFile = includeFile.split(`,`).join(`/`) + `.*`;
		}

		const files = glob.sync(globPath, {
			cwd,
			absolute: true,
			nocase: true,
		});

		if (files.length >= 1) {
			const file = files[0];

			const content = readFileSync(file, { encoding: `utf-8` });
			return {
				found: true,
				uri: file,
				lines: content.split(`\n`)
			}
		}

		return {
			found: false
		};
	});

	parser.setTableFetch(async (table: string, aliases = false) => {
		// Can't support tables in CLI mode I suppose?
		return [];
	});

	return parser;
}