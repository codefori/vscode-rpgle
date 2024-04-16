// rpglint CLI
// --files [glob]
// --cwd "[path]"

import glob from "glob";
import { readFileSync } from 'fs';

import Parser from '../../language/parser';
import Linter from '../../language/linter';
import { Rules } from '../../language/parserTypes';
import path from 'path';

type FormatTypes = "standard" | "flc";

main();

async function main() {
	const parms = process.argv.slice(2);

	let cwd = process.cwd();
	let scanGlob = `**/*.{SQLRPGLE,sqlrpgle,RPGLE,rpgle}`;
	let maxErrors: number|undefined;
	let outputType: FormatTypes;

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

			case `-m`:
			case `--max`:
				maxErrors = Number(parms[i + 1]);
				i++;
				break;
			
			case `-o`:
			case `--output`:
				outputType = (parms[i+1] as FormatTypes);
				i++;
				break;

			case `-h`:
			case `--help`:
				console.log(`rpglint (derived from vscode-rpgle).`);
				console.log(``);
				console.log(`See who's contributed: https://github.com/halcyon-tech/vscode-rpgle/graphs/contributors`);
				console.log();
				console.log(`Rules are inherited from the 'rpglint.json' found in the working directory.`);
				console.log(`This configuration file usually lives in '.vscode/rpglint.json'.`);
				console.log();
				console.log(`\t-d`)
				console.log(`\t--cwd\t\tTo see the directory of where source code lives.`);
				console.log(`\t\t\tThe default is the current working directory.`);
				console.log();
				console.log(`\t-f`);
				console.log(`\t--files\t\tGlob used to search for sources in the working directory.`);
				console.log(`\t\t\tDefaults to '${scanGlob}'`);
				console.log();
				console.log(`\t-m`);
				console.log(`\t--max\t\tThe max limit of errored files before the process ends itself.`);
				console.log();
				console.log(`\t-o`);
				console.log(`\t--output\tFormat of the lint errors in standard out.`);
				console.log(`\t\t\tDefaults to standard. Available: standard, flc`);
				console.log();
				process.exit(0);
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
		error(e.message || e);
		process.exit(1);
	}

	const ruleCount = Object.keys(rules).length;

	if (ruleCount === 0) {
		error(`rpglint.json does not have any rules. Exiting.`);
		process.exit();
	}

	let totalFailures = 0;

	console.log(`Linting ${files.length} file${files.length !== 1 ? `s` : ``}.`);

	if (files.length > 500) {
		console.log(`Looks like you are linting a lot of files! It might be worth checking out the '--max' parameter to limit the amount of errors that will be produced. This is useful to end the lint process after so many issues.`);
	}

	console.log();

	for (const filePath of files) {
		if (maxErrors && totalFailures >= maxErrors) {
			console.log();
			console.log(`Max errors of ${maxErrors} has been reached. Ending.`);
			break;
		}

		try {
			const content = readFileSync(filePath, { encoding: `utf-8` });
			const eol = content.includes(`\r\n`) ? `\r\n` : `\n`;
			const eolIndexes: number[] = [];

			for (let i = 0; i < content.length; i++) {
				if (content.substring(i, i + eol.length) === eol) {
					eolIndexes.push(i);
				}
			}

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

					switch (outputType) {
						case `flc`:
							if (lintResult.indentErrors.length) {
								lintResult.indentErrors.forEach(indentError => {
									console.log(`${filePath}:${indentError.line + 1}:${indentError.currentIndent}:Expected indent of ${indentError.expectedIndent}`);
								});
							}
		
							if (lintResult.errors.length) {
								lintResult.errors.forEach(error => {
									const line = eolIndexes.findIndex(index => index > error.offset.position);
									const offset = error.offset.position - (eolIndexes[line-1] || 0);
									console.log(`${filePath}:${line+1}:${offset}:${Linter.getErrorText(error.type)}`);
								});
							}
							break;

						case `standard`:
						default:
							const relative = path.relative(cwd, filePath);
							console.log(`${relative}: ${totalErrors} error${totalErrors !== 1 ? `s` : ``}.`);
							if (lintResult.indentErrors.length) {
								lintResult.indentErrors.forEach(indentError => {
									console.log(`\tLine ${indentError.line + 1}: expected indent of ${indentError.expectedIndent}, got ${indentError.currentIndent}`);
								});
							}
		
							if (lintResult.errors.length) {
								lintResult.errors.forEach(error => {
									const line = eolIndexes.findIndex(index => index > error.offset.position);
									const offset = error.offset.position - (eolIndexes[line-1] || 0);
									console.log(`\tLine ${line+1}, column ${offset}: ${Linter.getErrorText(error.type)}`);
								});
							}
							break;
					}
				}
			}

		} catch (e) {
			error(`Failed to lint ${filePath}: ${e.message || e}`);
			error(`Report this issue to us with an example: github.com/halcyon-tech/vscode-rpgle/issues`);
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

function error(line: string) {
	process.stdout.write(line + `\n`);
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
		
		includeFile = path.join(`**`, includeFile);
		const files = glob.sync(includeFile, {
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