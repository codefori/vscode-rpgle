

import glob from "glob";
import { existsSync, readFileSync, writeFileSync } from 'fs';

import Parser from '../../../language/parser';
import { setupParser } from './parser';
import { Targets } from './targets';
import { Project } from './project';
import path from 'path';

main();

async function main() {
	const parms = process.argv.slice(2);

	let cwd = process.cwd();
	let scanGlob = `**/*.{SQLRPGLE,sqlrpgle,RPGLE,rpgle}`;

	for (let i = 0; i < parms.length; i++) {
		switch (parms[i]) {
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

			case `-i`:
			case `--init`:
				initProject(cwd);
				process.exit(0);

			case `-h`:
			case `--help`:
				process.exit(0);
		}
	}

	let parser: Parser;
	let files: string[];

	try {
		parser = setupParser(cwd, scanGlob);
		files = getFiles(cwd, scanGlob);
	} catch (e) {
		error(e.message || e);
		process.exit(1);
	}

	const targets = new Targets(cwd);

	for (const filePath of files) {
		try {
			const content = readFileSync(filePath, { encoding: `utf-8` });

			const docs = await parser.getDocs(
				filePath,
				content,
				{
					withIncludes: true
				}
			);

			targets.createTarget(filePath, docs);

		} catch (e) {
			error(`Failed to parse ${filePath}: ${e.message || e}`);
			error(`Report this issue to us with an example: github.com/halcyon-tech/vscode-rpgle/issues`);
			process.exit(1);
		}
	}

	targets.determineLibraries();

	const project = new Project(cwd, targets);

	console.log(project.getMakefile().join(`\n`));
}

function getFiles(cwd: string, globPath: string): string[] {
	return glob.sync(globPath, {
		cwd,
		absolute: true,
		nocase: true,
	});
}

function error(line: string) {
	console.log(`ERROR: ${line}`);
}

function initProject(cwd) {
	console.log(`Initialising in ${cwd}`);

	const iprojPath = path.join(cwd, `iproj.json`);

	let base = {};
	const iprojExists = existsSync(iprojPath);

	try {
		console.log(`iproj.json already exists. Will append new properties.`);
		base = JSON.parse(readFileSync(iprojPath, {encoding: `utf-8`}));
	} catch (e) {
		error(`Failed to parse iproj.json. Aborting`);
		process.exit(1);
	}

	base = {
		...base,
		...Project.getDefaultSettings()
	};

	writeFileSync(iprojPath, JSON.stringify(base, null, 2));

	console.log(`Written to ${iprojPath}`);
}