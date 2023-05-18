

import glob from "glob";
import { existsSync, readFileSync, writeFileSync } from 'fs';

import Parser from '../../../language/parser';
import { setupParser } from './parser';
import { ILEObject, Targets } from './targets';
import { Project } from './project';
import path from 'path';
import { cliSettings, info } from './cli';

let cwd = process.cwd();

main();

async function main() {
	const parms = process.argv.slice(2);
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

			case `--verbose`:
				cliSettings.infoMessages = true;
				break;

			case `--nofile`:
				cliSettings.createMakefile = false;
				break;

			case `-l`:
				cliSettings.createMakefile = false;
				cliSettings.lookup = parms[i+1];
				i++;
				break;

			case `-h`:
			case `--help`:
				console.log(`rmake (derived from vscode-rpgle).`);
				console.log(``);
				console.log(`See who's contributed: https://github.com/halcyon-tech/vscode-rpgle/graphs/contributors`);
				console.log(``);
				console.log(`rmake is a very simple tool. It looks for .{rpgle,sqlrpgle} sources,`);
				console.log(`parses them to find dependencies and generates a makefile to be used with GNU Make.`);
				console.log(``);
				console.log(`\t-d <dir>`)
				console.log(`\t--cwd <dir>\tTo see the directory of where source code lives.`);
				console.log(`\t\t\tThe default is the current working directory.`);
				console.log(``);
				console.log(`\t-l <obj>\tPrint an object and what depends on it.`);
				console.log(``);
				console.log(`\t-i`);
				console.log(`\t--init\t\tAdd default compile options to 'iproj.json' file`);
				console.log(``);
				console.log(`\t--verbose\tPrint all the detail.`);
				console.log(``);
				console.log(`\t--nofile\tDo not create the makefile.`);
				console.log(``);
				process.exit(0);
		}
	}

	let parser: Parser;
	let files: string[];

	try {
		parser = setupParser(cwd, scanGlob);
		files = getFiles(cwd, scanGlob);

		info(`Found ${files.length} file${files.length === 1 ? `` : `s`} with '${scanGlob}'.`);
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

			targets.createRpgTarget(filePath, docs);

		} catch (e) {
			error(`Failed to parse ${filePath}: ${e.message || e}`);
			error(`Report this issue to us with an example: github.com/halcyon-tech/vscode-rpgle/issues`);
			process.exit(1);
		}
	}

	targets.resolveBinder();

	const project = new Project(cwd, targets);

	if (cliSettings.lookup) {
		listDeps(targets, cliSettings.lookup);
	}

	if (cliSettings.createMakefile) {
		writeFileSync(path.join(cwd, `makefile`), project.getMakefile().join(`\n`));
	}
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

	if (iprojExists) {
		try {
			console.log(`iproj.json already exists. Will append new properties.`);
			base = JSON.parse(readFileSync(iprojPath, {encoding: `utf-8`}));
		} catch (e) {
			error(`Failed to parse iproj.json. Aborting`);
			process.exit(1);
		}
	}

	base = {
		...base,
		...Project.getDefaultSettings()
	};

	writeFileSync(iprojPath, JSON.stringify(base, null, 2));

	console.log(`Written to ${iprojPath}`);
}

/**
 * @param query Can be object (ABCD.PGM) or relative path
 */
function listDeps(targets: Targets, query: string) {
	const fullPath = path.join(cwd, query);

	let [name, type] = query.split(`.`);

	name = name.toUpperCase();
	type = type.toUpperCase();

	let theObject = targets.getResolvedObjects().find(o => o.name === name && o.type === type);

	if (!theObject) {
	  theObject = targets.resolveObject(fullPath);
	}

	if (!theObject) {
		console.log(`No object found for '${query}'`);
	}

	let indent = 0;
	
	const allDeps = targets.getDeps();
	let currentTree: ILEObject[] = [];

	function lookupObject(ileObject: ILEObject) {
		console.log(`${''.padEnd(currentTree.length, `\t`)}${ileObject.name}.${ileObject.type} (${ileObject.relativePath || `no source`})`);

		currentTree.push(ileObject);

		for (const target of allDeps) {
			const containsLookup = target.deps.some(d => d.name === ileObject.name && d.type === ileObject.type);
			const circurlar = currentTree.some(d => d.name === target.name && d.type === target.type);

			if (containsLookup && !circurlar) {
				lookupObject(target);
			}
		}

		currentTree.pop();
	}

	lookupObject(theObject);
}