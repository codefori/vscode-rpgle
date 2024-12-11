import Parser from '../language/parser';

import glob from "glob";
import path from 'path';
import fs from 'fs';

import { readFile } from 'fs/promises';

import tables from './tables';
import { dspffdToRecordFormats } from '../extension/server/src/data';

const TEST_INCLUDE_DIR = process.env.INCLUDE_DIR || path.join(__dirname, `..`, `tests`);

export default function setupParser(projectRoot = TEST_INCLUDE_DIR): Parser {
	const parser = new Parser();
	let ignore: string[] = [];

	if (projectRoot === TEST_INCLUDE_DIR) {
		ignore.push("sources/**")
	}

	let globSettings = {
		cwd: projectRoot,
		absolute: true,
		ignore,
		nocase: true,
	}

	const globCache = glob.sync(`**/*.*rpg*`, globSettings);

	parser.setIncludeFileFetch(async (baseFile: string, includeFile: string) => {
		if (includeFile.startsWith(`'`) && includeFile.endsWith(`'`)) {
			includeFile = includeFile.substring(1, includeFile.length - 1);
		}

		if (includeFile.includes(`,`) || !includeFile.includes(`.`)) {
			includeFile = includeFile.split(`,`).join(`/`) + `.*rpgl*`;
		}


		const globPath = path.join(`**`, includeFile);
		const files: string[] = glob.sync(globPath, {cache: globCache, ...globSettings});

		if (files.length >= 1) {
			const file = files.find(f => f.toLowerCase().endsWith(`rpgleinc`)) || files[0];

			const content = await readFile(file, { encoding: `utf-8` });
			
			return {
				found: true,
				uri: file,
				content
			}
		}

		return {
			found: false
		};
	});

	parser.setTableFetch(async (table: string, aliases = false) => {
		const upperName = table.toUpperCase();
		
		const data = tables[upperName] ? tables[upperName] : [];
		
		return dspffdToRecordFormats(data, aliases);
	});

	return parser;
}

export function getTestProjectsDir(): string[] {
	// get a list of directories in the test directory using fs
	const sourcesDir = path.join(TEST_INCLUDE_DIR, `sources`);
	return fs.readdirSync(sourcesDir)
		.filter((f) => fs.statSync(path.join(TEST_INCLUDE_DIR, `sources`, f)).isDirectory())
		.map((f) => path.join(sourcesDir, f));
}

export function getSourcesList(fullDirPath: string): string[] {
	return glob.sync(`**/*.{rpgle,sqlrpgle}`, {
		cwd: fullDirPath,
		absolute: true,
		nocase: true,
	});
}

export function getFileContent(fullPath: string) {
	return readFile(fullPath, { encoding: `utf-8` });
}