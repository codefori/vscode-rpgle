import Parser from '../language/parser';

import glob from "glob";
import path from 'path';

import { readFile } from 'fs/promises';

import tables from './tables';
import { dspffdToRecordFormats } from '../extension/server/src/data';

const includeDir = process.env.INCLUDE_DIR || path.join(__dirname, `..`, `tests`);

export default function setupParser(): Parser {
	const parser = new Parser();

	parser.setIncludeFileFetch(async (baseFile: string, includeFile: string) => {
		if (includeFile.startsWith(`'`) && includeFile.endsWith(`'`)) {
			includeFile = includeFile.substring(1, includeFile.length - 1);
		}

		if (includeFile.includes(`,`) || !includeFile.includes(`.`)) {
			includeFile = includeFile.split(`,`).join(`/`) + `.*`;
		}

		const globPath = path.join(`**`, includeFile);
		const files = glob.sync(globPath, {
			cwd: includeDir,
			absolute: true,
			nocase: true,
		});

		if (files.length >= 1) {
			const file = files[0];

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

export function getSourcesList(): string[] {
	return glob.sync(`**/*.{rpgle,sqlrpgle}`, {
		cwd: path.join(includeDir, `sources`),
		nocase: true,
	});
}

export function getSourcesContent(name: string) {
	return readFile(path.join(includeDir, `sources`, name), { encoding: `utf-8` });
}