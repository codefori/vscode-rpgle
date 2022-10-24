import Parser from '../server/src/language/parser';
import glob from "glob";
import { readFile } from 'fs/promises';
import path from 'path';

export default function setupParser(): Parser {
	const parser = new Parser();

	parser.setIncludeFileFetch(async (baseFile: string, includeFile: string) => {
		if (includeFile.startsWith(`'`) && includeFile.endsWith(`'`)) {
			includeFile = includeFile.substring(1, includeFile.length - 1);
		}

		if (includeFile.includes(`,`)) {
			includeFile = includeFile.split(`,`).join(`/`) + `.*`;
		}

		const globPath = path.join(`**`, includeFile);
		const files = glob.sync(globPath, {
			cwd: path.join(__dirname, `..`),
			absolute: true,
			nocase: true,
		});

		if (files.length >= 1) {
			const file = files[0];

			const content = await readFile(file, { encoding: `utf-8` });
			return {
				found: true,
				uri: file,
				lines: content.split(`\n`)
			}
		}

		return {
			found: false
		};
	})

	return parser;
}