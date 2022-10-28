import Parser from '../server/src/language/parser';
import Declaration from '../server/src/language/models/declaration';
import { getPrettyType } from '../server/src/language/models/fixed';

import glob from "glob";
import path from 'path';

import { readFile } from 'fs/promises';

import tables from './tables';

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
	});

	parser.setTableFetch(async (table: string, aliases = false) => {
		let recordFormats: {[name: string]: Declaration} = {};
		const upperName = table.toUpperCase();
		
		const data = tables[upperName] ? tables[upperName] : [];
		
		data.forEach((row: any) => {
			const {
				WHNAME: formatName,
				WHFLDT: type,
				WHFLDB: strLength, 
				WHFLDD: digits,
				WHFLDP: decimals,
				WHFTXT: text,
			} = row;
	
			const name = aliases ? row.WHALIS || row.WHFLDE : row.WHFLDE;
	
			if (name.trim() === ``) return;
			if (name.startsWith(`*`)) return;
	
			let recordFormat;
			if (recordFormats[formatName]) {
				recordFormat = recordFormats[formatName];
			} else {
				recordFormat = new Declaration(`struct`);
				recordFormat.name = formatName;
				recordFormats[formatName] = recordFormat;
			}
	
			const currentSubfield = new Declaration(`subitem`);
			currentSubfield.name = name;
			const keywords = [];
	
			if (row.WHVARL === `Y`) keywords.push(`VARYING`);
	
			currentSubfield.keywords = [getPrettyType({
				type,
				len: digits === 0 ? strLength : digits,
				decimals: decimals,
				keywords,
			})];
			currentSubfield.description = text.trim();
	
			recordFormat.subItems.push(currentSubfield);
		});
	
		return Object.values(recordFormats);
	});

	return parser;
}