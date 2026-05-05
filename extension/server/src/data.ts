import Declaration from '../../../language/models/declaration';
import { getPrettyType } from '../../../language/models/fixed';

export function isInMerlin(): boolean {
	const { MACHINE_EXEC_PORT } = process.env;
	return MACHINE_EXEC_PORT !== undefined;
}

export function parseMemberUri(path: string): {asp?: string, library?: string, file?: string, name: string} {
	const parts = path.split(`/`).map(s => s.split(`,`)).flat().filter(s => s.length >= 1);
	return {
		name: parts[parts.length - 1],
		file: parts[parts.length - 2],
		library: parts[parts.length - 3],
		asp: parts[parts.length - 4]
	}
};

function getAliasNames(row: any): string[] {
	const aliasKeys = Object.keys(row)
		.filter(key => key === `WHALIS` || /^WHALI\d+$/.test(key))
		.sort((a, b) => {
			const aNum = a === `WHALIS` ? 0 : Number(a.replace(`WHALI`, ``));
			const bNum = b === `WHALIS` ? 0 : Number(b.replace(`WHALI`, ``));
			return aNum - bNum;
		});

	return aliasKeys
		.map(key => row[key])
		.filter((value): value is string => typeof value === `string`)
		.map(value => value.trim())
		.filter(value => value.length > 0);
}

export function dspffdToRecordFormats(data: any, aliases = false): Declaration[] {
	let recordFormats: {[name: string]: Declaration} = {};

	data.forEach((row: any) => {
		const {
			WHNAME: formatName,
			WHFLDT: type,
			WHFLDB: strLength,
			WHFLDD: digits,
			WHFLDP: decimals,
			WHFTXT: text,
		} = row;

		const aliasNames = getAliasNames(row);
		const systemName = (row.WHFLDE as string).trim();
		const names: string[] = [];

		if (aliases && aliasNames.length > 0) {
			for (const alias of aliasNames) {
				if (alias !== systemName && !names.includes(alias)) {
					names.push(alias);
				}
			}
		}
		names.push(systemName);

		if (names.some(n => n === `` || n.startsWith(`*`))) return;

		let recordFormat;
		if (recordFormats[formatName]) {
			recordFormat = recordFormats[formatName];
		} else {
			recordFormat = new Declaration(`struct`);
			recordFormat.name = formatName;
			recordFormats[formatName] = recordFormat;
		}

		let keywords: {[key: string]: string|true} = {};
		if (row.WHVARL === `Y`) keywords[`VARYING`] = true;

		const keywordValue = getPrettyType({
			type,
			len: digits === 0 ? strLength : digits,
			decimals: decimals,
			keywords,
			field: ``,
			pos: ``
		});

		for (const name of names) {
			const currentSubfield = new Declaration(`subitem`);
			currentSubfield.name = name;
			currentSubfield.keyword = keywordValue;
			currentSubfield.tags.push({tag: `description`, content: text.trim()});
			recordFormat.subItems.push(currentSubfield);
		}
	});

	return Object.values(recordFormats);
}