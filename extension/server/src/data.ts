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

		const aliasName: string|undefined = row.WHALIS ? row.WHALIS.trim() : undefined;
		const name = aliases ? aliasName || row.WHFLDE : row.WHFLDE;

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
		let keywords: {[key: string]: string|true} = {};

		if (row.WHVARL === `Y`) keywords[`VARYING`] = true;

		currentSubfield.keyword = getPrettyType({
			type,
			len: digits === 0 ? strLength : digits,
			decimals: decimals,
			keywords,
			field: ``,
			pos: ``
		});
		currentSubfield.description = text.trim();

		recordFormat.subItems.push(currentSubfield);
	});

	return Object.values(recordFormats);
}