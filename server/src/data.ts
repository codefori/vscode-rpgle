import Declaration from './language/models/declaration';
import { getPrettyType } from './language/models/fixed';

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
}