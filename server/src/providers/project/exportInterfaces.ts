import path = require('path');
import { APIInterface } from '../apis';
import { isEnabled } from '.';
import { parser } from '..';

export function getInterfaces(): APIInterface[] {
	let interfaces: APIInterface[] = [];

	if (isEnabled) {
		const parsedFiles = Object.keys(parser.parsedCache);

		parsedFiles.forEach(uri => {
			const basename = path.basename(uri);
			const baseNameLower = basename.toLowerCase();

			if (baseNameLower.endsWith(`.rpgleinc`) === false && baseNameLower.includes(`.pgm.`) === false) {
				const cache = parser.getParsedCache(uri);

				if (cache) {
					cache.procedures.forEach(proc => {
						if (proc.keyword[`EXPORT`]) {
							let keywords = proc.keywords
								.map(keyword => keyword.toLowerCase())
								.filter(keyword => keyword !== `export`);

							keywords.push(`extproc('${proc.name.toUpperCase()}')`);

							const prototype = [
								`dcl-pr ${proc.name} ${keywords.join(` `)};`,
								...proc.subItems.map(subItem => 
									`  ${subItem.name} ${subItem.keywords.map(keyword => keyword.toLowerCase()).join(` `)};`
								),
								`end-pr;`
							];

							interfaces.push({
								name: proc.name,
								insertText: proc.name,
								detail: `export function`,
								type: `function`,
								prototype
							});
						}
					});
				}
			}
		})
	}

	return interfaces;
}