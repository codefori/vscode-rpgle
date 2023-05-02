import path = require('path');
import { APIInterface } from '../apis';
import { isEnabled } from '.';
import { parser } from '..';

export function getInterfaces(): APIInterface[] {
	let interfaces: APIInterface[] = [];

	if (isEnabled) {
		const parsedFiles = Object.keys(parser.parsedCache);

		parsedFiles.forEach(uri => {
			const uriDetail = path.parse(uri);
			const basename = uriDetail.base;
			const nameDetail = path.parse(basename);
			const objectName = nameDetail.name.toUpperCase();

			if (basename.toLowerCase().endsWith(`.rpgleinc`) === false) {
				const cache = parser.getParsedCache(uri);

				if (cache) {
					const entryPoint = cache.keyword[`MAIN`];

					if (entryPoint) {
						// Okay, there MAIN keyword exists. It's a program for sure.
						if (typeof entryPoint === "string") {
							const entryFunction = cache.procedures.find(proc => proc.name.toUpperCase() === entryPoint.toUpperCase());

							if (entryFunction) {

								let keywords = entryFunction.keywords
									.map(keyword => keyword.toLowerCase());

								// We assume the file name is the name of the object
								keywords.push(`extpgm('${objectName}')`);

								const prototype = [
									`dcl-pr ${entryFunction.name} ${keywords.join(` `)};`,
									...entryFunction.subItems.map(subItem =>
										`  ${subItem.name} ${subItem.keywords.map(keyword => keyword.toLowerCase()).join(` `)};`
									),
									`end-pr;`
								];

								interfaces.push({
									name: objectName,
									insertText: `${objectName}(${entryFunction.subItems.map((parm, index) => `\${${index + 1}:${parm.name}}`).join(`:`)})`,
									detail: objectName,
									description: entryFunction.description,
									type: `function`,
									prototype
								});
							}
						}

					} else
					if (cache.keyword[`NOMAIN`]) {
						// This might mean it is a module. Look for EXPORTs
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
									insertText: `${proc.name}(${proc.subItems.map((parm, index) => `\${${index + 1}:${parm.name}}`).join(`:`)})`,
									detail: proc.name,
									description: proc.description,
									type: `function`,
									prototype
								});
							}
						});
					}
				}
			}
		})
	}

	return interfaces;
}