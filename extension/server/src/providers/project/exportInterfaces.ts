import path = require('path');
import { APIInterface } from '../apis';
import { isEnabled } from '.';
import { parser, prettyKeywords } from '..';

const TEST_FUNCTIONS = [`SETUPSUITE`, `TEARDOWNSUITE`, `SETUP`, `TEARDOWN`];

export function getInterfaces(): APIInterface[] {
	let interfaces: APIInterface[] = [];

	if (isEnabled) {
		const parsedFiles = Object.keys(parser.parsedCache);

		parsedFiles.forEach(uri => {
			const uriDetail = path.parse(uri);
			const basename = uriDetail.base;
			const nameDetail = path.parse(basename);
			let objectName = path.basename(nameDetail.name).toUpperCase();

			if (objectName.endsWith(`.PGM`)) objectName = objectName.substring(0, objectName.length-4);

			if (basename.toLowerCase().endsWith(`.rpgleinc`) === false) {
				const cache = parser.getParsedCache(uri);

				if (cache) {
					const entryPoint = cache.keyword[`MAIN`];

					if (entryPoint) {
						// Okay, there MAIN keyword exists. It's a program for sure.
						if (typeof entryPoint === "string") {
							const entryFunction = cache.procedures.find(proc => proc.name.toUpperCase() === entryPoint.toUpperCase());

							if (entryFunction) {

								// We assume the file name is the name of the object
								const useKeywords = {...entryFunction.keyword};
								useKeywords[`EXTPGM`] = `'${objectName}'`;

								const prototype = [
									`dcl-pr ${entryFunction.name} ${prettyKeywords(useKeywords, true)};`,
									...entryFunction.subItems.map(subItem =>
										`  ${subItem.name} ${prettyKeywords(subItem.keyword)};`
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

								if (TEST_FUNCTIONS.includes(proc.name.toUpperCase())) {
									return; // Skip test functions
								}

								if (proc.name.toUpperCase().startsWith(`TEST`)) {
									return; // Skip user test functions
								}

								const useKeywords = {...proc.keyword};
								useKeywords[`EXTPROC`] = `'${proc.name.toUpperCase()}'`;

								const prototype = [
									`dcl-pr ${proc.name} ${prettyKeywords(useKeywords, true)};`,
									...proc.subItems.map(subItem =>
										`  ${subItem.name} ${prettyKeywords(subItem.keyword)};`
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