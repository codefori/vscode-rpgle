import { TextDocument } from 'vscode-languageserver-textdocument';
import { Range } from "vscode-languageserver";
import Cache from "../../../../language/models/cache";

export function caseInsensitiveReplaceAll(text: string, search: string, replace: string) {
	return text.replace(new RegExp(search, `gi`), replace);
}

export function createExtract(document: TextDocument, userRange: Range, docs: Cache) {
	const range = Range.create(userRange.start.line, 0, userRange.end.line, 1000);
	const references = docs.referencesInRange(document.uri, {start: document.offsetAt(range.start), end: document.offsetAt(range.end)});
	const validRefs = references.filter(ref => [`struct`, `subitem`, `variable`].includes(ref.dec.type));

	const nameDiffSize = 1; // Always once since we only add 'p' at the start
	const newParamNames = validRefs.map(ref => `p${ref.dec.name}`);
	let newBody = document.getText(range);

	const rangeStartOffset = document.offsetAt(range.start);

	// Fix the found offset lengths to be relative to the new procedure
	for (let i = validRefs.length - 1; i >= 0; i--) {
		for (let y = validRefs[i].refs.length - 1; y >= 0; y--) {
			validRefs[i].refs[y] = {
				start: validRefs[i].refs[y].start - rangeStartOffset,
				end: validRefs[i].refs[y].end - rangeStartOffset
			};
		}
	}

	// Then let's fix the references to use the new names
	for (let i = validRefs.length - 1; i >= 0; i--) {
		for (let y = validRefs[i].refs.length - 1; y >= 0; y--) {
			const ref = validRefs[i].refs[y];

			newBody = newBody.slice(0, ref.start) + newParamNames[i] + newBody.slice(ref.end);
			ref.end += nameDiffSize;

			// Then we need to update the offset of the next references
			for (let z = i - 1; z >= 0; z--) {
				for (let x = validRefs[z].refs.length - 1; x >= 0; x--) {
					if (validRefs[z].refs[x].start > ref.end) {
						validRefs[z].refs[x] = {
							start: validRefs[z].refs[x].start + nameDiffSize,
							end: validRefs[z].refs[x].end + nameDiffSize
						};
					}
				}
			}
		}
	}

	return {
		newBody,
		newParamNames,
		references: validRefs,
		range
	}
}