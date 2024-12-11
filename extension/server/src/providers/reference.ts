import { Location, Range, ReferenceParams } from 'vscode-languageserver';
import { documents, getWordRangeAtPosition, parser } from '.';
import Linter from '../../../../language/linter';
import { calculateOffset } from './linter';

import * as Project from "./project";
import { findAllProjectReferences as getAllProcedureReferences } from './project/references';
import Cache from '../../../../language/models/cache';

export async function referenceProvider(params: ReferenceParams): Promise<Location[]|undefined> {
	const uri = params.textDocument.uri;
	const position = params.position;
	const document = documents.get(uri);

	if (document) {
		const uri = params.textDocument.uri;
		const currentPos = params.position;
		const document = documents.get(uri);
	
		if (document) {
			const doc = await parser.getDocs(uri, document.getText());
	
			if (doc) {
				const def = Cache.referenceByOffset(uri, doc, document.offsetAt(currentPos));

				if (def) {
					let locations: Location[] = [];
					if (Project.isEnabled) {
						const procRefs = await getAllProcedureReferences(def);
						locations.push(...procRefs);
					}

					for (const ref of def.references) {
						let refDoc = documents.get(ref.uri);
						if (refDoc) {
							locations.push(Location.create(
								ref.uri,
								calculateOffset(refDoc, ref)
							));
						}
					}

					return locations;
				}
			}
		}
	}

	return;
}