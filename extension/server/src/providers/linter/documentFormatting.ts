
import { DocumentFormattingParams, ProgressToken, Range, TextEdit, WorkDoneProgress } from 'vscode-languageserver';
import { calculateOffset, getActions, getLintOptions } from '.';
import { documents, parser } from '..';
import Linter from '../../../../../language/ile/linter';
import { getFixableIndentationErrors } from '../../../../../language/ile/indentation';
import { getIndentationEdits } from './indentation';

export default async function documentFormattingProvider(params: DocumentFormattingParams): Promise<TextEdit[] | undefined> {
	const uri = params.textDocument.uri;
	const document = documents.get(uri);

	if (document) {
		const isFree = (document.getText(Range.create(0, 0, 0, 6)).toUpperCase() === `**FREE`);
		if (isFree) {
			let options = (await getLintOptions(document.uri));
			let docs = await parser.getDocs(document.uri);

			// If no lint config is provided, then set a default for indent
			if (Object.keys(options).length === 0) {
				options.indent = params.options.tabSize;
			}

			if (docs) {
				// Need to fetch the docs again incase comments were added
				// as part of RequiresProcedureDescription
				docs = await parser.getDocs(document.uri, document.getText(), {
					ignoreCache: true,
					withIncludes: true
				});

				// Next up, let's fix all the other things!
				const detail = Linter.getErrors({
					uri: document.uri,
					content: document.getText()
				}, options, docs);


				const actions = getActions(
					document,
					detail.errors.filter(error => error.type !== `RequiresProcedureDescription`)
				);

				let linesChanged: number[] = [];
				let skippedChanges: number = 0;

				let fixes: TextEdit[] = [];

				actions
					.filter(action => action.edit)
					.forEach(action => {
						if (action.edit && action.edit.changes) {
							const uris = action.edit.changes;
							const suggestedEdits = uris[document.uri];
							const editedLineBefore = suggestedEdits[0] ? linesChanged.includes(suggestedEdits[0].range.start.line) : false;
							if (!editedLineBefore) {
								suggestedEdits.forEach(edit => {
									const changedLine = edit.range.start.line;
									fixes.push(edit);

									if (!linesChanged.includes(changedLine)) {
										linesChanged.push(changedLine);
									}
								});
							} else {
								skippedChanges += 1;
							}
						}
					});


				const indentFixes = getIndentationEdits(document, getFixableIndentationErrors(detail));

				return [...fixes, ...indentFixes];
			}
		}
	}
	return [];
}
