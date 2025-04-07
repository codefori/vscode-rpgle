import { CodeAction, CodeActionKind, CodeActionParams, Position, Range, TextEdit } from 'vscode-languageserver';
import { TextDocument } from 'vscode-languageserver-textdocument';
import { documents, parser, prettyKeywords } from '..';
import Cache from '../../../../../language/models/cache';

export default async function genericCodeActionsProvider(params: CodeActionParams): Promise<CodeAction[]|undefined> {
	const uri = params.textDocument.uri;
	const range = params.range;
	const document = documents.get(uri);

	if (document) {
		const isFree = (document.getText(Range.create(0, 0, 0, 6)).toUpperCase() === `**FREE`);
		if (isFree) {
			const docs = await parser.getDocs(document.uri);

			if (docs) {
				const testCaseOption = getTestCaseAction(document, docs, range);
				if (testCaseOption) {
					return [testCaseOption];
				}
			}
		}
	}

	return;
}

export function getTestCaseAction(document: TextDocument, docs: Cache, range: Range): CodeAction|undefined {
	const currentProcedure = docs.procedures.find(sub => range.start.line >= sub.position.range.line && sub.range.start && sub.range.end);
	if (currentProcedure) {

		const refactorAction = CodeAction.create(`Create IBM i test case`, CodeActionKind.RefactorExtract);

		const preCall = currentProcedure.subItems.map(s => `dcl-s ${s.name} ${prettyKeywords(s.keyword)};`);

		refactorAction.edit = {
			changes: {
				['mynewtest.rpgle']: [
					TextEdit.insert(
						Position.create(0, 0), // Insert at the start of the new test case file
						[
							`**free`,
							``,
							`dcl-proc test_${currentProcedure.name.toLowerCase()} export;`,
							``,
							`end-proc;`
						].join(`\n`)
						
					)
				]
			},
		};

		return refactorAction;
	}
}