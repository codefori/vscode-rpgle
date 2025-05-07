import { CodeAction, CodeActionKind, CodeActionParams, CreateFile, Position, Range, TextEdit } from 'vscode-languageserver';
import { TextDocument } from 'vscode-languageserver-textdocument';
import { documents, parser, prettyKeywords } from '.';
import Cache from '../../../../language/models/cache';
import { getLinterCodeActions } from './linter/codeActions';
import { createExtract, caseInsensitiveReplaceAll } from './language';
import { Keywords } from '../../../../language/parserTypes';
import path = require('path');
import { URI } from 'vscode-uri';

export default async function genericCodeActionsProvider(params: CodeActionParams): Promise<CodeAction[] | undefined> {
	const uri = params.textDocument.uri;
	const range = params.range;
	const document = documents.get(uri);

	let actions: CodeAction[] = [];

	if (document) {
		const docs = await parser.getDocs(document.uri);
		if (docs) {
			const isFree = (document.getText(Range.create(0, 0, 0, 6)).toUpperCase() === `**FREE`);
			if (isFree) {
				const subroutineOption = getSubroutineActions(document, docs, range);
				if (subroutineOption) {
					return [subroutineOption];
				}

				const extractOption = getExtractProcedureAction(document, docs, range);
				if (extractOption) {
					return [extractOption];
				}

				const linterActions = await getLinterCodeActions(docs, document, range);
				if (linterActions) {
					actions = actions.concat(linterActions);
				}
			}

			const testCaseOption = getTestCaseAction(document, docs, range);
			if (testCaseOption) {
				actions.push(testCaseOption);
			}
		}
	}

	return actions;
}

export function getTestCaseAction(document: TextDocument, docs: Cache, range: Range): CodeAction | undefined {
	const currentProcedure = docs.procedures.find(sub => range.start.line >= sub.position.range.line && sub.range.start && sub.range.end && sub.keyword[`EXPORT`]);
	if (currentProcedure) {

		const refactorAction = CodeAction.create(`Create IBM i test case`, CodeActionKind.RefactorExtract);
		const parsedName = path.parse(document.uri);
		const newFileName = `${parsedName.name}.test${parsedName.ext}`;

		refactorAction.edit = {
			documentChanges: [
				CreateFile.create(`GEBERATE URI here`, {ignoreIfExists: true})
			],
			changes: {
				// should be .test.rpgle
				// basename should be the same as the current file

				// TODO: does the file already exist?
				// TODO: how does it handle different file systems?

				[`abcd.rpgle`]: [
					TextEdit.insert(
						Position.create(0, 0), // Insert at the start of the new test case file
						[
							`**free`,
							``,
							`dcl-proc test_${currentProcedure.name.toLowerCase()} export;`,
							``,
							`  dcl-pr ${currentProcedure.name} extproc;`,
							...currentProcedure.subItems.map(s => `  ${s.name} ${prettyKeywords(s.keyword)}`),
							`  end-pr;`,
							``,
							// TODO: what if parameter is a DS?
							...currentProcedure.subItems.map(s => `  // dcl-s ${s.name} ${prettyKeywords(s.keyword)};`),
							``,
							`  // ${currentProcedure.name}(${currentProcedure.subItems.map(s => s.name).join(`:`)});`,
							``,
							...currentProcedure.subItems.map(s => `  // aEquals(${s.name} = '');`),
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

function determineType(keywords: Keywords) {
	let type = `unknown`;

	if (keywords[`CHAR`] || keywords[`VARCHAR`]) {
		type = `string`;
	}

	return type;
}

export function getSubroutineActions(document: TextDocument, docs: Cache, range: Range): CodeAction|undefined {
	if (range.start.line === range.end.line) {
		const currentGlobalSubroutine = docs.subroutines.find(sub => sub.position.range.line === range.start.line && sub.range.start && sub.range.end);

		if (currentGlobalSubroutine) {
			const subroutineRange = Range.create(
				Position.create(currentGlobalSubroutine.range.start!, 0),
				Position.create(currentGlobalSubroutine.range.end!, 1000)
			);

			const bodyRange = Range.create(
				Position.create(currentGlobalSubroutine.range.start! + 1, 0),
				Position.create(currentGlobalSubroutine.range.end! - 1, 0)
			);

			// First, let's create the extract data
			const extracted = createExtract(document, bodyRange, docs);

			// Create the new procedure body
			const newProcedure = [
				`Dcl-Proc ${currentGlobalSubroutine.name};`,
				`  Dcl-Pi *N;`,
					  ...extracted.references.map((ref, i) => `    ${extracted.newParamNames[i]} ${ref.dec.type === `struct` ? `LikeDS` : `Like`}(${ref.dec.name});`),
				`  End-Pi;`,
				``,
				caseInsensitiveReplaceAll(extracted.newBody, `leavesr`, `return`),
				`End-Proc;`
			].join(`\n`)

			// Then update the references that invokes this subroutine
			const referenceUpdates: TextEdit[] = currentGlobalSubroutine.references.map(ref => {
				const lineNumber = document.positionAt(ref.offset.start).line;
				// If this reference is outside of the subroutine
				if (lineNumber < currentGlobalSubroutine.range.start! || lineNumber > currentGlobalSubroutine.range.end!) {
					return TextEdit.replace(
						Range.create(
							// - 5 `EXSR `
							document.positionAt(ref.offset.start - 5),
							document.positionAt(ref.offset.end)
						),
						currentGlobalSubroutine.name + `(${extracted.references.map(r => r.dec.name).join(`:`)})`
					);
				}
			}).map(x => x) as TextEdit[];

			const refactorAction = CodeAction.create(`Convert to procedure`, CodeActionKind.RefactorExtract);
			refactorAction.edit = {
				changes: {
					[document.uri]: [
						...referenceUpdates,
						TextEdit.replace(subroutineRange, newProcedure)
					]
				},
			};

			return refactorAction;
		}
	}
}

export function getExtractProcedureAction(document: TextDocument, docs: Cache, range: Range): CodeAction|undefined {
	if (range.end.line > range.start.line) {
			const lastLine = document.offsetAt({line: document.lineCount, character: 0});

			const extracted = createExtract(document, range, docs);

			const newProcedure = [
				`Dcl-Proc NewProcedure;`,
				`  Dcl-Pi *N;`,
					  ...extracted.references.map((ref, i) => `    ${extracted.newParamNames[i]} ${ref.dec.type === `struct` ? `LikeDS` : `Like`}(${ref.dec.name});`),
				`  End-Pi;`,
				``,
				extracted.newBody,
				`End-Proc;`
			].join(`\n`)

			const newAction = CodeAction.create(`Extract to new procedure`, CodeActionKind.RefactorExtract);

			// First do the exit
			newAction.edit = {
				changes: {
					[document.uri]: [
						TextEdit.replace(extracted.range, `NewProcedure(${extracted.references.map(r => r.dec.name).join(`:`)});`),
						TextEdit.insert(document.positionAt(lastLine), `\n\n`+newProcedure)
					]
				},
			};

			// Then format the document
			newAction.command = {
				command: `editor.action.formatDocument`,
				title: `Format`
			};

			return newAction;
	}
}