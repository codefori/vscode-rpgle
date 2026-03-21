import { CodeAction, CodeActionKind, CodeActionParams, Position, Range, TextEdit } from 'vscode-languageserver';
import { TextDocument } from 'vscode-languageserver-textdocument';
import { documents, parser } from '.';
import Cache from '../../../../language/models/cache';
import { getLinterCodeActions } from './linter/codeActions';
import { createExtract, caseInsensitiveReplaceAll } from './language';
import Declaration from '../../../../language/models/declaration';

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
					actions.push(extractOption);
				}

				const exportedProcedures = docs.procedures.filter(proc => proc.keyword[`EXPORT`]);
				if (exportedProcedures.length > 0) {
					const generateOption = getGenerateBinderSourceAction(exportedProcedures);
					actions.push(generateOption);
				}

				const linterActions = await getLinterCodeActions(docs, document, range);
				if (linterActions) {
					actions = actions.concat(linterActions);
				}
			}

			const monitorAction = surroundWithMonitorAction(isFree, document, docs, range);
			if (monitorAction) {
				actions.push(monitorAction);
			}
		}
	}

	return actions;
}

function lineAt(document: TextDocument, line: number): string {
	return document.getText(Range.create(line, 0, line, 1000)).trimEnd();
}

function getBodyRangeForRange(docs: Cache, rangeStart: number, rangeEnd: number, document: TextDocument) {
	let validStart = -1;
	let validEnd = -1;

	const currentProcedure = docs.procedures.find(sub => rangeStart >= sub.position.range.line && rangeEnd <= sub.range.end!);

	if (currentProcedure && currentProcedure.scope && currentProcedure.range.end) {
		validStart = currentProcedure.scope.getDefinitionBlockEnd(document.uri) + 1;
		validEnd = currentProcedure.range.end - 1;
	} else {
		validStart = docs.getDefinitionBlockEnd(document.uri) + 1;
		const firstProc = docs.procedures.find(p => !Object.keys(p.keyword).some(k => k.toLowerCase().startsWith(`ext`)));
		validEnd = firstProc && firstProc.range.start ? firstProc.range.start - 1 : document.lineCount - 1;
	}

	if (validStart < 0 || validEnd < 0) {
		return;
	}

	return { validStart, validEnd };
}

function determineIndent(line: string): number {
	const match = line.match(/^\s+/);
	if (match) {
		return match[0].length;
	}
	return 0;
}

export function surroundWithMonitorAction(isFree: boolean, document: TextDocument, docs: Cache, range: Range): CodeAction | undefined {
	let rangeStart = range.start.line;
	let rangeEnd = range.end.line;

	if (rangeStart === rangeEnd) {
		// Only works for multi-line selections
		return;
	}

	const validRange = getBodyRangeForRange(docs, rangeStart, rangeEnd, document);

	if (!validRange) {
		return;
	}

	let indent = 0;

	if (isFree) {
		indent = determineIndent(lineAt(document, rangeStart));
	} else {
		const freePortion = lineAt(document, rangeStart).substring(7);
		indent = determineIndent(freePortion) + 7;
	}

	const newLine = `\n`;
	const indentStr = ` `.repeat(indent);
	const space = `${newLine}${indentStr}`;

	if (rangeStart >= validRange.validStart && rangeEnd <= validRange.validEnd) {
		const refactorAction = CodeAction.create(`Surround with monitor`, CodeActionKind.RefactorRewrite);

		refactorAction.edit = {
			changes: {
				[document.uri]: [
					TextEdit.insert(
						Position.create(rangeStart, 0),
						`${indentStr}monitor;${newLine}`
					),
					TextEdit.insert(
						Position.create(rangeEnd + 1, 0),
						`${space}on-error *all;${space}  // TODO: implement${space}endmon;${newLine}`
					)
				]
			},
		};

		if (isFree) {
			// Then format the document
			refactorAction.command = {
				command: `editor.action.formatDocument`,
				title: `Format`
			};
		}

		return refactorAction;
	}


	return;
}

export function getSubroutineActions(document: TextDocument, docs: Cache, range: Range): CodeAction | undefined {
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

export function getExtractProcedureAction(document: TextDocument, docs: Cache, range: Range): CodeAction | undefined {
	const rangeStart = range.start.line;
	const rangeEnd = range.end.line;
	if (rangeEnd > rangeStart) {

		const validRange = getBodyRangeForRange(docs, rangeStart, rangeEnd, document);

		if (!validRange) {
			return;
		}

		// Ensure the selected range is within a body range
		if (rangeStart >= validRange.validStart && rangeEnd <= validRange.validEnd) {
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
			const lastLine = document.offsetAt({ line: document.lineCount, character: 0 });

			// First do the exit
			newAction.edit = {
				changes: {
					[document.uri]: [
						TextEdit.replace(extracted.range, `NewProcedure(${extracted.references.map(r => r.dec.name).join(`:`)});`),
						TextEdit.insert(document.positionAt(lastLine), `\n\n` + newProcedure)
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
}

export function getGenerateBinderSourceAction(exportedProcedures: Declaration[]): CodeAction {
	const exportSymbols = exportedProcedures.map(proc => `  EXPORT SYMBOL('${proc.name.toUpperCase()}')`);

	const binderSource = [
		`STRPGMEXP PGMLVL(*CURRENT) SIGNATURE('V1')`,
		...exportSymbols,
		`ENDPGMEXP`
	];

	const newAction = CodeAction.create(`Generate binder source`, CodeActionKind.RefactorExtract);
	newAction.command = {
		title: 'Generate Binder Source',
		command: 'vscode-rpgle.generateBinderSource',
		arguments: [
			binderSource.join('\n')
		]
	};

	return newAction;
}