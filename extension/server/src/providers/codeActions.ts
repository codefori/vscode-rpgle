import { CodeAction, CodeActionKind, CodeActionParams, CreateFile, Position, Range, TextDocumentEdit, TextEdit, WorkspaceFolder } from 'vscode-languageserver';
import { TextDocument } from 'vscode-languageserver-textdocument';
import { documents, parser, prettyKeywords } from '.';
import Cache from '../../../../language/models/cache';
import { getLinterCodeActions } from './linter/codeActions';
import { createExtract, caseInsensitiveReplaceAll } from './language';
import { Keywords } from '../../../../language/parserTypes';
import path = require('path');
import { URI } from 'vscode-uri';
import { getWorkspaceFolder } from '../connection';
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
					return [extractOption];
				}

				const linterActions = await getLinterCodeActions(docs, document, range);
				if (linterActions) {
					actions = actions.concat(linterActions);
				}
			}

			const testActions = await getTestActions(document, docs, range);
			if (testActions) {
				actions.push(...testActions);
			}
		}
	}

	return actions;
}

export async function getTestActions(document: TextDocument, docs: Cache, range: Range): Promise<CodeAction[] | undefined> {
	const codeActions: CodeAction[] = [];

	const exportProcedures = docs.procedures.filter(proc => proc.keyword[`EXPORT`]);
	if (exportProcedures.length > 0) {
		const parsedPath = path.parse(document.uri);
		const fileName = parsedPath.base;
		const testFileName = `${parsedPath.name}.test${parsedPath.ext}`;
		const testFileUri = `${parsedPath.dir}/${testFileName}`;
		const workspaceFolder = await getWorkspaceFolder(document.uri);

		// Test case generation
		const currentProcedure = exportProcedures.find(sub => sub.range.start && sub.range.end && range.start.line >= sub.range.start && range.end.line <= sub.range.end);
		if (currentProcedure) {
			const newTestCase = generateTestCase(workspaceFolder, currentProcedure, docs.structs);
			const newTestSuite = generateTestSuite([...newTestCase.prototype, ``], newTestCase.testCase, newTestCase.includes);
			const testCaseAction = CodeAction.create(`Generate RPGUnit test case for '${currentProcedure.name}'`, CodeActionKind.RefactorExtract);
			testCaseAction.edit = {
				documentChanges: [
					CreateFile.create(testFileUri, { ignoreIfExists: true }),
					TextDocumentEdit.create({ uri: testFileUri, version: null }, [TextEdit.insert(Position.create(0, 0), newTestSuite.join(`\n`))])
				]
			};
			codeActions.push(testCaseAction);
		}

		// Test suite generation
		const newTestCases = exportProcedures.map(proc => generateTestCase(workspaceFolder, proc, docs.structs));
		const prototypes = newTestCases.map(tc => [...tc.prototype, ``]).flat();
		const testCases = newTestCases.map(tc => [...tc.testCase, ``]).flat();
		const includes = newTestCases.map(tc => tc.includes).flat();
		const newTestSuite = generateTestSuite(prototypes, testCases, includes);
		const testSuiteAction = CodeAction.create(`Generate RPGUnit test suite for '${fileName}'`, CodeActionKind.RefactorExtract);
		testSuiteAction.edit = {
			documentChanges: [
				CreateFile.create(testFileUri, { ignoreIfExists: true }),
				TextDocumentEdit.create({ uri: testFileUri, version: null }, [TextEdit.insert(Position.create(0, 0), newTestSuite.join(`\n`))])
			]
		};
		codeActions.push(testSuiteAction);
	}

	return codeActions;
}

function generateTestSuite(prototypes: string[], testCases: string[], includes: string[]) {
	const uniqueIncludes = [...new Set(includes)];

	return [
		`**free`,
		``,
		`ctl-opt nomain;`,
		``,
		...prototypes,
		...uniqueIncludes.map(include => `/include '${include}'`),
		`/include qinclude,TESTCASE`,
		``,
		...testCases
	]
}

function generateTestCase(workspaceFolder: WorkspaceFolder | undefined, procedure: Declaration, structs: Declaration[]) {
	const inputs = getInputs(workspaceFolder, procedure);
	const actualReturns = getReturns(workspaceFolder, structs, procedure, `actual`);
	const expectedReturns = getReturns(workspaceFolder, structs, procedure, `expected`);
	const includes = [...new Set([...inputs.includes, ...actualReturns.includes])];

	// TODO: Can we check if the prototype already exists?
	const prototype = [
		`dcl-pr ${procedure.name} ${prettyKeywords(procedure.keyword, true)} extproc('${procedure.name.toLocaleUpperCase()}');`,
		...procedure.subItems.map(s => `  ${s.name} ${prettyKeywords(s.keyword, true)};`),
		`end-pr;`,
	];

	const testCase = [
		`dcl-proc test_${procedure.name.toLowerCase()} export;`,
		`  dcl-pi *n extproc(*dclcase) end-pi;`,
		``,
		...inputs.declarations,
		...actualReturns.declarations,
		...expectedReturns.declarations,
		``,
		`  // Input`,
		...inputs.initializations,
		``,
		`  // Actual results`,
		`  actual = ${procedure.name}(${procedure.subItems.map(s => s.name).join(` : `)});`,
		``,
		`  // Expected results`,
		...expectedReturns.initializations,
		``,
		`  // Assertions`,
		...expectedReturns.assertions,
		`end-proc;`
	]

	return {
		prototype,
		testCase,
		includes
	};
}

function getInputs(workspaceFolder: WorkspaceFolder | undefined, currentProcedure: Declaration) {
	const declarations: string[] = [];
	const initializations: string[] = [];
	const includes: string[] = [];

	const inputs = currentProcedure.subItems;
	for (const input of inputs) {
		const type = getType(input.keyword);
		const declaration = type === `struct` ? 'dcl-ds' : 'dcl-s';
		declarations.push(`  ${declaration} ${input.name} ${prettyKeywords(input.keyword, true)};`);
		if (type === `struct`) {
			for (const subItem of input.subItems) {
				initializations.push(`  ${input.name}.${subItem.name} = ${getDefaultValue(getType(subItem.keyword))};`)

				const structPath = subItem.position.path;
				if (workspaceFolder) {
					const relativePath = asPosix(path.relative(workspaceFolder.uri, structPath));
					if (!includes.includes(relativePath)) {
						includes.push(relativePath);
					}
				}
			}
		} else {
			initializations.push(`  ${input.name} = ${getDefaultValue(type)};`);
		}
	}

	return {
		declarations,
		initializations,
		includes
	}
}

function getReturns(workspaceFolder: WorkspaceFolder | undefined, structs: Declaration[], currentProcedure: Declaration, name: 'actual' | 'expected') {
	const declarations: string[] = [];
	const initializations: string[] = [];
	const assertions: string[] = [];
	const includes: string[] = [];

	const type = getType(currentProcedure.keyword);
	const declaration = type === `struct` ? 'dcl-ds' : 'dcl-s';
	if (type === `struct`) {
		declarations.push(`  ${declaration} ${name} likeDS(${currentProcedure.keyword['LIKE']});`);
		const struct = structs.find(struct => struct.name === currentProcedure.keyword[`LIKE`]);
		if (struct) {
			for (const subItem of struct.subItems) {
				const subItemType = getType(subItem.keyword);
				const subItemDefaultValue = getDefaultValue(subItemType);
				initializations.push(`  ${name}.${subItem.name} = ${subItemDefaultValue};`)

				const structPath = subItem.position.path;
				if (workspaceFolder) {
					const relativePath = asPosix(path.relative(workspaceFolder.uri, structPath));
					if (!includes.includes(relativePath)) {
						includes.push(relativePath);
					}
				}

				const assertion = getAssertion(subItemType);
				if (assertion === `assert`) {
					assertions.push(`  ${assertion}(expected.${subItem.name} = actual.${subItem.name} : '${subItem.name}');`);
				} else {
					assertions.push(`  ${assertion}(expected.${subItem.name} : actual.${subItem.name} : '${subItem.name}');`);
				}
			}
		}
	} else {
		declarations.push(`  ${declaration} ${name} ${prettyKeywords(currentProcedure.keyword, true)};`);
		initializations.push(`  ${name} = ${getDefaultValue(type)};`);

		const assertion = getAssertion(type);
		if (assertion === `assert`) {
			assertions.push(`  ${assertion}(expected = actual);`);
		} else {
			assertions.push(`  ${assertion}(expected : actual);`);
		}
	}

	return {
		declarations,
		initializations,
		assertions,
		includes
	}
}

function getType(keywords: Keywords): string {
	let type = `unknown`;

	// TODO: Add all types
	if (keywords[`CHAR`] || keywords[`VARCHAR`]) {
		type = `string`;
	} else if (keywords[`INT`] || keywords[`UNS`]) {
		type = `int`;
	} else if (keywords[`PACKED`] || keywords[`ZONED`]) {
		type = `decimal`;
	} else if (keywords[`IND`]) {
		type = `boolean`;
	} else if (keywords[`DATE`]) {
		type = `date`;
	} else if (keywords[`TIME`]) {
		type = `boolean`;
	} else if (keywords[`TIMESTAMP`]) {
		type = `boolean`;
	} else if (keywords[`LIKEDS`] || keywords[`LIKE`]) {
		type = `struct`;
	}

	return type;
}

function getDefaultValue(type: string): string | undefined {
	let defaultValue = `unknown`;

	if (type === `string`) {
		defaultValue = `''`;
	} else if (type === `int`) {
		defaultValue = `0`;
	} else if (type === `decimal`) {
		defaultValue = `0`;
	} else if (type === `boolean`) {
		defaultValue = `*off`;
	} else if (type === `date`) {
		defaultValue = `'0001-01-01'`;
	} else if (type === `time`) {
		defaultValue = `'00:00:00'`;
	} else if (type === `timestamp`) {
		defaultValue = `'0001-01-01-00.00.00.000000'`;
	}

	return defaultValue;
}

function getAssertion(type: string): string {
	let assertion = `assert`;

	if (type === `string`) {
		assertion = `aEqual`;
	} else if (type === `int`) {
		assertion = `iEqual`;
	} else if (type === `boolean`) {
		assertion = `nEqual`;
	}

	return assertion;
}


function asPosix(inPath?: string) {
	return inPath ? inPath.split(path.sep).join(path.posix.sep) : ``;
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
				...extracted.references.map((ref, i) => `    ${extracted.newParamNames[i]} ${ref.dec.type === `struct` ? `likeDS` : `like`}(${ref.dec.name});`),
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
	if (range.end.line > range.start.line) {
		const lastLine = document.offsetAt({ line: document.lineCount, character: 0 });

		const extracted = createExtract(document, range, docs);

		const newProcedure = [
			`Dcl-Proc NewProcedure;`,
			`  Dcl-Pi *N;`,
			...extracted.references.map((ref, i) => `    ${extracted.newParamNames[i]} ${ref.dec.type === `struct` ? `likeDS` : `like`}(${ref.dec.name});`),
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