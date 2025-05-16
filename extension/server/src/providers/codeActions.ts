import { CodeAction, CodeActionKind, CodeActionParams, CreateFile, Position, Range, TextDocumentEdit, TextEdit, WorkspaceFolder } from 'vscode-languageserver';
import { TextDocument } from 'vscode-languageserver-textdocument';
import { documents, parser, prettyKeywords } from '.';
import Cache, { RpgleTypeDetail, RpgleVariableType } from '../../../../language/models/cache';
import { getLinterCodeActions } from './linter/codeActions';
import { createExtract, caseInsensitiveReplaceAll } from './language';
import { Keywords } from '../../../../language/parserTypes';
import path = require('path');
import { getWorkspaceFolder } from '../connection';
import Declaration from '../../../../language/models/declaration';

interface TestCaseSpec {
	prototype: string[];
	testCase: string[];
	includes: string[];
}

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
		const workspaceFolder = await getWorkspaceFolder(document.uri); // TODO: Can workspace folder not be a requirement?
		if (workspaceFolder) {
			// Build new test file uri
			const parsedPath = path.parse(document.uri);
			const fileName = parsedPath.base;
			const testFileName = `${parsedPath.name}.test${parsedPath.ext}`;
			const testFileUri = workspaceFolder ?
				`${workspaceFolder.uri}/qtestsrc/${testFileName}` :
				`${parsedPath.dir}/${testFileName}`;

			// Test case generation
			const currentProcedure = exportProcedures.find(sub => sub.range.start && sub.range.end && range.start.line >= sub.range.start && range.end.line <= sub.range.end);
			if (currentProcedure) {
				const testCaseSpec = await getTestCaseSpec(docs, currentProcedure, workspaceFolder);
				const newTestSuite = generateTestSuite([testCaseSpec]);
				const testCaseAction = CodeAction.create(`Generate test case for '${currentProcedure.name}'`, CodeActionKind.RefactorExtract);
				testCaseAction.edit = {
					documentChanges: [
						CreateFile.create(testFileUri, { ignoreIfExists: true }),
						TextDocumentEdit.create({ uri: testFileUri, version: null }, [TextEdit.insert(Position.create(0, 0), newTestSuite.join(`\n`))])
					]
				};
				codeActions.push(testCaseAction);
			}

			// Test suite generation
			const newTestCases = await Promise.all(exportProcedures.map(async proc => await getTestCaseSpec(docs, proc, workspaceFolder)));
			const newTestSuite = generateTestSuite(newTestCases);
			const testSuiteAction = CodeAction.create(`Generate test suite for '${fileName}'`, CodeActionKind.RefactorExtract);
			testSuiteAction.edit = {
				documentChanges: [
					CreateFile.create(testFileUri, { ignoreIfExists: true }),
					TextDocumentEdit.create({ uri: testFileUri, version: null }, [TextEdit.insert(Position.create(0, 0), newTestSuite.join(`\n`))])
				]
			};
			codeActions.push(testSuiteAction);
		}
	}

	return codeActions;
}


function generateTestSuite(testCaseSpecs: TestCaseSpec[]) {
	const prototypes = testCaseSpecs.map(tc => tc.prototype.length > 0 ? [``, ...tc.prototype] : tc.prototype).flat();
	const testCases = testCaseSpecs.map(tc => tc.testCase.length > 0 ? [``, ...tc.testCase] : tc.testCase).flat();
	const allIncludes = testCaseSpecs.map(tc => tc.includes).flat();
	const uniqueIncludes = [...new Set(allIncludes)];

	return [
		`**free`,
		``,
		`ctl-opt nomain;`,
		...prototypes,
		``,
		`/include qinclude,TESTCASE`,
		...uniqueIncludes,
		...testCases
	]
}

async function getTestCaseSpec(docs: Cache, procedure: Declaration, workspaceFolder: WorkspaceFolder): Promise<TestCaseSpec> {
	// Get procedure prototype
	const prototype = await getPrototype(procedure);

	// Get inputs
	const inputDecs: string[] = [];
	const inputInits: string[] = [];
	const inputIncludes: string[] = [];
	for (const subItem of procedure.subItems) {
		const subItemType = docs.resolveType(subItem);

		const subItemDec = getDeclaration(subItemType, `${subItem.name}`);
		inputDecs.push(...subItemDec);

		const subItemInits = getInitializations(docs, subItemType, `${subItem.name}`);
		inputInits.push(...subItemInits);

		const subItemIncludes = getIncludes(subItemType, workspaceFolder);
		inputIncludes.push(...subItemIncludes);
	}

	// Get return
	const resolvedType = docs.resolveType(procedure);
	const actualDec = getDeclaration(resolvedType, 'actual');
	const expectedDec = getDeclaration(resolvedType, 'expected');
	const expectedInits = getInitializations(docs, resolvedType, 'expected');
	const returnIncludes = getIncludes(resolvedType, workspaceFolder);

	// Get unique includes
	const includes = [...new Set([...inputIncludes, ...returnIncludes])];

	// Get assertions
	const assertions = getAssertions(docs, resolvedType, 'expected', 'actual');

	const testCase = [
		`dcl-proc test_${procedure.name} export;`,
		`  dcl-pi *n extproc(*dclcase) end-pi;`,
		``,
		...inputDecs.map(dec => `  ${dec}`),
		...actualDec.map(dec => `  ${dec}`),
		...expectedDec.map(dec => `  ${dec}`),
		``,
		`  // Input`,
		...inputInits.map(init => `  ${init}`),
		``,
		`  // Actual results`,
		`  actual = ${procedure.name}(${procedure.subItems.map(s => s.name).join(` : `)});`,
		``,
		`  // Expected results`,
		...expectedInits.map(init => `  ${init}`),
		``,
		`  // Assertions`,
		...assertions.map(assert => `  ${assert}`),
		`end-proc;`
	];

	return {
		prototype,
		testCase,
		includes
	};
}

function getDeclaration(detail: RpgleTypeDetail, name: string): string[] {
	const declarations: string[] = [];

	if (detail) {
		if (detail.type) {
			declarations.push(`dcl-s ${name} ${detail.type.name}${detail.type.value ? `(${detail.type.value})` : ``};`);
		} else if (detail.reference) {
			declarations.push(`dcl-ds ${name} likeDs(${detail.reference.name});`);
		}
	}

	return declarations;
}

function getInitializations(docs: Cache, detail: RpgleTypeDetail, name: string): string[] {
	const inits: string[] = [];

	if (detail) {
		if (detail.type) {
			const defaultValue = getDefaultValue(detail.type.name);
			inits.push(`${name} = ${defaultValue};`);
		} else if (detail.reference) {
			for (const subItem of detail.reference.subItems) {
				const subItemType = docs.resolveType(subItem);
				const subItemInits = subItemType ?
					getInitializations(docs, subItemType, `${name}.${subItem.name}`) : [];
				inits.push(...subItemInits);
			}
		}
	}

	return inits;
}

async function getPrototype(procedure: Declaration): Promise<string[]> {
	for (const reference of procedure.references) {
		const docs = await parser.getDocs(reference.uri);
		if (docs) {
			const prototype = docs.procedures.some(proc => proc.name === procedure.name && proc.keyword['EXTPROC'])
			if (prototype) {
				return [];
			}
		}
	}

	return [
		`dcl-pr ${procedure.name} ${prettyKeywords(procedure.keyword, true)} extproc('${procedure.name.toLocaleUpperCase()}');`,
		...procedure.subItems.map(s => `  ${s.name} ${prettyKeywords(s.keyword, true)};`),
		`end-pr;`
	];
}

function getIncludes(detail: RpgleTypeDetail, workspaceFolder: WorkspaceFolder): string[] {
	const includes: string[] = [];

	if (detail.reference) {
		const structPath = detail.reference.position.path;
		if (workspaceFolder) {
			const relativePath = asPosix(path.relative(workspaceFolder.uri, structPath));
			if (!includes.includes(relativePath)) {
				includes.push(`/include ${relativePath}`);
			}
		}
	}

	return includes;
}

function getAssertions(docs: Cache, detail: RpgleTypeDetail, expected: string, actual: string): string[] {
	const assertions: string[] = [];

	if (detail) {
		if (detail.type) {
			const assertion = getAssertion(detail.type.name);
			const fieldName = actual.split(`.`).pop();
			if (assertion === `assert`) {
				assertions.push(`${assertion}(${expected} = ${actual}${fieldName ? ` : '${fieldName}'` : ``});`);
			} else {
				assertions.push(`${assertion}(${expected} : ${actual}${fieldName ? ` : '${fieldName}'` : ``});`);
			}
		} else if (detail.reference) {
			for (const subItem of detail.reference.subItems) {
				const subItemType = docs.resolveType(subItem);
				const subItemAssertions = subItemType ?
					getAssertions(docs, subItemType, `${expected}.${subItem.name}`, `${actual}.${subItem.name}`) : [];
				assertions.push(...subItemAssertions);
			}
		}
	}

	return assertions;
}

function getDefaultValue(type: RpgleVariableType): string {
	switch (type) {
		case `char`:
		case `varchar`:
			return `''`;
		case `int`:
		case `uns`:
			return `0`;
		case `packed`:
		case `zoned`:
			return `0.0`;
		case `ind`:
			return `*off`;
		case `date`:
			return `%date('0001-01-01' : *iso)`;
		case `time`:
			return `%time('00.00.00' : *iso)`;
		case `timestamp`:
			return `%timestamp('0001-01-01-00.00.00.000000' : *iso)`;
		case `pointer`:
			return `*null`;
		default:
			return 'unknown';
	}
}

function getAssertion(type: RpgleVariableType): string {
	switch (type) {
		case `char`:
		case `varchar`:
			return `aEqual`;
		case `int`:
		case `uns`:
			return `iEqual`;
		case `packed`:
		case `zoned`:
			return `assert`;
		case `ind`:
			return `nEqual`;
		case `date`:
			return `assert`;
		case `time`:
			return `assert`;
		case `timestamp`:
			return `assert`;
		case `pointer`:
			return `assert`;
		default:
			return 'unknown';
	}
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