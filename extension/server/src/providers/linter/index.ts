import path = require('path');
import { CodeAction, CodeActionKind, Diagnostic, DiagnosticSeverity, DidChangeWatchedFilesParams, DidOpenTextDocumentParams, DidSaveTextDocumentParams, Position, Range, TextDocumentChangeEvent, TextEdit, WorkspaceEdit, _Connection } from 'vscode-languageserver';
import { TextDocument } from 'vscode-languageserver-textdocument';
import { URI } from 'vscode-uri';
import { documents, parser } from '..';
import { IssueRange, Rules } from '../../../../../language/parserTypes';
import Linter from '../../../../../language/linter';
import Cache from '../../../../../language/models/cache';
import codeActionsProvider from './codeActions';
import documentFormattingProvider from './documentFormatting';

import * as Project from "../project";
import { connection, getFileRequest, getWorkingDirectory, resolvedMembers, resolvedStreamfiles, validateUri, watchedFilesChangeEvent } from '../../connection';
import { parseMemberUri } from '../../data';

export let jsonCache: { [uri: string]: string } = {};

export function initialise(connection: _Connection) {
	connection.onCodeAction(codeActionsProvider);
	connection.onDocumentFormatting(documentFormattingProvider);

	// This only works for local workspace files
	watchedFilesChangeEvent.push((params: DidChangeWatchedFilesParams) => {
		let runLinter = false;

		params.changes.forEach(file => {
			const uri = file.uri;
			const lowerUri = uri.toLowerCase();
			const basename = path.basename(lowerUri);

			if (basename === `rpglint.json`) {
				const validKey = Object.keys(jsonCache).find(key => key.toLowerCase() === lowerUri);
				if (validKey && jsonCache[validKey]) {
					delete jsonCache[validKey];
				}

				boundLintConfig = {};

				runLinter = true;
			}
		});

		if (runLinter) {
			documents.all().forEach(document => {
				if (document.languageId === `rpgle`) {
					parser.getDocs(
						document.uri,
						document.getText(),
						{
							withIncludes: true,
							ignoreCache: true,
							collectReferences: true
						}
					).then(cache => {
						if (cache) {
							refreshLinterDiagnostics(document, cache);
						}
					});
				}
			});
		}
	});

	documents.onDidOpen(async e => {
		const uriString = e.document.uri;

		const uri = URI.parse(uriString);

		// If we open a new RPGLE file that is remote
		// we need to refresh the lint config so we can 
		// make sure it's the latest.
		if ([`member`, `streamfile`].includes(uri.scheme)) {
			boundLintConfig = {};
			jsonCache = {}
		}
	})

	documents.onDidClose(async e => {
		const uriString = e.document.uri;
		resolvedMembers[uriString] = {};
		resolvedStreamfiles[uriString] = {};
	})
}

export function calculateOffset(document: TextDocument, error: IssueRange) {
	const offset = error.offset;
	
	return Range.create(
		document.positionAt(error.offset.position),
		document.positionAt(error.offset.end)
	);
};

enum ResolvedState {
	Found,
	NotFound
};

let boundLintConfig: {[workingUri: string]: {resolved: ResolvedState, uri: string}} = {};

export async function getLintConfigUri(workingUri: string) {
	const uri = URI.parse(workingUri);
	let cleanString: string | undefined;

	const cached = boundLintConfig[workingUri];

	if (cached) {
		return cached.resolved === ResolvedState.Found ? cached.uri : undefined;
	}

	switch (uri.scheme) {
		case `member`:
			const memberPath = parseMemberUri(uri.path);
			cleanString = [
				``,
				memberPath.library,
				`VSCODE`,
				`RPGLINT.JSON`
			].join(`/`);

			cleanString = URI.from({
				scheme: `member`,
				path: cleanString
			}).toString();

			if (jsonCache[cleanString]) return cleanString;
			cleanString = await validateUri(cleanString);
			break;
		
		case `streamfile`:
			const workingDir = await getWorkingDirectory();
			if (workingDir) {
				cleanString = URI.from({
					scheme: `streamfile`,
					path: path.posix.join(workingDir, `.vscode`, `rpglint.json`)
				}).toString();
				
				cleanString = await validateUri(cleanString, uri.scheme);
			}
			break;

		case `file`:
			cleanString = await validateUri(`rpglint.json`, uri.scheme);
			break;
	}

	if (cleanString) {
		boundLintConfig[workingUri] = {
			resolved: ResolvedState.Found,
			uri: cleanString
		}
	} else {
		boundLintConfig[workingUri] = {
			resolved: ResolvedState.NotFound,
			uri: ``
		};
	}

	return cleanString;
}

export async function getLintOptions(workingUri: string): Promise<Rules> {
	const possibleUri = await getLintConfigUri(workingUri);
	let result = {};

	if (possibleUri) {
		if (jsonCache[possibleUri]) return JSON.parse(jsonCache[possibleUri]);
		try {
			jsonCache[possibleUri] = `{}`;
			const fileContent = await getFileRequest(possibleUri);
			if (fileContent) {
				result = JSON.parse(fileContent);
				jsonCache[possibleUri] = fileContent;
			}
		} catch (e: any) {
			delete jsonCache[possibleUri];
			// Maybe some default options?
			console.log(`Error getting lint config for ${possibleUri}: ${e.message}`);
			console.log(e.stack);
		}
	}

	return result;
}

const hintDiagnositcs: (keyof Rules)[] = [`SQLRunner`, `StringLiteralDupe`]

export async function refreshLinterDiagnostics(document: TextDocument, docs: Cache, updateDiagnostics = true) {
	const isFree = (document.getText(Range.create(0, 0, 0, 6)).toUpperCase() === `**FREE`);
	if (isFree) {
		const text = document.getText();

		const indentDiags: Diagnostic[] = [];
		const generalDiags: Diagnostic[] = [];

		const options: Rules = await getLintOptions(document.uri);

		let detail;

		let availableIncludes: string[] | undefined;
		if (Project.isEnabled) {
			const headers = await Project.getIncludes(document.uri);
			availableIncludes = headers.map(header => header.relative);
		}

		// Turn on for SQLRunner suggestions
		options.SQLRunner = true;
		
		options.StringLiteralDupe = true;

		try {
			detail = Linter.getErrors({
				uri: document.uri,
				content: text,
				availableIncludes
			}, options, docs);
		} catch (e: any) {
			console.log(`Error linting ${document.uri}: ${e.message}`);
			console.log(e.stack);

			return;
		}

		const indentErrors = detail.indentErrors;
		const errors = detail.errors;

		if (indentErrors.length > 0) {
			indentErrors.forEach(error => {
				const range = Range.create(error.line, 0, error.line, error.currentIndent);

				indentDiags.push(Diagnostic.create(
					range,
					`Incorrect indentation. Expected ${error.expectedIndent}, got ${error.currentIndent}`,
					DiagnosticSeverity.Warning
				));
			});
		}

		if (errors.length > 0) {
			errors.forEach(error => {
				const range = calculateOffset(document, error);

				const diagnostic = Diagnostic.create(
					range,
					Linter.getErrorText(error.type),
					error.type && hintDiagnositcs.includes(error.type) ? DiagnosticSeverity.Hint : DiagnosticSeverity.Warning
				);

				generalDiags.push(diagnostic);
			});
		}

		if (updateDiagnostics) {
			connection.sendDiagnostics({ uri: document.uri, diagnostics: [...indentDiags, ...generalDiags] });
		}
		return detail;
	}
}

export function getActions(document: TextDocument, errors: IssueRange[]) {
	let actions: CodeAction[] = [];

	// We need to move subroutine to the end and reverse the contents
	const NoGlobalSubroutines = errors.filter(e => e.type === `NoGlobalSubroutines`);

	// Then remove them from the error list
	errors = errors.filter(e => e.type !== `NoGlobalSubroutines`);

	// Before reversing an adding them back
	NoGlobalSubroutines.reverse();
	errors.push(...NoGlobalSubroutines);

	errors.forEach(error => {
		let action;
		let errorRange = calculateOffset(document, error);

		switch (error.type) {
			case `UppercaseConstants`:
				if (error.newValue) {
					action = CodeAction.create(`Convert constant name to uppercase`, CodeActionKind.QuickFix);
					action.edit = {
						changes: {
							[document.uri]: [TextEdit.replace(errorRange, error.newValue)]
						},
					}
					actions.push(action);
				}
				break;

			case `ForceOptionalParens`:
				action = CodeAction.create(`Add brackets around expression`, CodeActionKind.QuickFix);
				action.edit = {
					changes: {
						[document.uri]: [
							TextEdit.insert(errorRange.end, `)`),
							TextEdit.insert(errorRange.start, `(`),
						]
					},
				}
				actions.push(action);
				break;

			case `UselessOperationCheck`:
				action = CodeAction.create(`Remove operation code`, CodeActionKind.QuickFix);
				action.edit = {
					changes: {
						[document.uri]: [
							TextEdit.del(errorRange),
						]
					},
				}
				actions.push(action);
				break;

			case `SpecificCasing`:
			case `IncorrectVariableCase`:
			case `DirectiveCase`:
				if (error.newValue) {
					action = CodeAction.create(`Correct casing to '${error.newValue}'`, CodeActionKind.QuickFix);
					action.edit = {
						changes: {
							[document.uri]: [
								TextEdit.replace(errorRange, error.newValue)
							]
						},
					}
					actions.push(action);
				}
				break;

			case `RequiresProcedureDescription`:
				action = CodeAction.create(`Add title and description`, CodeActionKind.QuickFix);
				action.edit = {
					changes: {
						[document.uri]: [
							TextEdit.insert(errorRange.start, `///\n// Title\n// Description\n///\n`)
						]
					},
				}
				actions.push(action);
				break;

			case `RequireBlankSpecial`:
				if (error.newValue) {
					action = CodeAction.create(`Convert empty string literal to *BLANK`, CodeActionKind.QuickFix);
					action.edit = {
						changes: {
							[document.uri]: [
								TextEdit.replace(errorRange, error.newValue)
							]
						},
					}
					actions.push(action);
				}
				break;

			case `SQLHostVarCheck`:
			case `CopybookDirective`:
			case `NoGlobalSubroutines`:
				if (error.newValue) {
					action = CodeAction.create(`Switch to '${error.newValue}'`, CodeActionKind.QuickFix);
					action.edit = {
						changes: {
							[document.uri]: [
								TextEdit.replace(errorRange, error.newValue)
							]
						},
					}
					actions.push(action);
				}
				break;

			case `StringLiteralDupe`:
				if (error.newValue) {
					action = CodeAction.create(`Switch to '${error.newValue}'`, CodeActionKind.RefactorExtract);
					action.edit = {
						changes: {
							[document.uri]: [
								TextEdit.replace(errorRange, error.newValue)
							]
						},
					}
					actions.push(action);
				}
				break;

			case `IncludeMustBeRelative`:
				if (error.newValue) {
					action = CodeAction.create(`Correct path to ${error.newValue}`, CodeActionKind.QuickFix);
					action.edit = {
						changes: {
							[document.uri]: [
								TextEdit.replace(errorRange, error.newValue)
							]
						},
					}
					actions.push(action);
				}
				break;

			case `PrettyComments`:
				if (error.newValue) {
					action = CodeAction.create(`Fix comment formatting`, CodeActionKind.QuickFix);
					action.edit = {
						changes: {
							[document.uri]: [
								TextEdit.replace(errorRange, error.newValue)
							]
						},
					}
					actions.push(action);
				}
				break;

			case `SQLRunner`:
				if (error.newValue) {
					action = CodeAction.create(`Run statement in Db2 for i`, CodeActionKind.Empty);
					action.command = {
						title: `Run statement`,
						command: `vscode-db2i.runEditorStatement`,
						arguments: [{
							content: error.newValue,
							qualifier: `statement`,
							open: true,
							viewColumn: -2
						}]
					};
					actions.push(action);
				}
		}
	});

	return actions;
}

export function getSubroutineActions(document: TextDocument, docs: Cache, range: Range): CodeAction|undefined {
	if (range.start.line === range.end.line) {
		const currentGlobalSubroutine = docs.subroutines.find(sub => sub.position.line === range.start.line);

		if (currentGlobalSubroutine) {
			const subroutineRange = Range.create(
				Position.create(currentGlobalSubroutine.range.start, 0),
				Position.create(currentGlobalSubroutine.range.end, 1000)
			);

			const bodyRange = Range.create(
				Position.create(currentGlobalSubroutine.range.start + 1, 0),
				Position.create(currentGlobalSubroutine.range.end - 1, 0)
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
				const lineNumber = document.positionAt(ref.offset.position).line;
				// If this reference is outside of the subroutine
				if (lineNumber < currentGlobalSubroutine.range.start || lineNumber > currentGlobalSubroutine.range.end) {
					return TextEdit.replace(
						Range.create(
							// - 5 `EXSR `
							document.positionAt(ref.offset.position - 5),
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

function caseInsensitiveReplaceAll(text: string, search: string, replace: string) {
	return text.replace(new RegExp(search, `gi`), replace);
}
	

function createExtract(document: TextDocument, userRange: Range, docs: Cache) {
	const range = Range.create(userRange.start.line, 0, userRange.end.line, 1000);
	const references = docs.referencesInRange(document.uri, {position: document.offsetAt(range.start), end: document.offsetAt(range.end)});
	const validRefs = references.filter(ref => [`struct`, `subitem`, `variable`].includes(ref.dec.type));

	const nameDiffSize = 1; // Always once since we only add 'p' at the start
	const newParamNames = validRefs.map(ref => `p${ref.dec.name}`);
	let newBody = document.getText(range);

	const rangeStartOffset = document.offsetAt(range.start);

	// Fix the found offset lengths to be relative to the new procedure
	for (let i = validRefs.length - 1; i >= 0; i--) {
		for (let y = validRefs[i].refs.length - 1; y >= 0; y--) {
			validRefs[i].refs[y] = {
				position: validRefs[i].refs[y].position - rangeStartOffset,
				end: validRefs[i].refs[y].end - rangeStartOffset
			};
		}
	}

	// Then let's fix the references to use the new names
	for (let i = validRefs.length - 1; i >= 0; i--) {
		for (let y = validRefs[i].refs.length - 1; y >= 0; y--) {
			const ref = validRefs[i].refs[y];

			newBody = newBody.slice(0, ref.position) + newParamNames[i] + newBody.slice(ref.end);
			ref.end += nameDiffSize;

			// Then we need to update the offset of the next references
			for (let z = i - 1; z >= 0; z--) {
				for (let x = validRefs[z].refs.length - 1; x >= 0; x--) {
					if (validRefs[z].refs[x].position > ref.end) {
						validRefs[z].refs[x] = {
							position: validRefs[z].refs[x].position + nameDiffSize,
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