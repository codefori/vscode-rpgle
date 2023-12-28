import path = require('path');
import { CodeAction, CodeActionKind, Diagnostic, DiagnosticSeverity, DidChangeWatchedFilesParams, DidOpenTextDocumentParams, DidSaveTextDocumentParams, Range, TextDocumentChangeEvent, TextEdit, WorkspaceEdit, _Connection } from 'vscode-languageserver';
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
							ignoreCache: true
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
			options.CollectReferences = true;
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
			case `LowercaseDirectives`:
			case `UppercaseDirectives`:
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