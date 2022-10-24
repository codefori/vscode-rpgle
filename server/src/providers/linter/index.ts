import { CodeAction, CodeActionKind, Diagnostic, DiagnosticSeverity, Range, TextEdit, WorkspaceEdit, _Connection } from 'vscode-languageserver';
import { TextDocument } from 'vscode-languageserver-textdocument';
import { URI } from 'vscode-uri';
import { connection, getFileRequest, validateUri } from '../../connection';
import { IssueRange } from '../../language/index';
import Linter from '../../language/linter';
import Cache from '../../language/models/cache';

export function calculateOffset(document: TextDocument, error: IssueRange) {
	const offset = error.offset;

	if (offset && offset.position !== undefined && offset.end !== undefined) {
		const docOffsetStart = document.offsetAt(error.range.start) + offset.position;
		const docOffsetEnd = document.offsetAt(error.range.start) + offset.end;
		return Range.create(
			document.positionAt(docOffsetStart),
			document.positionAt(docOffsetEnd)
		);
	} else {
		return Range.create(error.range.start, error.range.end);
	}
};

export async function getLintConfigUri(workingUri: string) {
	const uri = URI.parse(workingUri);
	let cleanString: string|undefined;

	switch (uri.scheme) {
		case `member`:
			const [_, baseLibrary, baseSourceFile, basename] = uri.path.split(`/`);
			cleanString = [
				``,
				baseLibrary,
				`VSCODE`,
				`RPGLINT.JSON`
			].join(`/`);

			cleanString = URI.from({
				scheme: `member`,
				path: cleanString
			}).toString();

			cleanString = await validateUri(cleanString);
			break;

		case `file`:
			cleanString = await validateUri(`rpglint.json`, uri.scheme);
			break;
	}

	return cleanString;
}

export async function getLintOptions(workingUri: string) {
	const possibleUri = await getLintConfigUri(workingUri);
	let result = {};
	
	if (possibleUri) {
		try {
			const fileContent = await getFileRequest(possibleUri);
			if (fileContent) {
				result = JSON.parse(fileContent);
			}
		} catch (e: any) {
			// Maybe some default options?
			console.log(`Error getting lint config for ${possibleUri}: ${e.message}`);
			console.log(e.stack);
		}
	}

	return result;
}

export async function refreshDiagnostics(document: TextDocument, docs: Cache) {
	const isFree = (document.getText(Range.create(0, 0, 0, 6)).toUpperCase() === `**FREE`);
	if (isFree) {
		const text = document.getText();

		const indentDiags: Diagnostic[] = [];
		const generalDiags: Diagnostic[] = [];

		const options = await getLintOptions(document.uri);

		let detail;

		try {
			detail = Linter.getErrors({
				uri: document.uri,
				content: text,
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
					DiagnosticSeverity.Warning
				);

				generalDiags.push(diagnostic);
			});
		}

		connection.sendDiagnostics({ uri: document.uri, diagnostics: [...indentDiags, ...generalDiags] });
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
				action = CodeAction.create(`Convert constant name to uppercase`, CodeActionKind.QuickFix);
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
		case `StringLiteralDupe`:
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
		}
	});

	return actions;
}