import { Diagnostic, DiagnosticSeverity, Range, _Connection } from 'vscode-languageserver';
import { TextDocument } from 'vscode-languageserver-textdocument';
import { connection } from '../connection';
import { IssueRange } from '../language';
import Linter from '../language/linter';
import Cache from '../language/models/cache';

const calculateOffset = (document: TextDocument, error: IssueRange) => {
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


export function refreshDiagnostics(document: TextDocument, docs: Cache) {
	const isFree = (document.getText(Range.create(0, 0, 0, 6)).toUpperCase() === `**FREE`);
	if (isFree) {
		const text = document.getText();

		const indentDiags: Diagnostic[] = [];
		const generalDiags: Diagnostic[] = [];

		//const options = await this.getLinterOptions(document.uri);
		const options = {
			indent: 2,
			IncorrectVariableCase: true,
			NoUnreferenced: true
		};

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