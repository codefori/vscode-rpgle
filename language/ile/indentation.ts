import type Document from './document';
import type { Token } from './types';
import type { IndentError } from './linter';
import oneLineTriggers from '../models/oneLineTriggers';

export function getLeadingWhitespace(line: string): string {
	return line.match(/^[ \t]*/)![0];
}

export function isInlineDeclaration(opcode: string, tokens: Token[]): boolean {
	const triggers = oneLineTriggers[opcode as keyof typeof oneLineTriggers];
	return triggers?.some(trigger => tokens.some(token => token.value?.toUpperCase() === trigger)) ?? false;
}

const blockClosers: Record<string, string> = {
	IF: `ENDIF`, FOR: `ENDFOR`, 'FOR-EACH': `ENDFOR`, DOW: `ENDDO`, DOU: `ENDDO`,
	MONITOR: `ENDMON`, BEGSR: `ENDSR`, SELECT: `ENDSL`, 'DCL-PROC': `END-PROC`,
	'DCL-DS': `END-DS`, 'DCL-PI': `END-PI`, 'DCL-PR': `END-PR`, 'DCL-ENUM': `END-ENUM`,
};
const closingOpcodes = new Set(Object.values(blockClosers));

const middleOpcodes = new Set([`ELSE`, `ELSEIF`, `WHEN`, `WHEN-IS`, `WHEN-IN`, `OTHER`, `ON-ERROR`, `ON-EXCP`]);
const middleOwners: Record<string, string> = {
	ELSE: `ENDIF`, ELSEIF: `ENDIF`, WHEN: `ENDSL`, 'WHEN-IS': `ENDSL`, 'WHEN-IN': `ENDSL`,
	OTHER: `ENDSL`, 'ON-ERROR': `ENDMON`, 'ON-EXCP': `ENDMON`, 'ON-EXIT': `END-PROC`,
};
type Block = { closer: string, line: number };
type State = { depth: number, onExit: boolean, blocks: Block[], invalidFrom?: number, includeLine?: number };
type Conditional = { line: number, entry: State, branches: State[], hasElse: boolean, invalid: boolean };
const clone = (state: State): State => ({ ...state, blocks: state.blocks.map(block => ({ ...block })) });
const compatible = (left: State, right: State) => left.invalidFrom === undefined && right.invalidFrom === undefined
	&& left.depth === right.depth && left.onExit === right.onExit && left.blocks.length === right.blocks.length
	&& left.blocks.every((block, index) => block.closer === right.blocks[index].closer);

export function getIndentationMessage(error: IndentError): string {
	return error.hasTabs
		? `Incorrect indentation. Expected ${error.expectedIndent} spaces; found tab characters.`
		: `Incorrect indentation. Expected ${error.expectedIndent}, got ${error.currentIndent}`;
}

export interface IndentationSafety {
	indentationUnsafeFrom?: number;
	indentationUnsafeRanges?: { start: number, end: number }[];
}

export function getFixableIndentationErrors(result: IndentationSafety & { indentErrors: IndentError[] }): IndentError[] {
	return result.indentErrors.filter(error => error.expectedIndent >= 0
		&& (result.indentationUnsafeFrom === undefined || error.line < result.indentationUnsafeFrom)
		&& !result.indentationUnsafeRanges?.some(range => error.line >= range.start && error.line <= range.end));
}

/** Analyze each branch from the same entry state; never accumulate mutually exclusive blocks. */
export function analyzeIndentation(doc: Document, indent: number): IndentationSafety & { expectedIndents: number[] } {
	let state: State = { depth: 0, onExit: false, blocks: [] };
	const conditionals: Conditional[] = [];
	const unsafeRanges: { start: number, end: number }[] = [];
	const expectedIndents: number[] = [];
	let unsafeFrom: number | undefined;
	let skipNext = false;
	const invalidate = (line: number) => {
		state.invalidFrom = Math.min(state.invalidFrom ?? line, line);
		if (conditionals.length === 0) unsafeFrom = Math.min(unsafeFrom ?? line, line);
	};

	for (const statement of doc.statements) {
		const first = statement.tokens[0];
		const opcode = first?.value?.toUpperCase() ?? ``;
		const line = statement.range.line;
		if (first?.type === `comment`) {
			const comment = first.value?.slice(2).trim();
			skipNext = comment === `@rpglint-skip` || comment === `@rpglint-skip-indent`;
		} else if (first?.type === `directive` && opcode === `/IF`) {
			conditionals.push({ line, entry: clone(state), branches: [], hasElse: false, invalid: false });
		} else if (first?.type === `directive` && [`/ELSE`, `/ELSEIF`].includes(opcode)) {
			const conditional = conditionals[conditionals.length - 1];
			if (!conditional) invalidate(line);
			else {
				conditional.branches.push(clone(state));
				conditional.invalid ||= conditional.hasElse;
				conditional.hasElse ||= opcode === `/ELSE`;
				state = clone(conditional.entry);
			}
		} else if (first?.type === `directive` && opcode === `/ENDIF`) {
			const conditional = conditionals.pop();
			if (!conditional) invalidate(line);
			else {
				conditional.branches.push(clone(state));
				if (!conditional.hasElse) conditional.branches.push(clone(conditional.entry));
				const firstBranch = conditional.branches[0];
				if (!conditional.invalid && conditional.branches.every(branch => compatible(firstBranch, branch))) {
					state = clone(firstBranch);
					state.blocks.forEach((block, index) => {
						block.line = Math.min(...conditional.branches.map(branch => branch.blocks[index].line));
					});
					const includes = conditional.branches.flatMap(branch => branch.includeLine === undefined ? [] : [branch.includeLine]);
					state.includeLine = includes.length ? Math.min(...includes) : undefined;
				} else {
					state = clone(conditional.entry);
					invalidate(conditional.line);
				}
				unsafeRanges.push({ start: conditional.line, end: line });
			}
		} else {
			if (first?.type === `directive` && [`/COPY`, `/INCLUDE`].includes(opcode)) state.includeLine ??= line;
			if (closingOpcodes.has(opcode) || middleOpcodes.has(opcode)) {
				state.depth--;
				if (opcode === `ENDSL`) state.depth--;
				if (opcode === `END-PROC` && state.onExit) {
					state.onExit = false;
					state.depth--;
				}
				// A skipped closer can belong to an unseen /COPY opener.
				if (skipNext && state.depth < 0) state.depth = 0;
				if (state.depth < 0) invalidate(state.blocks[0]?.line ?? line);
			}
			if (middleOwners[opcode] && state.blocks[state.blocks.length - 1]?.closer !== middleOwners[opcode]) {
				invalidate(Math.min(state.blocks[0]?.line ?? line, state.includeLine ?? line));
			}
			if (closingOpcodes.has(opcode)) {
				if (state.blocks[state.blocks.length - 1]?.closer === opcode) state.blocks.pop();
				else invalidate(Math.min(state.blocks[0]?.line ?? line, state.includeLine ?? line));
			}
			expectedIndents.push(state.depth * indent);
			if ((blockClosers[opcode] || middleOpcodes.has(opcode) || opcode === `ON-EXIT`)
				&& !isInlineDeclaration(opcode, statement.tokens)) {
				if (blockClosers[opcode]) state.blocks.push({ closer: blockClosers[opcode], line });
				state.depth += opcode === `SELECT` ? 2 : 1;
				if (opcode === `ON-EXIT`) state.onExit = true;
			}
			skipNext = false;
			continue;
		}
		expectedIndents.push(state.depth * indent);
		if (first?.type !== `comment`) skipNext = false;
	}
	if (conditionals.length) unsafeFrom = Math.min(unsafeFrom ?? Infinity, conditionals[0].line);
	if (state.blocks.length) unsafeFrom = Math.min(unsafeFrom ?? Infinity, state.blocks[0].line);
	return { expectedIndents, indentationUnsafeFrom: unsafeFrom, indentationUnsafeRanges: unsafeRanges };
}
