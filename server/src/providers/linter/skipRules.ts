import { CompletionItem, CompletionItemKind, CompletionParams, InsertTextFormat, Position, Range } from 'vscode-languageserver';

const skipAll = CompletionItem.create(`@rpglint-skip`);
skipAll.kind = CompletionItemKind.Snippet;
skipAll.detail = `The next line will skip rules and indentation checking.`;

const skipIndent = CompletionItem.create(`@rpglint-skip-indent`);
skipIndent.kind = CompletionItemKind.Snippet;
skipIndent.detail = `The next line will skip indentation checking.`;

const skipRules = CompletionItem.create(`@rpglint-skip-rules`);
skipRules.kind = CompletionItemKind.Snippet;
skipRules.detail = `The next line will skip rules checking.`;

export default [skipAll, skipIndent, skipRules];