import { DecorationOptions, TextEditor, window, Range, ExtensionContext, TextDocument } from "vscode";
import { getBlockRanges } from '../../../language/blocks';

const colourDecorations = [
  window.createTextEditorDecorationType({
    backgroundColor: 'rgba(50, 66, 168, 0.1)'
  }),
  window.createTextEditorDecorationType({
    backgroundColor: 'rgba(50, 66, 168, 0.2)'
  }),
  window.createTextEditorDecorationType({
    backgroundColor: 'rgba(50, 66, 168, 0.3)'
  }),
  window.createTextEditorDecorationType({
    backgroundColor: 'rgba(50, 66, 168, 0.4)'
  }),
  window.createTextEditorDecorationType({
    backgroundColor: 'rgba(50, 66, 168, 0.5)'
  }),
  window.createTextEditorDecorationType({
    backgroundColor: 'rgba(50, 66, 168, 0.6)'
  }),
]

const currentBlockDecoration = window.createTextEditorDecorationType({
  backgroundColor: 'rgba(50, 66, 168, 0.1)'
});

export function registerBlockAssist(context: ExtensionContext) {
  let lastEditVersion: number = -1;
  context.subscriptions.push(
    window.onDidChangeActiveTextEditor((e) => {
      if (e) {
        if (isValidRpgDocument(e?.document)) {
          lastEditVersion = e?.document.version || -1;
          renderBlockDecorations(e, true);
        }
      }
    }),
    window.onDidChangeTextEditorSelection((e) => {
      if (isValidRpgDocument(e.textEditor.document)) {
        if (e.textEditor) {
          renderBlockDecorations(e.textEditor);
        }
      }
    }),
  )
}

function isValidRpgDocument(document: TextDocument) {
  return document.languageId === 'rpgle' && document.getText(new Range(0, 0, 0, 6)).toLowerCase() !== '**free';
}

const maxIndent = 5;

export function renderBlockDecorations(editor: TextEditor, allRanges = false) {
  const blocks = getBlockRanges(editor.document.getText());

  const currentLine = editor.selection.active.line;

  let visibleRanges: number[] = [];
  editor.visibleRanges.forEach(range => {
    for (let i = range.start.line; i <= range.end.line; i++) {
      visibleRanges.push(i);
    }
  });

  const visibleBlocks = allRanges ? blocks : blocks.filter(block => visibleRanges.some(line => block.start <= line && block.end && block.end >= line));

  let indentBlocks: { [indent: string]: DecorationOptions[] } = {};

  for (let i = 0; i <= maxIndent; i++) {
    indentBlocks[String(i)] = [];
  }

  visibleBlocks.forEach((block) => {
    if (indentBlocks[String(block.indent)]) {   
      for (let i = block.start; i <= block.end!; i++) {
        indentBlocks[String(block.indent)].push({
          range: new Range(i, block.indent, i, maxIndent),
        });
      }
    }
  });

  for (let i = 0; i <= maxIndent; i++) {
    editor.setDecorations(colourDecorations[i], indentBlocks[i]);
  }

  const selectedBlock = visibleBlocks.find(block => block.start <= currentLine && block.end && block.end >= currentLine);

  if (selectedBlock) {
    editor.setDecorations(currentBlockDecoration, [
      {
        range: new Range(selectedBlock.start, maxIndent, selectedBlock.start, 100),
      },
      {
        range: new Range(selectedBlock.end!, maxIndent, selectedBlock.end!, 100),
      }
    ]);
  } else {
    editor.setDecorations(currentBlockDecoration, []);
  }
}