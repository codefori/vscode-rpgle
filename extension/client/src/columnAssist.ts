
import { DecorationOptions, ExtensionContext, Range, window } from 'vscode';
import * as Configuration from "./configuration";

const currentArea = window.createTextEditorDecorationType({
  backgroundColor: `rgba(242, 242, 109, 0.3)`,
  border: `1px solid grey`,
});

const notCurrentArea = window.createTextEditorDecorationType({
  backgroundColor: `rgba(242, 242, 109, 0.1)`,
  border: `1px solid grey`,
});

const specs = require(`./schemas/specs`);

const getAreasForLine = (line: string, index: number) => {
  if (line.length < 6) return undefined;
  if (line[6] === `*`) return undefined;
  
  const specLetter = line[5].toUpperCase();
  if (specs[specLetter]) {
    const specification = specs[specLetter];

    const active = specification.findIndex((box: any) => index >= box.start && index <= box.end);

    return {
      specification,
      active
    };
  }
}

export function registerColumnAssist(context: ExtensionContext) {
  context.subscriptions.push(
    window.onDidChangeTextEditorSelection(e => {
      if (Configuration.get(`showFixedFormatOutline`)) {
        const editor = e.textEditor;
        const document = editor.document;

        if (document.languageId === `rpgle`) {
          if (document.getText(new Range(0, 0, 0, 6)).toUpperCase() !== `**FREE`) {
            const lineNumber = editor.selection.start.line;
            const positionIndex = editor.selection.start.character;

            const positionsData = getAreasForLine(
              document.getText(new Range(lineNumber, 0, lineNumber, 100)), 
              positionIndex
            );

            if (positionsData) {
              let decorations: DecorationOptions[] = [];

              positionsData.specification.forEach((box: any, index: number) => {
                if (index === positionsData.active) {
                  //There should only be one current.
                  editor.setDecorations(currentArea, [{
                    hoverMessage: box.name,
                    range: new Range(lineNumber, box.start, lineNumber, box.end+1)
                  }]);

                } else {
                  decorations.push({
                    hoverMessage: box.name,
                    range: new Range(lineNumber, box.start, lineNumber, box.end+1)
                  })
                }
              });
              editor.setDecorations(notCurrentArea, decorations);

            } else {
              editor.setDecorations(currentArea, []);
              editor.setDecorations(notCurrentArea, []);
            }
          }
        }
      }
    }),
  )
}
