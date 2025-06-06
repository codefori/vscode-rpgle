
import { commands, DecorationOptions, ExtensionContext, Range, Selection, TextDocument, ThemeColor, window } from 'vscode';
import * as Configuration from "../configuration";
import { loadBase } from '../base';

const currentArea = window.createTextEditorDecorationType({
  backgroundColor: `rgba(242, 242, 109, 0.3)`,
  border: `1px solid grey`,
});

const notCurrentArea = window.createTextEditorDecorationType({
  backgroundColor: `rgba(242, 242, 109, 0.1)`,
  border: `1px solid grey`,
});

const outlineBar = window.createTextEditorDecorationType({
  backgroundColor: new ThemeColor(`editor.background`),
  isWholeLine: true,
  opacity: `0`,
});

let rulerEnabled = Configuration.get(Configuration.RULER_ENABLED_BY_DEFAULT) || false
let currentEditorLine = -1;

import { SpecFieldDef, SpecFieldValue, SpecRulers, specs } from '../schemas/specs';

const getAreasForLine = (line: string, index: number) => {
  if (line.length < 6) return undefined;
  if (line[6] === `*`) return undefined;
  
  const specLetter = line[5].toUpperCase();
  if (specs[specLetter]) {
    const specification = specs[specLetter];

    const active = specification.findIndex((box: any) => index >= box.start && index <= box.end);

    return {
      specification,
      active,
      outline: SpecRulers[specLetter]
    };
  }
}

function documentIsFree(document: TextDocument) {
  if (document.languageId === `rpgle`) {
    const line = document.getText(new Range(0, 0, 0, 6)).toUpperCase();
    return line === `**FREE`;
  }
  
  return false;
}

export function registerColumnAssist(context: ExtensionContext) {
  context.subscriptions.push(
    commands.registerCommand(`vscode-rpgle.assist.launchUI`, async () => {
      const editor = window.activeTextEditor;
      if (editor) {
        const document = editor.document;

        if (document.languageId === `rpgle`) {
          if (!documentIsFree(document)) { 
            const lineNumber = editor.selection.start.line;
            const positionIndex = editor.selection.start.character;

            const positionsData = await promptLine(
              document.getText(new Range(lineNumber, 0, lineNumber, 100)), 
              positionIndex
            );

            if (positionsData) {
              window.showTextDocument(document).then(newEditor => {
                newEditor.edit(editBuilder => {
                  editBuilder.replace(new Range(lineNumber, 0, lineNumber, 80), positionsData);
                });
              })
            }
          }
        }
      }
    }),

    commands.registerCommand(`vscode-rpgle.assist.toggleFixedRuler`, async () => {
      rulerEnabled = !rulerEnabled;
      
      if (rulerEnabled) {
        updateRuler();
      } else {
        clearRulers();
      }
    }),

    commands.registerCommand(`vscode-rpgle.assist.moveLeft`, () => {
      moveFromPosition(`left`);
    }),
    commands.registerCommand(`vscode-rpgle.assist.moveRight`, () => {
      moveFromPosition(`right`);
    }),

    window.onDidChangeTextEditorSelection(e => {
      const editor = e.textEditor;
      if (rulerEnabled) {
        updateRuler(editor);
      } else {
        clearRulers(editor);
      }
    }),
  )
}

function moveFromPosition(direction: "left"|"right", editor = window.activeTextEditor) {
  if (editor && editor.document.languageId === `rpgle` && !documentIsFree(editor.document)) {
    const document = editor.document;
    const lineNumber = editor.selection.start.line;
    const positionIndex = editor.selection.start.character;

    const positionsData = getAreasForLine(
      document.getText(new Range(lineNumber, 0, lineNumber, 100)), 
      positionIndex
    );

    if (positionsData) {
      let newIndex: number|undefined;
      if (direction === `left`) {
        newIndex = positionsData.active - 1;
      } else
      if (direction === `right`) {
        newIndex = positionsData.active + 1;
      }

      if (newIndex !== undefined && newIndex >= 0 && newIndex < positionsData.specification.length) {
        const box = positionsData.specification[newIndex];
        if (box) {
          editor.selection = new Selection(lineNumber, box.start, lineNumber, box.start);
        }
      }
    }
  }
}

function updateRuler(editor = window.activeTextEditor) {
  let clear = true;

  if (editor) {
    const document = editor.document;
    if (document.languageId === `rpgle`) {
      if (!documentIsFree(document)) {
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

          if (currentEditorLine !== lineNumber && lineNumber >= 1) {
            editor.setDecorations(outlineBar, [
              {
                range: new Range(lineNumber-1, 0, lineNumber-1, 80),
                renderOptions: {
                  before: {
                    contentText: positionsData.outline,
                    color: new ThemeColor(`editorLineNumber.foreground`),
                  }
                }
              },
            ]);
          }

          clear = false;
        }

        currentEditorLine = lineNumber;
      }
    }
  }

  if (clear) {
    clearRulers(editor);
  }
}

function clearRulers(editor = window.activeTextEditor) {
  if (editor) {
    editor.setDecorations(currentArea, []);
    editor.setDecorations(notCurrentArea, []);
    editor.setDecorations(outlineBar, []);
  }
}

interface FieldBox {
  id: string,
  text: string,
  content: string
  values?: SpecFieldValue[],
  maxLength?: number
}

async function promptLine (line: string, _index: number): Promise<string|undefined> {
  const base = loadBase();

  if (!base) {
    window.showErrorMessage(`Code for IBM i is not installed. It is required due to required UI tools.`);
    return undefined;
  };


  if (line.length < 6) return undefined;
  if (line[6] === `*`) return undefined;
  line = line.padEnd(80);
  
  const specLetter = line[5].toUpperCase();
  if (specs[specLetter]) {
    const specification = specs[specLetter];

    let parts: FieldBox[] = [];

    specification.forEach(box => {
      parts.push({
        id: box.id,
        text: box.name,
        content: line.substring(box.start, box.end+1).trimEnd(),
        values: box.values,
        maxLength: box.values ? undefined : (box.end+1)-box.start
      });
    });

    const ui = base.customUI();

    parts.forEach((box, index) => {
      if (box.values) {
        //Select box
        ui.addSelect(box.id, box.text, box.values.map(item => ({
          selected: item.value.toUpperCase() === box.content.toUpperCase(),
          value: item.value,
          description: item.value,
          text: item.text
        })))

      } else {
        //Input field
        ui.addInput(box.id, box.text);
        ui.fields[index].default = box.content;
        ui.fields[index].maxlength = box.maxLength;
      }
    });

    ui.addButtons(
      { id: `apply`, label: `Apply changes` },
      { id: `cancel`, label: `Cancel` }
    );

    const result = await ui.loadPage<{[key: string]: string}>(`Column Assistant`);

    if (result && result.data) {
      result.panel.dispose();
      const data = result.data;

      if (data.buttons !== `cancel`) {
        let spot: SpecFieldDef|undefined, length: number;
        for (const key in data) {
          spot = specification.find(box => box.id === key);
          if (spot) {
            length = (spot.end+1)-spot.start;

            if (data[key].length > length) data[key] = data[key].substr(0, length);

            line = line.substring(0, spot.start) + (spot.padStart ? data[key].padStart(length) : data[key].padEnd(length)) + line.substring(spot.end+1);
          }
        }

        return line.trimEnd();
      }
    }

    return undefined;
  } else {
    return undefined;
  }
}