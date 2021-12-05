
const vscode = require(`vscode`);

const Configuration = require(`./configuration`);

const base = vscode.extensions.getExtension(`halcyontechltd.code-for-ibmi`);

const currentArea = vscode.window.createTextEditorDecorationType({
  backgroundColor: `rgba(242, 242, 109, 0.3)`,
  border: `1px solid grey`,
});

const notCurrentArea = vscode.window.createTextEditorDecorationType({
  backgroundColor: `rgba(242, 242, 109, 0.1)`,
  border: `1px solid grey`,
});

const specs = require(`./models/specs.js`);

/**
 * @param {string} line 
 * @param {number} index 
 * @returns {{id, name, start, end}|undefined}
 */
const getInfoFromLine = (line, index) => {
  if (line.length < 6) return undefined;
  if (line[6] === `*`) return undefined;

  const specLetter = line[5].toUpperCase();
  if (specs[specLetter]) {
    const specification = specs[specLetter];

    const item = specification.find(box => index >= box.start && index <= box.end);
    if (item) {
      return item;
    }
  }
  
  return undefined;
}

/**
 * @param {string} line 
 * @param {number} index The current piece the cursor is over
 * @returns {{specification: {id, name, start, end}[], active?: number}}|undefined}
 */
const getAreasForLine = (line, index) => {
  if (line.length < 6) return undefined;
  if (line[6] === `*`) return undefined;
  
  const specLetter = line[5].toUpperCase();
  if (specs[specLetter]) {
    const specification = specs[specLetter];

    const active = specification.findIndex(box => index >= box.start && index <= box.end);

    return {
      specification,
      active
    };
  }
}

/**
 * @param {string} line 
 * @param {number} index The current piece the cursor is over
 * @returns {Promise<string|undefined>} New line
 */
const promptLine = async (line, index) => {
  if (!base) {
    vscode.window.showErrorMessage(`Code for IBMi is not installed. It is required due to required UI tools.`);
    return undefined;
  };

  const { CustomUI, Field } = base.exports;

  if (line.length < 6) return undefined;
  if (line[6] === `*`) return undefined;
  line = line.padEnd(80);
  
  const specLetter = line[5].toUpperCase();
  if (specs[specLetter]) {
    const specification = specs[specLetter];

    let parts = [];

    specification.forEach(box => {
      parts.push({
        id: box.id,
        text: box.name,
        content: line.substring(box.start, box.end+1).trimEnd(),
        values: box.values
      });
    });

    let ui = new CustomUI();

    parts.forEach((box, index) => {
      if (box.values) {
        //Select box
        ui.addField(new Field(`select`, box.id, box.text));
        ui.fields[index].items = box.values.map(item => ({
          selected: item.value.toUpperCase() === box.content.toUpperCase(),
          value: item.value,
          description: item.value,
          text: item.text
        }));

      } else {
        //Input field
        ui.addField(new Field(`input`, box.id, box.text));
        ui.fields[index].default = box.content;
      }
    });

    ui.addField(new Field(`submit`, `submitButton`, `Update`));

    const {panel, data} = await ui.loadPage(`Column Assistant`);

    if (data) {
      panel.dispose();

      let spot, length;
      for (const key in data) {
        spot = specification.find(box => box.id === key);
        length = (spot.end+1)-spot.start;

        if (data[key].length > length) data[key] = data[key].substr(0, length);

        line = line.substring(0, spot.start) + (spot.padStart ? data[key].padStart(length) : data[key].padEnd(length)) + line.substring(spot.end+1);
      }

      return line.trimEnd();
    }

    return undefined;
  } else {
    return undefined;
  }
}

/**
 * 
 * @param {vscode.ExtensionContext} context 
 */
function registerColumnAssist(context) {
  context.subscriptions.push(
    vscode.commands.registerCommand(`vscode-rpgle.rpgleColumnAssistant`, async () => {
      const editor = vscode.window.activeTextEditor;
      if (editor) {
        const document = editor.document;

        if (document.languageId === `rpgle`) {
          if (document.getText(new vscode.Range(0, 0, 0, 6)).toUpperCase() !== `**FREE`) { 
            const lineNumber = editor.selection.start.line;
            const positionIndex = editor.selection.start.character;

            const positionsData = await promptLine(
              document.getText(new vscode.Range(lineNumber, 0, lineNumber, 100)), 
              positionIndex
            );

            if (positionsData) {
              editor.edit(editBuilder => {
                editBuilder.replace(new vscode.Range(lineNumber, 0, lineNumber, 80), positionsData);
              });
            }
          }
        }
      }
    }),

    vscode.window.onDidChangeTextEditorSelection(e => {
      if (Configuration.get(`showFixedFormatOutline`)) {
        const editor = e.textEditor;
        const document = editor.document;

        if (document.languageId === `rpgle`) {
          if (document.getText(new vscode.Range(0, 0, 0, 6)).toUpperCase() !== `**FREE`) {
            const lineNumber = editor.selection.start.line;
            const positionIndex = editor.selection.start.character;

            const positionsData = getAreasForLine(
              document.getText(new vscode.Range(lineNumber, 0, lineNumber, 100)), 
              positionIndex
            );

            if (positionsData) {
              let decorations = [];

              positionsData.specification.forEach((box, index) => {
                if (index === positionsData.active) {
                  //There should only be one current.
                  editor.setDecorations(currentArea, [{
                    hoverMessage: box.name,
                    range: new vscode.Range(lineNumber, box.start, lineNumber, box.end+1)
                  }]);

                } else {
                  decorations.push({
                    hoverMessage: box.name,
                    range: new vscode.Range(lineNumber, box.start, lineNumber, box.end+1)
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

module.exports = {
  registerColumnAssist
};