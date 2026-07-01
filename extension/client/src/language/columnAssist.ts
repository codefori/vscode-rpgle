import { commands, DecorationOptions, ExtensionContext, Range, Selection, TextDocument, ThemeColor, window, workspace } from 'vscode';
import * as Configuration from "../configuration";
import { loadBase } from '../base';
import { isIleFileByUri, isOpmFileByUri } from '../../../../language/utils/fileRouting';
import { getCol } from '../rpgtools-comment-helpers';

const currentArea = window.createTextEditorDecorationType({
    backgroundColor: `rgba(242, 242, 109, 0.3)`,
    border: `1px solid grey`,
});

const notCurrentArea = window.createTextEditorDecorationType({
    backgroundColor: `rgba(242, 242, 109, 0.1)`,
    border: `1px solid grey`,
});

const outlineBarFill = window.createTextEditorDecorationType({});
const outlineBar = window.createTextEditorDecorationType({});

let rulerEnabled = Configuration.get(Configuration.RULER_ENABLED_BY_DEFAULT) || false;
let currentEditorLine = -1;
let overlayRevision = 0;

import { SpecFieldDef, SpecFieldValue, SpecRulers, specs, opmSpecs, opmSpecRulers } from '../schemas/specs';

function sourceDatesVisible(): boolean {
    const codeForIbmiConfig = workspace.getConfiguration(`codeforibmi`);
    const vscodeIbmiConfig = workspace.getConfiguration(`vscode-ibmi`);

    return codeForIbmiConfig.get<boolean>(`enableSourceDates`) === true
        || vscodeIbmiConfig.get<boolean>(`enableSourceDates`) === true;
}

function documentType(document: TextDocument): 'opm' | 'ile' | undefined {
    const uri = document.uri.toString();

    if (isOpmFileByUri(uri) || document.languageId === 'rpg') {
        return 'opm';
    }

    if (isIleFileByUri(uri) || document.languageId === 'rpgle') {
        return 'ile';
    }

    return undefined;
}


function getInputRulerKey(line: string): 'I' | 'IC' | 'IJ' | 'IX' | 'JX' {
    const fileName = getCol(line, 7, 16).trim();
    const shortFile = getCol(line, 7, 15).trim();
    const andOr = getCol(line, 16, 18).toLowerCase().trim();
    const fromPos = getCol(line, 37, 41).trim();
    const toPos = getCol(line, 42, 46).trim();
    const seqType = getCol(line, 17, 18).trim();
    const indyVoidArea = getCol(line, 23, 46).trim();
    const extField = getCol(line, 21, 30).trim();
    const fieldName = getCol(line, 49, 63).trim();
    const extVoidArea = getCol(line, 31, 48).trim();
    const extVoidRec = getCol(line, 23, 80).trim();
    if (!shortFile && (andOr === `and` || andOr === `or`)) {
        return `IC`;
    }

    if (/^\d+$/.test(fromPos) || /^\d+$/.test(toPos)) {
        return `IJ`;
    }

    if (!fileName && !extVoidArea && (!!extField || fieldName)) {
        return `JX`;
    }

    if (fileName && indyVoidArea.length === 0) {
        return `IX`;
    }
    if (seqType.length > 0 || indyVoidArea.length > 0) {
        return `I`;
    }

    return `IX`;
}


function getOutputRulerKey(line: string): 'OX' | 'OC' | 'O' | 'OP' {
    const andOrKeyword = getCol(line, 16, 20).trim().toUpperCase();
    if (andOrKeyword === `AND` || andOrKeyword === `OR`) {
        return `OC`;
    }

    const filename = getCol(line, 7, 16).trim();
    const continueType = getCol(line, 16, 18).trim().toLowerCase();
    const cyleTime = getCol(line, 17, 17).trim().toLowerCase();
    const fieldName = getCol(line, 30, 43).trim();
    const endPos = getCol(line, 47, 51).trim();
    const constant = getCol(line, 53, 80).trim();

    if (continueType == 'and' || continueType == 'or') return `OC`;
    if (filename || (!filename && cyleTime)) return `O`;
    if (endPos && (fieldName || constant)) return `OP`;
    if (!fieldName && constant) return `OP`;
    if (fieldName) return `OP`;

    return `OX`;
}

function getDefinitionRulerKey(line: string): 'D' | 'DX' {
    const src = line.padEnd(80, ` `);
    const markerToken = src.substring(6, 80).trim();
    const markerBody = markerToken.endsWith(`...`)
        ? markerToken.slice(0, -3)
        : (markerToken.endsWith(`…`) ? markerToken.slice(0, -1) : ``);

    if (markerBody && !/\s/.test(markerBody)) {
        return `DX`;
    }

    return `D`;
}

const EXTENDED_FACTOR2_OPCODES = new Set([
    `CALLP`,
    `DATA-GEN`,
    `DATA-INTO`,
    `DOU`,
    `DOW`,
    `ELSEIF`,
    `EVAL`,
    `EVAL-CORR`,
    `EVALR`,
    `FOR`,
    `FOR-EACH`,
    `IF`,
    `ON-ERROR`,
    `ON-EXCP`,
    `ON-EXIT`,
    `RETURN`,
    `SND-MSG`,
    `SORTA`,
    `WHEN`,
    `XML-INTO`,
    `XML-SAX`,
]);

function getCalculationRulerKey(line: string): 'C' | 'CX' {
    const src = line.padEnd(80, ` `);

    const opRaw = src.substring(25, 35).trim().toUpperCase();
    const opcode = opRaw.replace(/\(.*\)/, ``).trim();

    const condIndicators = src.substring(8, 11).trim();
    const factor1 = src.substring(11, 25).trim();
    const extendedFactor2 = src.substring(35, 80).trim();
    const preExtendedZone = src.substring(6, 35).trim();

    if (opcode.length === 0) {
        if (condIndicators.length === 0 && factor1.length === 0 && extendedFactor2.length > 0) {
            return `CX`;
        }
        return `C`;
    }

    if (!EXTENDED_FACTOR2_OPCODES.has(opcode)) {
        return `C`;
    }

    if (extendedFactor2.length > 0 || preExtendedZone.length === 0) {
        return `CX`;
    }

    return `C`;
}

const getAreasForLine = (line: string, index: number, languageId: string = 'rpgle') => {
    if (line.length < 6) return undefined;
    if (line[6] === `*` || line[6] === `/`) return undefined;

    const specDefinitions = languageId === 'rpg' ? opmSpecs : specs;
    const rulerDefinitions = languageId === 'rpg' ? opmSpecRulers : SpecRulers;

    const specLetter = line[5].toUpperCase();
    const rulerKey = languageId !== 'rpg'
        ? (specLetter === `O`
            ? getOutputRulerKey(line)
            : (specLetter === `I`
                ? getInputRulerKey(line)
                : (specLetter === `D`
                    ? getDefinitionRulerKey(line)
                    : (specLetter === `C` ? getCalculationRulerKey(line) : specLetter))))
        : specLetter;
    const specKey = specDefinitions[rulerKey] ? rulerKey : specLetter;

    if (specDefinitions[specKey]) {
        const specification = specDefinitions[specKey];
        const active = specification.findIndex((box: any) => index >= box.start && index <= box.end);

        return {
            specification,
            active,
            outline: rulerDefinitions[rulerKey] || rulerDefinitions[specKey] || rulerDefinitions[specLetter],
        };
    } else if (rulerDefinitions[rulerKey] || rulerDefinitions[specKey] || rulerDefinitions[specLetter]) {
        return {
            specification: [] as SpecFieldDef[],
            active: -1,
            outline: rulerDefinitions[rulerKey] || rulerDefinitions[specKey] || rulerDefinitions[specLetter],
        };
    }
};

function documentIsFree(document: TextDocument) {
    const type = documentType(document);

    if (type === 'opm') {
        return false;
    } else if (type === 'ile') {
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

                const type = documentType(document);
                if (type) {
                    if (!documentIsFree(document)) {
                        const lineNumber = editor.selection.start.line;
                        const positionIndex = editor.selection.start.character;

                        const positionsData = await promptLine(
                            document.getText(new Range(lineNumber, 0, lineNumber, 100)),
                            positionIndex,
                            type === 'opm' ? 'rpg' : 'rpgle'
                        );

                        if (positionsData) {
                            window.showTextDocument(document).then(newEditor => {
                                newEditor.edit(editBuilder => {
                                    editBuilder.replace(new Range(lineNumber, 0, lineNumber, 80), positionsData);
                                });
                            });
                        }
                    }
                }
            }
        }),

        commands.registerCommand(`vscode-rpgle.assist.toggleFixedRuler`, async () => {
            rulerEnabled = !rulerEnabled;
            currentEditorLine = -1;

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

        workspace.onDidChangeTextDocument(e => {
            if (!rulerEnabled) return;

            const editor = window.visibleTextEditors.find(item => item.document.uri.toString() === e.document.uri.toString()) || window.activeTextEditor;
            if (!editor || editor.document.uri.toString() !== e.document.uri.toString()) return;

            overlayRevision++;
            updateRuler(editor);
        }),

        window.onDidChangeTextEditorSelection(e => {
            const editor = e.textEditor;
            if (rulerEnabled) {
                updateRuler(editor);
            } else {
                clearRulers(editor);
            }
        }),

        window.onDidChangeActiveTextEditor(editor => {
            if (!editor) return;

            if (rulerEnabled) {
                overlayRevision++;
                updateRuler(editor);
            } else {
                clearRulers(editor);
            }
        }),

        window.onDidChangeTextEditorVisibleRanges(e => {
            if (rulerEnabled) {
                overlayRevision++;
                updateRuler(e.textEditor);
            }
        }),
    );
}

function moveFromPosition(direction: "left" | "right", editor = window.activeTextEditor) {
    if (editor && !documentIsFree(editor.document)) {
        const document = editor.document;
        const type = documentType(document);
        if (!type) return;
        const lineNumber = editor.selection.start.line;
        const positionIndex = editor.selection.start.character;

        const positionsData = getAreasForLine(
            document.getText(new Range(lineNumber, 0, lineNumber, 100)),
            positionIndex,
            type === 'opm' ? 'rpg' : 'rpgle'
        );

        if (positionsData) {
            let newIndex: number | undefined;
            if (direction === `left`) {
                newIndex = positionsData.active - 1;
            } else if (direction === `right`) {
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
        const type = documentType(document);
        if (type) {
            if (!documentIsFree(document)) {
                const lineNumber = editor.selection.start.line;
                const positionIndex = editor.selection.start.character;

                const positionsData = getAreasForLine(
                    document.getText(new Range(lineNumber, 0, lineNumber, 100)),
                    positionIndex,
                    type === 'opm' ? 'rpg' : 'rpgle'
                );

                if (positionsData) {
                    const decorations: DecorationOptions[] = [];

                    positionsData.specification.forEach((box: any, index: number) => {
                        if (index === positionsData.active) {
                            editor.setDecorations(currentArea, [{
                                hoverMessage: box.name,
                                range: new Range(lineNumber, box.start, lineNumber, box.end + 1)
                            }]);
                        } else {
                            decorations.push({
                                hoverMessage: box.name,
                                range: new Range(lineNumber, box.start, lineNumber, box.end + 1)
                            });
                        }
                    });

                    editor.setDecorations(notCurrentArea, decorations);

                    const repaintNonce = ((lineNumber + positionIndex + overlayRevision) % 2) === 0 ? `\u200B` : `\u200C`;
                    const fillText = `${`\u00A0`.repeat(240)}${repaintNonce}`;
                    const outlineText = `${positionsData.outline}${repaintNonce}`;
                    const rulerStart = sourceDatesVisible() ? 7 : 0;

                    editor.setDecorations(outlineBarFill, [{
                        range: new Range(lineNumber, rulerStart, lineNumber, rulerStart),
                        renderOptions: {
                            before: {
                                contentText: fillText,
                                color: new ThemeColor(`editor.background`),
                                backgroundColor: new ThemeColor(`editor.background`),
                                textDecoration: `none; position: absolute; top: -1.4em; z-index: 0; opacity: 1; white-space: pre; display: inline-block; line-height: 1.4em; pointer-events: none;`,
                            }
                        }
                    }]);

                    editor.setDecorations(outlineBar, [{
                        range: new Range(lineNumber, rulerStart, lineNumber, rulerStart),
                        renderOptions: {
                            after: {
                                contentText: outlineText,
                                color: new ThemeColor(`editor.foreground`),
                                textDecoration: `none; position: absolute; top: -1.4em; z-index: 1; opacity: 1; white-space: pre; display: inline-block; line-height: 1.4em; pointer-events: none;`,
                            }
                        }
                    }]);

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
    currentEditorLine = -1;
    if (editor) {
        editor.setDecorations(currentArea, []);
        editor.setDecorations(notCurrentArea, []);
        editor.setDecorations(outlineBarFill, []);
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

async function promptLine(line: string, _index: number, languageId: string = 'rpgle'): Promise<string | undefined> {
    const base = loadBase();

    if (!base) {
        window.showErrorMessage(`Code for IBM i is not installed. It is required due to required UI tools.`);
        return undefined;
    };

    if (line.length < 6) return undefined;
    if (line[6] === `*`) return undefined;
    line = line.padEnd(80);

    const specDefinitions = languageId === 'rpg' ? opmSpecs : specs;

    const specLetter = line[5].toUpperCase();
    const specKey = languageId !== 'rpg'
        ? (specLetter === `O`
            ? getOutputRulerKey(line)
            : (specLetter === `I`
                ? getInputRulerKey(line)
                : (specLetter === `D`
                    ? getDefinitionRulerKey(line)
                    : (specLetter === `C` ? getCalculationRulerKey(line) : specLetter))))
        : specLetter;
    const effectiveSpecKey = specDefinitions[specKey] ? specKey : specLetter;

    if (specDefinitions[effectiveSpecKey]) {
        const specification = specDefinitions[effectiveSpecKey];

        const parts: FieldBox[] = [];

        specification.forEach(box => {
            parts.push({
                id: box.id,
                text: box.name,
                content: line.substring(box.start, box.end + 1).trimEnd(),
                values: box.values,
                maxLength: box.values ? undefined : (box.end + 1) - box.start
            });
        });

        const ui = base.customUI();

        parts.forEach((box, index) => {
            if (box.values) {
                ui.addSelect(box.id, box.text, box.values.map(item => ({
                    selected: item.value.toUpperCase() === box.content.toUpperCase(),
                    value: item.value,
                    description: item.value,
                    text: item.text
                })));
            } else {
                ui.addInput(box.id, box.text);
                ui.fields[index].default = box.content;
                ui.fields[index].maxlength = box.maxLength;
            }
        });

        ui.addButtons(
            { id: `apply`, label: `Apply changes` },
            { id: `cancel`, label: `Cancel` }
        );

        const result = await ui.loadPage<{ [key: string]: string }>(`Column Assistant`);

        if (result && result.data) {
            result.panel.dispose();
            const data = result.data;

            if (data.buttons !== `cancel`) {
                let spot: SpecFieldDef | undefined, length: number;
                for (const key in data) {
                    spot = specification.find(box => box.id === key);
                    if (spot) {
                        length = (spot.end + 1) - spot.start;

                        if (data[key].length > length) data[key] = data[key].substr(0, length);

                        line = line.substring(0, spot.start) + (spot.padStart ? data[key].padStart(length) : data[key].padEnd(length)) + line.substring(spot.end + 1);
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
