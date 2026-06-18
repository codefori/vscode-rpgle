// SPDX-License-Identifier: Apache-2.0
// Copyright (c) 1996-2026 by R. Cozzi, Jr.
// @author BobCozzi

import * as vscode from 'vscode';
import * as rpgle from './rpgtools-comment-helpers';

export function registerCommentStatementCommand(context: vscode.ExtensionContext) {
  const disposable = vscode.commands.registerCommand('vscode-rpgle.commentStatement', async () => {
    const editor = vscode.window.activeTextEditor;
    rpgle.log('Comment Statement Handler evoked');

    if (!editor) {
      vscode.window.showErrorMessage('No active editor window found. Comment statement cancelled.');
      return;
    }

    const doc = editor.document;
    const allLines = doc.getText().split(rpgle.getEOL());
    const isCompletelyFreeFormat = !rpgle.isFixedFormatRPG(doc);
    const edits: vscode.TextEdit[] = [];

    try {
      for (const sel of editor.selections) {
        const start = Math.min(sel.start.line, sel.end.line);
        const end = Math.max(sel.start.line, sel.end.line);

        for (let lineIndex = start; lineIndex <= end; lineIndex++) {
          if (lineIndex >= allLines.length) continue;

          const line = allLines[lineIndex];

          // Skip lines that are already comments
          if (rpgle.isComment(line, doc)) continue;

          // Skip empty/blank/directive lines
          if (rpgle.isSkipStmt(line, doc)) continue;

          let commentedLine = '';

          // If the file is **FREE, always use free format comments
          if (isCompletelyFreeFormat) {
            commentedLine = commentFreeFormatLine(line);
          } else {
            // For mixed format files, determine format by checking if the line has a valid spec type
            const specType = rpgle.getSpecType(line);

            // specType is valid only if it's a non-whitespace character (c, d, f, h, p, etc.)
            if (specType && specType.trim() !== '') {
              // Fixed format: Insert asterisk at column 7 (position 6, 0-indexed)
              commentedLine = commentFixedFormatLine(line);
            } else {
              // Free format: Add // to the beginning
              commentedLine = commentFreeFormatLine(line);
            }
          }

          if (commentedLine !== '') {
            const range = doc.lineAt(lineIndex).range;
            edits.push(vscode.TextEdit.replace(range, commentedLine));
          }
        }
      }

      if (edits.length > 0) {
        const edit = new vscode.WorkspaceEdit();
        edit.set(doc.uri, edits);
        await vscode.workspace.applyEdit(edit);
      } else {
        vscode.window.showInformationMessage('No lines to comment.');
      }
    } catch (e) {
      rpgle.log('Error in comment statement: ' + (e as Error).message);
      vscode.window.showErrorMessage('An error occurred while commenting: ' + (e as Error).message);
    }
  });

  context.subscriptions.push(disposable);
}

export function registerUncommentStatementCommand(context: vscode.ExtensionContext) {
  const disposable = vscode.commands.registerCommand('vscode-rpgle.uncommentStatement', async () => {
    const editor = vscode.window.activeTextEditor;
    rpgle.log('Uncomment Statement Handler evoked');

    if (!editor) {
      vscode.window.showErrorMessage('No active editor window found. Uncomment statement cancelled.');
      return;
    }

    const doc = editor.document;
    const allLines = doc.getText().split(rpgle.getEOL());
    const isCompletelyFreeFormat = !rpgle.isFixedFormatRPG(doc);
    const edits: vscode.TextEdit[] = [];

    try {
      for (const sel of editor.selections) {
        const start = Math.min(sel.start.line, sel.end.line);
        const end = Math.max(sel.start.line, sel.end.line);

        for (let lineIndex = start; lineIndex <= end; lineIndex++) {
          if (lineIndex >= allLines.length) continue;

          const line = allLines[lineIndex];

          // Only process comment lines
          if (!rpgle.isComment(line, doc)) continue;

          let uncommentedLine = '';

          // If the file is **FREE, always use free format uncomment
          if (isCompletelyFreeFormat) {
            uncommentedLine = uncommentFreeFormatLine(line);
          } else {
            // For mixed format files, determine format by checking for asterisk in column 7
            if (line.length > 6 && line[6] === '*') {
              // Fixed format: Remove asterisk from column 7 (position 6, 0-indexed)
              uncommentedLine = uncommentFixedFormatLine(line);
            } else {
              // Free format: Remove // from the beginning
              uncommentedLine = uncommentFreeFormatLine(line);
            }
          }

          if (uncommentedLine !== '') {
            const range = doc.lineAt(lineIndex).range;
            edits.push(vscode.TextEdit.replace(range, uncommentedLine));
          }
        }
      }

      if (edits.length > 0) {
        const edit = new vscode.WorkspaceEdit();
        edit.set(doc.uri, edits);
        await vscode.workspace.applyEdit(edit);
      } else {
        vscode.window.showInformationMessage('No lines to uncomment.');
      }
    } catch (e) {
      rpgle.log('Error in uncomment statement: ' + (e as Error).message);
      vscode.window.showErrorMessage('An error occurred while uncommenting: ' + (e as Error).message);
    }
  });

  context.subscriptions.push(disposable);
}

/**
 * Toggle comment for Cmd+/ (Mac) or Ctrl+/ (Windows/Linux) keyboard shortcut
 * If any line in the selection is uncommented, comment all lines
 * If all lines are commented, uncomment all lines
 */
export function registerToggleCommentCommand(context: vscode.ExtensionContext) {
  const disposable = vscode.commands.registerCommand('vscode-rpgle.toggleComment', async () => {
    const editor = vscode.window.activeTextEditor;
    rpgle.log('Toggle Comment Handler evoked');

    if (!editor) {
      vscode.window.showErrorMessage('No active editor window found. Toggle comment cancelled.');
      return;
    }

    const doc = editor.document;
    const allLines = doc.getText().split(rpgle.getEOL());
    const isCompletelyFreeFormat = !rpgle.isFixedFormatRPG(doc);

    try {
      // First pass: determine if we should comment or uncomment
      // If ANY line is uncommented (and not empty/directive), we comment all
      // If ALL lines are commented or empty/directive, we uncomment
      let hasUncommentedCode = false;

      for (const sel of editor.selections) {
        const start = Math.min(sel.start.line, sel.end.line);
        const end = Math.max(sel.start.line, sel.end.line);

        for (let lineIndex = start; lineIndex <= end; lineIndex++) {
          if (lineIndex >= allLines.length) continue;

          const line = allLines[lineIndex];

          // Skip empty/blank/directive lines for the decision
          if (rpgle.isSkipStmt(line, doc) && !rpgle.isComment(line, doc)) continue;

          // If we find any uncommented code line, we'll comment everything
          if (!rpgle.isComment(line, doc)) {
            hasUncommentedCode = true;
            break;
          }
        }

        if (hasUncommentedCode) break;
      }

      // Second pass: apply the appropriate action
      const edits: vscode.TextEdit[] = [];

      if (hasUncommentedCode) {
        // Comment all lines (same logic as commentStatement)
        for (const sel of editor.selections) {
          const start = Math.min(sel.start.line, sel.end.line);
          const end = Math.max(sel.start.line, sel.end.line);

          for (let lineIndex = start; lineIndex <= end; lineIndex++) {
            if (lineIndex >= allLines.length) continue;

            const line = allLines[lineIndex];

            // Skip lines that are already comments
            if (rpgle.isComment(line, doc)) continue;

            // Skip empty/blank/directive lines
            if (rpgle.isSkipStmt(line, doc)) continue;

            let commentedLine = '';

            if (isCompletelyFreeFormat) {
              commentedLine = commentFreeFormatLine(line);
            } else {
              const specType = rpgle.getSpecType(line);
              if (specType && specType.trim() !== '') {
                commentedLine = commentFixedFormatLine(line);
              } else {
                commentedLine = commentFreeFormatLine(line);
              }
            }

            if (commentedLine !== '') {
              const range = doc.lineAt(lineIndex).range;
              edits.push(vscode.TextEdit.replace(range, commentedLine));
            }
          }
        }
      } else {
        // Uncomment all lines (same logic as uncommentStatement)
        for (const sel of editor.selections) {
          const start = Math.min(sel.start.line, sel.end.line);
          const end = Math.max(sel.start.line, sel.end.line);

          for (let lineIndex = start; lineIndex <= end; lineIndex++) {
            if (lineIndex >= allLines.length) continue;

            const line = allLines[lineIndex];

            // Only process comment lines
            if (!rpgle.isComment(line, doc)) continue;

            let uncommentedLine = '';

            if (isCompletelyFreeFormat) {
              uncommentedLine = uncommentFreeFormatLine(line);
            } else {
              if (line.length > 6 && line[6] === '*') {
                uncommentedLine = uncommentFixedFormatLine(line);
              } else {
                uncommentedLine = uncommentFreeFormatLine(line);
              }
            }

            if (uncommentedLine !== '') {
              const range = doc.lineAt(lineIndex).range;
              edits.push(vscode.TextEdit.replace(range, uncommentedLine));
            }
          }
        }
      }

      if (edits.length > 0) {
        const edit = new vscode.WorkspaceEdit();
        edit.set(doc.uri, edits);
        await vscode.workspace.applyEdit(edit);
      }
    } catch (e) {
      rpgle.log('Error in toggle comment: ' + (e as Error).message);
      vscode.window.showErrorMessage('An error occurred while toggling comments: ' + (e as Error).message);
    }
  });

  context.subscriptions.push(disposable);
}

/**
 * Comment a fixed format RPG line by inserting an asterisk at column 7 (position 6)
 * and shifting everything from column 7 onwards one position to the right
 * @param line The fixed format line to comment
 * @returns The commented line
 */
function commentFixedFormatLine(line: string): string {
  // Ensure line is at least 6 characters to have columns 1-6
  if (line.length < 6) {
    line = line.padEnd(6, ' ');
  }

  // Extract the parts:
  // Columns 1-6 (positions 0-5): Line sequence/label and form type
  // Columns 7+ (positions 6+): The statement
  const prefix = line.substring(0, 6);      // Columns 1-6
  const statement = line.substring(6);      // Columns 7+

  // Create commented line: prefix + '*' + statement
  // This inserts the asterisk at column 7 and shifts everything else right
  let commentedLine = prefix + '*' + statement;

  return commentedLine;
}

/**
 * Uncomment a fixed format RPG line by removing the asterisk at column 7 (position 6)
 * and shifting everything from column 8 onwards one position to the left
 * @param line The commented fixed format line
 * @returns The uncommented line
 */
function uncommentFixedFormatLine(line: string): string {
  // Extract the parts:
  // Columns 1-6 (positions 0-5): Line sequence/label and form type
  // Column 7 (position 6): Should be '*'
  // Columns 8+ (positions 7+): The statement
  const prefix = line.substring(0, 6);      // Columns 1-6
  const statement = line.substring(7);      // Columns 8+

  // Create uncommented line: prefix + statement
  // This removes the asterisk and shifts everything back left
  let uncommentedLine = prefix + statement;

  return uncommentedLine;
}

/**
 * Comment a free format RPG line by adding // to the beginning
 * Respects the indentation of the original line
 * @param line The free format line to comment
 * @returns The commented line
 */
function commentFreeFormatLine(line: string): string {
  // Get the leading whitespace
  const leadingWhitespace = line.match(/^\s*/)?.[0] || '';
  const trimmedLine = line.trim();

  // If the line is already a comment, skip it
  if (trimmedLine.startsWith('//')) {
    return '';
  }

  // Add // comment marker, preserving indentation
  return leadingWhitespace + '// ' + trimmedLine;
}

/**
 * Uncomment a free format RPG line by removing the // prefix
 * @param line The commented free format line
 * @returns The uncommented line
 */
function uncommentFreeFormatLine(line: string): string {
  // Get the leading whitespace
  const leadingWhitespace = line.match(/^\s*/)?.[0] || '';
  const trimmedLine = line.trim();

  // If the line doesn't start with //, skip it
  if (!trimmedLine.startsWith('//')) {
    return '';
  }

  // Remove the // comment marker and optional space after it
  const uncommentedContent = trimmedLine.replace(/^\/\/\s?/, '');

  // Restore the indentation
  return leadingWhitespace + uncommentedContent;
}
