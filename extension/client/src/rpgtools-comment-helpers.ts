// SPDX-License-Identifier: Apache-2.0
// Copyright (c) 1996-2026 by R. Cozzi, Jr.
// @author BobCozzi
//
// Minimal RPG helper functions for comment/uncomment support
// Extracted from rpgiv2free extension's rpgtools.ts

import * as vscode from 'vscode';

// Regex patterns used by isDirective
const freeFormDirectiveRegex = /^\/[A-Za-z0-9]+(\s|$)/; // Matches /COPY, /INCLUDE, etc.
const singleCharDirectiveRegex = /^\/[A-Za-z0-9]\s?/;   // Matches /X or /X[whitespace]
const validCharRegex = /^[A-Za-z0-9]/;                  // Matches valid characters after '/'

export function log(...args: any[]) {
  console.log('[vscode-rpgle]', ...args);
}

export function getEOL(): string {
  const editor = vscode.window.activeTextEditor;
  if (!editor) return '\n'; // Default to LF if no editor
  return editor.document.eol === vscode.EndOfLine.CRLF ? '\r\n' : '\n';
}

export function getCol(line: string | null | undefined, from: number, to?: number): string {
  if (!line || from < 1) return '';
  const end = to ?? from; // default 'to' to 'from' if not provided
  if (end < from) return '';
  line = line.padEnd(end, ' ');
  return line.substring(from - 1, end);
}

// Return the RPG IV Fixed Format Specification Code (H F D I C O P) in lowercase
// or blank/empty when it is not one of those or when position 7 is * or /
export function getSpecType(line: string): string {
  if (line.length < 6) return '';
  const col7 = line[6];
  if (col7 === '*' || col7 === '/') return '';
  return line[5].toLowerCase();
}

export function isFixedFormatRPG(document?: vscode.TextDocument): boolean {
  let doc: vscode.TextDocument | undefined = document;
  if (!doc) {
    const editor = vscode.window.activeTextEditor;
    if (!editor) return false;
    doc = editor.document;
  }
  if (doc.lineCount === 0) return true; // Treat empty document as fixed format
  const firstLine = doc.lineAt(0).text;

  // Check if **FREE appears in columns 1-6 (case-insensitive)
  // Per IBM rules, **FREE must be the only content on the line,
  // however, anything after `**FREE` should be ignored, thus the real test
  // is if line 1, position 1 to 6 contains `**FREE` case ignored.
  const bIsFreeFormat = (firstLine.length >= 6 &&
    firstLine.substring(0, 6).toUpperCase() === '**FREE');

  return !bIsFreeFormat;
}

export function isFreeFormatRPG(document?: vscode.TextDocument): boolean {
  let doc: vscode.TextDocument | undefined = document;
  if (!doc) {
    const editor = vscode.window.activeTextEditor;
    if (!editor) return false;
    doc = editor.document;
  }

  if (doc.lineCount === 0) return false; // Treat empty document as fixed format
  const firstLine = doc.lineAt(0).text;

  // Check if **FREE appears in columns 1-6 (case-insensitive)
  // Per IBM rules, **FREE must be the only content but the compiler
  // ignores anything after `**FREE` that it detects
  const bIsFreeFormat = (firstLine.length >= 6 &&
    firstLine.substring(0, 6).toUpperCase() === '**FREE');

  return bIsFreeFormat;
}

export function isComment(line: string, document?: vscode.TextDocument): boolean {
  const isFreeFormat = document ? isFreeFormatRPG(document) : false;
  // Classic RPG IV column-7 '*' comments are not valid in fully free-format (**FREE) source
  const classicRPGStyle = !isFreeFormat && line.length > 6 && line[6] === '*';
  const cppStyle = getCol(line, 8, 80).trimStart().startsWith('//') ||
    getCol(line, 1, 80).trimStart().startsWith('//');
  return classicRPGStyle || cppStyle;
}

export function isDirective(line: string, bFreeFormOnly?: boolean): boolean {
  // Classic RPG directive: column 7 is '/' and column 8 is not '/'
  const bDirective = (
    line.length > 7 &&
    line[6] === '/' &&
    line[7] !== '/'
  );
  if (bFreeFormOnly !== true) {
    if (bDirective) return true;
  }

  //  /FREE
  //  /END-FREE
  //  /TITLE
  //  /EJECT
  //  /SPACE
  //  /SET     [CCSID() | DATFMT() | TIMFMT()]
  //  /RESTORE /* restored a prior /SET operation to what it was before /SET */
  //  /OVERLOAD [DETAIL | NODETAIL]
  //  /CHARCOUNT [NATURAL | STDCHARSIZE]
  //  /COPY  library/file,source-member
  //  /INCLUDE ["hiarchical-file-system-name" | library/source-file,source-member]
  //  /DEFINE symbol
  //  /UNDEFINE symbol
  //  /IF [NOT [condition | DEFINED(symbol)]]
  //  /ELSE
  //  /ENDIF

  // Free-form or modern: line starts with '/' followed by A-Za-z0-9
  const trimmed = getCol(line, 7, 80).trim(); // Avoid converting to uppercase unnecessarily
  if (!trimmed.startsWith('//') &&
    trimmed.startsWith('/') &&
    trimmed.length > 1 &&
    validCharRegex.test(trimmed[1])
  ) {
    // Accept if only /X or /X[whitespace...]
    if (singleCharDirectiveRegex.test(trimmed)) {
      return true;
    }
    // Accept if /COPY, /INCLUDE, etc.
    if (freeFormDirectiveRegex.test(trimmed)) {
      return true;
    }
  }

  return false;
}

/**
 * Determines if a line represents an empty statement (no executable content).
 * Supports fixed format, hybrid free format (column 8-80), and full free format RPG IV.
 * Includes directives as empty statements.
 *
 * @param line - The source line to check
 * @param document - Optional document for format detection; if not provided, uses heuristics
 * @returns true if the line is an empty statement (non-comment with no content) or a directive
 */
export function isEmptyStmt(line: string, document?: vscode.TextDocument): boolean {
  // Comments are handled separately by isComment(), not here
  if (isComment(line, document)) {
    return false;
  }

  // Directives are treated as empty statements
  if (isDirective(line)) {
    return true;
  }

  // Determine RPG format type if document is provided
  const isFreeFormat = document ? isFreeFormatRPG(document) : null;

  if (isFreeFormat === true) {
    // Full free-format RPG IV (**FREE directive)
    // A statement is empty if the entire line is only whitespace
    return line.trim() === '';
  }

  // Fixed format or hybrid free format (respects column 8-80 boundary)
  // In both cases, the statement content area is columns 8-80
  // A statement is empty if this area contains only whitespace
  const statementArea = getCol(line, 8, 80).trim();
  return statementArea === '';
}

export function isSkipStmt(line: string, document?: vscode.TextDocument): boolean {
  const bComment = isComment(line, document);
  const bEmptyStmt = isEmptyStmt(line, document);
  const bDirective = isDirective(line);
  return bComment || bDirective || bEmptyStmt;
}
