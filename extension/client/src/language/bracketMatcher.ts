import * as vscode from 'vscode';
import { isInSqlBlock, isInCommentOrString } from '../../../../language/utils/sqlDetection';
import { RPGLE_BLOCK_PAIRS, BlockPair, BlockMatch } from '../../../../language/utils/blockParser';

type BracketPair = BlockPair;

// Comprehensive list of SQL keywords that might conflict with RPG keywords
// These keywords will be excluded from bracket matching when inside EXEC SQL blocks
const SQL_KEYWORDS = [
  'select', 'from', 'where', 'join', 'inner', 'outer', 'left', 'right', 'full',
  'insert', 'update', 'delete', 'into', 'set', 'values',
  'declare', 'cursor', 'open', 'fetch', 'close',
  'for', 'when', 'case', 'end', 'then', 'else', 'elseif',
  'if', 'and', 'or', 'not', 'in', 'exists', 'between', 'like',
  'order', 'group', 'having', 'union', 'intersect', 'except',
  'create', 'alter', 'drop', 'table', 'index', 'view',
  'as', 'on', 'using', 'with', 'option',
  'commit', 'rollback', 'savepoint',
  'json_table', 'json_object', 'json_array',
  'distinct', 'all', 'any', 'some',
  'begin', 'do', 'while', 'loop', 'repeat', 'until',
  'call', 'return', 'exit', 'continue'
];

// Highlight style for matched brackets
const decorationType = vscode.window.createTextEditorDecorationType({
  backgroundColor: 'rgba(255, 255, 0, 0.2)', // Light yellow with transparency
  border: '1px solid rgba(255, 200, 0, 0.6)', // Darker yellow border
  borderRadius: '3px',
  fontWeight: 'bold', // Make text bold
  fontStyle: 'italic' // Make text italic
});

// Highlight style for mismatched closing keywords (errors)
const errorDecorationType = vscode.window.createTextEditorDecorationType({
  backgroundColor: 'rgba(255, 0, 0, 0.3)', // Light red with transparency
  border: '2px solid rgba(255, 0, 0, 0.8)', // Red border
  borderRadius: '3px',
  fontWeight: 'bold',
  textDecoration: 'wavy underline red'
});

let currentBlockInfo: { startLine: number; endLine: number; ranges: vscode.Range[]; blockType: string; condition: string } | undefined;
let currentErrorRanges: { range: vscode.Range; keyword: string }[] = [];

// Register bracket matching functionality
export function registerBracketMatcher(context: vscode.ExtensionContext) {
  let timeout: any = undefined;

  // Register hover provider to show block info and error info
  const hoverProvider = vscode.languages.registerHoverProvider('rpgle', {
    provideHover(_document, position) {
      // First check if hovering over an error (unmatched keyword)
      for (const errorInfo of currentErrorRanges) {
        if (errorInfo.range.contains(position)) {
          const keyword = errorInfo.keyword.toUpperCase();
          const markdown = new vscode.MarkdownString();
          markdown.appendText(`⚠️ Unmatched ${keyword} statement\n\n`);
          markdown.appendText('This ENDxx keyword has no matching opening block.\n\n');
          markdown.appendText('If it is a DS subfield: use DCL-SUBF\n\n');
          markdown.appendText('If it is a parameter: use DCL-PARM');
          return new vscode.Hover(markdown);
        }
      }

      // Then check if hovering over a matched block
      if (!currentBlockInfo) return undefined;

      // Check if cursor is on a highlighted keyword
      const isOnHighlightedWord = currentBlockInfo.ranges.some(range => range.contains(position));

      if (isOnHighlightedWord) {
        const hoverText = `${currentBlockInfo.condition}\n\nStart: line ${currentBlockInfo.startLine + 1}\nEnd: line ${currentBlockInfo.endLine + 1}`;
        return new vscode.Hover(hoverText);
      }

      return undefined;
    }
  });

  context.subscriptions.push(hoverProvider);

  // Update decorations when selection changes
  vscode.window.onDidChangeTextEditorSelection(event => {
    const editor = event.textEditor;
    if (editor && editor.document.languageId === 'rpgle') {
      if (timeout) {
        clearTimeout(timeout);
      }
      timeout = setTimeout(() => updateDecorations(editor), 100);
    }
  }, null, context.subscriptions);

  // Update decorations when active editor changes
  vscode.window.onDidChangeActiveTextEditor(editor => {
    if (editor && editor.document.languageId === 'rpgle') {
      updateDecorations(editor);
    }
  }, null, context.subscriptions);

  // Initialize for current editor
  if (vscode.window.activeTextEditor && vscode.window.activeTextEditor.document.languageId === 'rpgle') {
    updateDecorations(vscode.window.activeTextEditor);
  }
}

function updateDecorations(editor: vscode.TextEditor) {
  const document = editor.document;
  const position = editor.selection.active;
  const text = document.getText();

  // First, find and highlight ALL mismatched closing keywords in the document
  const allMatches = findAllMatches(text, document);
  const allErrorRangesWithInfo = findAllMismatchedClosingKeywords(document, allMatches);
  const allErrorRanges = allErrorRangesWithInfo.map(e => e.range);

  // Store error ranges for hover provider
  currentErrorRanges = allErrorRangesWithInfo;

  // Always show errors in red
  editor.setDecorations(errorDecorationType, allErrorRanges);

  // Get word at cursor position for block highlighting
  const wordRange = document.getWordRangeAtPosition(position, /[a-zA-Z][\w-]*/);
  if (!wordRange) {
    editor.setDecorations(decorationType, []);
    currentBlockInfo = undefined;
    return;
  }

  const word = document.getText(wordRange).toLowerCase();

  // Check if we're clicking on any SQL keyword inside an SQL block
  if (SQL_KEYWORDS.includes(word)) {
    const offset = document.offsetAt(position);
    if (isInSqlBlock(text, offset)) {
      // Don't highlight SQL keywords inside SQL blocks
      editor.setDecorations(decorationType, []);
      currentBlockInfo = undefined;
      return;
    }
  }

  // Find matching bracket pair
  // Special handling for closing keywords that can close multiple block types
  let matchingPair: BracketPair | undefined;

  // Check if this is a closing keyword
  const isClosingKeyword = RPGLE_BLOCK_PAIRS.some(p => p.close.includes(word));

  if (isClosingKeyword && (word === 'end' || word === 'enddo')) {
    // For END and ENDDO, we need to find which block it actually closes
    const currentOffset = document.offsetAt(wordRange.start);

    // Find current match index
    let currentIndex = -1;
    for (let i = 0; i < allMatches.length; i++) {
      if (allMatches[i].offset === currentOffset) {
        currentIndex = i;
        break;
      }
    }

    if (currentIndex !== -1) {
      // Use findMatchingOpenForClosing to determine which block this closes
      const openIndex = findMatchingOpenForClosing(allMatches, currentIndex, word);
      if (openIndex !== -1) {
        const openWord = allMatches[openIndex].word;
        matchingPair = RPGLE_BLOCK_PAIRS.find(p => p.open.includes(openWord));
      }
    }
  } else {
    matchingPair = findMatchingPair(word);
  }

  if (!matchingPair) {
    editor.setDecorations(decorationType, []);
    currentBlockInfo = undefined;
    return;
  }

  // Find all related keywords in the block (only valid ones for yellow highlighting)
  const relatedRanges = findAllRelatedKeywords(document, wordRange, matchingPair);

  if (relatedRanges.length > 0) {
    // Highlight valid keywords in yellow (excluding error ranges)
    const validRanges = relatedRanges.filter(range =>
      !allErrorRanges.some((errorRange: vscode.Range) => errorRange.isEqual(range))
    );
    editor.setDecorations(decorationType, validRanges);

    // Determine block type
    const blockType = getBlockTypeName(matchingPair);

    // Extract condition from first line of block
    const startLine = relatedRanges[0].start.line;
    const endLine = relatedRanges[relatedRanges.length - 1].start.line;
    const condition = extractBlockCondition(document, startLine);

    currentBlockInfo = {
      startLine,
      endLine,
      ranges: relatedRanges,
      blockType,
      condition
    };
  } else {
    editor.setDecorations(decorationType, []);
    currentBlockInfo = undefined;
  }
}

function findMatchingPair(word: string): BracketPair | undefined {
  return RPGLE_BLOCK_PAIRS.find(pair =>
    pair.open.includes(word) || pair.close.includes(word) || (pair.middle && pair.middle.includes(word))
  );
}

// Get human-readable block type name
function getBlockTypeName(pair: BracketPair): string {
  // Determine block type based on opening keyword
  const openWord = pair.open[0].toUpperCase();

  const typeMap: { [key: string]: string } = {
    'IF': 'IF',
    'DOW': 'DOW',
    'DOU': 'DOU',
    'FOR': 'FOR',
    'SELECT': 'SELECT',
    'MONITOR': 'MONITOR',
    'DCL-PROC': 'PROCEDURE',
    'DCL-DS': 'DATA STRUCTURE',
    'DCL-PR': 'PROTOTYPE',
    'DCL-PI': 'PROCEDURE INTERFACE',
    'DCL-ENUM': 'ENUMERATION',
    'BEGSR': 'SUBROUTINE'
  };

  return typeMap[openWord] || openWord;
}

function extractBlockCondition(document: vscode.TextDocument, lineNumber: number): string {
  const line = document.lineAt(lineNumber);
  let text = line.text.trim();

  // Remove trailing comments
  const commentIndex = text.indexOf('//');
  if (commentIndex !== -1) {
    text = text.substring(0, commentIndex).trim();
  }

  // Remove trailing semicolon if present
  if (text.endsWith(';')) {
    text = text.substring(0, text.length - 1).trim();
  }

  return text;
}

// Helper function to check if a position is in a compiler directive
function isInCompilerDirective(text: string, offset: number): boolean {
  // Find the start of the line
  let lineStart = offset;
  while (lineStart > 0 && text[lineStart - 1] !== '\n' && text[lineStart - 1] !== '\r') {
    lineStart--;
  }

  // Get the text from line start to the offset
  const lineBeforeOffset = text.substring(lineStart, offset + 1);

  // Check if the line starts with / (compiler directive)
  // Pattern: optional whitespace, then /, then optional whitespace, then the keyword
  return /^\s*\//.test(lineBeforeOffset);
}

// Helper function to check if a keyword is actually being used as a variable
// This handles cases like: end = 5; end += 1; dow (x < end); dcl-s end int(10); etc.
function isVariableContext(text: string, matchOffset: number, matchLength: number): boolean {
  const matchWord = text.substring(matchOffset, matchOffset + matchLength);
  const afterKeyword = text.substring(matchOffset + matchLength);
  const beforeKeyword = text.substring(0, matchOffset);

  // Check if preceded by declaration keywords (dcl-s, dcl-c, dcl-pr, dcl-proc, dcl-pi, dcl-subf, dcl-parm, etc.)
  // ALL dcl- keywords indicate the next word is an identifier, not a keyword
  // Examples:
  //   dcl-s end pointer;        - standalone variable
  //   dcl-proc end;             - procedure name
  //   dcl-subf end int(10);     - data structure subfield with keyword name
  //   dcl-parm end char(10);    - parameter with keyword name
  const declMatch = beforeKeyword.match(/\b(dcl-[a-z]+)\s+$/i);
  if (declMatch) {
    return true;
  }

  // Check if preceded by FOR loop clause keywords (to, downto, by)
  // These indicate the next word is an expression/value, not a control keyword
  // Pattern: for i = 1 to end; or for i = 10 downto end by 2;
  const forClauseMatch = beforeKeyword.match(/\b(to|downto|by)\s+$/i);
  if (forClauseMatch) {

    return true;
  }

  // Check what comes after the keyword (skipping whitespace)
  const afterMatch = afterKeyword.match(/^\s*(.)/);
  if (!afterMatch) {
    return false;
  }

  const nextChar = afterMatch[1];

  // If followed by closing paren or comma, it's likely a variable in expression/parameter
  // e.g., dow (x < end) or func(a, end)
  if (nextChar === ')' || nextChar === ',') {
    return true;
  }

  // If followed by assignment operators, it's a variable
  // Matches: =, +=, -=, *=, /=
  if (nextChar === '=' || nextChar === '+' || nextChar === '-' || nextChar === '*' || nextChar === '/') {
    const twoChars = afterKeyword.substring(afterMatch[0].length - 1, afterMatch[0].length + 1);
    if (twoChars === '+=' || twoChars === '-=' || twoChars === '*=' || twoChars === '/=' || nextChar === '=') {
      return true;
    }
  }

  // Check what comes BEFORE the keyword - look for comparison operators or other expression contexts
  // e.g., dow (x < end) or if (y > end) or result = x + end
  const beforeMatch = beforeKeyword.match(/([<>=+\-*/.(,])\s*$/);

  // If followed by opening parenthesis or dot, check if it's actually array/DS access
  // vs. control flow keyword with condition
  // Examples:
  //   arr(if)      → preceded by '(', followed by ')' → VARIABLE
  //   ds.if(x)     → preceded by '.', followed by '(' → VARIABLE
  //   func(x, if)  → preceded by ',', followed by ')' → VARIABLE
  //   if (cond)    → NOT preceded by '.,(', followed by '(' → KEYWORD (control flow)
  if (nextChar === '(' || nextChar === '.') {
    // Only treat as variable if preceded by operators that indicate it's being used as a value
    if (beforeMatch) {
      const op = beforeMatch[1];
      return true;
    } else {
      // Not preceded by operator context → it's a control flow keyword like if (condition)
      return false;
    }
  }

  // Check for other operator contexts
  if (beforeMatch) {
    const op = beforeMatch[1];
    return true;
  }

  return false;
}

function findAllMatches(text: string, _document: vscode.TextDocument): BlockMatch[] {
  const allKeywords: string[] = [];
  RPGLE_BLOCK_PAIRS.forEach(pair => {
    allKeywords.push(...pair.open, ...pair.close);
    if (pair.middle) {
      allKeywords.push(...pair.middle);
    }
  });

  // Sort keywords by length (longest first) to match longer keywords before shorter ones
  // This ensures 'end-proc' is matched before 'end'
  const sortedKeywords = allKeywords.sort((a, b) => b.length - a.length);

  // Use word boundary that works with hyphens: (?<![a-zA-Z0-9-]) and (?![a-zA-Z0-9-])
  // Or simpler: match the keyword followed by non-alphanumeric-hyphen character
  const regex = new RegExp(`\\b(${sortedKeywords.map(k => k.replace(/-/g, '\\-')).join('|')})\\b`, 'gi');
  const matches: BlockMatch[] = [];

  let match;
  regex.lastIndex = 0;
  while ((match = regex.exec(text)) !== null) {
    const matchWord = match[0].toLowerCase();

    if (isInCommentOrString(text, match.index)) continue;

    // Skip compiler directives (e.g., /END-FREE, /FREE, /COPY)
    if (isInCompilerDirective(text, match.index)) {
      continue;
    }

    // Skip SQL keywords when inside EXEC SQL blocks
    if (SQL_KEYWORDS.includes(matchWord) && isInSqlBlock(text, match.index)) {
      continue;
    }

    // Skip keywords that are actually variables in expression/assignment context
    // Only check for simple keywords (no hyphens) that could be valid variable names
    // Keywords with hyphens (end-proc, dcl-proc, etc.) cannot be variables
    if (!matchWord.includes('-') && isVariableContext(text, match.index, match[0].length)) {
      continue;
    }

    matches.push({
      offset: match.index,
      word: matchWord,
      length: match[0].length
    });
  }

  return matches;
}

// Helper function specifically for finding the opening block for an END keyword
function findMatchingOpenForEnd(
  matches: { offset: number; word: string; length: number }[],
  endIndex: number
): number {
  return findMatchingOpenForClosing(matches, endIndex, 'end');
}

// Helper function for finding the opening block for any closing keyword
function findMatchingOpenForClosing(
  matches: { offset: number; word: string; length: number }[],
  closeIndex: number,
  closingWord: string
): number {
  // Build a stack to track all open blocks
  const stack: { index: number; pair: BracketPair }[] = [];

  for (let i = 0; i < closeIndex; i++) {
    const word = matches[i].word;

    // Check if this word opens any block
    const openingPair = RPGLE_BLOCK_PAIRS.find(p => p.open.includes(word));
    if (openingPair) {
      stack.push({ index: i, pair: openingPair });
    }

    // Check if this word closes a block
    // Special handling for shared closers like 'end' and 'enddo'
    if (word === 'end') {
      // Find the most recent block that can be closed by 'END'
      for (let j = stack.length - 1; j >= 0; j--) {
        if (stack[j].pair.close.includes('end')) {
          stack.splice(j, 1);
          break;
        }
      }
    } else if (word === 'enddo') {
      // Find the most recent block that can be closed by 'ENDDO' (DOW, DOU, or DO)
      for (let j = stack.length - 1; j >= 0; j--) {
        if (stack[j].pair.close.includes('enddo')) {
          stack.splice(j, 1);
          break;
        }
      }
    } else {
      // Other specific closers (endif, endfor, endsl, etc.)
      // Find the most recent block that can be closed by this keyword
      for (let j = stack.length - 1; j >= 0; j--) {
        if (stack[j].pair.close.includes(word)) {
          stack.splice(j, 1);
          break;
        }
      }
    }
  }

  // Find the most recent unclosed block that can be closed by the closing word
  for (let j = stack.length - 1; j >= 0; j--) {
    if (stack[j].pair.close.includes(closingWord)) {
      return stack[j].index;
    }
  }

  return -1;
}

function findAllRelatedKeywords(
  document: vscode.TextDocument,
  startRange: vscode.Range,
  pair: BracketPair
): vscode.Range[] {
  const text = document.getText();
  const startOffset = document.offsetAt(startRange.start);

  // Use findAllMatches to get ALL keywords in the document
  const allMatches = findAllMatches(text, document);

  // Find index of current match
  let currentIndex = -1;
  for (let i = 0; i < allMatches.length; i++) {
    if (allMatches[i].offset === startOffset) {
      currentIndex = i;
      break;
    }
  }

  if (currentIndex === -1) return [startRange];

  // Find block containing current keyword
  const blockIndices = findBlockIndices(allMatches, currentIndex, pair);
  if (!blockIndices) return [startRange];

  // Convert indices to ranges
  const ranges: vscode.Range[] = [];
  for (const idx of blockIndices) {
    const m = allMatches[idx];
    const start = document.positionAt(m.offset);
    const end = document.positionAt(m.offset + m.length);
    ranges.push(new vscode.Range(start, end));
  }

  return ranges;
}

// Find all mismatched closing keywords in the entire document
function findAllMismatchedClosingKeywords(
  document: vscode.TextDocument,
  matches: { offset: number; word: string; length: number }[]
): { range: vscode.Range; keyword: string }[] {
  const errorRanges: { range: vscode.Range; keyword: string }[] = [];
  const text = document.getText();

  // Check each closing keyword
  for (let i = 0; i < matches.length; i++) {
    const match = matches[i];
    const isClosing = RPGLE_BLOCK_PAIRS.some(p => p.close.includes(match.word));

    if (isClosing) {
      // Validate this closing keyword
      const isValid = validateClosingKeyword(text, matches, i);
      if (!isValid) {
        const start = document.positionAt(match.offset);
        const end = document.positionAt(match.offset + match.length);
        errorRanges.push({
          range: new vscode.Range(start, end),
          keyword: match.word
        });
      }
    }
  }

  return errorRanges;
}

// New function that validates block matching and returns separate lists for valid and error ranges
function findAllRelatedKeywordsWithValidation(
  document: vscode.TextDocument,
  startRange: vscode.Range,
  pair: BracketPair
): { validRanges: vscode.Range[]; errorRanges: vscode.Range[] } {
  const text = document.getText();
  const startOffset = document.offsetAt(startRange.start);

  // Use findAllMatches to get ALL keywords in the document
  const allMatches = findAllMatches(text, document);

  // Find index of current match
  let currentIndex = -1;
  for (let i = 0; i < allMatches.length; i++) {
    if (allMatches[i].offset === startOffset) {
      currentIndex = i;
      break;
    }
  }

  if (currentIndex === -1) return { validRanges: [startRange], errorRanges: [] };

  // Find block containing current keyword
  const blockIndices = findBlockIndices(allMatches, currentIndex, pair);
  if (!blockIndices) return { validRanges: [startRange], errorRanges: [] };

  // Validate all closing keywords in the block
  const validRanges: vscode.Range[] = [];
  const errorRanges: vscode.Range[] = [];

  for (const idx of blockIndices) {
    const m = allMatches[idx];
    const start = document.positionAt(m.offset);
    const end = document.positionAt(m.offset + m.length);
    const range = new vscode.Range(start, end);

    // Check if this is a closing keyword
    const isClosing = RPGLE_BLOCK_PAIRS.some(p => p.close.includes(m.word));

    if (isClosing) {
      // Validate that this closing keyword matches its opening keyword
      const isValid = validateClosingKeyword(document.getText(), allMatches, idx);
      if (isValid) {
        validRanges.push(range);
      } else {
        errorRanges.push(range);
      }
    } else {
      // Opening and middle keywords are always valid
      validRanges.push(range);
    }
  }

  return { validRanges, errorRanges };
}

// Validate that a closing keyword matches its corresponding opening keyword
function validateClosingKeyword(
  text: string,
  matches: { offset: number; word: string; length: number }[],
  closeIndex: number
): boolean {
  const closeWord = matches[closeIndex].word;

  // Find the opening keyword that this closing keyword should match
  const openIndex = findMatchingOpenForAnyClosing(text, matches, closeIndex);

  if (openIndex === -1) {
    // No matching opening found - this is an error
    return false;
  }

  const openWord = matches[openIndex].word;

  // Find the pair that contains this opening keyword
  const openPair = RPGLE_BLOCK_PAIRS.find(p => p.open.includes(openWord));

  if (!openPair) {
    return false;
  }

  // Check if the closing keyword is valid for this opening keyword
  // Specific closers (endif, endfor, endsl, etc.) can ONLY close their specific block type
  // Generic closers (end, enddo) can close multiple types

  if (closeWord === 'end' || closeWord === 'enddo') {
    // Generic closers: check if they can close this type of block
    return openPair.close.includes(closeWord);
  } else {
    // Specific closers: must match the EXACT block type
    // For example, 'endif' can ONLY close 'if' blocks, not 'dow' or 'dou'
    // 'end-proc' can ONLY close 'dcl-proc', etc.

    // A specific closer is valid if:
    // 1. The opening pair includes this closer in its close array
    // 2. This closer is the ONLY specific closer for that pair (not 'end' or 'enddo')

    // Check if this closer is in the opening pair's close array
    if (!openPair.close.includes(closeWord)) {
      return false;
    }

    // For pairs with multiple closers (e.g., ['endif', 'end']),
    // the specific closer (endif) should only match if it's the primary one
    // For pairs with single specific closer (e.g., ['end-proc']), it should always match

    // If the pair has only one closer, it must match
    if (openPair.close.length === 1) {
      return true;
    }

    // If the pair has multiple closers, check if this is the specific (non-generic) one
    // The specific closer is the one that's NOT 'end' or 'enddo'
    const specificClosers = openPair.close.filter(c => c !== 'end' && c !== 'enddo');
    return specificClosers.includes(closeWord);
  }
}

// Find the opening keyword for any closing keyword (similar to findMatchingOpenForClosing but more general)
function findMatchingOpenForAnyClosing(
  _text: string,
  matches: { offset: number; word: string; length: number }[],
  closeIndex: number
): number {
  const closeWord = matches[closeIndex].word;

  // Build a stack to track all open blocks
  const stack: { index: number; pair: BracketPair }[] = [];

  for (let i = 0; i < closeIndex; i++) {
    const word = matches[i].word;

    // Check if this word opens any block
    const openingPair = RPGLE_BLOCK_PAIRS.find(p => p.open.includes(word));
    if (openingPair) {
      stack.push({ index: i, pair: openingPair });
    }

    // Check if this word closes a block
    if (word === 'end') {
      // Generic closer 'END': Find the most recent block that can be closed by 'END'
      for (let j = stack.length - 1; j >= 0; j--) {
        if (stack[j].pair.close.includes('end')) {
          stack.splice(j, 1);
          break;
        }
      }
    } else if (word === 'enddo') {
      // Generic closer 'ENDDO': Find the most recent block that can be closed by 'ENDDO'
      for (let j = stack.length - 1; j >= 0; j--) {
        if (stack[j].pair.close.includes('enddo')) {
          stack.splice(j, 1);
          break;
        }
      }
    } else {
      // Specific closers (endif, endfor, endsl, end-proc, end-ds, etc.)
      // These can ONLY close their specific block type AND only if it's the last open block

      // Find which pair this closer belongs to
      const closerPair = RPGLE_BLOCK_PAIRS.find(p => {
        if (!p.close.includes(word)) return false;
        // For single-closer pairs, always match
        if (p.close.length === 1) return true;
        // For multi-closer pairs, match only the specific (non-generic) closer
        const specificClosers = p.close.filter(c => c !== 'end' && c !== 'enddo');
        return specificClosers.includes(word);
      });

      if (closerPair && stack.length > 0) {
        // For specific closers, search the stack for a matching block type
        // This maintains stack integrity by removing the block even when nesting is incorrect
        // The validation of correctness happens in validateClosingKeyword
        for (let j = stack.length - 1; j >= 0; j--) {
          if (stack[j].pair === closerPair) {
            stack.splice(j, 1);
            break;
          }
        }
      }
    }
  }

  // Find the most recent unclosed block that can be closed by the closing word
  if (closeWord === 'end' || closeWord === 'enddo') {
    // Generic closers: find any block that accepts this closer
    for (let j = stack.length - 1; j >= 0; j--) {
      if (stack[j].pair.close.includes(closeWord)) {
        return stack[j].index;
      }
    }
  } else {
    // Specific closers: can ONLY close the last block if it's of the correct type
    const closerPair = RPGLE_BLOCK_PAIRS.find(p => {
      if (!p.close.includes(closeWord)) return false;
      // For single-closer pairs, always match
      if (p.close.length === 1) return true;
      // For multi-closer pairs, match only the specific (non-generic) closer
      const specificClosers = p.close.filter(c => c !== 'end' && c !== 'enddo');
      return specificClosers.includes(closeWord);
    });

    if (closerPair && stack.length > 0) {
      // Check if the last block is of the correct type
      const lastBlock = stack[stack.length - 1];
      if (lastBlock.pair === closerPair) {
        return lastBlock.index;
      }
      // If not, this is an error (no matching opener)
    }
  }

  return -1;
}

// Find all indices of keywords belonging to the same block
function findBlockIndices(
  matches: { offset: number; word: string; length: number }[],
  currentIndex: number,
  pair: BracketPair
): number[] | undefined {
  const currentWord = matches[currentIndex].word;

  // Determine if current keyword is opening, closing, or middle
  const isOpen = pair.open.includes(currentWord);
  const isClose = pair.close.includes(currentWord);
  const isMiddle = pair.middle?.includes(currentWord);

  let openIndex = -1;
  let closeIndex = -1;

  if (isOpen) {
    // If opening, find matching close
    openIndex = currentIndex;
    closeIndex = findMatchingClose(matches, currentIndex, pair);
  } else if (isClose) {
    // If closing, find matching open
    closeIndex = currentIndex;
    openIndex = findMatchingOpen(matches, currentIndex, pair);
  } else if (isMiddle) {
    // If middle keyword, find both open and close
    openIndex = findMatchingOpen(matches, currentIndex, pair);
    if (openIndex !== -1) {
      closeIndex = findMatchingClose(matches, openIndex, pair);
    }
  }

  if (openIndex === -1 || closeIndex === -1) return undefined;

  // Collect all block indices (open, close, and same-level middle keywords)
  const blockIndices: number[] = [openIndex, closeIndex];

  // Add middle keywords at the same nesting level
  if (pair.middle) {
    let depth = 0;
    for (let i = openIndex; i <= closeIndex; i++) {
      const word = matches[i].word;

      if (pair.open.includes(word)) {
        depth++;
      }

      if (depth === 1 && pair.middle.includes(word) && i !== openIndex && i !== closeIndex) {
        blockIndices.push(i);
      }

      if (pair.close.includes(word)) {
        depth--;
      }
    }
  }

  return blockIndices.sort((a, b) => a - b);
}

// Find closing keyword index for an opening keyword
function findMatchingClose(
  matches: { offset: number; word: string; length: number }[],
  openIndex: number,
  pair: BracketPair
): number {
  // Track ALL open blocks, not just the same type
  const stack: BracketPair[] = [];
  stack.push(pair); // Our target block

  for (let i = openIndex + 1; i < matches.length; i++) {
    const word = matches[i].word;

    // Check if this word opens any block
    const openingPair = RPGLE_BLOCK_PAIRS.find(p => p.open.includes(word));
    if (openingPair) {
      stack.push(openingPair);
      continue;
    }

    // Check if this word closes a block
    // For 'END', it closes the most recent block that accepts 'end' as closer
    if (word === 'end') {
      // Find the most recent block that can be closed by 'END'
      for (let j = stack.length - 1; j >= 0; j--) {
        if (stack[j].close.includes('end')) {
          if (j === 0) {
            // This END closes our target block
            return i;
          }
          // Remove this block from stack
          stack.splice(j, 1);
          break;
        }
      }
    } else {
      // Specific closing keyword (endif, enddo, etc.)
      // Find the most recent block that can be closed by this keyword
      for (let j = stack.length - 1; j >= 0; j--) {
        if (stack[j].close.includes(word)) {
          if (j === 0) {
            // This closes our target block
            return i;
          }
          // Remove this block from stack
          stack.splice(j, 1);
          break;
        }
      }
    }
  }

  return -1;
}

// Find opening keyword index for a closing or middle keyword
function findMatchingOpen(
  matches: { offset: number; word: string; length: number }[],
  startIndex: number,
  pair: BracketPair
): number {
  const startWord = matches[startIndex].word;

  // Special handling for 'END' - find the most recent compatible opening
  if (startWord === 'end') {
    // Build a stack to track all open blocks
    const stack: { index: number; pair: BracketPair }[] = [];

    for (let i = 0; i < startIndex; i++) {
      const word = matches[i].word;

      // Check if this word opens any block
      const openingPair = RPGLE_BLOCK_PAIRS.find(p => p.open.includes(word));
      if (openingPair) {
        stack.push({ index: i, pair: openingPair });
      }

      // Check if this word closes a block
      // Find the most recent block that can be closed by this keyword
      for (let j = stack.length - 1; j >= 0; j--) {
        if (stack[j].pair.close.includes(word)) {
          stack.splice(j, 1);
          break;
        }
      }
    }

    // Find the most recent unclosed block that can be closed by 'END'
    for (let j = stack.length - 1; j >= 0; j--) {
      if (stack[j].pair.close.includes('end')) {
        return stack[j].index;
      }
    }

    return -1;
  }

  // Standard logic for specific closing keywords (endif, enddo, etc.)
  let depth = 1;

  for (let i = startIndex - 1; i >= 0; i--) {
    const word = matches[i].word;

    if (pair.close.includes(word)) {
      depth++;
    } else if (pair.open.includes(word)) {
      depth--;
      if (depth === 0) {
        return i;
      }
    }
  }

  return -1;
}

export function deactivateBracketMatcher() {
  decorationType.dispose();
  errorDecorationType.dispose();
}