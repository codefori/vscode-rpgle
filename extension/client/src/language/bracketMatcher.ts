import * as vscode from 'vscode';
import { isInSqlBlock, isInCommentOrString } from '../utils/sqlDetection';

// Defines block structure with opening, closing, and optional middle keywords
interface BracketPair {
  open: string[];
  close: string[];
  middle?: string[]; // Middle keywords like else, elseif, when, other
}

// RPGLE block structures for bracket matching
const RPGLE_BRACKET_PAIRS: BracketPair[] = [
  { open: ['if'], close: ['endif', 'end'], middle: ['else', 'elseif'] },
  { open: ['dow'], close: ['enddo', 'end'] },
  { open: ['dou'], close: ['enddo', 'end'] },
  { open: ['for', 'for-each'], close: ['endfor', 'end'] },
  { open: ['select'], close: ['endsl', 'end'], middle: ['when', 'when-is', 'when-in', 'other'] },
  { open: ['monitor'], close: ['endmon'], middle: ['on-error', 'on-excp'] },
  { open: ['dcl-proc'], close: ['end-proc'] },
  { open: ['dcl-ds'], close: ['end-ds'] },
  { open: ['dcl-pr'], close: ['end-pr'] },
  { open: ['dcl-pi'], close: ['end-pi'] },
  { open: ['dcl-enum'], close: ['end-enum'] },
  { open: ['begsr'], close: ['endsr'] }
];

// Highlight style for matched brackets
const decorationType = vscode.window.createTextEditorDecorationType({
  backgroundColor: 'rgba(255, 255, 0, 0.2)', // Light yellow with transparency
  border: '1px solid rgba(255, 200, 0, 0.6)', // Darker yellow border
  borderRadius: '3px',
  fontWeight: 'bold', // Make text bold
  fontStyle: 'italic' // Make text italic
});

let currentBlockInfo: { startLine: number; endLine: number; ranges: vscode.Range[]; blockType: string; condition: string } | undefined;

// Register bracket matching functionality
export function registerBracketMatcher(context: vscode.ExtensionContext) {
  let timeout: any = undefined;

  // Register hover provider to show block info
  const hoverProvider = vscode.languages.registerHoverProvider('rpgle', {
    provideHover(document, position) {
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
  
  // Get word at cursor position
  const wordRange = document.getWordRangeAtPosition(position, /[a-zA-Z][\w-]*/);
  if (!wordRange) {
    editor.setDecorations(decorationType, []);
    currentBlockInfo = undefined;
    return;
  }

  const word = document.getText(wordRange).toLowerCase();
  
  // Check if we're clicking on SELECT inside an SQL block
  if (word === 'select') {
    const text = document.getText();
    const offset = document.offsetAt(position);
    if (isInSqlBlock(text, offset)) {
      // Don't highlight SELECT inside SQL blocks
      editor.setDecorations(decorationType, []);
      currentBlockInfo = undefined;
      return;
    }
  }
  
  // Find matching bracket pair
  // Special handling for 'END' - need to determine which pair it belongs to
  let matchingPair: BracketPair | undefined;
  
  if (word === 'end') {
    // For END, we need to find which block it actually closes
    const text = document.getText();
    const allMatches = findAllMatches(text, document);
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
      // Use findMatchingOpen to determine which block this END closes
      const openIndex = findMatchingOpenForEnd(allMatches, currentIndex);
      if (openIndex !== -1) {
        const openWord = allMatches[openIndex].word;
        matchingPair = RPGLE_BRACKET_PAIRS.find(p => p.open.includes(openWord));
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

  // Find all related keywords in the block
  const relatedRanges = findAllRelatedKeywords(document, wordRange, matchingPair);

  if (relatedRanges.length > 0) {
    // Highlight all related keywords
    editor.setDecorations(decorationType, relatedRanges);
    
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
  return RPGLE_BRACKET_PAIRS.find(pair =>
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

// Helper function to find all keyword matches in the document
function findAllMatches(text: string, document: vscode.TextDocument): { offset: number; word: string; length: number }[] {
  // Build regex for all block keywords from all pairs
  const allKeywords: string[] = [];
  RPGLE_BRACKET_PAIRS.forEach(pair => {
    allKeywords.push(...pair.open, ...pair.close);
    if (pair.middle) {
      allKeywords.push(...pair.middle);
    }
  });
  
  const regex = new RegExp(`\\b(${allKeywords.join('|')})\\b`, 'gi');
  const matches: { offset: number; word: string; length: number }[] = [];
  
  let match;
  regex.lastIndex = 0;
  while ((match = regex.exec(text)) !== null) {
    const matchWord = match[0].toLowerCase();
    
    // Skip matches inside comments or strings
    if (isInCommentOrString(text, match.index)) continue;
    
    // Skip SELECT if inside SQL block
    if (matchWord === 'select' && isInSqlBlock(text, match.index)) continue;
    
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
  // Build a stack to track all open blocks
  const stack: { index: number; pair: BracketPair }[] = [];
  
  for (let i = 0; i < endIndex; i++) {
    const word = matches[i].word;
    
    // Check if this word opens any block
    const openingPair = RPGLE_BRACKET_PAIRS.find(p => p.open.includes(word));
    if (openingPair) {
      stack.push({ index: i, pair: openingPair });
    }
    
    // Check if this word closes a block
    const closingPair = RPGLE_BRACKET_PAIRS.find(p => p.close.includes(word));
    if (closingPair) {
      // Remove the most recent matching open block from stack
      for (let j = stack.length - 1; j >= 0; j--) {
        if (stack[j].pair === closingPair) {
          stack.splice(j, 1);
          break;
        }
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
    const openingPair = RPGLE_BRACKET_PAIRS.find(p => p.open.includes(word));
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
      const closingPair = RPGLE_BRACKET_PAIRS.find(p => p.close.includes(word));
      if (closingPair) {
        // Find the most recent block of this type
        for (let j = stack.length - 1; j >= 0; j--) {
          if (stack[j] === closingPair) {
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
      const openingPair = RPGLE_BRACKET_PAIRS.find(p => p.open.includes(word));
      if (openingPair) {
        stack.push({ index: i, pair: openingPair });
      }
      
      // Check if this word closes a block
      const closingPair = RPGLE_BRACKET_PAIRS.find(p => p.close.includes(word));
      if (closingPair) {
        // Remove the most recent matching open block from stack
        for (let j = stack.length - 1; j >= 0; j--) {
          if (stack[j].pair === closingPair) {
            stack.splice(j, 1);
            break;
          }
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
}