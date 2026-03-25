import { FoldingRange, FoldingRangeParams, FoldingRangeKind } from 'vscode-languageserver';
import { documents } from '.';
import Document from '../../../../language/document';

// Defines opening and closing keywords for code blocks
interface BlockPair {
  open: string[];
  close: string[];
}

// RPGLE block structures that can be folded
const RPGLE_BLOCK_PAIRS: BlockPair[] = [
  { open: ['if'], close: ['endif'] },
  { open: ['dow'], close: ['enddo'] },
  { open: ['dou'], close: ['enddo'] },
  { open: ['for', 'for-each'], close: ['endfor'] },
  { open: ['select'], close: ['endsl'] },
  { open: ['monitor'], close: ['endmon'] },
  { open: ['dcl-proc'], close: ['end-proc'] },
  { open: ['dcl-ds'], close: ['end-ds'] },
  { open: ['dcl-pr'], close: ['end-pr'] },
  { open: ['dcl-pi'], close: ['end-pi'] },
  { open: ['dcl-enum'], close: ['end-enum'] },
  { open: ['begsr'], close: ['endsr'] }
];

// Provides folding ranges for RPGLE code blocks
export default function foldingRangeProvider(params: FoldingRangeParams): FoldingRange[] {
  const document = documents.get(params.textDocument.uri);
  if (!document) return [];

  const text = document.getText();
  const doc = new Document(text);
  const foldingRanges: FoldingRange[] = [];

  // Build regex pattern for all block keywords
  const allKeywords: string[] = [];
  RPGLE_BLOCK_PAIRS.forEach(pair => {
    allKeywords.push(...pair.open, ...pair.close);
  });
  const regex = new RegExp(`\\b(${allKeywords.join('|')})\\b`, 'gi');

  // Find all keyword matches in the document
  interface Match {
    offset: number;
    word: string;
    line: number;
  }
  
  const matches: Match[] = [];
  let match;
  regex.lastIndex = 0;
  
  while ((match = regex.exec(text)) !== null) {
    // Skip matches inside comments
    if (!isInComment(text, match.index)) {
      const line = document.positionAt(match.index).line;
      matches.push({
        offset: match.index,
        word: match[0].toLowerCase(),
        line: line
      });
    }
  }

  // Track open blocks using a stack
  interface OpenBlock {
    pair: BlockPair;
    startLine: number;
    matchIndex: number;
  }
  
  const stack: OpenBlock[] = [];

  // Process matches to find matching pairs
  for (let i = 0; i < matches.length; i++) {
    const current = matches[i];
    
    // Find the corresponding block pair
    const pair = RPGLE_BLOCK_PAIRS.find(p => 
      p.open.includes(current.word) || p.close.includes(current.word)
    );
    
    if (!pair) continue;

    if (pair.open.includes(current.word)) {
      // Opening keyword - push to stack
      stack.push({
        pair: pair,
        startLine: current.line,
        matchIndex: i
      });
    } else if (pair.close.includes(current.word)) {
      // Closing keyword - find matching opener in stack
      for (let j = stack.length - 1; j >= 0; j--) {
        if (stack[j].pair === pair) {
          const openBlock = stack[j];
          
          // Create folding range only if block spans multiple lines
          if (current.line > openBlock.startLine) {
            foldingRanges.push(FoldingRange.create(
              openBlock.startLine,
              current.line,
              undefined,
              undefined,
              FoldingRangeKind.Region
            ));
          }
          
          // Remove matched block from stack
          stack.splice(j, 1);
          break;
        }
      }
    }
  }

  return foldingRanges;
}

// Check if a position is inside a comment or string
function isInComment(text: string, offset: number): boolean {
  // Find the start of the line
  let lineStart = offset;
  while (lineStart > 0 && text[lineStart - 1] !== '\n' && text[lineStart - 1] !== '\r') {
    lineStart--;
  }
  
  // Extract line content before the offset
  const lineBeforeOffset = text.substring(lineStart, offset);
  
  // Check if there's a comment marker before this position
  const commentIndex = lineBeforeOffset.indexOf('//');
  if (commentIndex !== -1) {
    return true;
  }
  
  // Check if the offset is inside a string delimited by single quotes
  let inString = false;
  for (let i = 0; i < lineBeforeOffset.length; i++) {
    if (lineBeforeOffset[i] === "'") {
      inString = !inString;
    }
  }
  
  if (inString) {
    return true;
  }
  
  return false;
}