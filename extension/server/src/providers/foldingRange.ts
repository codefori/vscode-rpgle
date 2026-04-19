import { FoldingRange, FoldingRangeParams, FoldingRangeKind } from 'vscode-languageserver';
import { documents } from '.';
import Document from '../../../../language/document';
import { isInSqlBlock, isInCommentOrString } from '../utils/sqlDetection';

// Defines opening and closing keywords for code blocks
interface BlockPair {
  open: string[];
  close: string[];
}

// RPGLE block structures that can be folded
const RPGLE_BLOCK_PAIRS: BlockPair[] = [
  { open: ['if', 'ifeq', 'ifne', 'ifgt', 'iflt', 'ifge', 'ifle'], close: ['endif','end'] },
  { open: ['dow', 'doweq', 'downe', 'dowgt', 'dowlt', 'dowge', 'dowle'], close: ['enddo','end'] },
  { open: ['dou', 'doueq', 'doune', 'dougt', 'doult', 'douge', 'doule'], close: ['enddo','end'] },
  { open: ['do'], close: ['enddo','end'] },
  { open: ['for', 'for-each'], close: ['endfor','end'] },
  { open: ['select'], close: ['endsl','end'] },
  { open: ['monitor'], close: ['endmon'] },
  { open: ['dcl-proc'], close: ['end-proc'] },
  { open: ['dcl-ds'], close: ['end-ds'] },
  { open: ['dcl-pr'], close: ['end-pr'] },
  { open: ['dcl-pi'], close: ['end-pi'] },
  { open: ['dcl-enum'], close: ['end-enum'] },
  { open: ['begsr'], close: ['endsr'] },
  { open: ['casxx', 'caseq', 'casne', 'casgt', 'caslt', 'casge', 'casle'], close: ['endcs'] },
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
    const matchWord = match[0].toLowerCase();
    
    // Skip matches inside comments or strings
    if (isInCommentOrString(text, match.index)) continue;
    
    // Skip SELECT if inside SQL block
    if (matchWord === 'select' && isInSqlBlock(text, match.index)) continue;
    
    const line = document.positionAt(match.index).line;
    matches.push({
      offset: match.index,
      word: matchWord,
      line: line
    });
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
      // Special handling for 'END' - it can close any compatible block
      if (current.word === 'end') {
        // Find the most recent block that can be closed by 'END'
        for (let j = stack.length - 1; j >= 0; j--) {
          // Check if 'END' is a valid closer for this block
          if (stack[j].pair.close.includes('end')) {
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
      } else {
        // Specific closing keyword (endif, enddo, etc.) - match exact pair
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
  }

  return foldingRanges;
}
