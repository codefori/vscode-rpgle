import { FoldingRange, FoldingRangeParams, FoldingRangeKind } from 'vscode-languageserver';
import { documents } from '.';
import Document from '../../../../language/ile/document';
import { isInSqlBlock, isInCommentOrString } from '../../../../language/utils/sqlDetection';
import { RPGLE_BLOCK_PAIRS, BlockPair } from '../../../../language/utils/blockParser';

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
  
  // Sort keywords by length (longest first) to match longer keywords before shorter ones
  // This ensures 'end-proc' is matched before 'end'
  const sortedKeywords = allKeywords.sort((a, b) => b.length - a.length);
  
  // Escape hyphens in keywords for regex
  const regex = new RegExp(`\\b(${sortedKeywords.map(k => k.replace(/-/g, '\\-')).join('|')})\\b`, 'gi');

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
      } else if (current.word === 'enddo') {
        // ENDDO can close DOW, DOU, or DO blocks
        for (let j = stack.length - 1; j >= 0; j--) {
          if (stack[j].pair.close.includes('enddo')) {
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
        // Specific closing keyword (endif, endfor, endsl, end-proc, end-ds, etc.)
        // These can ONLY close their specific block type AND only if it's the last open block
        
        // Find which pair this specific closer belongs to
        const closerPair = RPGLE_BLOCK_PAIRS.find(p => {
          if (!p.close.includes(current.word)) return false;
          // For single-closer pairs, always match
          if (p.close.length === 1) return true;
          // For multi-closer pairs, match only the specific (non-generic) closer
          const specificClosers = p.close.filter(c => c !== 'end' && c !== 'enddo');
          return specificClosers.includes(current.word);
        });
        
        if (closerPair && stack.length > 0) {
          // A specific closer can ONLY close the last block if it's of the correct type
          const lastBlock = stack[stack.length - 1];
          if (lastBlock.pair === closerPair) {
            // Create folding range only if block spans multiple lines
            if (current.line > lastBlock.startLine) {
              foldingRanges.push(FoldingRange.create(
                lastBlock.startLine,
                current.line,
                undefined,
                undefined,
                FoldingRangeKind.Region
              ));
            }
            
            // Remove matched block from stack
            stack.pop();
          }
          // If it's not the correct type, don't remove anything (it's an error - skip this closer)
        }
      }
    }
  }

  return foldingRanges;
}
