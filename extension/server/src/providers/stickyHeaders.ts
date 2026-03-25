import { DocumentSymbolParams, DocumentSymbol, SymbolKind } from 'vscode-languageserver';
import { documents } from '.';
import Document from '../../../../language/document';
import { isInSqlBlock, isInCommentOrString } from '../utils/sqlDetection';

// Provides sticky header context for RPGLE code blocks
export default function stickyHeadersProvider(params: DocumentSymbolParams): DocumentSymbol[] {
  const document = documents.get(params.textDocument.uri);
  if (!document) return [];

  const text = document.getText();
  const doc = new Document(text);
  const symbols: DocumentSymbol[] = [];

  // RPGLE block structures that should appear in sticky headers
  const blockPatterns = [
    { pattern: /\b(dcl-proc)\s+(\w+)/gi, kind: SymbolKind.Function },
    { pattern: /\b(begsr)\s+(\w+)/gi, kind: SymbolKind.Function },
    { pattern: /\b(if)\s+(.+?)(?:;|$)/gi, kind: SymbolKind.Null },
    { pattern: /\b(dow|dou)\s+(.+?)(?:;|$)/gi, kind: SymbolKind.Null },
    { pattern: /\b(for|for-each)\s+(.+?)(?:;|$)/gi, kind: SymbolKind.Null },
    { pattern: /\b(select)\b/gi, kind: SymbolKind.Null },
    { pattern: /\b(monitor)\b/gi, kind: SymbolKind.Null },
    { pattern: /\b(dcl-ds)\s+(\w+)/gi, kind: SymbolKind.Struct },
  ];

  const lines = text.split('\n');
  
  for (let lineNum = 0; lineNum < lines.length; lineNum++) {
    const line = lines[lineNum];
    
    // Skip comments
    if (isCommentLine(line)) continue;

    for (const { pattern, kind } of blockPatterns) {
      pattern.lastIndex = 0;
      const match = pattern.exec(line);
      
      if (match) {
        // Check if the match is inside a string
        if (isInString(line, match.index)) continue;
        
        const keyword = match[1].toLowerCase();
        
        // Skip SELECT if it's inside an SQL block
        if (keyword === 'select' && isInSqlBlock(text, document.offsetAt({ line: lineNum, character: match.index }))) {
          continue;
        }
        
        const name = match[2] || '';
        
        // Build display name
        let displayName = keyword.toUpperCase();
        if (name) {
          displayName += ` ${name}`;
        } else if (match[0].length > keyword.length) {
          // Include condition for control structures
          const condition = match[0].substring(keyword.length).trim();
          if (condition && condition !== ';') {
            displayName += ` ${condition.replace(/;$/, '')}`;
          }
        }

        const startPos = document.positionAt(document.offsetAt({ line: lineNum, character: 0 }));
        const endPos = document.positionAt(document.offsetAt({ line: lineNum, character: line.length }));

        symbols.push(DocumentSymbol.create(
          displayName,
          undefined,
          kind,
          { start: startPos, end: endPos },
          { start: startPos, end: endPos }
        ));
        
        break; // Only match one pattern per line
      }
    }
  }

  return symbols;
}

// Check if a line is a comment
function isCommentLine(line: string): boolean {
  const trimmed = line.trim();
  return trimmed.startsWith('//') || trimmed.startsWith('*');
}

// Check if a position in a line is inside a string
function isInString(line: string, position: number): boolean {
  let inString = false;
  for (let i = 0; i < position; i++) {
    if (line[i] === "'") {
      inString = !inString;
    }
  }
  return inString;
}