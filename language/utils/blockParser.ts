export interface BlockPair {
  open: string[];
  close: string[];
  middle?: string[];
}

export const RPGLE_BLOCK_PAIRS: BlockPair[] = [
  { open: ['if', 'ifeq', 'ifne', 'ifgt', 'iflt', 'ifge', 'ifle'], close: ['endif', 'end'], middle: ['else', 'elseif'] },
  { open: ['dow', 'doweq', 'downe', 'dowgt', 'dowlt', 'dowge', 'dowle'], close: ['enddo', 'end'] },
  { open: ['dou', 'doueq', 'doune', 'dougt', 'doult', 'douge', 'doule'], close: ['enddo', 'end'] },
  { open: ['do'], close: ['enddo', 'end'] },
  { open: ['for', 'for-each'], close: ['endfor', 'end'] },
  { open: ['select'], close: ['endsl', 'end'], middle: ['when', 'wheneq', 'whenne', 'whengt', 'whenlt', 'whenge', 'whenle', 'when-is', 'when-in', 'other'] },
  { open: ['monitor'], close: ['endmon'], middle: ['on-error', 'on-excp'] },
  { open: ['dcl-proc'], close: ['end-proc'] },
  { open: ['dcl-ds'], close: ['end-ds'] },
  { open: ['dcl-pr'], close: ['end-pr'] },
  { open: ['dcl-pi'], close: ['end-pi'] },
  { open: ['dcl-enum'], close: ['end-enum'] },
  { open: ['begsr'], close: ['endsr'] },
  { open: ['casxx', 'caseq', 'casne', 'casgt', 'caslt', 'casge', 'casle'], close: ['endcs'] },
];

export interface BlockMatch {
  offset: number;
  word: string;
  length: number;
}

export function findAllBlockMatches(
  text: string,
  isInCommentOrString: (text: string, offset: number) => boolean,
  isInSqlBlock: (text: string, offset: number) => boolean
): BlockMatch[] {
  // Helper function to check if a position is in a compiler directive
  const isInCompilerDirective = (text: string, offset: number): boolean => {
    // Find the start of the line
    let lineStart = offset;
    while (lineStart > 0 && text[lineStart - 1] !== '\n' && text[lineStart - 1] !== '\r') {
      lineStart--;
    }

    // Get the text from line start to the offset
    const lineBeforeOffset = text.substring(lineStart, offset + 1);

    // Check if the line starts with / (compiler directive)
    return /^\s*\//.test(lineBeforeOffset);
  };

  // Helper function to check if a keyword is actually being used as a variable
  const isVariableContext = (text: string, matchOffset: number, matchLength: number): boolean => {
    const afterKeyword = text.substring(matchOffset + matchLength);
    const beforeKeyword = text.substring(0, matchOffset);

    // Find the start of the current line
    let lineStart = beforeKeyword.lastIndexOf('\n');
    if (lineStart === -1) lineStart = 0;
    else lineStart++; // Move past the newline

    // Get the current line
    const currentLine = text.substring(lineStart, beforeKeyword.length + matchLength + 50);
    
    // Check if the current line is a comment (starts with // or * in column 7+)
    // Also check for /FREE to /END-FREE compiler directives
    if (/^\s*\/\//.test(currentLine) || /^\s*\*/.test(currentLine)) {
      // Current line is a comment - don't treat keyword as anything special
      return false;
    }

    // Check if preceded by declaration keywords (dcl-s, dcl-c, dcl-pr, dcl-proc, dcl-pi, etc.)
    // ALL dcl- keywords indicate the next word is an identifier, not a keyword
    const declMatch = beforeKeyword.match(/\b(dcl-[a-z]+)\s+$/i);
    if (declMatch) {
      return true;
    }

    // Check what comes after the keyword (skipping whitespace)
    const afterMatch = afterKeyword.match(/^\s*(.)/);
    if (!afterMatch) return false;

    const nextChar = afterMatch[1];

    // If followed by closing paren or comma, it's likely a variable in expression/parameter
    if (nextChar === ')' || nextChar === ',') {
      return true;
    }

    // If followed by assignment operators, it's a variable
    if (nextChar === '=' || nextChar === '+' || nextChar === '-' || nextChar === '*' || nextChar === '/') {
      const twoChars = afterKeyword.substring(afterMatch[0].length - 1, afterMatch[0].length + 1);
      if (twoChars === '+=' || twoChars === '-=' || twoChars === '*=' || twoChars === '/=' || nextChar === '=') {
        return true;
      }
    }

    // Check if at the start of a line (preceded only by whitespace and newlines)
    // If so, it's a statement keyword, NOT a variable in an expression
    const lineStartMatch = beforeKeyword.match(/[\n\r]\s*$/);
    if (lineStartMatch) {
      // Keyword is at the start of a line → it's a statement keyword
      return false;
    }

    // Check what comes BEFORE the keyword - look for comparison operators or expression contexts
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
        return true;
      } else {
        // Not preceded by operator context → it's a control flow keyword like if (condition)
        return false;
      }
    }

    // Check for other operator contexts
    if (beforeMatch) {
      return true;
    }

    return false;
  };

  const allKeywords: string[] = [];
  RPGLE_BLOCK_PAIRS.forEach(pair => {
    allKeywords.push(...pair.open, ...pair.close);
  });

  const regex = new RegExp(`\\b(${allKeywords.join('|')})\\b`, 'gi');
  const matches: BlockMatch[] = [];

  let match;
  regex.lastIndex = 0;
  while ((match = regex.exec(text)) !== null) {
    const matchWord = match[0].toLowerCase();

    if (isInCommentOrString(text, match.index)) continue;
    // Skip compiler directives (e.g., /END-FREE, /FREE, /COPY)
    if (isInCompilerDirective(text, match.index)) continue;
    if (matchWord === 'select' && isInSqlBlock(text, match.index)) continue;
    if (matchWord === 'for' && isInSqlBlock(text, match.index)) continue;

    // Skip keywords that are actually variables in expression/assignment context
    // Only check for simple keywords (no hyphens) that could be valid variable names
    if (!matchWord.includes('-')) {
      if (isVariableContext(text, match.index, match[0].length)) {
        continue;
      }
    }

    matches.push({
      offset: match.index,
      word: matchWord,
      length: match[0].length
    });
  }

  return matches;
}
