export interface BlockPair {
  open: string[];
  close: string[];
  middle?: string[];
}

export const RPGLE_BLOCK_PAIRS: BlockPair[] = [
  { open: ['if', 'ifeq', 'ifne', 'ifgt', 'iflt', 'ifge', 'ifle'], close: ['endif','end'], middle: ['else', 'elseif'] },
  { open: ['dow', 'doweq', 'downe', 'dowgt', 'dowlt', 'dowge', 'dowle'], close: ['enddo','end'] },
  { open: ['dou', 'doueq', 'doune', 'dougt', 'doult', 'douge', 'doule'], close: ['enddo','end'] },
  { open: ['do'], close: ['enddo','end'] },
  { open: ['for', 'for-each'], close: ['endfor','end'] },
  { open: ['select'], close: ['endsl','end'], middle: ['when', 'wheneq', 'whenne', 'whengt', 'whenlt', 'whenge', 'whenle', 'when-is', 'when-in', 'other'] },
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
    if (matchWord === 'select' && isInSqlBlock(text, match.index)) continue;
    if (matchWord === 'for' && isInSqlBlock(text, match.index)) continue;
    
    matches.push({
      offset: match.index,
      word: matchWord,
      length: match[0].length
    });
  }
  
  return matches;
}
