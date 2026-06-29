import * as vscode from 'vscode';
import { isInSqlBlock, isInCommentOrString } from '../../../../language/utils/sqlDetection';
import { RPGLE_BLOCK_PAIRS, BlockPair, BlockMatch } from '../../../../language/utils/blockParser';
import * as rpgle from '../rpgtools-comment-helpers';

type BracketPair = BlockPair;

// Configuration keys for bracket matching features
const CONFIG_KEY = 'bracketHighlightingEnabled';
const MISMATCH_STYLE_KEY = 'bracketMismatchStyle';
const JUMP_ENABLED_KEY = 'bracketJumpEnabled';

// Cache infrastructure for document analysis
interface CacheEntry {
  version: number;
  text: string;
  matches: BlockMatch[];
  errorRanges: { range: vscode.Range; keyword: string }[];
  // O(1) lookup maps built at preload time so cursor moves never re-scan
  matchIndexByOffset: Map<number, number>;     // offset → index in matches[]
  blockIndicesByMatch: Map<number, number[]>;  // match index → block indices
}

const analysisCache = new Map<string, CacheEntry>();

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

let decorationType: vscode.TextEditorDecorationType | undefined;

// Highlight style for mismatched closing keywords (errors)
let errorDecorationType: vscode.TextEditorDecorationType | undefined;

// Factory function to create error decoration type based on setting
function createErrorDecorationType(): vscode.TextEditorDecorationType {
  const config = vscode.workspace.getConfiguration('vscode-rpgle');
  const style = config.get<string>(MISMATCH_STYLE_KEY, 'box');

  if (style === 'underline') {
    return vscode.window.createTextEditorDecorationType({
      textDecoration: 'wavy underline red'
    });
  } else {
    // Default 'box' style
    return vscode.window.createTextEditorDecorationType({
      backgroundColor: 'rgba(255, 0, 0, 0.3)', // Light red with transparency
      border: '2px solid rgba(255, 0, 0, 0.8)', // Red border
      borderRadius: '3px',
      fontWeight: 'bold',
      textDecoration: 'wavy underline red'
    });
  }
}

let currentBlockInfo: { startLine: number; endLine: number; ranges: vscode.Range[]; blockType: string; condition: string } | undefined;
let currentErrorRanges: { range: vscode.Range; keyword: string }[] = [];

// Store disposables for cleanup
let bracketMatcherDisposables: vscode.Disposable[] = [];

// Register bracket matching functionality
export function registerBracketMatcher(context: vscode.ExtensionContext) {
  // Listen for configuration changes (always register this listener)
  const configChangeDisposable = vscode.workspace.onDidChangeConfiguration(e => {
    if (e.affectsConfiguration('vscode-rpgle.' + CONFIG_KEY)) {
      const newConfig = vscode.workspace.getConfiguration('vscode-rpgle');
      const newEnabled = newConfig.get<boolean>(CONFIG_KEY, true);

      if (!newEnabled) {
        // Feature disabled - dispose and clear decorations
        disposeBracketMatcher();
      } else {
        // Feature enabled - activate if not already active
        if (bracketMatcherDisposables.length === 0) {
          activateBracketMatcher();
        }
      }
    }
  });
  context.subscriptions.push(configChangeDisposable);

  // Check if feature is enabled initially
  const config = vscode.workspace.getConfiguration('vscode-rpgle');
  const isEnabled = config.get<boolean>(CONFIG_KEY, true);

  if (isEnabled) {
    activateBracketMatcher();
  }
}

// Activate the bracket matching feature
function activateBracketMatcher() {
  // Create decoration types if they don't exist
  if (!decorationType) {
    decorationType = vscode.window.createTextEditorDecorationType({
      backgroundColor: 'rgba(255, 255, 0, 0.2)', // Light yellow with transparency
      border: '1px solid rgba(255, 200, 0, 0.6)', // Darker yellow border
      borderRadius: '3px',
      fontWeight: 'bold', // Make text bold
      fontStyle: 'italic' // Make text italic
    });
  }

  if (!errorDecorationType) {
    errorDecorationType = createErrorDecorationType();
  }

  // Listen for mismatch style configuration changes
  const styleChangeDisposable = vscode.workspace.onDidChangeConfiguration(e => {
    if (e.affectsConfiguration('vscode-rpgle.' + MISMATCH_STYLE_KEY)) {
      // Dispose old decoration type and create new one
      if (errorDecorationType) {
        errorDecorationType.dispose();
        errorDecorationType = createErrorDecorationType();
      }
      // Update all visible editors
      vscode.window.visibleTextEditors.forEach(editor => {
        if (editor.document.languageId === 'rpgle') {
          updateDecorations(editor);
        }
      });
    }
  });
  bracketMatcherDisposables.push(styleChangeDisposable);

  // Listen for document changes to invalidate cache
  const docChangeDisposable = vscode.workspace.onDidChangeTextDocument(event => {
    const docUri = event.document.uri.toString();
    analysisCache.delete(docUri);
  });
  bracketMatcherDisposables.push(docChangeDisposable);

  let timeout: any = undefined;

  // Register hover provider to show block info and error info
  const hoverProvider = vscode.languages.registerHoverProvider('rpgle', {
    provideHover(_document, position) {
      // First check if hovering over an error (unmatched keyword)
      for (const errorInfo of currentErrorRanges) {
        if (errorInfo.range.contains(position)) {
          const keyword = errorInfo.keyword.toUpperCase();
          const markdown = new vscode.MarkdownString();

          // Bold first line, while keeping keyword safely as plain text.
          markdown.appendMarkdown('**Unmatched ');
          markdown.appendText(keyword);
          markdown.appendMarkdown(' statement**\n');
          markdown.appendMarkdown(
            [
              'This ENDxx keyword has no matching opening block.',
              '',
              '- For DS subfields, use **DCL-SUBF**',
              '- For parms, use **DCL-PARM**',
              '- For DCL-DS with either **LIKEDS(...)** or **LIKEREC(...)** an END-DS is **not** allowed.'
            ].join('\n')
          );
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

  bracketMatcherDisposables.push(hoverProvider);

  // Update decorations when selection changes
  const selectionChangeDisposable = vscode.window.onDidChangeTextEditorSelection(event => {
    const editor = event.textEditor;
    if (editor && editor.document.languageId === 'rpgle') {
      if (timeout) {
        clearTimeout(timeout);
      }
      timeout = setTimeout(() => updateDecorations(editor), 100);
    }
  });
  bracketMatcherDisposables.push(selectionChangeDisposable);

  // Update decorations when active editor changes
  const editorChangeDisposable = vscode.window.onDidChangeActiveTextEditor(editor => {
    if (editor && editor.document.languageId === 'rpgle') {
      updateDecorations(editor);
    }
  });
  bracketMatcherDisposables.push(editorChangeDisposable);

  // Initialize for current editor (defer off activation path)
  setTimeout(() => {
    if (vscode.window.activeTextEditor && vscode.window.activeTextEditor.document.languageId === 'rpgle') {
      updateDecorations(vscode.window.activeTextEditor);
    }

    // Preload bracket analysis for all currently visible RPGLE documents
    // Runs after activation completes so it doesn't block the extension host
    vscode.window.visibleTextEditors.forEach(editor => {
      if (editor.document.languageId === 'rpgle') {
        preloadCache(editor.document);
      }
    });
  }, 0);

  // Also preload when any new document is opened (deferred to avoid blocking)
  const openDocDisposable = vscode.workspace.onDidOpenTextDocument(document => {
    if (document.languageId === 'rpgle') {
      setTimeout(() => preloadCache(document), 0);
    }
  });
  bracketMatcherDisposables.push(openDocDisposable);
}

function buildLookupMaps(
  text: string,
  matches: BlockMatch[]
): { matchIndexByOffset: Map<number, number>; blockIndicesByMatch: Map<number, number[]> } {
  const matchIndexByOffset = new Map<number, number>();
  for (let i = 0; i < matches.length; i++) {
    matchIndexByOffset.set(matches[i].offset, i);
  }

  const blockIndicesByMatch = new Map<number, number[]>();
  for (let i = 0; i < matches.length; i++) {
    if (blockIndicesByMatch.has(i)) continue; // already covered by an earlier block traversal

    const matchWord = matches[i].word;
    let pair: BracketPair | undefined;

    const isClose = RPGLE_BLOCK_PAIRS.some(p => p.close.includes(matchWord));
    if (isClose && (matchWord === 'end' || matchWord === 'enddo')) {
      const openIdx = findMatchingOpenForClosing(text, matches, i, matchWord);
      if (openIdx !== -1) {
        pair = RPGLE_BLOCK_PAIRS.find(p => p.open.includes(matches[openIdx].word));
      }
    } else {
      pair = findMatchingPair(matchWord);
    }

    if (!pair) continue;

    const indices = findBlockIndices(text, matches, i, pair);
    if (!indices) continue;

    // Store under every index in the block so opener, closer, and middles all resolve
    for (const idx of indices) {
      if (!blockIndicesByMatch.has(idx)) {
        blockIndicesByMatch.set(idx, indices);
      }
    }
  }

  return { matchIndexByOffset, blockIndicesByMatch };
}

function preloadCache(document: vscode.TextDocument) {
  try {
    const docUri = document.uri.toString();
    if (!analysisCache.has(docUri) || analysisCache.get(docUri)!.version !== document.version) {
      const text = document.getText();
      const matches = findAllMatches(text, document);
      const errorRanges = findAllMismatchedClosingKeywords(document, matches);
      const { matchIndexByOffset, blockIndicesByMatch } = buildLookupMaps(text, matches);
      analysisCache.set(docUri, {
        version: document.version,
        text: text,
        matches: matches,
        errorRanges: errorRanges.map(e => ({ range: e.range, keyword: e.keyword })),
        matchIndexByOffset,
        blockIndicesByMatch
      });
    }
  } catch {
    // Silently ignore errors during background preload — cache miss on first use is acceptable
  }
}

// Dispose of bracket matcher resources
function disposeBracketMatcher() {
  // Clear decorations from all visible editors
  vscode.window.visibleTextEditors.forEach(editor => {
    if (editor.document.languageId === 'rpgle') {
      if (decorationType) {
        editor.setDecorations(decorationType, []);
      }
      if (errorDecorationType) {
        editor.setDecorations(errorDecorationType, []);
      }
    }
  });

  // Dispose all tracked disposables
  bracketMatcherDisposables.forEach(d => d.dispose());
  bracketMatcherDisposables = [];

  // Clear current block info
  currentBlockInfo = undefined;
}

function updateDecorations(editor: vscode.TextEditor) {
  // Check if decorations are initialized (feature is enabled)
  if (!decorationType || !errorDecorationType) {
    return;
  }

  const document = editor.document;
  const position = editor.selection.active;
  const text = document.getText();
  const docUri = document.uri.toString();

  // Never run selection-based block matching/decorations on comment lines.
  if (rpgle.isComment(document.lineAt(position.line).text, document)) {
    editor.setDecorations(decorationType, []);
    currentBlockInfo = undefined;
    return;
  }

  // Check cache for this document
  let allMatches: BlockMatch[];
  let allErrorRangesWithInfo: { range: vscode.Range; keyword: string }[];
  let matchIndexByOffset: Map<number, number>;
  let blockIndicesByMatch: Map<number, number[]>;

  const cached = analysisCache.get(docUri);
  if (cached && cached.version === document.version && cached.text === text) {
    // Cache hit — pure O(1) lookups from here
    allMatches = cached.matches;
    allErrorRangesWithInfo = cached.errorRanges;
    matchIndexByOffset = cached.matchIndexByOffset;
    blockIndicesByMatch = cached.blockIndicesByMatch;
  } else {
    // Cache miss — compute everything and store with lookup maps
    allMatches = findAllMatches(text, document);
    allErrorRangesWithInfo = findAllMismatchedClosingKeywords(document, allMatches);
    const maps = buildLookupMaps(text, allMatches);
    matchIndexByOffset = maps.matchIndexByOffset;
    blockIndicesByMatch = maps.blockIndicesByMatch;
    analysisCache.set(docUri, {
      version: document.version,
      text: text,
      matches: allMatches,
      errorRanges: allErrorRangesWithInfo,
      matchIndexByOffset,
      blockIndicesByMatch
    });
  }

  const allErrorRanges = allErrorRangesWithInfo.map(e => e.range);

  // Store error ranges for hover provider
  currentErrorRanges = allErrorRangesWithInfo;

  // Always show errors in red
  editor.setDecorations(errorDecorationType, allErrorRanges);

  // Get word at cursor position for block highlighting
  // Include RPG IV special characters and international CCSID variants:
  // CCSID 37: #, $, @
  // CCSID 277 (Norwegian/Danish): Æ, æ, Ø, ø
  // CCSID 273 (German): §
  // CCSID 280 (Italian): §, £
  // CCSID 297 (French): £, à, À
  const wordRange = document.getWordRangeAtPosition(position, /[a-zA-Z_#@$§£ÆæØøàÀ][\w#@$§£ÆæØøàÀ-]*/);
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
      editor.setDecorations(decorationType, []);
      currentBlockInfo = undefined;
      return;
    }
  }

  // O(1) lookup: find the match index for the cursor offset
  const cursorOffset = document.offsetAt(wordRange.start);
  const matchIndex = matchIndexByOffset.get(cursorOffset);

  if (matchIndex === undefined) {
    // Cursor is on a word that isn't a tracked block keyword
    editor.setDecorations(decorationType, []);
    currentBlockInfo = undefined;
    return;
  }

  // O(1) lookup: get the precomputed block indices for this keyword
  const blockIndices = blockIndicesByMatch.get(matchIndex);

  if (!blockIndices) {
    // Keyword has no matching block (unmatched, LIKEDS, etc.)
    editor.setDecorations(decorationType, []);
    currentBlockInfo = undefined;
    return;
  }

  // Derive the pair from the opener word (always blockIndices[0])
  const openerWord = allMatches[blockIndices[0]].word;
  const matchingPair = findMatchingPair(openerWord);
  if (!matchingPair) {
    editor.setDecorations(decorationType, []);
    currentBlockInfo = undefined;
    return;
  }

  // Convert precomputed indices to ranges — O(k) where k = block keyword count (typically 2-5)
  const relatedRanges = blockIndices.map(idx => {
    const m = allMatches[idx];
    return new vscode.Range(document.positionAt(m.offset), document.positionAt(m.offset + m.length));
  });

  // Highlight valid keywords in yellow (excluding error ranges)
  const validRanges = relatedRanges.filter(range =>
    !allErrorRanges.some((errorRange: vscode.Range) => errorRange.isEqual(range))
  );
  editor.setDecorations(decorationType, validRanges);

  if (validRanges.length > 0) {
    const blockType = getBlockTypeName(matchingPair);
    const startLine = relatedRanges[0].start.line;
    const endLine = relatedRanges[relatedRanges.length - 1].start.line;
    const condition = extractBlockCondition(document, startLine);
    currentBlockInfo = { startLine, endLine, ranges: relatedRanges, blockType, condition };
  } else {
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
  // Restrict context checks to the current physical line so punctuation in
  // previous comment lines cannot influence keyword classification.
  const lineStart = text.lastIndexOf('\n', matchOffset) + 1;
  const lineEndIndex = text.indexOf('\n', matchOffset);
  const lineEnd = lineEndIndex === -1 ? text.length : lineEndIndex;
  const lineText = text.substring(lineStart, lineEnd);

  const localOffset = matchOffset - lineStart;
  const beforeKeyword = lineText.substring(0, localOffset);
  const afterKeyword = lineText.substring(localOffset + matchLength);

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

  // Handle wrapped declarations where the DCL-* keyword is on the previous line.
  // Example:
  //   dcl-s
  //     end pointer;
  if (/^\s*$/.test(beforeKeyword)) {
    const prevLineEnd = lineStart - 1;
    if (prevLineEnd > 0) {
      const prevLineStart = text.lastIndexOf('\n', prevLineEnd - 1) + 1;
      const prevLineText = text.substring(prevLineStart, prevLineEnd).replace(/\r$/, '');
      const prevLineWithoutComments = stripComments(prevLineText);

      if (/^\s*dcl-[a-z]+\s*$/i.test(prevLineWithoutComments)) {
        return true;
      }
    }

    // Inside an open DCL-DS block, a first token followed by declaration text
    // is typically a subfield name (for example: "end pointer;", "endif pointer;").
    const afterTrimmed = afterKeyword.trimStart();
    if (afterTrimmed.length > 0 &&
      afterTrimmed[0] !== ';' &&
      isInsideOpenDclDsBlock(text, lineStart)) {
      return true;
    }
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
}

function isInsideOpenDclDsBlock(text: string, lineStart: number): boolean {
  const priorText = text.substring(0, lineStart);
  const lines = priorText.split(/\r?\n/);
  let depth = 0;

  for (let i = lines.length - 1; i >= 0; i--) {
    const line = stripComments(lines[i]).trim().toLowerCase();
    if (!line) {
      continue;
    }

    if (/^end-ds\b/.test(line)) {
      depth++;
      continue;
    }

    if (/^dcl-ds\b/.test(line)) {
      // dcl-ds with likeds()/likerec() is a single-line declaration, not a block.
      if (/likeds\s*\(|likerec\s*\(/.test(line)) {
        continue;
      }

      if (depth === 0) {
        return true;
      }

      depth--;
    }
  }

  return false;
}

function findAllMatches(text: string, document: vscode.TextDocument): BlockMatch[] {
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

  // Custom word boundary that accounts for RPG IV special characters and international CCSID variants
  // These characters are valid in RPG IV identifiers, so we need to exclude them from matches
  // Examples: Wrk@End, #endif, §Betrag, £TaxAmt, àFeld should NOT match embedded keywords
  // CCSID 37: #, $, @
  // CCSID 277 (Norwegian/Danish): Æ, æ, Ø, ø
  // CCSID 273 (German): §
  // CCSID 280 (Italian): §, £
  // CCSID 297 (French): £, à, À
  // Pattern: (?<![A-Za-z0-9_#@$§£ÆæØøàÀ-])keyword(?![A-Za-z0-9_#@$§£ÆæØøàÀ-])
  const rpgIdentifierChars = '[A-Za-z0-9_#@$§£ÆæØøàÀ-]';
  const regex = new RegExp(
    `(?<!${rpgIdentifierChars})(${sortedKeywords.map(k => k.replace(/-/g, '\\-')).join('|')})(?!${rpgIdentifierChars})`,
    'gi'
  );
  const matches: BlockMatch[] = [];

  let match;
  regex.lastIndex = 0;
  while ((match = regex.exec(text)) !== null) {
    const matchWord = match[0].toLowerCase();

    // Skip keywords found on RPG comment lines (fixed-format '*' comments and // comments).
    const lineNumber = document.positionAt(match.index).line;
    if (rpgle.isComment(document.lineAt(lineNumber).text, document)) {
      continue;
    }

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

// Helper function to strip comments from a line
function stripComments(line: string): string {
  // Remove // comments
  const commentIndex = line.indexOf('//');
  if (commentIndex !== -1) {
    return line.substring(0, commentIndex);
  }
  return line;
}

// Helper function to check whether END-DS is coded inline on the same DCL-DS statement.
function hasInlineEndDsOnDclDsLine(text: string, offset: number): boolean {
  const lineStart = text.lastIndexOf('\n', offset) + 1;
  const lineEnd = text.indexOf('\n', offset);
  const lineContent = text.substring(lineStart, lineEnd === -1 ? text.length : lineEnd);
  const lineWithoutComments = stripComments(lineContent).toLowerCase();

  if (!/\bdcl-ds\b/.test(lineWithoutComments)) {
    return false;
  }

  return /\bend-ds\b/.test(lineWithoutComments);
}

function isInlineEndDsForLikedsOrLikerec(text: string, offset: number): boolean {
  return hasInlineEndDsOnDclDsLine(text, offset) && isDclDsWithLikedsOrLikerec(text, offset);
}
// Helper function to check if dcl-ds line has likeds() or likerec()
function isDclDsWithLikedsOrLikerec(text: string, offset: number): boolean {
  const lineStart = text.lastIndexOf('\n', offset) + 1;
  const lineEnd = text.indexOf('\n', offset);
  const lineContent = text.substring(lineStart, lineEnd === -1 ? text.length : lineEnd);

  // Strip comments before checking
  const lineWithoutComments = stripComments(lineContent).toLowerCase();

  // LIKEDS/LIKEREC declarations are one-line and should not be treated as block openers,
  // regardless of whether END-DS is written inline on the same statement.
  return /likeds\s*\(/.test(lineWithoutComments) || /likerec\s*\(/.test(lineWithoutComments);
}

function hasPriorLikedsOrLikerecDclDs(
  text: string,
  matches: { offset: number; word: string; length: number }[],
  closeIndex: number
): boolean {
  for (let i = closeIndex - 1; i >= 0; i--) {
    const word = matches[i].word;

    // Avoid crossing procedure boundaries when looking for a related declaration.
    if (word === 'dcl-proc' || word === 'end-proc') {
      break;
    }

    if (word === 'dcl-ds') {
      if (hasInlineEndDsOnDclDsLine(text, matches[i].offset)) {
        continue;
      }

      if (isDclDsWithLikedsOrLikerec(text, matches[i].offset)) {
        return true;
      }
    }
  }

  return false;
}

// Helper function specifically for finding the opening block for an END keyword
function findMatchingOpenForEnd(
  text: string,
  matches: { offset: number; word: string; length: number }[],
  endIndex: number
): number {
  return findMatchingOpenForClosing(text, matches, endIndex, 'end');
}

// Helper function for finding the opening block for any closing keyword
function findMatchingOpenForClosing(
  text: string,
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
      // Special handling for dcl-ds: skip if it uses likeds() or likerec()
      // These create single-line declarations that don't require end-ds
      if (word === 'dcl-ds' && isDclDsWithLikedsOrLikerec(text, matches[i].offset)) {
        continue;
      }

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
  pair: BracketPair,
  precomputedMatches?: BlockMatch[]
): vscode.Range[] {
  const text = document.getText();
  const startOffset = document.offsetAt(startRange.start);

  // Use pre-computed matches from cache if provided, otherwise compute
  const allMatches = precomputedMatches ?? findAllMatches(text, document);

  // Find index of current match
  let currentIndex = -1;
  for (let i = 0; i < allMatches.length; i++) {
    if (allMatches[i].offset === startOffset) {
      currentIndex = i;
      break;
    }
  }

  if (currentIndex === -1) return [];

  // Find block containing current keyword
  const blockIndices = findBlockIndices(text, allMatches, currentIndex, pair);
  if (!blockIndices) return [];

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

  if (currentIndex === -1) return { validRanges: [], errorRanges: [] };

  // Find block containing current keyword
  const blockIndices = findBlockIndices(text, allMatches, currentIndex, pair);
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
  text: string,
  matches: { offset: number; word: string; length: number }[],
  closeIndex: number
): number {
  const closeWord = matches[closeIndex].word;

  // Build a stack to track all open blocks
  const stack: { index: number; pair: BracketPair }[] = [];

  for (let i = 0; i < closeIndex; i++) {
    const word = matches[i].word;

    // Ignore inline END-DS written on LIKEDS/LIKEREC one-line declarations.
    if (word === 'end-ds' && isInlineEndDsForLikedsOrLikerec(text, matches[i].offset)) {
      continue;
    }

    // Check if this word opens any block
    const openingPair = RPGLE_BLOCK_PAIRS.find(p => p.open.includes(word));
    if (openingPair) {
      // Special handling for dcl-ds: skip if it uses likeds() or likerec()
      // These create single-line declarations that don't require end-ds
      if (word === 'dcl-ds' && isDclDsWithLikedsOrLikerec(text, matches[i].offset)) {
        continue;
      }

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
        // For specific closers, only close the top of the stack when it matches.
        // If it does not match, treat it as a local mismatch and do not consume
        // outer block state (prevents mismatch cascade to later ENDxx tokens).
        const topPair = stack[stack.length - 1].pair;
        const isMatch = (topPair === closerPair ||
          (topPair.close.includes(word) && closerPair.close.includes(word)));

        if (isMatch) {
          stack.pop();
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
  text: string,
  matches: { offset: number; word: string; length: number }[],
  currentIndex: number,
  pair: BracketPair
): number[] | undefined {
  const currentWord = matches[currentIndex].word;

  // Determine if current keyword is opening, closing, or middle
  const isOpen = pair.open.includes(currentWord);
  const isClose = pair.close.includes(currentWord);
  const isMiddle = pair.middle?.includes(currentWord);

  // Special check for dcl-ds with likeds/likerec - it's NOT a block opener
  if (isOpen && currentWord === 'dcl-ds' && isDclDsWithLikedsOrLikerec(text, matches[currentIndex].offset)) {
    // This is a single-line declaration, not a block opener
    return undefined;
  }

  let openIndex = -1;
  let closeIndex = -1;

  if (isOpen) {
    // If opening, find matching close
    openIndex = currentIndex;
    closeIndex = findMatchingClose(text, matches, currentIndex, pair);
  } else if (isClose) {
    // If closing, find matching open
    closeIndex = currentIndex;
    openIndex = findMatchingOpen(text, matches, currentIndex, pair);
  } else if (isMiddle) {
    // If middle keyword, find both open and close
    openIndex = findMatchingOpen(text, matches, currentIndex, pair);
    if (openIndex !== -1) {
      closeIndex = findMatchingClose(text, matches, openIndex, pair);
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
  text: string,
  matches: { offset: number; word: string; length: number }[],
  openIndex: number,
  pair: BracketPair
): number {
  // Track ALL open blocks, not just the same type
  const stack: BracketPair[] = [];
  stack.push(pair); // Our target block

  for (let i = openIndex + 1; i < matches.length; i++) {
    const word = matches[i].word;

    // Ignore inline END-DS written on LIKEDS/LIKEREC one-line declarations.
    if (word === 'end-ds' && isInlineEndDsForLikedsOrLikerec(text, matches[i].offset)) {
      continue;
    }

    // Check if this word opens any block
    const openingPair = RPGLE_BLOCK_PAIRS.find(p => p.open.includes(word));
    if (openingPair) {
      // Special handling for dcl-ds: skip if it uses likeds() or likerec()
      if (word === 'dcl-ds' && isDclDsWithLikedsOrLikerec(text, matches[i].offset)) {
        continue; // Skip this dcl-ds, it's not a block opener
      }

      stack.push(openingPair);
      continue;
    }

    // Check if this word closes a block
    // For 'END' or 'ENDDO', find the most recent block that accepts it
    if (word === 'end' || word === 'enddo') {
      // Generic closers: search stack for matching block
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
    } else {
      // Check if this word is actually a closing keyword
      const closingPair = RPGLE_BLOCK_PAIRS.find(p => p.close.includes(word));

      if (closingPair) {
        // Specific closing keyword (endif, endfor, end-proc, etc.)
        // ONLY close the top of the stack, maintaining proper LIFO discipline
        // If it does not match, keep stack state intact so the mismatch remains
        // local and does not invalidate downstream valid closers.
        if (stack.length > 0) {
          const topPair = stack[stack.length - 1];

          if (topPair.close.includes(word)) {
            // This closer matches the top of the stack
            if (stack.length === 1) {
              // This closes our target block
              return i;
            }
            // Pop this matched block
            stack.pop();
          }
        }
      }
      // else: word is neither opener nor closer (might be middle keyword like else/elseif/when) - ignore it
    }
  }

  return -1;
}

// Find opening keyword index for a closing or middle keyword
function findMatchingOpen(
  text: string,
  matches: { offset: number; word: string; length: number }[],
  startIndex: number,
  pair: BracketPair
): number {
  const startWord = matches[startIndex].word;

  // Use stack-based approach for ALL closing keywords to properly handle
  // nested blocks and error recovery (mirrors findMatchingClose logic)
  const stack: { index: number; pair: BracketPair }[] = [];

  for (let i = 0; i < startIndex; i++) {
    const word = matches[i].word;

    // Ignore inline END-DS written on LIKEDS/LIKEREC one-line declarations.
    if (word === 'end-ds' && isInlineEndDsForLikedsOrLikerec(text, matches[i].offset)) {
      continue;
    }

    // Check if this word opens any block
    const openingPair = RPGLE_BLOCK_PAIRS.find(p => p.open.includes(word));
    if (openingPair) {
      // Special handling for dcl-ds: skip if it uses likeds() or likerec()
      if (word === 'dcl-ds' && isDclDsWithLikedsOrLikerec(text, matches[i].offset)) {
        continue; // Skip this dcl-ds, it's not a block opener
      }

      stack.push({ index: i, pair: openingPair });
      continue;
    }

    // Check if this word closes a block
    if (word === 'end' || word === 'enddo') {
      // Generic closers: search stack for matching block
      for (let j = stack.length - 1; j >= 0; j--) {
        if (stack[j].pair.close.includes(word)) {
          stack.splice(j, 1);
          break;
        }
      }
    } else {
      // Check if this word is actually a closing keyword
      const closingPair = RPGLE_BLOCK_PAIRS.find(p => p.close.includes(word));

      if (closingPair && stack.length > 0) {
        // Specific closing keyword (endif, endfor, end-proc, etc.)
        // ONLY close the top of the stack when it matches. If it does not match,
        // keep stack state intact to avoid propagating mismatch to later closers.
        const topPair = stack[stack.length - 1].pair;
        if (topPair.close.includes(word)) {
          stack.pop();
        }
      }
      // else: word is neither opener nor closer (might be middle keyword) - ignore it
    }
  }

  // Find the most recent unclosed block that can be closed by startWord
  if (startWord === 'end' || startWord === 'enddo') {
    // Generic closers: find any block that accepts this closer
    for (let j = stack.length - 1; j >= 0; j--) {
      if (stack[j].pair.close.includes(startWord)) {
        return stack[j].index;
      }
    }
  } else {
    // Specific closers: can ONLY close the last block if it's of the correct type
    if (stack.length > 0) {
      const lastBlock = stack[stack.length - 1];
      // Check if the last unclosed block can be closed by this keyword
      if (lastBlock.pair.close.includes(startWord)) {
        return lastBlock.index;
      }

      // Middle keywords (else/elseif/when/on-error/etc.) belong to the nearest
      // active block of the same pair and should highlight that whole block.
      if (pair.middle?.includes(startWord) && lastBlock.pair === pair) {
        return lastBlock.index;
      }
    }
  }

  return -1;
}

// Export function to register the jump to matching block command
export function registerJumpToMatchingBlock(context: vscode.ExtensionContext) {
  const jumpCommand = vscode.commands.registerTextEditorCommand(
    'vscode-rpgle.jumpToMatchingBlock',
    (editor: vscode.TextEditor) => {
      // Check if jump feature is enabled
      const config = vscode.workspace.getConfiguration('vscode-rpgle');
      const jumpEnabled = config.get<boolean>(JUMP_ENABLED_KEY, true);
      if (!jumpEnabled) {
        return;
      }

      const document = editor.document;
      const position = editor.selection.active;
      const text = document.getText();
      const docUri = document.uri.toString();

      // Use cached matches and lookup maps if available, else compute
      let allMatches: BlockMatch[];
      let matchIndexByOffset: Map<number, number>;
      let blockIndicesByMatch: Map<number, number[]>;
      const cached = analysisCache.get(docUri);
      if (cached && cached.version === document.version && cached.text === text) {
        allMatches = cached.matches;
        matchIndexByOffset = cached.matchIndexByOffset;
        blockIndicesByMatch = cached.blockIndicesByMatch;
      } else {
        allMatches = findAllMatches(text, document);
        const maps = buildLookupMaps(text, allMatches);
        matchIndexByOffset = maps.matchIndexByOffset;
        blockIndicesByMatch = maps.blockIndicesByMatch;
      }

      // First try the word directly under the cursor, then fall back to scanning
      // the entire current line — so the jump works even when the cursor is
      // anywhere on the same line as the keyword (not just on top of it).
      const wordPattern = /[a-zA-Z_#@$§£ÆæØøàÀ][\w#@$§£ÆæØøàÀ-]*/;
      let matchIndex = -1;
      let word = '';

      const wordRangeAtCursor = document.getWordRangeAtPosition(position, wordPattern);
      if (wordRangeAtCursor) {
        const candidateOffset = document.offsetAt(wordRangeAtCursor.start);
        // O(1) lookup instead of findIndex scan
        const idx = matchIndexByOffset.get(candidateOffset);
        if (idx !== undefined && blockIndicesByMatch.has(idx)) {
          matchIndex = idx;
          word = allMatches[idx].word;
        }
      }

      // Fall back: first block keyword with precomputed block indices on the current line
      if (matchIndex === -1) {
        const lineStart = document.offsetAt(new vscode.Position(position.line, 0));
        const lineEnd = document.offsetAt(new vscode.Position(position.line + 1, 0));
        for (let i = 0; i < allMatches.length; i++) {
          if (allMatches[i].offset >= lineStart && allMatches[i].offset < lineEnd) {
            if (blockIndicesByMatch.has(i)) {
              matchIndex = i;
              word = allMatches[i].word;
              break;
            }
          }
        }
      }

      if (matchIndex === -1) return;

      // O(1) lookup for block indices — no findBlockIndices call needed
      const blockIndices = blockIndicesByMatch.get(matchIndex);
      if (!blockIndices || blockIndices.length < 2) return;

      // Determine which end to jump to: opener \u2192 closer, anything else \u2192 opener
      const openerWord = allMatches[blockIndices[0]].word;
      const isOpen = allMatches[matchIndex].word === openerWord && matchIndex === blockIndices[0];
      const targetIndex = isOpen ? blockIndices[1] : blockIndices[0];

      const targetMatch = allMatches[targetIndex];
      if (!targetMatch) return;

      const targetPos = document.positionAt(targetMatch.offset);
      const targetRange = new vscode.Range(targetPos, document.positionAt(targetMatch.offset + targetMatch.length));

      editor.selection = new vscode.Selection(targetPos, targetPos);
      editor.revealRange(targetRange, vscode.TextEditorRevealType.InCenterIfOutsideViewport);
    }
  );
  context.subscriptions.push(jumpCommand);
}

export function deactivateBracketMatcher() {
  if (decorationType) {
    decorationType.dispose();
    decorationType = undefined;
  }
  if (errorDecorationType) {
    errorDecorationType.dispose();
    errorDecorationType = undefined;
  }
}