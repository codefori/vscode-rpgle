/**
 * Utility functions for detecting SQL embedded blocks in RPGLE code
 */

/**
 * Check if a position is inside an embedded SQL block
 * @param text The full text content
 * @param offset The position to check
 * @returns true if the position is inside a SQL block
 */
export function isInSqlBlock(text: string, offset: number): boolean {
  // Normalize offset so partial-line checks are safe.
  const safeOffset = Math.max(0, Math.min(offset, text.length));

  // Track whether we are currently inside an SQL block while scanning
  // from the start of the source up to the target offset.
  let inSql = false;
  let sqlMode: 'free' | 'fixed' | undefined;

  // Keep SQL string state for free-form EXEC SQL blocks so semicolons inside
  // SQL literals do not terminate the block.
  let inSqlString = false;
  let sqlStringChar = '';

  // Fixed-format SQL accepts C/ in column 6 (often with sequence numbers in
  // columns 1-5), but we also allow leading-whitespace-only variants.
  const fixedExecRegex = /^(?:.{5}[cC]\/\s*exec\s+sql\b|\s*[cC]\/\s*exec\s+sql\b)/i;
  const fixedEndExecRegex = /^(?:.{5}[cC]\/\s*end-?exec\b|\s*[cC]\/\s*end-?exec\b)/i;
  const freeExecRegex = /^\s*exec\s+sql\b/i;

  let lineStart = 0;
  while (lineStart <= safeOffset) {
    let lineEnd = text.indexOf('\n', lineStart);
    if (lineEnd === -1) lineEnd = text.length;

    const fullLine = text.substring(lineStart, lineEnd).replace(/\r$/, '');
    const isTargetLine = safeOffset <= lineEnd;
    const targetColumn = Math.max(0, safeOffset - lineStart);
    const lineToProcess = isTargetLine ? fullLine.substring(0, targetColumn) : fullLine;

    if (!inSql) {
      const fixedExecMatch = lineToProcess.match(fixedExecRegex);
      if (fixedExecMatch) {
        const startIndex = lineStart + (fixedExecMatch.index ?? 0);
        if (!isInCommentOrString(text, startIndex)) {
          inSql = true;
          sqlMode = 'fixed';
        }
      } else {
        const freeExecMatch = lineToProcess.match(freeExecRegex);
        if (freeExecMatch) {
          const startIndex = lineStart + (freeExecMatch.index ?? 0);
          if (!isInCommentOrString(text, startIndex)) {
            inSql = true;
            sqlMode = 'free';

            const afterExec = lineToProcess.substring((freeExecMatch.index || 0) + freeExecMatch[0].length);
            if (scanForSqlTerminator(afterExec, (state) => {
              inSqlString = state.inString;
              sqlStringChar = state.stringChar;
            }, inSqlString, sqlStringChar)) {
              inSql = false;
              sqlMode = undefined;
              inSqlString = false;
              sqlStringChar = '';
            }
          }
        }
      }
    } else if (sqlMode === 'fixed') {
      const fixedEndMatch = lineToProcess.match(fixedEndExecRegex);
      if (fixedEndMatch) {
        const endIndex = lineStart + (fixedEndMatch.index ?? 0);
        if (!isInCommentOrString(text, endIndex)) {
          inSql = false;
          sqlMode = undefined;
          inSqlString = false;
          sqlStringChar = '';
        }
      }
    } else if (sqlMode === 'free') {
      if (scanForSqlTerminator(lineToProcess, (state) => {
        inSqlString = state.inString;
        sqlStringChar = state.stringChar;
      }, inSqlString, sqlStringChar)) {
        inSql = false;
        sqlMode = undefined;
        inSqlString = false;
        sqlStringChar = '';
      }
    }

    if (isTargetLine || lineEnd === text.length) {
      break;
    }

    lineStart = lineEnd + 1;
  }

  return inSql;
}

function scanForSqlTerminator(
  source: string,
  updateState: (state: { inString: boolean; stringChar: string }) => void,
  initialInString = false,
  initialStringChar = ''
): boolean {
  let inString = initialInString;
  let stringChar = initialStringChar;

  for (let i = 0; i < source.length; i++) {
    const char = source[i];

    if ((char === "'" || char === '"') && !inString) {
      inString = true;
      stringChar = char;
    } else if (char === stringChar && inString) {
      // SQL escaped quotes are doubled.
      if (i + 1 < source.length && source[i + 1] === stringChar) {
        i++;
      } else {
        inString = false;
        stringChar = '';
      }
    }

    if (char === ';' && !inString) {
      updateState({ inString, stringChar });
      return true;
    }
  }

  updateState({ inString, stringChar });
  return false;
}

/**
 * Scan a single source line tracking string/comment state, stopping before any
 * content that follows a `//` comment opener (when not inside a string).
 * Returns the string state at the end of the scan.
 * Handles RPG `''` escaped quotes (two consecutive single quotes inside a string).
 */
function scanLineStringState(line: string, initialInString: boolean): boolean {
  let inString = initialInString;
  for (let i = 0; i < line.length; i++) {
    const ch = line[i];
    if (!inString) {
      if (ch === '/' && i + 1 < line.length && line[i + 1] === '/') {
        break; // rest of line is a comment — stop
      }
      if (ch === "'") {
        inString = true;
      }
    } else {
      if (ch === "'") {
        // '' is an escaped quote inside the string — skip both characters
        if (i + 1 < line.length && line[i + 1] === "'") {
          i++;
        } else {
          inString = false;
        }
      }
    }
  }
  return inString;
}

/**
 * Return the string state that is carried into `lineStart` from prior
 * continuation lines.  A string can only carry over when the preceding
 * physical line ends with `+` (RPG free-form string continuation).
 */
function getPriorLineStringState(text: string, lineStart: number): boolean {
  if (lineStart <= 0) return false;

  // Find the previous physical line (skip the newline character before lineStart)
  let prevEnd = lineStart - 1;
  if (prevEnd > 0 && text[prevEnd - 1] === '\r') prevEnd--; // handle \r\n
  let prevStart = prevEnd;
  while (prevStart > 0 && text[prevStart - 1] !== '\n') {
    prevStart--;
  }

  const prevLine = text.substring(prevStart, prevEnd);
  // Continuation requires the last non-whitespace character to be '+'
  if (!prevLine.trimEnd().endsWith('+')) return false;

  // Recursively get the state carried into the previous line, then scan it
  const stateBeforePrev = getPriorLineStringState(text, prevStart);
  return scanLineStringState(prevLine, stateBeforePrev);
}

/**
 * Check if a position is inside a comment or string.
 * Handles RPG free-form line continuation: a string that opens on a line ending
 * with `+` carries over into the next physical line, so keywords on continuation
 * lines are correctly detected as being inside the string.
 * Also correctly handles `//` that appears inside a string (e.g. 'http://...').
 * @param text The full text content
 * @param offset The position to check
 * @returns true if the position is inside a comment or string
 */
export function isInCommentOrString(text: string, offset: number): boolean {
  // Find the start of the current physical line
  let lineStart = offset;
  while (lineStart > 0 && text[lineStart - 1] !== '\n' && text[lineStart - 1] !== '\r') {
    lineStart--;
  }

  // Carry over string state from prior continuation lines
  let inString = getPriorLineStringState(text, lineStart);

  // Scan from line start to the target offset
  const lineBeforeOffset = text.substring(lineStart, offset);
  for (let i = 0; i < lineBeforeOffset.length; i++) {
    const ch = lineBeforeOffset[i];
    if (!inString) {
      if (ch === '/' && i + 1 < lineBeforeOffset.length && lineBeforeOffset[i + 1] === '/') {
        return true; // position is after a line comment opener
      }
      if (ch === "'") {
        inString = true;
      }
    } else {
      if (ch === "'") {
        if (i + 1 < lineBeforeOffset.length && lineBeforeOffset[i + 1] === "'") {
          i++; // skip escaped quote pair
        } else {
          inString = false;
        }
      }
    }
  }

  return inString;
}