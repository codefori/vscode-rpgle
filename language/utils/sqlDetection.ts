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
 * Check if a position is inside a comment or string
 * @param text The full text content
 * @param offset The position to check
 * @returns true if the position is inside a comment or string
 */
export function isInCommentOrString(text: string, offset: number): boolean {
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