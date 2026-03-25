/**
 * Utility functions for detecting SQL blocks and comments/strings in RPGLE code
 */

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

/**
 * Check if a position is inside an embedded SQL block
 * @param text The full text content
 * @param offset The position to check
 * @returns true if the position is inside an SQL block (between EXEC SQL and ;)
 */
export function isInSqlBlock(text: string, offset: number): boolean {
  // Find the line containing the offset
  let lineStart = offset;
  while (lineStart > 0 && text[lineStart - 1] !== '\n' && text[lineStart - 1] !== '\r') {
    lineStart--;
  }
  
  // Check if this line starts with EXEC SQL (ignoring whitespace)
  const currentLine = text.substring(lineStart, offset);
  if (/^\s*exec\s+sql\b/i.test(currentLine)) {
    // We're on a line that starts with EXEC SQL, so we're in SQL
    return true;
  }
  
  // Look backwards for EXEC SQL on previous lines
  const textBefore = text.substring(0, lineStart);
  
  // Find all SQL block starts
  const execSqlRegex = /\b(exec\s+sql)\b/gi;
  
  let lastExecSql = -1;
  let lastExecSqlLine = -1;
  
  // Find last EXEC SQL
  let match;
  execSqlRegex.lastIndex = 0;
  while ((match = execSqlRegex.exec(textBefore)) !== null) {
    lastExecSql = match.index;
    // Count newlines to get line number
    lastExecSqlLine = textBefore.substring(0, match.index).split(/\r?\n/).length - 1;
  }
  
  // If we found an EXEC SQL on a previous line, check if there's a semicolon after it
  if (lastExecSql !== -1) {
    const textAfterExec = text.substring(lastExecSql, lineStart);
    
    // Look for semicolon that ends the SQL block
    const semicolonMatch = textAfterExec.match(/;/);
    
    // If we didn't find a semicolon, we're still in the SQL block
    if (!semicolonMatch) {
      return true;
    }
  }
  
  return false;
}