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
  
  // Look backwards for EXEC SQL
  const textBeforeOffset = text.substring(0, offset);
  
  // Find all SQL block starts and ends
  const execSqlRegex = /\b(exec\s+sql)\b/gi;
  
  let lastExecSql = -1;
  let lastExecSqlEnd = -1;
  
  // Find last EXEC SQL before current position
  let match;
  execSqlRegex.lastIndex = 0;
  while ((match = execSqlRegex.exec(textBeforeOffset)) !== null) {
    // Check if this EXEC SQL is inside a comment or string
    if (!isInCommentOrString(text, match.index)) {
      lastExecSql = match.index;
      lastExecSqlEnd = match.index + match[0].length;
    }
  }
  
  // If we found an EXEC SQL, check if there's a semicolon after it (before current position)
  if (lastExecSql !== -1) {
    const textAfterExec = text.substring(lastExecSqlEnd, offset);
    
    // Look for semicolon that ends the SQL block
    // We need to be careful not to match semicolons inside SQL strings
    let inString = false;
    let stringChar = '';
    
    for (let i = 0; i < textAfterExec.length; i++) {
      const char = textAfterExec[i];
      
      // Handle string delimiters (both single and double quotes in SQL)
      if ((char === "'" || char === '"') && !inString) {
        inString = true;
        stringChar = char;
      } else if (char === stringChar && inString) {
        // Check for escaped quotes (doubled quotes in SQL)
        if (i + 1 < textAfterExec.length && textAfterExec[i + 1] === stringChar) {
          i++; // Skip the escaped quote
        } else {
          inString = false;
          stringChar = '';
        }
      }
      
      // If we find a semicolon outside of a string, the SQL block has ended
      if (char === ';' && !inString) {
        return false;
      }
    }
    
    // No semicolon found, we're still in the SQL block
    return true;
  }
  
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