const fs = require('fs');

let content = fs.readFileSync('extension/client/src/commentStmt.ts', 'utf8');

// Replace commentFreeFormatLine(line, isCompletelyFreeFormat) -> commentFreeFormatLine(line)
content = content.replace(/commentFreeFormatLine\(line, isCompletelyFreeFormat\)/g, 'isCompletelyFreeFormat ? commentFreeFormatLine(line) : commentMixedFormatLine(line)');

// Replace uncommentFreeFormatLine(line, isCompletelyFreeFormat) -> uncommentFreeFormatLine(line)
content = content.replace(/uncommentFreeFormatLine\(line, isCompletelyFreeFormat\)/g, 'isCompletelyFreeFormat ? uncommentFreeFormatLine(line) : uncommentMixedFormatLine(line)');

// Define the replacement functions
const newFunctions = `/**
 * Comment a free format RPG line by adding // to the beginning
 * Respects the indentation of the original line
 * @param line The free format line to comment
 * @returns The commented line
 */
function commentFreeFormatLine(line: string): string {
  const leadingWhitespace = line.match(/^\\s*/)?.[0] || '';
  const trimmedLine = line.trim();

  // If the line is already a comment, skip it
  if (trimmedLine.startsWith('//')) {
    return '';
  }

  // Add // comment marker, preserving indentation
  return leadingWhitespace + '// ' + trimmedLine;
}

/**
 * Uncomment a free format RPG line by removing the // prefix
 * @param line The commented free format line
 * @returns The uncommented line
 */
function uncommentFreeFormatLine(line: string): string {
  const leadingWhitespace = line.match(/^\\s*/)?.[0] || '';
  const trimmedLine = line.trim();

  // If the line doesn't start with //, skip it
  if (!trimmedLine.startsWith('//')) {
    return '';
  }

  // Remove the // comment marker and optional space after it
  const uncommentedContent = trimmedLine.replace(/^\\/\\/\\s?/, '');

  // Restore the indentation
  return leadingWhitespace + uncommentedContent;
}

/**
 * Comment a free format line in a mixed-format file.
 * Preserves sequence numbers or developer tags in columns 1-7.
 */
function commentMixedFormatLine(line: string): string {
  // If the line is short, contains tabs in the first 7 chars,
  // or if there is a non-whitespace character at column 7 (index 6),
  // it might not be a standard tag. Fall back to standard free format comment.
  if (line.length < 7 || line.substring(0, 7).includes('\\t') || line[6] !== ' ') {
    return commentFreeFormatLine(line);
  }
  
  const prefix = line.substring(0, 7);
  const codePart = line.substring(7);
  
  const leadingWhitespace = codePart.match(/^\\s*/)?.[0] || '';
  const trimmedCode = codePart.trim();
  
  if (trimmedCode.startsWith('//')) return '';
  if (trimmedCode === '') return prefix + '//';
  
  return prefix + leadingWhitespace + '// ' + trimmedCode;
}

/**
 * Uncomment a free format line in a mixed-format file.
 */
function uncommentMixedFormatLine(line: string): string {
  const trimmedLine = line.trim();
  if (trimmedLine.startsWith('//')) {
    const firstSlash = line.indexOf('//');
    if (firstSlash < 7) {
      return uncommentFreeFormatLine(line);
    }
  }
  
  if (line.length < 7) return uncommentFreeFormatLine(line);
  
  const prefix = line.substring(0, 7);
  const codePart = line.substring(7);
  
  const leadingWhitespace = codePart.match(/^\\s*/)?.[0] || '';
  const trimmedCode = codePart.trim();
  
  if (!trimmedCode.startsWith('//')) {
    return uncommentFreeFormatLine(line);
  }
  
  const uncommentedContent = trimmedCode.replace(/^\\/\\/\\s?/, '');
  return prefix + leadingWhitespace + uncommentedContent;
}
`;

// Remove the old commentFreeFormatLine and uncommentFreeFormatLine
content = content.replace(/\/\*\*\r?\n \* Comment a free format RPG line(?:.|\n)*?function uncommentFreeFormatLine\(line: string(?:.|\n)*?\}\r?\n/m, newFunctions);

fs.writeFileSync('extension/client/src/commentStmt.ts', content);
