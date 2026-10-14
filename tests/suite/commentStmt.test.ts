import { expect, test, suite } from 'vitest';
import { commentFreeFormatLine, uncommentFreeFormatLine } from '../../extension/client/src/commentStmt';

suite('commentStmt pure functions', () => {
  test('commentFreeFormatLine: fully free format', () => {
    // Normal code
    expect(commentFreeFormatLine('  x = 1;', true)).toBe('  // x = 1;');
    // Already commented
    expect(commentFreeFormatLine('  // x = 1;', true)).toBe('');
  });

  test('commentFreeFormatLine: mixed format, tagged', () => {
    // Tagged line (columns 1-5 tag, 6-7 blank)
    expect(commentFreeFormatLine('TAG01  x = 1;', false)).toBe('TAG01  // x = 1;');
    
    // Tagged line, code starts far away
    expect(commentFreeFormatLine('TAG01      x = 1;', false)).toBe('TAG01      // x = 1;');
  });

  test('commentFreeFormatLine: mixed format fallback (Regression 2)', () => {
    // Code starting in column 7
    expect(commentFreeFormatLine('      x = 1;', false)).toBe('      // x = 1;');
    
    // Tab indented line
    expect(commentFreeFormatLine('\t\tx = 1;', false)).toBe('\t\t// x = 1;');
  });

  test('uncommentFreeFormatLine: fully free format', () => {
    expect(uncommentFreeFormatLine('  // x = 1;', true)).toBe('  x = 1;');
    expect(uncommentFreeFormatLine('  x = 1;', true)).toBe(''); // Not commented
  });

  test('uncommentFreeFormatLine: mixed format, tagged', () => {
    expect(uncommentFreeFormatLine('TAG01  // x = 1;', false)).toBe('TAG01  x = 1;');
  });

  test('uncommentFreeFormatLine: mixed format fallback (Regression 1)', () => {
    // Comment starting at col 1
    expect(uncommentFreeFormatLine('// old code', false)).toBe('old code');
    
    // Comment starting at col 5
    expect(uncommentFreeFormatLine('    // old code', false)).toBe('    old code');
    
    // Comment starting at col 7
    expect(uncommentFreeFormatLine('      // old code', false)).toBe('      old code');
  });

  test('Round trips', () => {
    const lines = [
      { line: 'TAG01  x = 1;', isCompletelyFreeFormat: false },
      { line: '  x = 1;', isCompletelyFreeFormat: true },
      { line: '      x = 1;', isCompletelyFreeFormat: false },
      { line: '\t\tx = 1;', isCompletelyFreeFormat: false },
    ];

    for (const { line, isCompletelyFreeFormat } of lines) {
      const commented = commentFreeFormatLine(line, isCompletelyFreeFormat);
      const uncommented = uncommentFreeFormatLine(commented, isCompletelyFreeFormat);
      expect(uncommented).toBe(line);
    }
  });
});
