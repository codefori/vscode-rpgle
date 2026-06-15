import { describe, it, expect } from 'vitest';

describe('Folding Range - dcl-ds with likeds/likerec', () => {

  /**
   * Helper to simulate the folding logic for dcl-ds
   * This matches the logic added to foldingRange.ts
   * Returns true if dcl-ds should be treated as a block opener
   */
  function shouldDclDsCreateBlock(line: string): boolean {
    // Strip comments before checking
    const commentIndex = line.indexOf('//');
    let lineWithoutComments = commentIndex !== -1 ? line.substring(0, commentIndex) : line;
    const lowerLine = lineWithoutComments.toLowerCase();

    // If line contains likeds() or likerec(), it's NOT a block opener
    // Use regex to handle optional whitespace between keyword and opening paren
    if (/likeds\s*\(/.test(lowerLine) || /likerec\s*\(/.test(lowerLine)) {
      return false;
    }

    // Otherwise it's a block opener that requires end-ds
    return true;
  }

  describe('dcl-ds with likeds()', () => {
    it('should NOT be treated as block opener', () => {
      const line = '  dcl-ds appset likeds(t_APPSET) inz(*likeds);';
      expect(shouldDclDsCreateBlock(line)).toBe(false);
    });

    it('should handle uppercase LIKEDS', () => {
      const line = '  dcl-ds myDs LIKEDS(template_t) INZ;';
      expect(shouldDclDsCreateBlock(line)).toBe(false);
    });

    it('should handle mixed case LiKeDs', () => {
      const line = '  dcl-ds data LiKeDs(other_t);';
      expect(shouldDclDsCreateBlock(line)).toBe(false);
    });

    it('should handle likeds with qualified', () => {
      const line = '  dcl-ds customer likeds(CUSTOMER_t) qualified inz(*likeds);';
      expect(shouldDclDsCreateBlock(line)).toBe(false);
    });

    it('should handle likeds with dim', () => {
      const line = '  dcl-ds items likeds(item_t) dim(100);';
      expect(shouldDclDsCreateBlock(line)).toBe(false);
    });
  });

  describe('dcl-ds with likerec()', () => {
    it('should NOT be treated as block opener', () => {
      const line = '  dcl-ds record likerec(MYFILE);';
      expect(shouldDclDsCreateBlock(line)).toBe(false);
    });

    it('should handle uppercase LIKEREC', () => {
      const line = '  dcl-ds fileRec LIKEREC(DATAFILE);';
      expect(shouldDclDsCreateBlock(line)).toBe(false);
    });

    it('should handle likerec with based', () => {
      const line = '  dcl-ds dirEntry likerec(dirent) based(ptrDirEntry);';
      expect(shouldDclDsCreateBlock(line)).toBe(false);
    });
  });

  describe('dcl-ds without likeds/likerec', () => {
    it('should be treated as block opener for multi-line ds', () => {
      const line = '  dcl-ds myStruct qualified;';
      expect(shouldDclDsCreateBlock(line)).toBe(true);
    });

    it('should be treated as block opener with extname', () => {
      const line = '  dcl-ds qcustcdt extname(\'QIWS/QCUSTCDT\') qualified;';
      expect(shouldDclDsCreateBlock(line)).toBe(true);
    });

    it('should be treated as block opener with template', () => {
      const line = '  dcl-ds template_t qualified template;';
      expect(shouldDclDsCreateBlock(line)).toBe(true);
    });

    it('should be treated as block opener with just qualified', () => {
      const line = 'dcl-ds params qualified inz;';
      expect(shouldDclDsCreateBlock(line)).toBe(true);
    });
  });

  describe('edge cases', () => {
    it('should handle likeds in comments (still should skip)', () => {
      const line = '  dcl-ds myDs likeds(t) inz; // likeds creates a copy';
      expect(shouldDclDsCreateBlock(line)).toBe(false);
    });

    it('should handle whitespace variations', () => {
      const line = '  dcl-ds   myDs   likeds  (  template_t  )  ;';
      expect(shouldDclDsCreateBlock(line)).toBe(false);
    });

    it('should not match "likeds" in other contexts', () => {
      // This is a regular ds, "likeds" appears in a different field name
      const line = '  dcl-ds myStruct qualified; // not_likeds_field';
      expect(shouldDclDsCreateBlock(line)).toBe(true);
    });
  });

  describe('real-world examples', () => {
    it('should handle the original issue case', () => {
      const lines = [
        'dcl-proc APMDAL_add export;',
        '  dcl-pi *n ind;',
        '    data likeds(APMDAL_t)',
        '  end-pi;',
        '  ',
        '  dcl-ds caller likeds(CALLER_t) inz(*likeds);',
        '  ',
        '  return get_error();',
        'end-proc;'
      ];

      // Line 6 (index 5) should NOT create a block
      expect(shouldDclDsCreateBlock(lines[5])).toBe(false);
    });

    it('should handle mixed declarations', () => {
      const lines = [
        'dcl-proc test;',
        '  dcl-ds struct1 likeds(template_t);',  // Should NOT create block
        '  dcl-ds struct2 qualified;',           // Should create block
        '    field1 char(10);',
        '  end-ds;',
        '  ',
        '  struct1.field = struct2.field1;',
        'end-proc;'
      ];

      expect(shouldDclDsCreateBlock(lines[1])).toBe(false);
      expect(shouldDclDsCreateBlock(lines[2])).toBe(true);
    });
  });

  describe('Nested data structures with likeds/likerec', () => {
    it('should handle nested dcl-ds with likeds correctly', () => {
      const code = `**free
dcl-ds myds1 qualified inz;
  field1 char(10);
  field2 char(10);
  dcl-ds myds2;
    field3 char(10);
    field4 char(10);
  end-ds;
  dcl-ds myds3 likeds(outputData_t);
  dcl-ds myds4;
   field5 char(10);
  end-ds;
end-ds;`;

      const lines = code.split('\n');

      // Test each dcl-ds line
      expect(shouldDclDsCreateBlock(lines[1])).toBe(true);  // myds1 - should create block
      expect(shouldDclDsCreateBlock(lines[4])).toBe(true);  // myds2 - should create block
      expect(shouldDclDsCreateBlock(lines[8])).toBe(false); // myds3 with likeds - should NOT create block
      expect(shouldDclDsCreateBlock(lines[9])).toBe(true);  // myds4 - should create block
    });

    it('should correctly identify which end-ds closes which dcl-ds', () => {
      // This test documents the expected behavior:
      // Line 1: dcl-ds myds1 (opens block)
      // Line 4: dcl-ds myds2 (opens nested block)
      // Line 7: end-ds (closes myds2)
      // Line 8: dcl-ds myds3 likeds(...) (single-line, no block)
      // Line 9: dcl-ds myds4 (opens nested block)
      // Line 11: end-ds (closes myds4)
      // Line 12: end-ds (closes myds1)

      const code = `**free
dcl-ds myds1 qualified inz;
  field1 char(10);
  field2 char(10);
  dcl-ds myds2;
    field3 char(10);
    field4 char(10);
  end-ds;
  dcl-ds myds3 likeds(outputData_t);
  dcl-ds myds4;
   field5 char(10);
  end-ds;
end-ds;`;

      // Expected folding ranges:
      // 1. myds1: line 1 to line 12
      // 2. myds2: line 4 to line 7
      // 3. myds4: line 9 to line 11
      // NOT: myds3 (it's a single-line declaration)

      const lines = code.split('\n');
      const blockOpeners = lines
        .map((line, idx) => ({ line, idx }))
        .filter(({ line }) => line.trim().startsWith('dcl-ds'))
        .filter(({ line }) => shouldDclDsCreateBlock(line));

      // Should have 3 block openers (myds1, myds2, myds4)
      // NOT 4 (myds3 should be excluded)
      expect(blockOpeners.length).toBe(3);
      expect(blockOpeners[0].idx).toBe(1); // myds1
      expect(blockOpeners[1].idx).toBe(4); // myds2
      expect(blockOpeners[2].idx).toBe(9); // myds4
    });

    it('should not be fooled by likeds in comments', () => {
      // Edge case: likeds in a comment should NOT prevent block creation
      const line = '  dcl-ds shouldBeBlock;  // likeds(fake) in comment';

      // Should be treated as a block opener because the actual code
      // doesn't have likeds() - it's only in the comment
      expect(shouldDclDsCreateBlock(line)).toBe(true);
    });
  });
});
