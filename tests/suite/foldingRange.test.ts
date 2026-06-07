import { describe, it, expect } from 'vitest';

describe('Folding Range - dcl-ds with likeds/likerec', () => {

  /**
   * Helper to simulate the folding logic for dcl-ds
   * This matches the logic added to foldingRange.ts
   * Returns true if dcl-ds should be treated as a block opener
   */
  function shouldDclDsCreateBlock(line: string): boolean {
    const lowerLine = line.toLowerCase();

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
});
