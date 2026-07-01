import { describe, it, expect } from 'vitest';
import { isSingleLineDclDs } from '../../language/utils/blockParser';

// `isSingleLineDclDs` is the shared decision used by BOTH the server-side
// folding provider and the client-side bracket matcher to tell a single-line
// `dcl-ds ... likeds()/likerec()` declaration (no end-ds) apart from a block
// opener. These tests exercise that exported function directly, so a regression
// in the real logic fails the suite.
describe('isSingleLineDclDs', () => {

  describe('dcl-ds with likeds() is a single-line declaration', () => {
    it('detects a plain likeds()', () => {
      expect(isSingleLineDclDs('  dcl-ds appset likeds(t_APPSET) inz(*likeds);')).toBe(true);
    });

    it('is case-insensitive (uppercase LIKEDS)', () => {
      expect(isSingleLineDclDs('  dcl-ds myDs LIKEDS(template_t) INZ;')).toBe(true);
    });

    it('is case-insensitive (mixed case LiKeDs)', () => {
      expect(isSingleLineDclDs('  dcl-ds data LiKeDs(other_t);')).toBe(true);
    });

    it('detects likeds() with qualified', () => {
      expect(isSingleLineDclDs('  dcl-ds customer likeds(CUSTOMER_t) qualified inz(*likeds);')).toBe(true);
    });

    it('detects likeds() with dim()', () => {
      expect(isSingleLineDclDs('  dcl-ds items likeds(item_t) dim(100);')).toBe(true);
    });

    it('tolerates whitespace before the paren', () => {
      expect(isSingleLineDclDs('  dcl-ds   myDs   likeds  (  template_t  )  ;')).toBe(true);
    });
  });

  describe('dcl-ds with likerec() is a single-line declaration', () => {
    it('detects a plain likerec()', () => {
      expect(isSingleLineDclDs('  dcl-ds record likerec(MYFILE);')).toBe(true);
    });

    it('is case-insensitive (uppercase LIKEREC)', () => {
      expect(isSingleLineDclDs('  dcl-ds fileRec LIKEREC(DATAFILE);')).toBe(true);
    });

    it('detects likerec() with based()', () => {
      expect(isSingleLineDclDs('  dcl-ds dirEntry likerec(dirent) based(ptrDirEntry);')).toBe(true);
    });
  });

  describe('dcl-ds without likeds/likerec is a block opener', () => {
    it('plain qualified ds', () => {
      expect(isSingleLineDclDs('  dcl-ds myStruct qualified;')).toBe(false);
    });

    it('extname ds', () => {
      expect(isSingleLineDclDs("  dcl-ds qcustcdt extname('QIWS/QCUSTCDT') qualified;")).toBe(false);
    });

    it('template ds', () => {
      expect(isSingleLineDclDs('  dcl-ds template_t qualified template;')).toBe(false);
    });

    it('unindented qualified ds', () => {
      expect(isSingleLineDclDs('dcl-ds params qualified inz;')).toBe(false);
    });
  });

  describe('comments are stripped before the check', () => {
    it('likeds() in real code still counts even with a trailing comment', () => {
      expect(isSingleLineDclDs('  dcl-ds myDs likeds(t) inz; // likeds creates a copy')).toBe(true);
    });

    it('likeds() only inside a comment does NOT suppress the block', () => {
      expect(isSingleLineDclDs('  dcl-ds shouldBeBlock;  // likeds(fake) in comment')).toBe(false);
    });

    it('a non-likeds word in a comment is ignored', () => {
      expect(isSingleLineDclDs('  dcl-ds myStruct qualified; // not_likeds_field')).toBe(false);
    });
  });

  describe('only applies to dcl-ds lines', () => {
    it('returns false for a non-dcl-ds line that mentions likeds', () => {
      expect(isSingleLineDclDs('    data likeds(APMDAL_t)')).toBe(false);
    });

    it('returns false for an unrelated statement', () => {
      expect(isSingleLineDclDs('  return get_error();')).toBe(false);
    });
  });

  describe('nested data structures classify each dcl-ds independently', () => {
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

    it('marks only the likeds() member as single-line', () => {
      const lines = code.split('\n');
      expect(isSingleLineDclDs(lines[1])).toBe(false); // myds1 - block opener
      expect(isSingleLineDclDs(lines[4])).toBe(false); // myds2 - block opener
      expect(isSingleLineDclDs(lines[8])).toBe(true);  // myds3 - single-line likeds
      expect(isSingleLineDclDs(lines[9])).toBe(false); // myds4 - block opener
    });

    it('yields exactly three block-opening dcl-ds (myds1, myds2, myds4)', () => {
      const blockOpeners = code.split('\n')
        .map((line, idx) => ({ line, idx }))
        .filter(({ line }) => line.trim().startsWith('dcl-ds'))
        .filter(({ line }) => !isSingleLineDclDs(line));

      expect(blockOpeners.map(b => b.idx)).toEqual([1, 4, 9]);
    });
  });
});
