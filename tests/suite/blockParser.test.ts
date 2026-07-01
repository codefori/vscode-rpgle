import { describe, it, expect } from 'vitest';
import { findAllBlockMatches, RPGLE_BLOCK_PAIRS, isSingleLineDclDs, isInsideOpenDclDsBlock } from '../../language/utils/blockParser';
import { isInCommentOrString, isInSqlBlock } from '../../language/utils/sqlDetection';

describe('blockParser', () => {
  describe('RPGLE_BLOCK_PAIRS', () => {
    it('should contain IF block with middle keywords', () => {
      const ifPair = RPGLE_BLOCK_PAIRS.find(p => p.open.includes('if'));
      expect(ifPair).to.exist;
      expect(ifPair?.middle).to.include('else');
      expect(ifPair?.middle).to.include('elseif');
      expect(ifPair?.close).to.include('endif');
      expect(ifPair?.close).to.include('end');
    });

    it('should contain SELECT block with WHEN keywords', () => {
      const selectPair = RPGLE_BLOCK_PAIRS.find(p => p.open.includes('select'));
      expect(selectPair).to.exist;
      expect(selectPair?.middle).to.include('when');
      expect(selectPair?.middle).to.include('other');
      expect(selectPair?.close).to.include('endsl');
    });

    it('should contain MONITOR block with ON-ERROR', () => {
      const monitorPair = RPGLE_BLOCK_PAIRS.find(p => p.open.includes('monitor'));
      expect(monitorPair).to.exist;
      expect(monitorPair?.middle).to.include('on-error');
      expect(monitorPair?.close).to.include('endmon');
    });
  });

  describe('findAllBlockMatches', () => {
    it('should find simple IF block', () => {
      const code = `if x > 0;
  y = 1;
endif;`;
      const matches = findAllBlockMatches(code, isInCommentOrString, isInSqlBlock);
      expect(matches).to.have.lengthOf(2);
      expect(matches[0].word).to.equal('if');
      expect(matches[1].word).to.equal('endif');
    });

    it('should find nested blocks', () => {
      const code = `if x > 0;
  dow y < 10;
    y += 1;
  enddo;
endif;`;
      const matches = findAllBlockMatches(code, isInCommentOrString, isInSqlBlock);
      expect(matches).to.have.lengthOf(4);
      expect(matches[0].word).to.equal('if');
      expect(matches[1].word).to.equal('dow');
      expect(matches[2].word).to.equal('enddo');
      expect(matches[3].word).to.equal('endif');
    });

    it('should skip keywords in comments', () => {
      const code = `if x > 0; // if this is true
  y = 1;
endif;`;
      const matches = findAllBlockMatches(code, isInCommentOrString, isInSqlBlock);
      expect(matches).to.have.lengthOf(2);
      expect(matches[0].word).to.equal('if');
      expect(matches[1].word).to.equal('endif');
    });

    it('should skip keywords in strings', () => {
      const code = `if x > 0;
  msg = 'if endif';
endif;`;
      const matches = findAllBlockMatches(code, isInCommentOrString, isInSqlBlock);
      expect(matches).to.have.lengthOf(2);
      expect(matches[0].word).to.equal('if');
      expect(matches[1].word).to.equal('endif');
    });

    it('should skip SELECT in SQL blocks', () => {
      const code = `exec sql
  select * from table;

if x > 0;
  y = 1;
endif;`;
      const matches = findAllBlockMatches(code, isInCommentOrString, isInSqlBlock);
      expect(matches).to.have.lengthOf(2);
      expect(matches[0].word).to.equal('if');
      expect(matches[1].word).to.equal('endif');
    });

    it('should find SELECT outside SQL blocks', () => {
      const code = `select;
  when x = 1;
    y = 1;
  other;
    y = 0;
endsl;`;
      const matches = findAllBlockMatches(code, isInCommentOrString, isInSqlBlock);
      expect(matches).to.have.lengthOf(2);
      expect(matches[0].word).to.equal('select');
      expect(matches[1].word).to.equal('endsl');
    });

    it('should not treat ENDIF and ENDSR after fixed-format SQL as SQL content', () => {
      const code = `00001C/EXEC SQL
00002C+ Set Option COMMIT = *NONE
00003C/END-EXEC

00004CSR   ELAB          BEGSR
00005C                   IF        IDARIF <> ''
00006C                   ENDIF
00007C                   ENDSR`;

      const matches = findAllBlockMatches(code, isInCommentOrString, isInSqlBlock);
      const words = matches.map(match => match.word);

      expect(words).to.include('begsr');
      expect(words).to.include('if');
      expect(words).to.include('endif');
      expect(words).to.include('endsr');
    });

    it('should find FOR-EACH blocks', () => {
      const code = `for-each item in list;
  process(item);
endfor;`;
      const matches = findAllBlockMatches(code, isInCommentOrString, isInSqlBlock);
      expect(matches.length).toBe(2);
      expect(matches[0].word).toBe('for');
      expect(matches[1].word).toBe('endfor');
    });

    it('should find DCL-PROC blocks', () => {
      const code = `dcl-proc myProc;
  dcl-pi *n;
  end-pi;
  return;
end-proc;`;
      const matches = findAllBlockMatches(code, isInCommentOrString, isInSqlBlock);
      expect(matches.length).toBe(4);
      expect(matches[0].word).toBe('dcl-proc');
      expect(matches[1].word).toBe('dcl-pi');
      expect(matches[2].word).toBe('end');
      expect(matches[3].word).toBe('end');
    });

    it('should handle END keyword', () => {
      const code = `if x > 0;
  y = 1;
end;`;
      const matches = findAllBlockMatches(code, isInCommentOrString, isInSqlBlock);
      expect(matches).to.have.lengthOf(2);
      expect(matches[0].word).to.equal('if');
      expect(matches[1].word).to.equal('end');
    });

    it('should find all IF variants', () => {
      const code = `ifeq x y;
endif;
ifne a b;
endif;
ifgt c d;
endif;`;
      const matches = findAllBlockMatches(code, isInCommentOrString, isInSqlBlock);
      expect(matches).to.have.lengthOf(6);
      expect(matches[0].word).to.equal('ifeq');
      expect(matches[2].word).to.equal('ifne');
      expect(matches[4].word).to.equal('ifgt');
    });
  });

  // `isSingleLineDclDs` is the shared decision used by BOTH the server-side
  // folding provider and the client-side bracket matcher to tell a single-line
  // `dcl-ds ... likeds()/likerec()` declaration (no end-ds) apart from a block
  // opener.
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

  // `isInsideOpenDclDsBlock` scans backwards to decide whether a line sits
  // inside an open data structure (so a leading `if`/`for`/etc. is a subfield
  // name rather than a control-flow keyword). Getting this wrong drops the
  // keyword from the block matcher and leaves its partner (e.g. `endif`)
  // reported as unmatched.
  describe('isInsideOpenDclDsBlock', () => {
    // Offset of the first character of the given 0-based line.
    const lineStartOffset = (text: string, line: number) =>
      text.split('\n').slice(0, line).reduce((n, l) => n + l.length + 1, 0);

    it('is true for a line inside a genuinely open dcl-ds block', () => {
      const text = `dcl-ds myds qualified;
  field1 char(10);
  field2 char(10);
end-ds;`;
      // line 1 ("  field1 ...") is inside the open block
      expect(isInsideOpenDclDsBlock(text, lineStartOffset(text, 1))).toBe(true);
    });

    it('is false after a balanced multi-line dcl-ds block', () => {
      const text = `dcl-ds myds qualified;
  field1 char(10);
end-ds;

if not bar();
endif;`;
      // line 4 ("if not bar();") is after end-ds, not inside a DS
      expect(isInsideOpenDclDsBlock(text, lineStartOffset(text, 4))).toBe(false);
    });

    it('is false after a single-line likeds() dcl-ds', () => {
      const text = `dcl-ds myds likeds(other_t);

if not bar();
endif;`;
      expect(isInsideOpenDclDsBlock(text, lineStartOffset(text, 2))).toBe(false);
    });

    it('is false after a self-closing "dcl-ds ... end-ds;" on one line', () => {
      // Regression: an EXTNAME structure closed inline with end-ds is a
      // single-line declaration and must NOT be seen as leaving a block open.
      const text = `dcl-ds FAXATT_t extname('G#FAXATT':*all) qualified template inz end-ds;

dcl-proc foo;
  if not bar();
  endif;
end-proc;`;
      // line 3 ("  if not bar();") is plain procedure code, not a DS subfield
      expect(isInsideOpenDclDsBlock(text, lineStartOffset(text, 3))).toBe(false);
    });
  });
});