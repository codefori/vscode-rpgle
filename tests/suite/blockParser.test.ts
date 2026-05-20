import { describe, it, expect } from 'vitest';
import { findAllBlockMatches, RPGLE_BLOCK_PAIRS } from '../../language/utils/blockParser';
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
});