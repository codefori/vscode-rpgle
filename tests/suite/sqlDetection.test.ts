import { describe, it, expect } from 'vitest';
import { isInSqlBlock, isInCommentOrString } from '../../language/utils/sqlDetection';

describe('sqlDetection', () => {
  describe('isInSqlBlock', () => {
    it('should detect position on EXEC SQL line', () => {
      const code = `exec sql select * from table;`;
      const offset = 10; // Inside "select"
      expect(isInSqlBlock(code, offset)).to.be.true;
    });

    it('should detect position in multi-line SQL block', () => {
      const code = `exec sql
  select *
  from table
  where id = 1;`;
      const offset = code.indexOf('from'); // Inside SQL block
      expect(isInSqlBlock(code, offset)).to.be.true;
    });

    it('should not detect position after SQL block ends', () => {
      const code = `exec sql
  select * from table;
  
if x > 0;
  y = 1;
endif;`;
      const offset = code.indexOf('if x'); // After SQL block
      expect(isInSqlBlock(code, offset)).to.be.false;
    });

    it('should handle SQL block without semicolon yet', () => {
      const code = `exec sql
  select *
  from table`;
      const offset = code.indexOf('from');
      expect(isInSqlBlock(code, offset)).to.be.true;
    });

    it('should not detect position before SQL block', () => {
      const code = `if x > 0;
  exec sql select * from table;
endif;`;
      const offset = code.indexOf('if'); // Before SQL block
      expect(isInSqlBlock(code, offset)).to.be.false;
    });

    it('should handle multiple SQL blocks', () => {
      const code = `exec sql select * from table1;
if x > 0;
  exec sql select * from table2;
endif;`;
      const offset1 = code.indexOf('table1');
      const offset2 = code.indexOf('if x');
      const offset3 = code.indexOf('table2');
      
      expect(isInSqlBlock(code, offset1)).to.be.true;
      expect(isInSqlBlock(code, offset2)).to.be.false;
      expect(isInSqlBlock(code, offset3)).to.be.true;
    });

    it('should handle EXEC SQL with whitespace', () => {
      const code = `  exec   sql
    select * from table;`;
      const offset = code.indexOf('select');
      expect(isInSqlBlock(code, offset)).to.be.true;
    });

    it('should be case insensitive', () => {
      const code = `EXEC SQL SELECT * FROM TABLE;`;
      const offset = code.indexOf('SELECT');
      expect(isInSqlBlock(code, offset)).to.be.true;
    });
  });

  describe('isInCommentOrString', () => {
    it('should detect position in line comment', () => {
      const code = `if x > 0; // this is a comment`;
      const offset = code.indexOf('comment');
      expect(isInCommentOrString(code, offset)).to.be.true;
    });

    it('should not detect position before comment', () => {
      const code = `if x > 0; // comment`;
      const offset = code.indexOf('if');
      expect(isInCommentOrString(code, offset)).to.be.false;
    });

    it('should detect position inside single-quoted string', () => {
      const code = `msg = 'hello world';`;
      const offset = code.indexOf('world');
      expect(isInCommentOrString(code, offset)).to.be.true;
    });

    it('should not detect position outside string', () => {
      const code = `msg = 'hello';`;
      const offset = code.indexOf('msg');
      expect(isInCommentOrString(code, offset)).to.be.false;
    });

    it('should handle multiple strings on same line', () => {
      const code = `msg = 'hello' + 'world';`;
      const offset1 = code.indexOf('hello');
      const offset2 = code.indexOf('+');
      const offset3 = code.indexOf('world');
      
      expect(isInCommentOrString(code, offset1)).to.be.true;
      expect(isInCommentOrString(code, offset2)).to.be.false;
      expect(isInCommentOrString(code, offset3)).to.be.true;
    });

    it('should handle string with escaped quotes', () => {
      const code = `msg = 'it''s working';`;
      const offset = code.indexOf('working');
      expect(isInCommentOrString(code, offset)).to.be.true;
    });

    it('should handle comment after string', () => {
      const code = `msg = 'hello'; // comment`;
      const offset1 = code.indexOf('hello');
      const offset2 = code.indexOf('comment');
      
      expect(isInCommentOrString(code, offset1)).to.be.true;
      expect(isInCommentOrString(code, offset2)).to.be.true;
    });

    it('should not detect in empty line', () => {
      const code = `if x > 0;

endif;`;
      const offset = code.indexOf('\n\n') + 1;
      expect(isInCommentOrString(code, offset)).to.be.false;
    });

    it('should handle line starting with comment', () => {
      const code = `// full line comment
if x > 0;`;
      const offset = code.indexOf('full');
      expect(isInCommentOrString(code, offset)).to.be.true;
    });

    it('should handle unclosed string at end of line', () => {
      const code = `msg = 'unclosed`;
      const offset = code.indexOf('unclosed');
      expect(isInCommentOrString(code, offset)).to.be.true;
    });
  });

  describe('integration tests', () => {
    it('should correctly identify SQL SELECT vs RPGLE SELECT', () => {
      const code = `exec sql
  select * from table;

select;
  when x = 1;
    y = 1;
endsl;`;
      
      const sqlSelectOffset = code.indexOf('select *');
      const rpgleSelectOffset = code.indexOf('select;');
      
      expect(isInSqlBlock(code, sqlSelectOffset)).to.be.true;
      expect(isInSqlBlock(code, rpgleSelectOffset)).to.be.false;
    });

    it('should handle commented SQL blocks', () => {
      const code = `// exec sql select * from table;
if x > 0;
  y = 1;
endif;`;
      
      const commentedSqlOffset = code.indexOf('select');
      const ifOffset = code.indexOf('if x');
      
      expect(isInCommentOrString(code, commentedSqlOffset)).to.be.true;
      expect(isInCommentOrString(code, ifOffset)).to.be.false;
    });

    it('should handle SQL in string literal', () => {
      const code = `msg = 'exec sql select * from table';`;
      const offset = code.indexOf('select');
      
      expect(isInCommentOrString(code, offset)).to.be.true;
      expect(isInSqlBlock(code, offset)).to.be.false;
    });
  });
});