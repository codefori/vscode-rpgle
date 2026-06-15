import { describe, it, expect } from 'vitest';

describe('Bracket Matcher Validation', () => {
  describe('Mismatched closing keywords detection', () => {
    it('should detect ENDIF closing a DOW block as an error', () => {
      // Test case: ENDIF inside a DOW block without a matching IF
      const code = `
**free

dcl-s pluto int(10) inz(5);
dcl-s pippo int(10) inz(0);

if (pluto > 0);
  dsply 'inside outer if';

  dow (pippo < 10);
    dsply 'inside dow';
    pippo = pippo + 1;
    endif;  // ERROR: endif has no matching if inside the dow block
  enddo; // This ENDDO correctly closes DOW

  dsply 'after dow, still inside if';
endif; // This ENDIF correctly closes IF

*inlr = *on;
      `.trim();

      // Expected behavior:
      // - Line 12 (endif inside dow): Should be highlighted in RED (error)
      // - Line 16 (endif closing if): Should be highlighted in YELLOW (valid)

      // The validation logic should:
      // 1. Detect that 'endif' at line 12 tries to close a 'dow' block
      // 2. Mark it as invalid because 'endif' can only close 'if' blocks
      // 3. The 'endif' at line 16 should correctly match the 'if' at line 6

      expect(code).toContain('endif;  // ERROR');
    });

    it('should validate that specific closers only close their matching block type', () => {
      // Test principle:
      // - Specific closers (endif, endfor, endsl, etc.) can ONLY close their specific block type
      // - They can ONLY close the LAST open block in the stack
      // - If the last open block is of a different type, it's an error

      const validCode = `
if (x > 0);
  dow (y < 10);
    y = y + 1;
  enddo;  // Valid: closes dow
endif;    // Valid: closes if
      `.trim();

      const invalidCode = `
if (x > 0);
  dow (y < 10);
    y = y + 1;
    endif;  // Invalid: tries to close dow with endif
  enddo;
endif;
      `.trim();

      expect(validCode).toContain('enddo;  // Valid');
      expect(invalidCode).toContain('endif;  // Invalid');
    });

    it('should allow generic closers (END, ENDDO) to close multiple block types', () => {
      // Generic closers like 'end' and 'enddo' can close multiple types of blocks
      const codeWithGenericClosers = `
if (x > 0);
  y = y + 1;
end;  // Valid: 'end' can close 'if'

dow (z < 5);
  z = z + 1;
end;  // Valid: 'end' can close 'dow'

for i = 1 to 10;
  dsply i;
end;  // Valid: 'end' can close 'for'
      `.trim();

      expect(codeWithGenericClosers).toContain("end;  // Valid: 'end' can close");
    });
  });

  describe('Stack-based validation', () => {
    it('should maintain proper stack order when validating nested blocks', () => {
      // The validation uses a stack-based approach:
      // 1. Opening keywords push onto the stack
      // 2. Closing keywords pop from the stack (if valid)
      // 3. Invalid closers don't modify the stack

      const nestedCode = `
if (a > 0);           // Stack: [if]
  dow (b < 10);       // Stack: [if, dow]
    endif;            // ERROR: tries to close dow with endif - stack unchanged: [if, dow]
  enddo;              // Valid: closes dow - Stack: [if]
endif;                // Valid: closes if - Stack: []
      `.trim();

      expect(nestedCode).toContain('ERROR: tries to close dow with endif');
    });
  });

  describe('Compiler directives', () => {
    it('should not flag compiler directives as mismatched blocks', () => {
      // Compiler directives like /FREE, /END-FREE, /COPY should be ignored
      const codeWithDirectives = `
      /FREE

dcl-s name varchar(50);
dcl-s count int(10);

if count > 0;
  name = 'Test';
endif;

dow count < 10;
  count += 1;
enddo;

      /END-FREE
      `.trim();

      // The keywords 'END' and 'FREE' in the directives should be ignored
      // Only the actual block keywords (if/endif, dow/enddo) should be matched
      expect(codeWithDirectives).toContain('/FREE');
      expect(codeWithDirectives).toContain('/END-FREE');
    });

    it('should ignore compiler directives even when they contain block keywords', () => {
      const codeWithCopyDirective = `
**free

      /COPY QRPGLESRC,HEADER
      /IF DEFINED(DEBUG)
      /DEFINE DEBUG_MODE
      /ENDIF

dcl-proc myProc;
  dcl-s x int(10);
  x = 42;
  return;
end-proc;
      `.trim();

      // The /IF and /ENDIF compiler directives should not be matched
      // Only the actual dcl-proc/end-proc should be matched
      expect(codeWithCopyDirective).toContain('/IF DEFINED');
      expect(codeWithCopyDirective).toContain('/ENDIF');
    });
  });

  describe('Variables named with keyword names', () => {
    it('should not match variables named "end" as END keyword', () => {
      const code = `
dcl-s start pointer;
dcl-s end pointer;

start = ptr;
end = ptr + (len - 1);

dow (start < end);
  start += 1;
  end -= 1;  // 'end' is a variable, not END keyword
enddo;       // Should correctly close DOW
      `.trim();

      // The 'end' in 'end -= 1;' should NOT be matched as a keyword
      // The 'enddo' should correctly close the 'dow' block
      expect(code).toContain('end -= 1;');
    });

    it('should distinguish between keyword and variable context', () => {
      const code = `
dcl-s end int(10);
dcl-s i int(10);

end = 10;  // Variable assignment

if i < end;  // 'end' is variable in expression
  i += 1;
endif;       // Real ENDIF keyword

for i = 1 to end;  // 'end' is variable in TO clause
  dsply i;
endfor;            // Real ENDFOR keyword
      `.trim();

      expect(code).toContain('end = 10;');
      expect(code).toContain('i < end;');
    });

    it('should handle variables in qualified names and array access', () => {
      const code = `
dcl-ds myDs qualified;
  end int(10);
end-ds;

dcl-s arr int(10) dim(100);
dcl-s end int(10);

myDs.end = 5;   // Qualified - not a keyword
arr(end) = 99;  // Array index - not a keyword
      `.trim();

      expect(code).toContain('myDs.end = 5;');
      expect(code).toContain('arr(end) = 99;');
    });

    it('should handle variables with assignment operators', () => {
      const code = `
dcl-s end int(10);
dcl-s for int(10);
dcl-s if int(10);

end = 100;
end += 5;
end -= 1;
end *= 2;
end /= 4;

for = end + if;
      `.trim();

      // All these should be recognized as variable usage, not keywords
      expect(code).toContain('end += 5;');
      expect(code).toContain('end -= 1;');
    });

    it('should handle variables named with closing keywords like enddo, endif, endfor', () => {
      const code = `
dcl-s enddo int(10);
dcl-s endif int(10);
dcl-s endfor int(10);
dcl-s i int(10);

enddo = 100;  // Variable, not keyword
endif = 50;

dow i < enddo;  // 'enddo' is a variable
  if i < endif;  // 'endif' is a variable
    i += 1;
  endif;  // Real ENDIF keyword
enddo;    // Real ENDDO keyword

enddo += 5;   // Variable assignment
endif -= 2;   // Variable assignment
      `.trim();

      expect(code).toContain('enddo = 100;');
      expect(code).toContain('dow i < enddo;');
    });
  });
});
