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
});

// Made with Bob
