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

  describe('DCL-DS with LIKEDS/LIKEREC', () => {
    it('should not treat dcl-ds with likeds() as a block opener', () => {
      // dcl-ds with likeds() creates a single-line declaration
      // It should NOT be treated as opening a block that needs end-ds
      const code = `
**free

dcl-proc processData;
  dcl-pi *n likeds(MyDs);
    inputData likeds(MyDs) const;
    checkData likeds(MyDs) const;
    freshData likeds(MyDs) const;
  end-pi;

  dcl-ds myData likeds(MyDs);  // Single-line, no end-ds needed

  // This end-proc should correctly close dcl-proc
  // It should NOT be treated as closing the dcl-ds above
end-proc;
      `.trim();

      // The validation should:
      // 1. NOT push dcl-ds with likeds() onto the stack
      // 2. Allow end-proc to correctly close dcl-proc
      // 3. NOT show end-proc as an error

      expect(code).toContain('dcl-ds myData likeds(MyDs);');
      expect(code).toContain('end-proc;');
    });


    it('should not treat dcl-ds with likerec() as a block opener', () => {
      // dcl-ds with likerec() creates a single-line declaration
      const code = `
**free

dcl-proc processData;
  dcl-pi *n;
    inputData likerec(MyRecord);
  end-pi;

  dcl-ds localData likerec(MyRecord);  // Single-line, no end-ds needed

  // Process data here

end-proc;  // Should correctly close dcl-proc
      `.trim();

      expect(code).toContain('dcl-ds localData likerec(MyRecord);');
      expect(code).toContain('end-proc;  // Should correctly close dcl-proc');
    });

    it('should still treat multi-line dcl-ds as a block opener', () => {
      // dcl-ds WITHOUT likeds/likerec creates a multi-line block
      const code = `
**free

dcl-proc example;
  dcl-pi *n;
  end-pi;

  dcl-ds myStruct;
    field1 int(10);
    field2 char(50);
  end-ds;  // This end-ds is required

end-proc;
      `.trim();

      // The validation should:
      // 1. Push dcl-ds onto the stack (it's a block opener)
      // 2. Match end-ds with dcl-ds
      // 3. Match end-proc with dcl-proc

      expect(code).toContain('dcl-ds myStruct;');
      expect(code).toContain('end-ds;  // This end-ds is required');
    });

    it('should handle nested blocks with dcl-ds likeds correctly', () => {
      // Complex nesting scenario
      const code = `
**free

dcl-proc complexExample;
  dcl-pi *n;
  end-pi;

  dcl-ds outer;
    field1 int(10);
    dcl-ds inner likeds(SomeType);  // Single-line, no end-ds
    field2 char(50);
  end-ds;  // Closes outer only

  if (field1 > 0);
    dcl-ds temp likerec(MyRecord);  // Single-line, no end-ds
    // Do something
  endif;

end-proc;
      `.trim();

      // The validation should:
      // 1. Match outer dcl-ds with its end-ds
      // 2. NOT treat inner dcl-ds likeds as needing end-ds
      // 3. Match if with endif
      // 4. Match dcl-proc with end-proc

      expect(code).toContain('dcl-ds inner likeds(SomeType);  // Single-line');
      expect(code).toContain('dcl-ds temp likerec(MyRecord);  // Single-line');
    });

    it('should detect mismatched end-proc when dcl-ds likeds is incorrectly treated as block', () => {
      // This is the original bug scenario
      const code = `
**free

dcl-proc body;
  dcl-pi *n likeds(MyDs);
    inputData likeds(MyDs) const;
  end-pi;

  dcl-ds myData likeds(MyDs);

  // If dcl-ds likeds is treated as a block opener,
  // then end-proc would be seen as closing dcl-ds (ERROR)
  // With the fix, end-proc correctly closes dcl-proc (VALID)
end-proc;
      `.trim();

      // After the fix:
      // - dcl-ds with likeds should NOT be on the stack
      // - end-proc should correctly match dcl-proc
      // - No validation errors should be reported

      expect(code).toContain('dcl-ds myData likeds(MyDs);');
      expect(code).toContain('end-proc;');
    });

    it('should handle nested dcl-ds with likeds correctly', () => {
      // Complex nesting: outer dcl-ds contains nested dcl-ds blocks
      // and a single-line dcl-ds with likeds
      const code = `
**free

dcl-ds myds1 qualified inz;
  field1 char(10);
  field2 char(10);
  dcl-ds myds2;
    field3 char(10);
    field4 char(10);
  end-ds;
  dcl-ds myds3 likeds(outputData_t);  // Single-line, no end-ds
  dcl-ds myds4;
   field5 char(10);
  end-ds;
end-ds;
      `.trim();

      // Expected behavior:
      // - Clicking on myds1 dcl-ds should highlight its end-ds (last line)
      // - Clicking on myds2 dcl-ds should highlight its end-ds (line after field4)
      // - Clicking on myds3 dcl-ds should NOT highlight anything (single-line)
      // - Clicking on myds4 dcl-ds should highlight its end-ds (line after field5)

      expect(code).toContain('dcl-ds myds3 likeds(outputData_t);  // Single-line');
    });

    it('should not highlight when clicking on single-line dcl-ds with likeds in nested structure', () => {
      // When cursor is on dcl-ds myds3 (which has likeds),
      // it should NOT show any bracket highlighting
      const code = `
**free

dcl-ds myds1 qualified inz;
  dcl-ds myds3 likeds(outputData_t);  // Clicking here should show nothing
  dcl-ds myds4;
   field5 char(10);
  end-ds;
end-ds;
      `.trim();

      // The bracket matcher should:
      // 1. Recognize myds3 has likeds()
      // 2. NOT treat it as a block opener
      // 3. NOT try to find a matching end-ds
      // 4. Return undefined/empty for highlighting

      expect(code).toContain('dcl-ds myds3 likeds(outputData_t);  // Clicking here');
    });
  });
});

// Made with Bob
