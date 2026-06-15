**free

// Test case: Mismatched block terminators
// Expected: FOR should close with ENDFOR, IF should close with ENDIF
// Actual code (incorrect): FOR closes with first ENDFOR found

dcl-s x int(10);
dcl-s c int(10);

// INCORRECT CODE - demonstrates the issue
for x = 1 to 10;
  x = 1 + 2;
  if (x > 4);
    c = x;
  endfor;  // WRONG! This should be ENDIF
endif;     // WRONG! This should be ENDFOR

// CORRECT CODE - for comparison
for x = 1 to 10;
  x = 1 + 2;
  if (x > 4);
    c = x;
  end;   // Correct but uses 'end' instead of 'endif'
endfor;    // Correct

// Test with nested blocks
for x = 1 to 10;
  if (x > 5);
    dow (x < 100);
      x = x * 2;
    enddo;
  endif;
endfor;

// Test with generic END opcode
if (x = 1);
  dow (x < 10);
    x = x + 1;
  end;  // Should close DOW (most recent)
end;    // Should close IF

*inlr = *on;
