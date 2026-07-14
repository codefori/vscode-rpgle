      /FREE

// Test case for /END-FREE and /FREE compiler directive issues
// These compiler directives should NOT be flagged as mismatched blocks

dcl-s name varchar(50);
dcl-s count int(10);

// Test IF block (should work normally)
if count > 0;
  name = 'Test';
endif;

// Test DOW loop (should work normally)
dow count < 10;
  count += 1;
enddo;

      /END-FREE
