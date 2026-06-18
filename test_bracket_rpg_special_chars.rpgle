**FREE

// Test file to verify bracket matching doesn't highlight keywords
// that are part of variable names with RPG IV special characters (#, @, $)

DCL-S Wrk@End CHAR(10);   // Should NOT highlight 'End'
DCL-S Wrk#End CHAR(10);   // Should NOT highlight 'End'
DCL-S Wrk$End CHAR(10);   // Should NOT highlight 'End'
DCL-S #end CHAR(10);      // Should NOT highlight - valid field name
DCL-S #endif CHAR(10);    // Should NOT highlight - valid field name
DCL-S @endif CHAR(10);    // Should NOT highlight - valid field name
DCL-S $enddo CHAR(10);    // Should NOT highlight - valid field name

// These SHOULD highlight when cursor is on them:
IF Wrk@End = 'test';      // IF/ENDIF should highlight
  Wrk@End = 'value';
ENDIF;

DOW *ON;                  // DOW/ENDDO should highlight
  // Loop content
ENDDO;

SELECT;                   // SELECT/ENDSL should highlight
  WHEN Wrk#End = '1';
    // Action
  WHEN Wrk$End = '2';
    // Action
  OTHER;
    // Default
ENDSL;

RETURN;
