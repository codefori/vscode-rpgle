**FREE

// Test file to verify bracket matching doesn't highlight keywords
// that are part of variable names with RPG IV special characters and
// international CCSID variants

// CCSID 37 (US/English): #, @, $
DCL-S Wrk@End CHAR(10);   // Should NOT highlight 'End'
DCL-S Wrk#End CHAR(10);   // Should NOT highlight 'End'
DCL-S Wrk$End CHAR(10);   // Should NOT highlight 'End'
DCL-S #end CHAR(10);      // Should NOT highlight - valid field name
DCL-S #endif CHAR(10);    // Should NOT highlight - valid field name
DCL-S @endif CHAR(10);    // Should NOT highlight - valid field name
DCL-S $enddo CHAR(10);    // Should NOT highlight - valid field name

// CCSID 273 (German): §
DCL-S §Betrag CHAR(10);   // Should NOT highlight - § is valid in German CCSID
DCL-S Wrk§End CHAR(10);   // Should NOT highlight 'End'

// CCSID 280 (Italian): §, £
DCL-S £Importo CHAR(10);  // Should NOT highlight - £ is valid in Italian CCSID
DCL-S Wrk£End CHAR(10);   // Should NOT highlight 'End'

// CCSID 297 (French): £, à, À
DCL-S £TaxAmt CHAR(10);   // Should NOT highlight - £ is valid in French CCSID
DCL-S àFeld CHAR(10);     // Should NOT highlight - à is valid in French CCSID
DCL-S ÀFeld CHAR(10);     // Should NOT highlight - À is valid in French CCSID
DCL-S Wrkàenddo CHAR(10); // Should NOT highlight 'enddo'

// CCSID 277 (Norwegian/Danish): Æ, æ, Ø, ø
DCL-S ÆndStatus CHAR(10); // Should NOT highlight - Æ is valid in Norwegian CCSID
DCL-S Økonomi CHAR(10);   // Should NOT highlight - Ø is valid in Norwegian CCSID
DCL-S værdiEnd CHAR(10);  // Should NOT highlight 'End'
DCL-S grønEnd CHAR(10);   // Should NOT highlight 'End'

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
  WHEN §Betrag > 100;
    // German example
  WHEN £TaxAmt > 50;
    // Italian/French example

RETURN;
