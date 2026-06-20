**FREE

// Test file to demonstrate unmatched END hover tooltip

dcl-ds struct qualified;
  parm1 char(10);
  end int(10);
end-ds;

dcl-s counter int(10);

// This IF has a matching ENDIF - no error
if counter > 10;
   dow counter < 100;
     counter += 1;
    endif;
  counter = 0;
endif;

// This IF has no matching ENDIF - will show error
if counter < 5;
  counter += 1;

// This ENDIF has no matching IF - should show "Unmatched ENDIF statement" on hover
endif;

// This DOW has no matching ENDDO - the END below is unmatched
dow counter < 100;
  counter += 10;

// Hover over this END should show "Unmatched END statement"
end;

// This END-PROC has no matching DCL-PROC - should show error on hover
end-proc;

return;
