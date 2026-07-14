**free

// Test file for DCL-* / END-* block validation

dcl-proc testProc;
  dcl-pi testProc;
    parm1 char(10);
  end-pi;

  dcl-ds myDataStruct;
    field1 char(20);
    field2 int(10);
  end-ds;

  // Procedure logic here
  dsply 'test';
end-proc;

// Test with nested structures
dcl-proc anotherProc;
  dcl-pi anotherProc;
  end-pi;

  dcl-ds outerDS;
    dcl-ds innerDS;
      innerField char(10);
    end-ds;
    outerField int(10);
  end-ds;

end-proc;

// INCORRECT: Missing end-proc
dcl-proc badProc;
  dsply 'missing end-proc';
// end-proc is missing - this should cause errors

*inlr = *on;
