**free

// Test case 1: END-PROC without DCL-PI (should be valid)
dcl-proc testProcNoPi;
  dcl-s localVar int(10);

  localVar = 42;
  return;
end-proc;

// Test case 2: END-PROC with DCL-PI...END-PI (should be valid)
dcl-proc testProcWithPi;
  dcl-pi *n;
    inputParam int(10);
  end-pi;

  dcl-s result int(10);
  result = inputParam * 2;
  return result;
end-proc;

// Test case 3: Nested blocks in procedure (should be valid)
dcl-proc complexProc;
  dcl-s counter int(10);

  if counter > 0;
    dow counter < 10;
      counter += 1;
    enddo;
  endif;

  return;
end-proc;

// Test case 4: Multiple procedures (should all be valid)
dcl-proc proc1;
  return;
end-proc;

dcl-proc proc2;
  dcl-s x int(10);
  x = 100;
  return;
end-proc;
