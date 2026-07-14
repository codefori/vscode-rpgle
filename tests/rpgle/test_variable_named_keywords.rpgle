**free

// Test case for variables named with keyword names
// Issue: Variables named 'end', 'for', 'if', etc. should not be matched as block keywords

// Test 1: Variable named 'end' (the reported issue)
dcl-proc reverse;
  dcl-pi *n;
    ptr pointer value;
    len uns(10) const;
  end-pi;

  dcl-s start pointer;
  dcl-s for int(10);
  dcl-s
        end pointer;  // Variable named 'end'

  dcl-s endif int(10);  // Var named `endif`
  dcl-s strEnd char(1) based(end);
  dcl-s strStart char(1) based(start);

  start = ptr;
  end = ptr + (len - 1);  // Assignment to 'end' variable
  endif += 1;

  dow (start < end);  // 'end' used in comparison - it's a variable
    strStart = %bitxor(strStart : strEnd);
    strEnd = %bitxor(strEnd : strStart);
    strStart = %bitxor(strStart : strEnd);
    start += 1;
    end -= 1;  // 'end' used in assignment - NOT the END keyword!
  enddo;  // This should correctly close the DOW, not be flagged as error

  return;
end-proc;

// Test 2: Variable named 'for'
dcl-proc testForVariable;
  dcl-s for int(10);  // Variable named 'for'
  dcl-s i int(10);

  for = 100;  // Assignment - 'for' is a variable

  dow i < for;  // 'for' used in expression
    i += 1;
  enddo;

  return;
end-proc;

// Test 3: Variable named 'if'
dcl-proc testIfVariable;
  dcl-s if int(10);  // Variable named 'if'
  dcl-s result int(10);

  if = 42;  // Assignment (not a condition)
     result = if * 2;  // 'if' in expression
  endif;

  return;
end-proc;

// Test 4: Mixed - keyword and variable with same name in different contexts
dcl-proc mixedContext;
  dcl-s end int(10);  // Variable
  dcl-s i int(10);

  end = 10;

  // Real IF keyword
  if i < end;  // 'end' is a variable here
    i += 1;
  endif;  // Real ENDIF keyword

  // Real FOR keyword
  for i = 1 to end;  // 'end' is a variable in the TO clause
    dsply i;
  endfor;  // Real ENDFOR keyword

  return;
end-proc;

// Test 5: Variable in qualified names and array access
dcl-proc qualifiedNames;
  dcl-ds myDs qualified;
    end int(10);  // Field named 'end'
    start int(10);
  end-ds;

  dcl-s arr int(10) dim(100);
  dcl-s end int(10);

  myDs.end = 5;  // Qualified name - 'end' is not a keyword
  end = 10;
  arr(end) = 99;  // Array access - 'end' is not a keyword

  return;
end-proc;

// Test 6: Variables named with closing keywords (enddo, endif, endfor, etc.)
dcl-proc testClosingKeywordNames;
  dcl-s enddo int(10);   // Variable named 'enddo'
  dcl-s endif int(10);   // Variable named 'endif'
  dcl-s endfor int(10);  // Variable named 'endfor'
  dcl-s endsl int(10);   // Variable named 'endsl'
  dcl-s i int(10);

  enddo = 100;   // Assignment - these are variables, not keywords
  endif = 50;
  endfor = 25;
  endsl = 10;

  // Use them in expressions
  dow i < enddo;  // 'enddo' is a variable here
    if i < endif;  // 'endif' is a variable here
      i += 1;
    endif;  // Real ENDIF keyword closes the IF
  enddo;    // Real ENDDO keyword closes the DOW

  // Use in assignments with operators
  enddo += 5;   // Variable assignment
  endif -= 2;   // Variable assignment
  endfor *= 2;  // Variable assignment
  endsl /= 2;   // Variable assignment

  return;
end-proc;

// Test 7: Variables in function/procedure calls
dcl-proc testVariablesInProcCalls;
  dcl-s end int(10);
  dcl-s for int(10);
  dcl-s if int(10);
  dcl-s i int(10);

  end = 100;
  for = 50;
  if = 25;

  // Variables as procedure parameters - should NOT be treated as keywords
  if i > someFunc(end);  // 'end' is a parameter, not END keyword
    i += 1;
  endif;  // Real ENDIF keyword

  dow anotherFunc(for, end) > 0;  // 'for' and 'end' are parameters
    i += 1;
  enddo;  // Real ENDDO keyword

  // Variables in expressions with procedure calls
  i = calculateValue(if) + end;  // 'if' and 'end' are variables

  return;
end-proc;

dcl-proc someFunc;
  dcl-pi *n int(10);
    value int(10);
  end-pi;
  return value * 2;
end-proc;

dcl-proc anotherFunc;
  dcl-pi *n int(10);
    a int(10);
    b int(10);
  end-pi;
  return a - b;
end-proc;

dcl-proc calculateValue;
  dcl-pi *n int(10);
    val int(10);
  end-pi;
  return val;
end-proc;

