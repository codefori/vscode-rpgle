**FREE
Ctl-Opt Main(MainLine);
/// -------------------------------------
// Main
/// -------------------------------------
Dcl-Proc MainLine;
  Dcl-Pi MainLine Extpgm('MAINTLINE');
    Iof Char(1);
  End-Pi;
  Dcl-S myString Varchar(20);

  myString = CvtToMixed(myString);
End-Proc;

/// -------------------------------------
// CvtToMixed
// Convert the passed string to mixed case or 
// what is normally called Title case.
// @param  Source string
// @return  Title cased string
/// -------------------------------------
Dcl-Proc CvtToMixed;
  Dcl-Pi CvtToMixed Extpgm('MAINTLINE');
    theString Varchar(100);
  End-Pi;

  return theString;
End-Proc;