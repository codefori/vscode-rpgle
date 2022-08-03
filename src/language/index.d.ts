
interface Keywords {
  [keyword]: string|true;
}

interface Rules {
  indent?: number;
  BlankStructNamesCheck?: boolean;
  QualifiedCheck?: boolean;
  PrototypeCheck?: boolean;
  ForceOptionalParens?: boolean;
  NoOCCURS?: boolean;
  NoSELECTAll?: boolean;
  UselessOperationCheck?: boolean;
  UppercaseConstants?: boolean;
  IncorrectVariableCase?: boolean;
  RequiresParameter?: boolean;
  RequiresProcedureDescription?: boolean;
  StringLiteralDupe?: boolean;
  literalMinimum?: number;
  RequireBlankSpecial?: boolean;
  CopybookDirective?: "copy"|"include";
  UppercaseDirectives?: boolean;
  NoSQLJoins?: boolean;
  NoGlobalsInProcedures?: boolean;
  SpecificCasing?: {operation: string, expected: string}[];
  NoCTDATA?: boolean;
  PrettyComments?: boolean;
  NoGlobalSubroutines?: boolean;
  NoLocalSubroutines?: boolean;
  NoUnreferenced?: boolean;
  NoExternalTo?: string[];
  NoExecuteImmediate?: boolean;
  NoExtProgramVariable?: boolean;
  IncludeMustBeRelative?: boolean;

  /** When true, will update Cache will references found in linter */
  CollectReferences?: boolean;
}

interface Offset {
  position?: number,
  length?: number
}

interface IssueRange {
  range: vscode.Range;
  offset?: Offset;
  type?: "BlankStructNamesCheck"|"QualifiedCheck"|"PrototypeCheck"|"ForceOptionalParens"|
  "NoOCCURS"|"NoSELECTAll"|"UselessOperationCheck"|"UppercaseConstants"|"SpecificCasing"|
  "InvalidDeclareNumber"|"IncorrectVariableCase"|"RequiresParameter"|
  "RequiresProcedureDescription"|"StringLiteralDupe"|"RequireBlankSpecial"|
  "CopybookDirective"|"UppercaseDirectives"|"NoSQLJoins"|"NoGlobalsInProcedures"|
  "NoCTDATA"|"PrettyComments"|"NoGlobalSubroutines"|"NoLocalSubroutines"|"UnexpectedEnd"|
  "NoUnreferenced"|"NoExternalTo"|"NoExecuteImmediate"|"NoExtProgramVariable"|"IncludeMustBeRelative";
  newValue?: string;
}