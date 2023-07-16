import Declaration from './models/declaration';
import {Range} from "./models/DataPoints";

export interface Keywords {
  [keyword: string]: string|true;
}

export interface IncludeStatement {
  /** vscode.Uri.path */
  toPath: string;
  line: number;
}

export interface CacheProps {
  parameters?: Declaration[];
  subroutines?: Declaration[];
  procedures?: Declaration[];
  files?: Declaration[];
  variables?: Declaration[];
  structs?: Declaration[];
  constants?: Declaration[];
  sqlReferences?: Declaration[];
  indicators?: Declaration[];
  includes?: IncludeStatement[];
}

export interface Rules {
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
  SQLHostVarCheck?: boolean;
  RequireOtherBlock?: boolean;

  /** When true, will update Cache will references found in linter */
  CollectReferences?: boolean;
}

export  interface DefinitionPosition {
  path: string;
  line: number;
}

export interface Offset {
  position?: number,
  end?: number
}

export interface IssueRange {
  offset: Offset;
  type?: "BlankStructNamesCheck"|"QualifiedCheck"|"PrototypeCheck"|"ForceOptionalParens"|
  "NoOCCURS"|"NoSELECTAll"|"UselessOperationCheck"|"UppercaseConstants"|"SpecificCasing"|
  "InvalidDeclareNumber"|"IncorrectVariableCase"|"RequiresParameter"|
  "RequiresProcedureDescription"|"StringLiteralDupe"|"RequireBlankSpecial"|
  "CopybookDirective"|"UppercaseDirectives"|"NoSQLJoins"|"NoGlobalsInProcedures"|
  "NoCTDATA"|"PrettyComments"|"NoGlobalSubroutines"|"NoLocalSubroutines"|"UnexpectedEnd"|
  "NoUnreferenced"|"NoExternalTo"|"NoExecuteImmediate"|"NoExtProgramVariable"|"IncludeMustBeRelative"|
  "SQLHostVarCheck"|"RequireOtherBlock";
  newValue?: string;
}

export interface SelectBlock {
  offset: Offset;
  otherBlockExists: boolean;
}