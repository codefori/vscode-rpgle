import Declaration from './models/declaration';
import {Range} from "./models/DataPoints";
import { IRange } from './types';

export interface Keywords {
  [keyword: string]: string|true;
}

export interface IncludeStatement {
  fromPath: string;
  toPath: string;
  line: number;
}

export interface CacheProps {
  parameters?: Declaration[];
  symbols?: Declaration[];
  sqlReferences?: Declaration[];
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
  DirectiveCase?: "lower"|"upper";
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

  /** Not for user definition */
  InvalidDeclareNumber?: void;
  UnexpectedEnd?: void;
  SQLRunner?: boolean;

  /** When true, will update Cache will references found in linter */
  CollectReferences?: boolean;
}

export  interface DefinitionPosition {
  path: string;
  line: number;
}

export interface Reference {
  uri: string;
  offset: IRange;
}

export interface IssueRange {
  offset: IRange;
  type?: keyof Rules;
  newValue?: string;
}

export interface SelectBlock {
  offset: IRange;
  otherBlockExists: boolean;
}