const vscode = require(`vscode`);

/** @interface */
module.exports = class ContentRange {
  constructor() {
    /** @type {vscode.Range} */
    this.range = null;
    
    /** @type {{position: number, length: number}|null} */
    this.offset = {
      position: null,
      length: null
    };

    /** @type {
          "BlankStructNamesCheck"|"QualifiedCheck"|"PrototypeCheck"|"ForceOptionalParens"|
          "NoOCCURS"|"NoSELECTAll"|"UselessOperationCheck"|"UppercaseConstants"|"SpecificCasing"|
          "InvalidDeclareNumber"|"IncorrectVariableCase"|"RequiresParameter"|
          "RequiresProcedureDescription"|"StringLiteralDupe"|"RequireBlankSpecial"|
          "CopybookDirective"|"UppercaseDirectives"|"NoSQLJoins"|"NoGlobalsInProcedures"|
          "NoCTDATA"|"PrettyComments"|"NoGlobalSubroutines"|"NoLocalSubroutines"|"UnexpectedEnd"|
          "NoUnreferenced"
       }
     * */
    this.type = null;

    /** @type {string|null} If wanted to replace the range, use this value */
    this.newValue = null;
  }
}