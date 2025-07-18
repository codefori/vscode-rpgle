/* eslint-disable no-case-declarations */

import Cache from "./models/cache";
import { tokenise } from "./tokens";
import oneLineTriggers from "./models/oneLineTriggers";
import { Range, Position } from "./models/DataPoints";
import opcodes from "./models/opcodes";
import Document from "./document";
import { IssueRange, Rules, SelectBlock } from "./parserTypes";
import Declaration from "./models/declaration";
import { IRange, Token } from "./types";
import { NO_NAME } from "./statement";

const BANNED_FROM_INCLUDES = [`NoUnreferenced`];
const INCLUDE_EXTENSIONS = [`rpgleinc`, `rpgleh`];

const errorText = {
  'BlankStructNamesCheck': `Struct names cannot be blank (\`*N\`).`,
  'QualifiedCheck': `Struct names must be qualified (\`QUALIFIED\`).`,
  'PrototypeCheck': `Prototypes can only be defined with either \`EXTPGM\` or \`EXTPROC\``,
  'ForceOptionalParens': `Expressions must be surrounded by brackets.`,
  'NoOCCURS': `\`OCCURS\` is not allowed.`,
  'NoSELECTAll': `\`SELECT *\` is not allowed in Embedded SQL.`,
  'UselessOperationCheck': `Redundant operation codes (EVAL, CALLP, DCL-PARM, DCL-SUBF) not allowed.`,
  'UppercaseConstants': `Constants must be in uppercase.`,
  'SpecificCasing': `Does not match required case.`,
  'InvalidDeclareNumber': `Variable names cannot start with a number`,
  'IncorrectVariableCase': `Variable name casing does not match definition.`,
  'RequiresParameter': `Procedure calls require brackets.`,
  'RequiresProcedureDescription': `Procedures require a title and description.`,
  'StringLiteralDupe': `Same string literal used more than once. Consider using a constant instead.`,
  'RequireBlankSpecial': `\`*BLANK\` should be used over empty string literals.`,
  'CopybookDirective': `Directive does not match requirement.`,
  'DirectiveCase': `Does not match required case.`,
  'NoSQLJoins': `SQL joins are not allowed. Consider creating a view instead.`,
  'NoGlobalsInProcedures': `Global variables should not be referenced in procedures.`,
  'NoCTDATA': `\`CTDATA\` is not allowed.`,
  'PrettyComments': `Comments must be correctly formatted.`,
  'NoGlobalSubroutines': `Subroutines should not be defined in the global scope.`,
  'NoLocalSubroutines': `Subroutines should not be defined in procedures.`,
  'UnexpectedEnd': `Statement unexpected. Likely missing the equivalent \`DCL..\`/\`BEG..\``,
  'NoUnreferenced': `No reference to definition.`,
  'NoExternalTo': `Cannot declare prototype to this external API.`,
  'NoExecuteImmediate': `EXECUTE IMMEDIATE is not allowed.`,
  'NoExtProgramVariable': `Not allowed to use variable in EXTPGM or EXTPROC.`,
  'IncludeMustBeRelative': `Path not valid. It must be relative to the project.`,
  'SQLHostVarCheck': `Also defined in scope. Should likely be host variable.`,
  'RequireOtherBlock': `OTHER block missing from SELECT block.`,
  'SQLRunner': `Execute this statement through Db2 for i`
};

const skipRules = {
  none: 0,
  single: 1, // Skips all checking
  singleIndent: 2, // Skips indent check
  singleRules: 3, // Skips rule check
};

export default class Linter {
  static getErrorText(error) {
    return errorText[error];
  }

  static getErrors(data: { uri: string, content: string, availableIncludes?: string[] }, rules: Rules, globalScope?: Cache) {
    const indentEnabled = rules.indent !== undefined;
    const indent = rules.indent || 2;

    let uriExtension = data.uri.split('.').pop().toLowerCase();
    if (uriExtension.includes(`?`)) {
      uriExtension = uriExtension.split(`?`)[0];
    }

    if (INCLUDE_EXTENSIONS.includes(uriExtension)) {
      for (const banned of BANNED_FROM_INCLUDES) {
        rules[banned] = false;
      }
    }

    // Excluding indent
    const ruleCount = Object.keys(rules).length - (rules.indent ? 1 : 0);

    if (!globalScope)
      globalScope = new Cache();

    const globalProcs = globalScope.procedures;

    interface ReferenceInfo {name: string, skipRules?: boolean};

    let inProcedure: ReferenceInfo|undefined;
    let inSubroutine: ReferenceInfo|undefined;
    let inStruct: string[] = [];
    let inPrototype = false;
    let inOnExit = false;

    let lineNumber = -1;

    /** @type {{line: number, expectedIndent: number, currentIndent: number}[]} */
    const indentErrors = [];

    // Offset is always the offset within the range

    const errors: IssueRange[] = [];

    let expectedIndent = 0;
    let currentIndent = 0;

    let skipIndentCheck = false;

    let opcode: string;

    // Make all external refs uppercase.
    if (rules.NoExternalTo && rules.NoExternalTo.length) {
      rules.NoExternalTo = rules.NoExternalTo.map(val => val.toUpperCase());
    }

    const selectBlocks: SelectBlock[] = [];

    const stringLiterals: { value: string, list: { line: number, offset: IRange }[] }[] = [];

    let directiveScope = 0;
    let currentRule = skipRules.none;

    const doc = new Document(data.content);

    for (let si = 0; si < doc.statements.length; si++) {
      const docStatement = doc.statements[si];
      const statement = docStatement.tokens.some(t => t.type === `newline`) ? docStatement.tokens.filter(t => t.type !== `newline`) : docStatement.tokens;
      lineNumber = docStatement.range.line;
      currentIndent = docStatement.indent;

      if (currentIndent >= 0) {
        skipIndentCheck = false;

        // Comment checking
        if (statement[0].type === `comment`) {
          const comment = statement[0].value.substring(2).trimEnd();
          if (rules.PrettyComments) {
            // We check for the slash because the documentation requires ///.
            if (comment && comment[0] !== `/`) {
              const startSpaces = comment.search(/\S/);

              if (startSpaces === 0) {
                errors.push({
                  offset: { start: statement[0].range.start, end: statement[0].range.start + 2 },
                  type: `PrettyComments`,
                  newValue: `// `,
                });
              }
            }
          } else {
            skipIndentCheck = true;
          }

          // Special comment check
          switch (comment.trim()) {
            case `@rpglint-skip`:
              currentRule = skipRules.single;
              continue;
            case `@rpglint-skip-indent`:
              currentRule = skipRules.singleIndent;
              continue;
            case `@rpglint-skip-rules`:
              currentRule = skipRules.singleRules;
              continue;
          }
        }

        // Linter checking
        if (ruleCount > 0 && ![skipRules.single, skipRules.singleRules].includes(currentRule)) {

          const currentScope = globalScope;

          let value;
          let isEmbeddedSQL = false;

          const isDirective = statement[0].type === `directive`;

          if (statement.length >= 1) {
            if (isDirective) {
              const directive = statement[0].value.toUpperCase();
              // We only want to process the EOF if it is not inside an IF scope
              if (directive === `/EOF` && directiveScope === 0) {
                // End of parsing for this file
                break;
              } else
                if (directive === `/IF`) {
                  // Directive IF
                  directiveScope += 1;
                } else
                  if (directive === `/ENDIF`) {
                    // Directive ENDIF
                    directiveScope -= 1;
                  }
            }

            switch (statement[0].type) {
              case `format`:
                if (lineNumber > 0 && statement[0].value.startsWith(`**`)) {
                  if (rules.NoCTDATA) {
                    errors.push({
                      offset: statement[0].range,
                      type: `NoCTDATA`,
                    });
                  }
                }
                break;

              case `directive`:
                value = statement[0].value;

                if (rules.CopybookDirective || rules.IncludeMustBeRelative) {
                  if ([`/COPY`, `/INCLUDE`].includes(value.toUpperCase())) {
                    if (rules.IncludeMustBeRelative) {
                      if (statement.length === 2) {
                        const path = statement[1];

                        if (path.type === `word`) {
                          // /INCLUDE MEMBER
                          // This is bad.
                          const pathValue = path.value.substring(1, path.value.length - 1).trim().toUpperCase();
                          const possibleValue = (data.availableIncludes && data.availableIncludes.length > 0) ? data.availableIncludes.find(cPathValue => cPathValue.toUpperCase().includes(pathValue.toUpperCase())) : undefined;

                          errors.push({
                            offset: { start: path.range.start, end: path.range.end + path.value.length },
                            type: `IncludeMustBeRelative`,
                            newValue: possibleValue ? `'${possibleValue}'` : undefined
                          });
                        } else if (path.type === `string`) {
                          // /INCLUDE 'path/to/file'
                          const pathValue = path.value.substring(1, path.value.length - 1).trim();

                          if (pathValue.startsWith(`/`) === true) {
                            // Bad. Path must not be absolute.
                            errors.push({
                              offset: { start: path.range.start, end: path.range.end },
                              type: `IncludeMustBeRelative`
                            });
                          } else
                            if (data.availableIncludes && data.availableIncludes.length > 0) {
                              const possibleValue = data.availableIncludes.find(cPathValue => cPathValue.toUpperCase().includes(pathValue.toUpperCase()));
                              if (possibleValue) {
                                // This means there was a possible match
                                if (pathValue !== possibleValue) {
                                  // But if they're not the same, offer a fix
                                  errors.push({
                                    offset: { start: path.range.start, end: path.range.end },
                                    type: `IncludeMustBeRelative`,
                                    newValue: `'${possibleValue}'`
                                  });
                                }
                              }
                              // If there's no match, we can't complain incase they're using incdir or something...
                            }
                        }
                      } else if (statement.length === 4 && statement[2].type === `comma`) {
                        // /COPY FILE/MEMBER

                        // We only make this suggestion if it exists locally
                        // Technically, even though we're developing in the IFS/locally,
                        // we can still include members.

                        if (data.availableIncludes && data.availableIncludes.length > 0) {
                          const pathValue = `${statement[1].value}/${statement[3].value}`.toUpperCase();
                          const possibleValue = data.availableIncludes.find(cPathValue => cPathValue.toUpperCase().includes(pathValue));
                          if (possibleValue) {
                            // This means there was a possible match
                            if (pathValue !== possibleValue) {
                              // But if they're not the same, offer a fix
                              errors.push({
                                offset: { start: statement[1].range.start, end: statement[3].range.end },
                                type: `IncludeMustBeRelative`,
                                newValue: `'${possibleValue}'`
                              });
                            }
                          }
                          // If there's no match, we can't complain incase they're using incdir or something...
                        }

                      } else {
                        // /INCLUDE or /COPY is way to long.
                        errors.push({
                          type: `IncludeMustBeRelative`,
                          offset: { start: statement[0].range.start, end: statement[statement.length - 1].range.end }
                        });
                      }
                    }

                    if (rules.CopybookDirective) {
                      let correctDirective = `/${rules.CopybookDirective.toUpperCase()}`;
                      let correctValue = value.toUpperCase();
                      if (rules.DirectiveCase === `lower`) {
                        correctDirective = correctDirective.toLowerCase();
                        correctValue = value.toLowerCase();
                      }
                      if (correctValue !== correctDirective) {
                        errors.push({
                          offset: statement[0].range,
                          type: `CopybookDirective`,
                          newValue: correctDirective
                        });
                      }
                    }
                  }
                }

                if (rules.DirectiveCase === `lower`) {
                  if (value !== value.toLowerCase()) {
                    errors.push({
                      offset: statement[0].range,
                      type: `DirectiveCase`,
                      newValue: value.toLowerCase()
                    });
                  }
                }

                if (rules.DirectiveCase === `upper`) {
                  if (value !== value.toUpperCase()) {
                    errors.push({
                      offset: statement[0].range,
                      type: `DirectiveCase`,
                      newValue: value.toUpperCase()
                    });
                  }
                }
                break;

              case `declare`:
                if (statement.length < 2) break;

                if (rules.SpecificCasing) {
                  const caseRule = rules.SpecificCasing.find(rule => [statement[0].value.toUpperCase(), `*DECLARE`].includes(rule.operation.toUpperCase()));
                  if (caseRule) {
                    let expected = caseRule.expected;
                    switch (expected.toUpperCase()) {
                      case `*UPPER`:
                        expected = statement[0].value.toUpperCase();
                        break;
                      case `*LOWER`:
                        expected = statement[0].value.toLowerCase();
                        break;
                    }
                    if (statement[0].value !== expected) {
                      errors.push({
                        offset: statement[0].range,
                        type: `SpecificCasing`,
                        newValue: expected
                      });
                    }
                  }
                }

                value = statement[1].value;

                if (value.match(/^\d/)) {
                  errors.push({
                    offset: { start: statement[1].range.start, end: statement[1].range.end },
                    type: `InvalidDeclareNumber`,
                  });
                }

                switch (statement[0].value.toUpperCase()) {
                  case `BEGSR`:
                    if (inSubroutine) {
                      errors.push({
                        offset: statement[0].range,
                        type: `UnexpectedEnd`,
                      });
                    }

                    inSubroutine = {name: statement[1].value, skipRules: statement[1].type === `special`};

                    if (inProcedure) {
                      if (rules.NoLocalSubroutines) {
                        errors.push({
                          type: `NoLocalSubroutines`,
                          offset: { start: statement[0].range.start, end: statement[statement.length - 1].range.end }
                        });
                      }
                    } else {
                      if (rules.NoGlobalSubroutines && inSubroutine.skipRules !== true) {
                        errors.push({
                          offset: statement[0].range,
                          type: `NoGlobalSubroutines`
                        });
                      }
                    }
                    break;
                  case `DCL-PROC`:
                    if (inSubroutine || inProcedure) {
                      errors.push({
                        offset: statement[0].range,
                        type: `UnexpectedEnd`,
                      });
                    }

                    value = statement[1].value;
                    inProcedure = {name: value};

                    if (statement.length < 2) break;
                    if (rules.RequiresProcedureDescription) {
                      const procDef = globalProcs.find(def => def.name.toUpperCase() === value.toUpperCase());
                      if (procDef) {
                        if (!procDef.tags.some(tag => tag.tag === `description`)) {
                          errors.push({
                            type: `RequiresProcedureDescription`,
                            offset: { start: statement[0].range.start, end: statement[statement.length - 1].range.end }
                          });
                        }
                      }
                    }
                    break;
                  case `DCL-C`:
                    if (rules.UppercaseConstants) {
                      if (value !== value.toUpperCase()) {
                        errors.push({
                          offset: { start: statement[1].range.start, end: statement[1].range.end },
                          type: `UppercaseConstants`,
                          newValue: value.toUpperCase()
                        });
                      }
                    }
                    break;

                  case `DCL-PI`:
                    if (!statement.some(s => s.type === `end`)) {
                      inPrototype = true;
                    }
                    break;

                  case `DCL-PR`:
                    inPrototype = true;
                    if (rules.PrototypeCheck || rules.NoExtProgramVariable) {

                      const extIndex = statement.findIndex(part => part.value && [`EXTPGM`, `EXTPROC`].includes(part.value.toUpperCase()));
                      if (extIndex >= 0) {
                        if (rules.NoExtProgramVariable) {

                          const keywordValue = statement.find((part, index) => index > extIndex && part.type === `word`);
                          if (keywordValue) {
                            errors.push({
                              offset: { start: keywordValue.range.start, end: keywordValue.range.end },
                              type: `NoExtProgramVariable`
                            });
                          }
                        }

                      } else if (rules.PrototypeCheck) {
                        // Not EXTPROC / EXTPGM found. Likely don't need this PR if it's for local procedure.
                        errors.push({
                          type: `PrototypeCheck`,
                          offset: { start: statement[0].range.start, end: statement[statement.length - 1].range.end }
                        });
                      }
                    }
                    break;

                  case `DCL-ENUM`:
                    inStruct.push(value);
                    break;

                  case `DCL-DS`:
                    if (rules.NoOCCURS) {
                      if (statement.some(part => part.value && part.value.toUpperCase() === `OCCURS`)) {
                        errors.push({
                          type: `NoOCCURS`,
                          offset: { start: statement[0].range.start, end: statement[statement.length - 1].range.end }
                        });
                      }
                    }

                    if (rules.QualifiedCheck) {
                      if (!statement.some(part => part.value && [`LIKEDS`, `LIKEREC`, `QUALIFIED`].includes(part.value.toUpperCase()))) {
                        errors.push({
                          type: `QualifiedCheck`,
                          offset: { start: statement[0].range.start, end: statement[statement.length - 1].range.end }
                        });
                      }
                    }

                    if (rules.BlankStructNamesCheck) {
                      if (statement.some(part => part.type === `special` && part.value.toUpperCase() === `*N`)) {
                        errors.push({
                          type: `BlankStructNamesCheck`,
                          offset: { start: statement[0].range.start, end: statement[statement.length - 1].range.end }
                        });
                      }
                    }

                    if (rules.NoCTDATA) {
                      if (statement.some(part => [`CTDATA`, `*CTDATA`].includes(part.value.toUpperCase()))) {
                        errors.push({
                          type: `NoCTDATA`,
                          offset: { start: statement[0].range.start, end: statement[statement.length - 1].range.end }
                        });
                      }
                    }

                    // If no one line ender is used, push current ds to scope
                    if (!oneLineTriggers["DCL-DS"].some(trigger => statement.map(t => t.value.toUpperCase()).includes(trigger))) {
                      inStruct.push(value);
                    }
                    break;


                  case `DCL-SUBF`:
                  case `DCL-PARM`:
                    if (rules.UselessOperationCheck) {
                      if (statement[1] && statement[1].value) {
                        const name = statement[1].value.toUpperCase();
                        if (!opcodes.includes(name)) {
                          errors.push({
                            offset: { start: statement[0].range.start, end: statement[0].range.end + 1 },
                            type: `UselessOperationCheck`,
                          });
                        }
                      }
                    }
                    break;
                }

                break;

              case `end`:
                value = statement[0].value.toUpperCase();

                if (rules.SpecificCasing) {
                  const caseRule = rules.SpecificCasing.find(rule => [value, `*DECLARE`].includes(rule.operation.toUpperCase()));
                  if (caseRule) {
                    let expected = caseRule.expected;
                    switch (expected.toUpperCase()) {
                      case `*UPPER`:
                        expected = statement[0].value.toUpperCase();
                        break;
                      case `*LOWER`:
                        expected = statement[0].value.toLowerCase();
                        break;
                    }
                    if (statement[0].value !== expected) {
                      errors.push({
                        offset: statement[0].range,
                        type: `SpecificCasing`,
                        newValue: expected
                      });
                    }
                  }
                }

                switch (value) {
                  case `ENDSR`:
                    if (inProcedure === undefined && inSubroutine) {
                      if (rules.NoGlobalSubroutines && inSubroutine.skipRules !== true) {
                        errors.push({
                          offset: statement[0].range,
                          type: `NoGlobalSubroutines`
                        });
                      }
                    }

                    if (!inSubroutine) {
                      errors.push({
                        offset: statement[0].range,
                        type: `UnexpectedEnd`,
                      });
                    } else {
                      inSubroutine = undefined;
                    }
                    break;
                    
                  case `END-DS`:
                  case `END-ENUM`:
                    inStruct.pop();
                    break;

                  case `END-PROC`:
                    if (inProcedure === undefined || inSubroutine) {
                      errors.push({
                        offset: statement[0].range,
                        type: `UnexpectedEnd`,
                      });
                    }

                    inProcedure = undefined;
                    break;
                  case `END-PR`:
                  case `END-PI`:
                    if (inPrototype === false) {
                      errors.push({
                        offset: statement[0].range,
                        type: `UnexpectedEnd`,
                      });
                    }

                    inPrototype = false;
                    break;
                }
                break;

              case `word`:
                value = statement[0].value.toUpperCase();

                if (rules.SpecificCasing) {
                  const caseRule = rules.SpecificCasing.find(rule => value === rule.operation.toUpperCase());
                  if (caseRule) {
                    let expected = caseRule.expected;
                    switch (expected.toUpperCase()) {
                      case `*UPPER`:
                        expected = statement[0].value.toUpperCase();
                        break;
                      case `*LOWER`:
                        expected = statement[0].value.toLowerCase();
                        break;
                    }
                    if (statement[0].value !== expected) {
                      errors.push({
                        offset: statement[0].range,
                        type: `SpecificCasing`,
                        newValue: expected
                      });
                    }
                  }
                }

                switch (value.toUpperCase()) {
                  case `EVAL`:
                  case `CALLP`:
                    if (statement[1] && statement[1].type !== `openbracket`) {
                      if (rules.UselessOperationCheck) {
                        errors.push({
                          offset: { start: statement[0].range.start, end: statement[0].range.end + 1 },
                          type: `UselessOperationCheck`,
                        });
                      }
                    }
                    break;
                  case `LEAVESR`:
                    if (rules.NoGlobalSubroutines && !inProcedure) {
                      errors.push({
                        type: `NoGlobalSubroutines`,
                        offset: { start: statement[0].range.start, end: statement[statement.length - 1].range.end }
                      });
                    }
                    break;
                  case `EXSR`:
                    if (rules.NoGlobalSubroutines) {
                      if (statement.length === 2) {
                        if (globalScope.subroutines.find(sub => sub.name.toUpperCase() === statement[1].value.toUpperCase())) {
                          errors.push({
                            type: `NoGlobalSubroutines`,
                            offset: { start: statement[0].range.start, end: statement[statement.length - 1].range.end }
                          });
                        }
                      }
                    }
                    break;

                  case `EXEC`:
                    const statementOffset = { start: statement[0].range.start, end: statement[statement.length - 1].range.end };
                    isEmbeddedSQL = true;

                    if (rules.NoSELECTAll) {
                      const selectIndex = statement.findIndex(part => part.value && part.value.toUpperCase() === `SELECT`);
                      const allIndex = statement.findIndex(part => part.value && part.value === `*`);
                      if (selectIndex >= 0) {
                        if (selectIndex + 1 === allIndex) {
                          errors.push({
                            offset: statementOffset,
                            type: `NoSELECTAll`,
                          });
                        }
                      }
                    }

                    if (rules.NoSQLJoins) {
                      if (statement.some(part => part.value && part.value.toUpperCase() === `JOIN`)) {
                        errors.push({
                          type: `NoSQLJoins`,
                          offset: statementOffset
                        });
                      }
                    }

                    if (rules.NoExecuteImmediate) {
                      const executeIndex = statement.findIndex(part => part.value && part.value.toUpperCase() === `EXECUTE`);
                      const immediateIndex = statement.findIndex(part => part.value && part.value.toUpperCase() === `IMMEDIATE`);

                      if (executeIndex >= 0) {
                        if (executeIndex + 1 === immediateIndex) {
                          errors.push({
                            type: `NoExecuteImmediate`,
                            offset: statementOffset
                          });
                        }
                      }
                    }

                    if (rules.SQLHostVarCheck) {
                      statement.forEach((part, index) => {
                        if (part.type === `word` && currentScope.findDefinition(lineNumber, part.value)) {
                          const prior = statement[index - 1];
                          if (prior && ![`dot`, `seperator`].includes(prior.type)) {
                            errors.push({
                              offset: part.range,
                              type: `SQLHostVarCheck`,
                              newValue: `:${part.value}`
                            });
                          }
                        }
                      });
                    }

                    if (rules.SQLRunner) {
                      // For running SQL statements
                      const validStatements = [`declare`, `with`, `select`, `merge`, `update`].includes(statement.find((t, i) => i >= 2 && t.type !== `newline`)?.value.toLowerCase());
                      if (validStatements) {
                        errors.push({
                          type: `SQLRunner`,
                          offset: statementOffset,
                          newValue: data.content.substring(statementOffset.start, statementOffset.end)
                        });
                      }
                    }
                    break;

                  case `SELECT`:
                    selectBlocks.push({
                      otherBlockExists: false,
                      offset: { start: statement[0].range.start, end: statement[statement.length - 1].range.end }
                    });
                    break;
                  case `OTHER`:
                    if (selectBlocks.length > 0) {
                      const latestSelect: SelectBlock = selectBlocks[selectBlocks.length - 1];
                      latestSelect.otherBlockExists = true;
                    }
                    // else - bad code?!
                    break;
                  case `ENDSL`:
                    if (selectBlocks.length > 0) {
                      const latestSelect = selectBlocks.pop();
                      if (rules.RequireOtherBlock && latestSelect && !latestSelect.otherBlockExists) {
                        errors.push({
                          type: `RequireOtherBlock`,
                          offset: { start: statement[0].range.start, end: statement[statement.length - 1].range.end }
                        });
                      }
                    }
                    break;

                  case `IF`:
                  case `ELSEIF`:
                  case `WHEN`:
                  case `DOW`:
                  case `DOU`:
                    if (rules.ForceOptionalParens) {
                      const lastStatement = statement[statement.length - 1];
                      if (lastStatement && statement[1] &&(statement[1].type !== `openbracket` || lastStatement.type !== `closebracket`)) {
                        errors.push({
                          type: `ForceOptionalParens`,
                          offset: { start: statement[1].range.start, end: statement[statement.length - 1].range.end }
                        });
                      }
                    }
                    break;
                }
                break;
            }
          }

          let part: Token;

          if (statement.length > 0) {
            // const isSQL = (statement[0].type === `word` && statement[0].value.toUpperCase() === `EXEC`);

            for (let i = 0; i < statement.length; i++) {
              part = statement[i];

              if (part.value) {
                switch (part.type) {
                  case `builtin`:
                    if (rules.SpecificCasing) {
                      const caseRule = rules.SpecificCasing.find(rule => [part.value.toUpperCase(), `*BIF`].includes(rule.operation.toUpperCase()));
                      if (caseRule) {
                        let expected = caseRule.expected;
                        switch (expected.toUpperCase()) {
                          case `*UPPER`:
                            expected = part.value.toUpperCase();
                            break;
                          case `*LOWER`:
                            expected = part.value.toLowerCase();
                            break;
                        }
                        if (part.value !== expected) {
                          errors.push({
                            offset: part.range,
                            type: `SpecificCasing`,
                            newValue: expected
                          });
                        }
                      }
                    }
                    break;

                  case `word`:
                    const upperName = part.value.toUpperCase();

                    if (rules.NoGlobalsInProcedures) {
                      if (inProcedure && !inPrototype) {
                        const existingVariable = globalScope.variables.find(variable => variable.name.toUpperCase() === upperName);
                        if (existingVariable) {
                          errors.push({
                            offset: part.range,
                            type: `NoGlobalsInProcedures`,
                          });
                        }
                      }
                    }

                    const isDeclare = [`declare`, `end`].includes(statement[0].type);

                    if ((isDeclare && i >= 2) || !isDeclare) {
                      if (rules.RequiresParameter && !inPrototype && !isDeclare) {
                        // Check the procedure reference has a block following it
                        const definedProcedure = globalProcs.find(proc => proc.name.toUpperCase() === upperName);
                        if (definedProcedure) {
                          let requiresBlock = false;
                          // Don't require parms for procedures found in Ctl-Opt or directives
                          if (isDeclare || isDirective) {
                            // do nothing
                          } else if (statement[i - 2] && statement[i - 2].type === `builtin` && statement[i - 2].value?.toUpperCase() === `%PADDR`) {
                            // Do nothing because you can pass a function to PADDR as a reference

                          } else if (statement.length <= i + 1) {
                            requiresBlock = true;
                          } else if (statement[i + 1].type !== `openbracket`) {
                            requiresBlock = true;
                          }

                          if (requiresBlock) {
                            errors.push({
                              offset: part.range,
                              type: `RequiresParameter`,
                            });
                          }
                        }
                      }
                    }
                    break;

                  case `string`:
                    if (part.value.substring(1, part.value.length - 1).trim() === `` && rules.RequireBlankSpecial && !isEmbeddedSQL) {
                      errors.push({
                        offset: part.range,
                        type: `RequireBlankSpecial`,
                        newValue: `*BLANK`
                      });

                    } else if (rules.StringLiteralDupe && !isEmbeddedSQL && statement[0].type !== `declare`) {
                      let foundBefore = stringLiterals.find(literal => literal.value === part.value);

                      // If it does not exist on our list, we can add it
                      if (!foundBefore) {
                        foundBefore = {
                          value: part.value,
                          list: []
                        };

                        stringLiterals.push(foundBefore);
                      }

                      // Then add our new found literal location to the list
                      foundBefore.list.push({
                        line: lineNumber,
                        offset: part.range
                      });
                    }
                    break;
                }
              }
            }
          }

        }

        // We don't want to lint CTDATA... so that's the end
        if (statement[0].type === `format` && statement[0].value.toUpperCase() === `**CTDATA`) {
          break;
        }

        // Next, check for indentation errors

        // Check to see if we are ending a multi-line conditional 
        // and now need to increase the expected indent level

        if (!skipIndentCheck) {
          // Check if this line should be skipped.
          skipIndentCheck = [skipRules.singleIndent, skipRules.single].includes(currentRule);
        }

        // We don't report lint issues for a statement that is on the same line as the last statement
        // While this isn't technically possible in RPG, we still check it because it's not an indent error

        if (doc.statements[si - 1] === undefined || (doc.statements[si - 1].range.line !== lineNumber)) {
          if (indentEnabled && skipIndentCheck === false) {
            opcode = statement[0].value.toUpperCase();

            if ([
              `ENDIF`, `ENDFOR`, `ENDDO`, `ELSE`, `ELSEIF`, `ON-ERROR`, `ON-EXCP`, `ENDMON`, `ENDSR`, `WHEN`, `WHEN-IS`, `WHEN-IN`, `OTHER`, `END-PROC`, `END-PI`, `END-PR`, `END-DS`, `END-ENUM`, `ENDSL`
            ].includes(opcode)) {
              expectedIndent -= indent;

              //Special case for `ENDSL` and `END-PROC
              if ([
                `ENDSL`
              ].includes(opcode)) {
                expectedIndent -= indent;
              }

              // Support for on-exit
              if (opcode === `END-PROC` && inOnExit) {
                inOnExit = false;
                expectedIndent -= indent;
              }
            }

            if (currentIndent !== expectedIndent) {
              indentErrors.push({
                line: lineNumber,
                expectedIndent,
                currentIndent
              });
            }

            if ([
              `IF`, `ELSE`, `ELSEIF`, `FOR`, `FOR-EACH`, `DOW`, `DOU`, `MONITOR`, `ON-ERROR`, `ON-EXCP`, `ON-EXIT`, `BEGSR`, `SELECT`, `WHEN`, `WHEN-IS`, `WHEN-IN`, `OTHER`, `DCL-PROC`, `DCL-PI`, `DCL-PR`, `DCL-DS`, `DCL-ENUM`
            ].includes(opcode)) {
              if ([`DCL-DS`, `DCL-PI`, `DCL-PR`].includes(opcode) && oneLineTriggers[opcode].some(trigger => statement.map(t => t.value.toUpperCase()).includes(trigger))) {
                //No change
              }
              else if (opcode === `SELECT`) {
                if (skipIndentCheck === false) expectedIndent += (indent * 2);
              }
              else if (opcode === `ON-EXIT`) {
                expectedIndent += indent;
                inOnExit = true;
              }
              else
                expectedIndent += indent;
            }

          }

          // Reset the rule back.
          currentRule = skipRules.none;
        }
      }

    }

    if (stringLiterals.length > 0) {
      const literalMinimum = rules.literalMinimum || 2;
      stringLiterals.forEach(literal => {
        if (literal.list.length >= literalMinimum) {
          literal.list.forEach(location => {
            const possibleConst = globalScope.findConstByValue(location.line, literal.value);
            errors.push({
              offset: location.offset,
              type: `StringLiteralDupe`,
              newValue: possibleConst ? possibleConst.name : undefined
            });
          });
        }
      });
    }

    if (rules.NoExternalTo && rules.NoExternalTo.length) {
      [
        globalScope,
        ...globalScope.procedures.filter(proc => proc.scope !== undefined).map(proc => proc.scope)
      ].forEach(scope => {
        scope.procedures.forEach(localDef => {
          if (localDef.keyword[`EXTPROC`] || localDef.keyword[`EXTPGM`]) {
            let callLoc = (localDef.keyword[`EXTPROC`] || localDef.keyword[`EXTPGM`]);

            if (callLoc === true) {
              callLoc = localDef.name.toUpperCase();
            } else {
              callLoc = callLoc.toUpperCase();
            }

            // Remove potential string around value
            callLoc = (callLoc.startsWith(`'`) && callLoc.endsWith(`'`) ? callLoc.substring(1, callLoc.length - 1) : callLoc);

            if (rules.NoExternalTo.includes(callLoc)) {
              const possibleStatement = doc.getStatementByLine(localDef.position.range.line);
              if (possibleStatement) {
                errors.push({
                  type: `NoExternalTo`,
                  offset: { start: possibleStatement.range.start, end: possibleStatement.range.end },
                });
              }
            }
          }
        });
      });
    }

    if (rules.NoUnreferenced || rules.IncorrectVariableCase) {
      const noRefCheck = (def: Declaration) => {
        if (def.name !== undefined && def.name.toUpperCase() !== NO_NAME && def.position.path === data.uri) {
          if (def.references.length <= 1) {
            const possibleStatement = doc.getStatementByLine(def.position.range.line);
            if (possibleStatement) {
              errors.push({
                type: `NoUnreferenced`,
                offset: { start: possibleStatement.range.start, end: possibleStatement.range.end }
              });
            }
          }
        }
      }
      const casingCheck = (def: Declaration) => {
        if (def.name !== undefined && def.name.toUpperCase() !== NO_NAME) {
          def.references.forEach(ref => {
            if (ref.uri === data.uri) { // We're only checking the active file
              if (!errors.some(e => e.offset.start === ref.offset.start)) { //This is required because LIKEDS shares subfields
                const contentPosition = data.content.substring(ref.offset.start, ref.offset.end);
                if (contentPosition !== def.name) {
                  errors.push({
                    type: `IncorrectVariableCase`,
                    offset: { start: ref.offset.start, end: ref.offset.end },
                    newValue: def.name
                  });
                }
              }
            }
          });
        }
      }

      [
        globalScope,
        ...globalScope.procedures.filter(proc => proc.scope !== undefined).map(proc => proc.scope)
      ].forEach(dec => {
        [...dec.constants, ...dec.variables]
          .forEach(def => {
            if (rules.NoUnreferenced) {
              noRefCheck(def);
            }

            if (rules.IncorrectVariableCase) {
              casingCheck(def);
            }
          });

        dec.subroutines
          .filter(def => def.name && ![`*INZSR`, `*PSSR`].includes(def.name.toUpperCase()))
          .forEach(def => {
            if (rules.NoUnreferenced) {
              noRefCheck(def);
            }

            if (rules.IncorrectVariableCase) {
              casingCheck(def);
            }
          });

        dec.procedures
          .forEach(proc => {
            if (!proc.keyword[`EXPORT`]) {
              if (rules.NoUnreferenced) {
                noRefCheck(proc);
              }

              if (rules.IncorrectVariableCase) {
                casingCheck(proc);
              }

              if (!proc.keyword[`EXTPGM`] && !proc.keyword[`EXTPROC`]) {
                proc.subItems.forEach(parm => {
                  if (rules.NoUnreferenced) {
                    noRefCheck(parm);
                  }

                  if (rules.IncorrectVariableCase) {
                    casingCheck(parm);
                  }
                });
              }
            }
          });

        dec.structs
          .forEach(struct => {

            struct.subItems.forEach(subf => {
              // We only check the subfields if the parent is never references.
              if (rules.NoUnreferenced && struct.references.length <= 1) {
                noRefCheck(subf);
              }

              if (rules.IncorrectVariableCase) {
                casingCheck(subf);
              }
            });

            const subFieldIsUsed = struct.subItems.some(subf => subf.references.length > 1);
            if (rules.NoUnreferenced && subFieldIsUsed === false) {
              noRefCheck(struct);
            }

            if (rules.IncorrectVariableCase) {
              casingCheck(struct);
            }
          });
      });
    }

    return {
      indentErrors,
      errors,
      doc
    };
  }
}