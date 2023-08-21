/* eslint-disable no-case-declarations */

import Cache from "./models/cache";
import { tokenise } from "./tokens";
import oneLineTriggers from "./models/oneLineTriggers";
import { Range, Position } from "./models/DataPoints";
import opcodes from "./models/opcodes";
import Document from "./document";
import { IssueRange, Offset, Rules, SelectBlock } from "./parserTypes";

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
  'UppercaseDirectives': `Directives must be in uppercase.`,
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
  'RequireOtherBlock': `OTHER block missing from SELECT block.`
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

    // Excluding indent
    const ruleCount = Object.keys(rules).length - (rules.indent ? 1 : 0);

    if (!globalScope)
      globalScope = new Cache();

    const globalProcs = globalScope.procedures;

    let inProcedure = false;
    let inSubroutine = false;
    let inStruct = 0;
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

    if (rules.NoUnreferenced) {
      // We need to collect references for this to work correctly.
      rules.CollectReferences = true;
    }

    // Clear out all the old references.
    if (rules.CollectReferences) {
      globalScope.clearReferences();
    }

    // Make all external refs uppercase.
    if (rules.NoExternalTo && rules.NoExternalTo.length) {
      rules.NoExternalTo = rules.NoExternalTo.map(val => val.toUpperCase());
    }

    const selectBlocks: SelectBlock[] = [];

    const stringLiterals: { value: string, definition?: string, list: { offset: Offset }[] }[] = [];

    let directiveScope = 0;
    let currentRule = skipRules.none;

    const doc = new Document(data.content);

    for (let si = 0; si < doc.statements.length; si++) {
      const docStatement = doc.statements[si];
      const statement = docStatement.tokens;
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
                  offset: { position: statement[0].range.start, end: statement[0].range.start + 2 },
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

          const currentProcedure = globalScope.procedures.find(proc => lineNumber >= proc.range.start && lineNumber <= proc.range.end);
          const currentScope = globalScope.merge(inProcedure && currentProcedure ? currentProcedure.scope : undefined);

          // Only fetch the names if we have a rule that requires it. It might be slow.
          const definedNames = (rules.IncorrectVariableCase || rules.SQLHostVarCheck ? currentScope.getNames() : []);

          let value;
          let isEmbeddedSQL = false;

          if (statement.length >= 1) {

            if (statement[0].type === `directive`) {
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
                      offset: { position: statement[0].range.start, end: statement[0].range.end },
                      type: `NoCTDATA`,
                    });
                  }
                }
                break;

              case `directive`:
                value = statement[0].value;
                if (rules.UppercaseDirectives) {
                  if (value !== value.toUpperCase()) {
                    errors.push({
                      offset: { position: statement[0].range.start, end: statement[0].range.end },
                      type: `UppercaseDirectives`,
                      newValue: value.toUpperCase()
                    });
                  }
                }

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
                            offset: { position: path.range.start, end: path.range.end + path.value.length },
                            type: `IncludeMustBeRelative`,
                            newValue: possibleValue ? `'${possibleValue}'` : undefined
                          });
                        } else if (path.type === `string`) {
                          // /INCLUDE 'path/to/file'
                          const pathValue = path.value.substring(1, path.value.length - 1).trim();

                          if (pathValue.startsWith(`/`) === true) {
                            // Bad. Path must not be absolute.
                            errors.push({
                              offset: { position: path.range.start, end: path.range.end },
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
                                    offset: { position: path.range.start, end: path.range.end },
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
                                offset: { position: statement[1].range.start, end: statement[3].range.end },
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
                          offset: { position: statement[0].range.start, end: statement[statement.length - 1].range.end }
                        });
                      }
                    }

                    if (rules.CopybookDirective) {
                      const correctDirective = `/${rules.CopybookDirective.toUpperCase()}`;
                      if (value.toUpperCase() !== correctDirective) {
                        errors.push({
                          offset: { position: statement[0].range.start, end: statement[0].range.end },
                          type: `CopybookDirective`,
                          newValue: correctDirective
                        });
                      }
                    }
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
                        offset: { position: statement[0].range.start, end: statement[0].range.end },
                        type: `SpecificCasing`,
                        newValue: expected
                      });
                    }
                  }
                }

                value = statement[1].value;

                if (value.match(/^\d/)) {
                  errors.push({
                    offset: { position: statement[1].range.start, end: statement[1].range.end },
                    type: `InvalidDeclareNumber`,
                  });
                }

                switch (statement[0].value.toUpperCase()) {
                  case `BEGSR`:
                    if (inSubroutine) {
                      errors.push({
                        offset: { position: statement[0].range.start, end: statement[0].range.end },
                        type: `UnexpectedEnd`,
                      });
                    }

                    inSubroutine = true;

                    if (inProcedure) {
                      if (rules.NoLocalSubroutines) {
                        errors.push({
                          type: `NoLocalSubroutines`,
                          offset: { position: statement[0].range.start, end: statement[statement.length - 1].range.end }
                        });
                      }
                    } else {
                      if (rules.NoGlobalSubroutines) {
                        errors.push({
                          offset: { position: statement[0].range.start, end: statement[0].range.end },
                          type: `NoGlobalSubroutines`,
                          newValue: `Dcl-Proc`
                        });
                      }
                    }
                    break;
                  case `DCL-PROC`:
                    if (inSubroutine || inProcedure) {
                      errors.push({
                        offset: { position: statement[0].range.start, end: statement[0].range.end },
                        type: `UnexpectedEnd`,
                      });
                    }

                    inProcedure = true;
                    if (statement.length < 2) break;
                    if (rules.RequiresProcedureDescription) {
                      value = statement[1].value;
                      const procDef = globalProcs.find(def => def.name.toUpperCase() === value.toUpperCase());
                      if (procDef) {
                        if (!procDef.description) {
                          errors.push({
                            type: `RequiresProcedureDescription`,
                            offset: { position: statement[0].range.start, end: statement[statement.length - 1].range.end }
                          });
                        }
                      }
                    }
                    break;
                  case `DCL-C`:
                    if (rules.UppercaseConstants) {
                      if (value !== value.toUpperCase()) {
                        errors.push({
                          offset: { position: statement[1].range.start, end: statement[1].range.end },
                          type: `UppercaseConstants`,
                          newValue: value.toUpperCase()
                        });
                      }
                    }

                    if (rules.StringLiteralDupe) {
                      if (statement[2].type === `string`) {
                        let foundBefore = stringLiterals.find(literal => literal.value === statement[2].value);

                        // If it does not exist on our list, we can add it
                        if (!foundBefore) {
                          foundBefore = {
                            definition: value,
                            value: statement[2].value,
                            list: []
                          };

                          stringLiterals.push(foundBefore);
                        }
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
                              offset: { position: keywordValue.range.start, end: keywordValue.range.end },
                              type: `NoExtProgramVariable`
                            });
                          }
                        }

                      } else if (rules.PrototypeCheck) {
                        // Not EXTPROC / EXTPGM found. Likely don't need this PR if it's for local procedure.
                        errors.push({
                          type: `PrototypeCheck`,
                          offset: { position: statement[0].range.start, end: statement[statement.length - 1].range.end }
                        });
                      }
                    }
                    break;

                  case `DCL-DS`:
                    inStruct += 1;
                    if (rules.NoOCCURS) {
                      if (statement.some(part => part.value && part.value.toUpperCase() === `OCCURS`)) {
                        errors.push({
                          type: `NoOCCURS`,
                          offset: { position: statement[0].range.start, end: statement[statement.length - 1].range.end }
                        });
                      }
                    }

                    if (rules.QualifiedCheck) {
                      if (!statement.some(part => part.value && [`LIKEDS`, `LIKEREC`, `QUALIFIED`].includes(part.value.toUpperCase()))) {
                        errors.push({
                          type: `QualifiedCheck`,
                          offset: { position: statement[0].range.start, end: statement[statement.length - 1].range.end }
                        });
                      }
                    }

                    if (rules.BlankStructNamesCheck) {
                      if (statement.some(part => part.type === `special` && part.value.toUpperCase() === `*N`)) {
                        errors.push({
                          type: `BlankStructNamesCheck`,
                          offset: { position: statement[0].range.start, end: statement[statement.length - 1].range.end }
                        });
                      }
                    }

                    if (rules.NoCTDATA) {
                      if (statement.some(part => [`CTDATA`, `*CTDATA`].includes(part.value.toUpperCase()))) {
                        errors.push({
                          type: `NoCTDATA`,
                          offset: { position: statement[0].range.start, end: statement[statement.length - 1].range.end }
                        });
                      }
                    }
                    break;


                  case `DCL-SUBF`:
                  case `DCL-PARM`:
                    if (rules.UselessOperationCheck) {
                      if (statement[1] && statement[1].value) {
                        const name = statement[1].value.toUpperCase();
                        if (!opcodes.includes(name)) {
                          errors.push({
                            offset: { position: statement[0].range.start, end: statement[0].range.end + 1 },
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
                        offset: { position: statement[0].range.start, end: statement[0].range.end },
                        type: `SpecificCasing`,
                        newValue: expected
                      });
                    }
                  }
                }

                switch (value) {
                  case `ENDSR`:
                    if (!inSubroutine) {
                      errors.push({
                        offset: { position: statement[0].range.start, end: statement[0].range.end },
                        type: `UnexpectedEnd`,
                      });
                    } else {
                      inSubroutine = false;
                    }

                    if (inProcedure === false) {
                      if (rules.NoGlobalSubroutines) {
                        errors.push({
                          offset: { position: statement[0].range.start, end: statement[0].range.end },
                          type: `NoGlobalSubroutines`,
                          newValue: `End-Proc`
                        });
                      }
                    }
                    break;
                  case `END-DS`:
                    inStruct -= 1;
                    break;
                  case `END-PROC`:
                    if (inProcedure === false || inSubroutine) {
                      errors.push({
                        offset: { position: statement[0].range.start, end: statement[0].range.end },
                        type: `UnexpectedEnd`,
                      });
                    }

                    inProcedure = false;
                    break;
                  case `END-PR`:
                  case `END-PI`:
                    if (inPrototype === false) {
                      errors.push({
                        offset: { position: statement[0].range.start, end: statement[0].range.end },
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
                        offset: { position: statement[0].range.start, end: statement[0].range.end },
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
                          offset: { position: statement[0].range.start, end: statement[0].range.end + 1 },
                          type: `UselessOperationCheck`,
                        });
                      }
                    }
                    break;
                  case `LEAVESR`:
                    if (rules.NoGlobalSubroutines && !inProcedure) {
                      errors.push({
                        type: `NoGlobalSubroutines`,
                        offset: { position: statement[0].range.start, end: statement[statement.length - 1].range.end },
                        newValue: `return`
                      });
                    }
                    break;
                  case `EXSR`:
                    if (rules.NoGlobalSubroutines) {
                      if (statement.length === 2) {
                        if (globalScope.subroutines.find(sub => sub.name.toUpperCase() === statement[1].value.toUpperCase())) {
                          errors.push({
                            type: `NoGlobalSubroutines`,
                            offset: { position: statement[0].range.start, end: statement[statement.length - 1].range.end },
                            newValue: `${statement[1].value}()`
                          });
                        }
                      }
                    }
                    break;
                  case `EXEC`:
                    isEmbeddedSQL = true;
                    if (rules.NoSELECTAll) {
                      const selectIndex = statement.findIndex(part => part.value && part.value.toUpperCase() === `SELECT`);
                      const allIndex = statement.findIndex(part => part.value && part.value === `*`);
                      if (selectIndex >= 0) {
                        if (selectIndex + 1 === allIndex) {
                          errors.push({
                            offset: { position: statement[0].range.start, end: statement[statement.length - 1].range.end },
                            type: `NoSELECTAll`,
                          });
                        }
                      }
                    }

                    if (rules.NoSQLJoins) {
                      if (statement.some(part => part.value && part.value.toUpperCase() === `JOIN`)) {
                        errors.push({
                          type: `NoSQLJoins`,
                          offset: { position: statement[0].range.start, end: statement[statement.length - 1].range.end }
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
                            offset: { position: statement[0].range.start, end: statement[statement.length - 1].range.end }
                          });
                        }
                      }
                    }

                    if (rules.SQLHostVarCheck) {
                      statement.forEach((part, index) => {
                        if (part.type === `word` && definedNames.some(name => name.toUpperCase() === part.value.toUpperCase())) {
                          const prior = statement[index - 1];
                          if (prior && ![`dot`, `seperator`].includes(prior.type)) {
                            errors.push({
                              offset: { position: part.range.start, end: part.range.end },
                              type: `SQLHostVarCheck`,
                              newValue: `:${part.value}`
                            });
                          }
                        }
                      });
                    }
                    break;

                  case `SELECT`:
                    selectBlocks.push({
                      otherBlockExists: false,
                      offset: { position: statement[0].range.start, end: statement[statement.length - 1].range.end }
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
                          offset: { position: statement[0].range.start, end: statement[statement.length - 1].range.end }
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
                      if (statement[1].type !== `openbracket` || lastStatement.type !== `closebracket`) {
                        errors.push({
                          type: `ForceOptionalParens`,
                          offset: { position: statement[0].range.start, end: statement[statement.length - 1].range.end }
                        });
                      }
                    }
                    break;
                }
                break;
            }
          }

          let part;

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
                            offset: { position: part.range.start, end: part.range.end },
                            type: `SpecificCasing`,
                            newValue: expected
                          });
                        }
                      }
                    }
                    break;


                  case `special`:
                    if (rules.CollectReferences) {
                      value = part.value.substring(1).toUpperCase();
                      const defRef = globalScope.find(value);

                      if (defRef) {
                        defRef.references.push({
                          offset: { position: part.range.start, end: part.range.end },
                          type: null,
                        });
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
                            offset: { position: part.range.start, end: part.range.end },
                            type: `NoGlobalsInProcedures`,
                          });
                        }
                      }
                    }

                    const isDeclare = [`declare`, `end`].includes(statement[0].type);

                    if (rules.IncorrectVariableCase) {
                      // Check the casing of the reference matches the definition
                      if ((isEmbeddedSQL === false || (isEmbeddedSQL && statement[i - 1] && statement[i - 1].type === `seperator`))) {
                        const possibleKeyword = (isDeclare || inPrototype || inStruct > 0) && i >= 0 && statement[i + 1] && statement[i + 1].type === `openbracket`;

                        if (!possibleKeyword) {
                          const definedName = definedNames.find(defName => defName.toUpperCase() === upperName);
                          if (definedName && definedName !== part.value) {
                            errors.push({
                              offset: { position: part.range.start, end: part.range.end },
                              type: `IncorrectVariableCase`,
                              newValue: definedName
                            });
                          }
                        }
                      }
                    }

                    if ((isDeclare && i >= 2) || !isDeclare) {
                      if (rules.RequiresParameter && !inPrototype) {

                        // Check the procedure reference has a block following it
                        const definedProcedure = globalProcs.find(proc => proc.name.toUpperCase() === upperName);
                        if (definedProcedure) {
                          let requiresBlock = false;
                          // Don't require parms for procedures found in Ctl-Opt
                          if (statement[0].value.toUpperCase() === `CTL-OPT` || statement[0].type === `directive`) {
                            // do nothing
                          } else if (statement.length <= i + 1) {
                            requiresBlock = true;
                          } else if (statement[i + 1].type !== `openbracket`) {
                            requiresBlock = true;
                          }

                          if (requiresBlock) {
                            errors.push({
                              offset: { position: part.range.start, end: part.range.end },
                              type: `RequiresParameter`,
                            });
                          }
                        }
                      }

                      if (rules.CollectReferences) {
                        if (statement[i - 1] && statement[i - 1].type === `dot`) break;

                        let defRef;
                        if (currentProcedure && currentProcedure.scope) {
                          defRef = currentProcedure.scope.find(upperName);

                          if (!defRef) {
                            defRef = currentProcedure.subItems.find(def => def.name.toUpperCase() === upperName);
                          }
                        }

                        if (!defRef) {
                          defRef = globalScope.find(upperName);
                        }

                        if (defRef) {
                          // `defRef.position` is usually undefined for predefined indicators (INXX)
                          if (defRef.position === undefined || (defRef.position && defRef.position.line !== lineNumber)) {
                            defRef.references.push({
                              offset: { position: part.range.start, end: part.range.end },
                            });
                          }

                          if (defRef.keyword[`QUALIFIED`]) {
                            let nextPartIndex = i + 1;

                            if (statement[nextPartIndex]) {
                              // First, check if there is an array call here and skip over it
                              if (statement[nextPartIndex].type === `openbracket`) {
                                nextPartIndex = statement.findIndex((value, index) => index > nextPartIndex && value.type === `closebracket`);

                                if (nextPartIndex >= 0) nextPartIndex++;
                              }

                              // Check if the next part is a dot
                              if (statement[nextPartIndex] && statement[nextPartIndex].type === `dot`) {
                                nextPartIndex++;

                                // Check if the next part is a word
                                if (statement[nextPartIndex] && statement[nextPartIndex].type === `word` && statement[nextPartIndex].value) {
                                  const subItemPart = statement[nextPartIndex];
                                  const subItemName = subItemPart.value.toUpperCase();

                                  // Find the subitem
                                  const subItemDef = defRef.subItems.find(subfield => subfield.name.toUpperCase() == subItemName);
                                  if (subItemDef) {
                                    subItemDef.references.push({
                                      offset: { position: subItemPart.range.start, end: subItemPart.range.end },
                                    });
                                  }
                                }
                              }
                            }
                          }
                        }
                      }
                    }
                    break;

                  case `string`:
                    if (part.value.substring(1, part.value.length - 1).trim() === `` && rules.RequireBlankSpecial && !isEmbeddedSQL) {
                      errors.push({
                        offset: { position: part.range.start, end: part.range.end },
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
                        offset: { position: part.range.start, end: part.range.end }
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

        if (doc.statements[si-1] === undefined || (doc.statements[si-1].range.line !== lineNumber)) {
          if (indentEnabled && skipIndentCheck === false) {
            opcode = statement[0].value.toUpperCase();

            if ([
              `ENDIF`, `ENDFOR`, `ENDDO`, `ELSE`, `ELSEIF`, `ON-ERROR`, `ENDMON`, `ENDSR`, `WHEN`, `OTHER`, `END-PROC`, `END-PI`, `END-PR`, `END-DS`, `ENDSL`
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
              `IF`, `ELSE`, `ELSEIF`, `FOR`, `FOR-EACH`, `DOW`, `DOU`, `MONITOR`, `ON-ERROR`, `ON-EXIT`, `BEGSR`, `SELECT`, `WHEN`, `OTHER`, `DCL-PROC`, `DCL-PI`, `DCL-PR`, `DCL-DS`
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
            errors.push({
              ...location,
              type: `StringLiteralDupe`,
              newValue: literal.definition
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
              const possibleStatement = doc.getStatementByLine(localDef.position.line);
              if (possibleStatement) {
                errors.push({
                  type: `NoExternalTo`,
                  offset: { position: possibleStatement.range.start, end: possibleStatement.range.end },
                });
              }
            }
          }
        });
      });
    }

    if (rules.NoUnreferenced) {
      [
        globalScope,
        ...globalScope.procedures.filter(proc => proc.scope !== undefined).map(proc => proc.scope)
      ].forEach(dec => {
        [...dec.constants, ...dec.variables]
          .filter(def => def.position.path === data.uri)
          .forEach(def => {
            if (def.references.length === 0) {
              // Add an error to def
              const possibleStatement = doc.getStatementByLine(def.position.line);
              if (possibleStatement) {
                errors.push({
                  type: `NoUnreferenced`,
                  offset: { position: possibleStatement.range.start, end: possibleStatement.range.end }
                });
              }
            }
          });

        dec.subroutines
          .filter(def => def.position.path === data.uri && def.name && ![`*INZSR`, `*PSSR`].includes(def.name.toUpperCase()))
          .forEach(def => {
            if (def.references.length === 0) {
              // Add an error to def
              const possibleStatement = doc.getStatementByLine(def.position.line);
              if (possibleStatement) {
                errors.push({
                  type: `NoUnreferenced`,
                  offset: { position: possibleStatement.range.start, end: possibleStatement.range.end }
                });
              }
            }
          });

        dec.procedures
          .filter(struct => struct.position.path === data.uri)
          .forEach(proc => {
            if (!proc.keyword[`EXPORT`]) {
              if (proc.references.length === 0) {
                // Add an error to proc
                const possibleStatement = doc.getStatementByLine(proc.position.line);
                if (possibleStatement) {
                  errors.push({
                    type: `NoUnreferenced`,
                    offset: { position: possibleStatement.range.start, end: possibleStatement.range.end }
                  });
                }
              }

              if (!proc.keyword[`EXTPGM`] && !proc.keyword[`EXTPROC`]) {
                proc.subItems.forEach(parm => {
                  if (parm.references.length === 0) {
                    const possibleStatement = doc.getStatementByLine(parm.position.line);
                    if (possibleStatement) {
                      errors.push({
                        type: `NoUnreferenced`,
                        offset: { position: possibleStatement.range.start, end: possibleStatement.range.end }
                      });
                    }
                  }
                });
              }
            }
          });

        dec.structs
          .filter(struct => struct.position.path === data.uri)
          .forEach(struct => {
            const subFieldIsUsed = struct.subItems.some(subf => subf.references.length > 0);

            if (struct.references.length === 0) {
              // We only check the subfields if the parent is never references.

              struct.subItems.forEach(subf => {
                if (subf.references.length === 0) {
                  // Add an error to subf
                  const possibleStatement = doc.getStatementByLine(subf.position.line);
                  if (possibleStatement) {
                    errors.push({
                      type: `NoUnreferenced`,
                      offset: { position: possibleStatement.range.start, end: possibleStatement.range.end }
                    });
                  }
                }
              });

              if (subFieldIsUsed === false) {
                const possibleStatement = doc.getStatementByLine(struct.position.line);
                if (possibleStatement) {
                  errors.push({
                    type: `NoUnreferenced`,
                    offset: { position: possibleStatement.range.start, end: possibleStatement.range.end }
                  });
                }
              }
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