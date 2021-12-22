
const vscode = require(`vscode`);

const Cache = require(`./models/cache`);
const Declaration = require(`./models/declaration`);
const Statement = require(`./statement`);
const oneLineTriggers = require(`./models/oneLineTriggers`);

const errorText = {
  'BlankStructNamesCheck': `Struct names cannot be blank (\`*N\`).`,
  'QualifiedCheck': `Struct names must be qualified (\`QUALIFIED\`).`,
  'PrototypeCheck': `Prototypes can only be defined with either \`EXT\`, \`EXTPGM\` or \`EXTPROC\``,
  'ForceOptionalParens': `Expressions must be surrounded by brackets.`,
  'NoOCCURS': `\`OCCURS\` is not allowed.`,
  'NoSELECTAll': `\`SELECT *\` is not allowed in Embedded SQL.`,
  'UselessOperationCheck': `Redundant operation codes (EVAL, CALLP) not allowed.`,
  'UppercaseConstants': `Constants must be in uppercase.`,
  'SpecificCasing': `Does not match required case.`,
  'InvalidDeclareNumber': `Variable names cannot start with a number`,
  'IncorrectVariableCase': `Variable name casing does not match definition.`,
  'RequiresParameter': `Procedure calls require brackets.`,
  'RequiresProcedureDescription': `Proceudres require a title and description.`,
  'StringLiteralDupe': `Same string literal used more than once. Consider using a constant instead.`,
  'RequireBlankSpecial': `\`*BLANK\` should be used over empty string literals.`,
  'CopybookDirective': `Directive does not match requirement.`,
  'UppercaseDirectives': `Directives must be in uppercase.`,
  'NoSQLJoins': `SQL joins are not allowed. Consider creating a view instead.`,
  'NoGlobalsInProcedures': `Global variables should not be referenced in procedures.`,
}

module.exports = class Linter {
  static getErrorText(error) {
    return errorText[error];
  }

  /**
   * @param {string} content 
   * @param {{
   *  indent?: number,
   *  BlankStructNamesCheck?: boolean,
   *  QualifiedCheck?: boolean,
   *  PrototypeCheck?: boolean,
   *  ForceOptionalParens?: boolean,
   *  NoOCCURS?: boolean,
   *  NoSELECTAll?: boolean,
   *  UselessOperationCheck?: boolean,
   *  UppercaseConstants?: boolean,
   *  IncorrectVariableCase?: boolean,
   *  RequiresParameter?: boolean,
   *  RequiresProcedureDescription?: boolean,
   *  StringLiteralDupe?: boolean,
   *  RequireBlankSpecial?: boolean,
   *  CopybookDirective?: "copy"|"include"
   *  UppercaseDirectives?: boolean,
   *  NoSQLJoins?: boolean,
   *  NoGlobalsInProcedures?: boolean,
   *  SpecificCasing?: {operation: string, expected: string}[],
   * }} rules 
   * @param {Cache|null} [globalScope]
   */
  static getErrors(content, rules, globalScope) {
    const newLineLength = (content.indexOf(`\r\n`) !== -1) ? 2 : 1;

    /** @type {string[]} */
    const lines = content.replace(new RegExp(`\\\r`, `g`), ``).split(`\n`);

    const indent = rules.indent || 2;

    // Excluding indent
    const ruleCount = Object.keys(rules).length - (rules.indent ? 1 : 0);

    /** @type {string[]} */
    let scopeNames = []

    /** @type {Declaration[]} */
    let scopeVariables = [];

    let globalProcs = [];

    if (globalScope) {
      globalProcs = globalScope.procedures;
    }

    let inProcedure = false;
    let inPrototype = false;

    let lineNumber = -1;

    /** @type {{line: number, expectedIndent: number, currentIndent: number}[]} */
    let indentErrors = [];

    // Offset is always the offset within the range

    /** @type {{
     *  range: vscode.Range, 
     *  offset?: {position: number, length: number}
     *  type: 
     *      "BlankStructNamesCheck"|"QualifiedCheck"|"PrototypeCheck"|"ForceOptionalParens"|
     *      "NoOCCURS"|"NoSELECTAll"|"UselessOperationCheck"|"UppercaseConstants"|"SpecificCasing"|
     *      "InvalidDeclareNumber"|"IncorrectVariableCase"|"RequiresParameter"|
     *      "RequiresProcedureDescription"|"StringLiteralDupe"|"RequireBlankSpecial"|
     *      "CopybookDirective"|"UppercaseDirectives"|"NoSQLJoins"|"NoGlobalsInProcedures", 
     *  newValue?: string}[]
     * } */
    let errors = [];

    /** @type {Number} */
    let expectedIndent = 0;
    let currentIndent = 0;

    /** @type {string[]} */
    let pieces;

    let continuedStatement = false, skipIndentCheck = false;

    let currentStatement = ``, opcode;

    /** @type {vscode.Position} */
    let statementStart;
    /** @type {vscode.Position} */
    let statementEnd;

    /** @type {{value: string, list: {range: vscode.Range, offset: {position: number, length: number}}[]}[]} */
    let stringLiterals = [];

    for (let currentLine of lines) {
      currentIndent = currentLine.search(/\S/);
      let line = currentLine.trimEnd();

      lineNumber += 1;

      if (line.trimStart().startsWith(`//`)) continue;

      if (currentIndent >= 0) {
        skipIndentCheck = false;

        if (continuedStatement) {
          skipIndentCheck = true;
          statementEnd = new vscode.Position(lineNumber, line.length);

          if (currentIndent < expectedIndent) {
            indentErrors.push({
              line: lineNumber,
              expectedIndent,
              currentIndent
            });
          }
        } else {
          statementStart = new vscode.Position(lineNumber, currentIndent);
          statementEnd = new vscode.Position(lineNumber, line.length);
        }

        if (line.endsWith(`;`)) {
          statementEnd = new vscode.Position(lineNumber, line.length - 1);
          line = line.substr(0, line.length-1);
          currentStatement += line;
          continuedStatement = false;

        } else {

          const semiIndex = line.lastIndexOf(`;`);
          const commentIndex = line.lastIndexOf(`//`);

          if (commentIndex > semiIndex && semiIndex >= 0) {
            statementEnd = new vscode.Position(lineNumber, line.lastIndexOf(`;`));
            line = line.substr(0, semiIndex);
          } else {
            continuedStatement = true;
          }
          currentStatement += line + ``.padEnd(newLineLength, ` `);
        }

        const upperLine = line.trim().toUpperCase();

        // Generally ignore comments
        if (upperLine.startsWith(`//`)) {
          currentStatement = ``;
        } else if (upperLine.startsWith(`/`)) {
          // But not directives!
          continuedStatement = false;
        }

        // Ignore free directive.
        if (upperLine === `**FREE`) {
          continuedStatement = false;
        }

        // Linter checking
        if (continuedStatement === false && currentStatement.length > 0 && ruleCount > 0) {
          const currentStatementUpper = currentStatement.toUpperCase();
          currentStatement = currentStatement.trim();

          const currentProcedure = globalScope.procedures.find(proc => lineNumber >= proc.range.start && lineNumber <= proc.range.end);
          const currentScope = globalScope.merge(inProcedure ? currentProcedure.scope : undefined);

          const statement = Statement.parseStatement(currentStatement);
          let value;

          if (statement.length >= 1) {
            switch (statement[0].type) {
            case `directive`:
              value = statement[0].value;
              if (rules.UppercaseDirectives) {
                if (value !== value.toUpperCase()) {
                  errors.push({
                    range: new vscode.Range(
                      statementStart,
                      statementEnd
                    ),
                    offset: {position: statement[0].position, length: statement[0].position + value.length},
                    type: `UppercaseDirectives`,
                    newValue: value.toUpperCase()
                  });
                }
              }

              if (rules.CopybookDirective) {
                if ([`/COPY`, `/INCLUDE`].includes(value.toUpperCase())) {
                  const correctDirective = `/${rules.CopybookDirective.toUpperCase()}`;
                  if (value.toUpperCase() !== correctDirective) {
                    errors.push({
                      range: new vscode.Range(
                        statementStart,
                        statementEnd
                      ),
                      offset: {position: statement[0].position, length: statement[0].position + value.length},
                      type: `CopybookDirective`,
                      newValue: correctDirective
                    });
                  }
                }
              }
              break;

            case `declare`:
              if (statement.length < 2) break;
              value = statement[1].value;

              if (value.match(/^\d/)) {
                errors.push({
                  range: new vscode.Range(
                    statementStart,
                    statementEnd
                  ),
                  offset: {position: statement[1].position, length: statement[1].position + value.length},
                  type: `InvalidDeclareNumber`
                });
              }

              switch (statement[0].value.toUpperCase()) {
              case `DCL-PROC`:
                inProcedure = true;
                if (statement.length < 2) break;
                if (rules.RequiresProcedureDescription) {
                  value = statement[1].value;
                  const procDef = globalProcs.find(def => def.name.toUpperCase() === value.toUpperCase());
                  if (procDef) {
                    if (!procDef.description) {
                      errors.push({
                        range: new vscode.Range(
                          statementStart,
                          statementEnd
                        ),
                        type: `RequiresProcedureDescription`
                      });
                    }
                  }
                }
                break;
              case `DCL-C`:
                if (rules.UppercaseConstants) {
                  if (value !== value.toUpperCase()) {
                    errors.push({
                      range: new vscode.Range(
                        statementStart,
                        statementEnd
                      ),
                      offset: {position: statement[1].position, length: statement[1].position + value.length},
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
                if (rules.PrototypeCheck) {
                  // Unneeded PR
                  if (!statement.some(part => part.value && part.value.toUpperCase().startsWith(`EXT`))) {
                    errors.push({
                      range: new vscode.Range(statementStart, statementEnd),
                      type: `PrototypeCheck`
                    });
                  }
                }
                break;

              case `DCL-DS`:
                if (rules.NoOCCURS) {
                  if (statement.some(part => part.value && part.value.toUpperCase() === `OCCURS`)) {
                    errors.push({
                      range: new vscode.Range(statementStart, statementEnd),
                      type: `NoOCCURS`
                    });
                  }
                }
    
                if (rules.QualifiedCheck) {
                  if (!statement.some(part => part.value && [`LIKEDS`, `QUALIFIED`].includes(part.value.toUpperCase()))) {
                    errors.push({
                      range: new vscode.Range(statementStart, statementEnd),
                      type: `QualifiedCheck`
                    });
                  }
                }
    
                if (rules.BlankStructNamesCheck) {
                  if (statement.some(part => part.type === `special`)) {
                    errors.push({
                      range: new vscode.Range(statementStart, statementEnd),
                      type: `BlankStructNamesCheck`
                    });
                  }
                }
                break;
              }

              break;

            case `end`:
              switch (statement[0].value.toUpperCase()) {
              case `END-PROC`:
                inProcedure = false;
                break;
              case `END-PI`:
                inPrototype = false;
                break;
              }
              break;

            case `word`:
              value = statement[0].value.toUpperCase();

              if (rules.SpecificCasing) {
                const caseRule = rules.SpecificCasing.find(rule => rule.operation.toUpperCase() === value);
                if (caseRule) {
                  if (statement[0].value !== caseRule.expected) {
                    errors.push({
                      range: new vscode.Range(
                        statementStart,
                        statementEnd
                      ),
                      offset: {position: statement[0].position, length: statement[0].position + value.length},
                      type: `SpecificCasing`,
                      newValue: caseRule.expected
                    });
                  }
                }
              }

              switch (value.toUpperCase()) {
              case `EVAL`:
              case `CALLP`:
                if (rules.UselessOperationCheck) {
                  errors.push({
                    range: new vscode.Range(
                      statementStart,
                      statementEnd
                    ),
                    offset: {position: statement[0].position, length: statement[0].position + value.length + 1},
                    type: `UselessOperationCheck`
                  });
                }
                break;
              case `EXEC`:
                if (rules.NoSELECTAll) {
                  if (currentStatementUpper.includes(`SELECT *`)) {
                    errors.push({
                      range: new vscode.Range(statementStart, statementEnd),
                      type: `NoSELECTAll`
                    });
                  }
                }

                if (rules.NoSQLJoins) {
                  if (statement.some(part => part.value && part.value.toUpperCase() === `JOIN`)) {
                    errors.push({
                      range: new vscode.Range(statementStart, statementEnd),
                      type: `NoSQLJoins`
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
                  const lastStatement = statement[statement.length-1];
                  if (statement[1].type !== `openbracket` || lastStatement.type !== `closebracket`) {
                    errors.push({
                      range: new vscode.Range(
                        new vscode.Position(statementStart.line, statementStart.character + statement[0].value.length + 1),
                        statementEnd
                      ),
                      type: `ForceOptionalParens`
                    });
                  }
                }
                break;
              }
              break;
            }
          }
          
          let part;

          if (statement.length > 0 && [`declare`, `end`].includes(statement[0].type) === false) {

            for (let i = 0; i < statement.length; i++) {
              part = statement[i];

              if (part.type === `word` && part.value) {
                const upperName = part.value.toUpperCase();

                if (rules.NoGlobalsInProcedures) {
                  if (inProcedure && !inPrototype) {
                    const existingVariable = globalScope.variables.find(variable => variable.name.toUpperCase() === upperName);
                    if (existingVariable) {
                      errors.push({
                        range: new vscode.Range(
                          statementStart,
                          statementEnd
                        ),
                        offset: {position: part.position, length: part.position + part.value.length},
                        type: `NoGlobalsInProcedures`
                      });
                    }
                  }
                }
              
                if (rules.IncorrectVariableCase) {
                  // Check the casing of the reference matches the definition
                  const definedNames = currentScope.getNames();
                  const definedName = definedNames.find(defName => defName.toUpperCase() === upperName);
                  if (definedName && definedName !== part.value) {
                    errors.push({
                      range: new vscode.Range(
                        statementStart,
                        statementEnd
                      ),
                      offset: {position: part.position, length: part.position + part.value.length},
                      type: `IncorrectVariableCase`,
                      newValue: definedName
                    });
                  }
                }

                if (rules.RequiresParameter) {
                  // Check the procedure reference has a block following it
                  const definedProcedure = globalProcs.find(proc => proc.name.toUpperCase() === upperName);
                  if (definedProcedure) {
                    let requiresBlock = false;
                    if (statement.length <= i+1) {
                      requiresBlock = true;
                    } else if (statement[i+1].type !== `openbracket`) {
                      requiresBlock = true;
                    }

                    if (requiresBlock) {
                      errors.push({
                        range: new vscode.Range(
                          statementStart,
                          statementEnd
                        ),
                        offset: {position: part.position, length: part.position + part.value.length},
                        type: `RequiresParameter`,
                      });
                    }
                  }
                }
              }

              if (part.type === `string`) {
                if (part.value.substring(1, part.value.length-1).trim() === `` && rules.RequireBlankSpecial) {
                  errors.push({
                    range: new vscode.Range(
                      statementStart,
                      statementEnd
                    ),
                    offset: {position: part.position, length: part.position + part.value.length},
                    type: `RequireBlankSpecial`,
                    newValue: `*BLANK`
                  });

                } else if (rules.StringLiteralDupe) {
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
                    range: new vscode.Range(
                      statementStart,
                      statementEnd
                    ),
                    offset: {position: part.position, length: part.position + part.value.length}
                  });
                }
              }
            }
          }
          currentStatement = ``;
        }

        // Next, check for indentation errors

        pieces = upperLine.split(` `).filter(piece => piece !== ``);
        opcode = pieces[0];

        if ([
          `ENDIF`, `ENDFOR`, `ENDDO`, `ELSE`, `ELSEIF`, `ON-ERROR`, `ENDMON`, `ENDSR`, `WHEN`, `OTHER`, `END-PROC`, `END-PI`, `END-PR`, `END-DS`
        ].includes(opcode)) {
          expectedIndent -= indent; 
        }

        //Special case for `ENDSL`
        if ([
          `ENDSL`
        ].includes(opcode)) {
          expectedIndent -= (indent*2); 
        }
          
        if (currentIndent !== expectedIndent && !skipIndentCheck) {
          indentErrors.push({
            line: lineNumber,
            expectedIndent,
            currentIndent
          });
        }

        if ([
          `IF`, `ELSE`, `ELSEIF`, `FOR`, `FOR-EACH`, `DOW`, `DOU`, `MONITOR`, `ON-ERROR`, `BEGSR`, `SELECT`, `WHEN`, `OTHER`, `DCL-PROC`, `DCL-PI`, `DCL-PR`, `DCL-DS`
        ].includes(opcode)) {
          if (opcode == `DCL-DS` && oneLineTriggers[opcode].some(trigger => upperLine.includes(trigger))) {
            //No change
          } 
          else if (opcode == `DCL-PI` && oneLineTriggers[opcode].some(trigger => upperLine.includes(trigger))) {
            //No change
          }
          else if (opcode == `SELECT`) {
            if (skipIndentCheck === false) expectedIndent += (indent*2); 
          }
          else {
            expectedIndent += indent; 
          }
        }
          
      }
    }

    if (stringLiterals.length > 0) {
      stringLiterals.forEach(literal => {
        if (literal.list.length > 1) {
          literal.list.forEach(location => {
            errors.push({
              ...location,
              type: `StringLiteralDupe`
            })
          });
        }
      })
    }

    return {
      indentErrors,
      errors
    };
  }

}