/* eslint-disable no-case-declarations */

import { ALLOWS_EXTENDED, createBlocks, tokenise } from "./tokens";

import Cache from "./models/cache";
import Declaration from "./models/declaration";

import oneLineTriggers from "./models/oneLineTriggers";
import { parseFLine, parseCLine, parsePLine, parseDLine, getPrettyType, prettyTypeFromToken } from "./models/fixed";
import { Token } from "./types";
import { Keywords } from "./parserTypes";

const HALF_HOUR = (30 * 60 * 1000);

export type tablePromise = (name: string, aliases?: boolean) => Promise<Declaration[]>;
export type includeFilePromise = (baseFile: string, includeString: string) => Promise<{found: boolean, uri?: string, content?: string}>;
export type TableDetail = {[name: string]: {fetched: number, fetching?: boolean, recordFormats: Declaration[]}};
export interface ParseOptions {withIncludes?: boolean, ignoreCache?: boolean, collectReferences?: boolean};

const lineTokens = (input: string, lineNumber: number, lineIndex: number): Token[] => {
  let tokens = tokenise(input, {
    baseIndex: lineIndex,
    lineNumber,
    ignoreTypes: [`tab`]
  });
  
  return tokens;
}

const PROGRAMPARMS_NAME = `PROGRAMPARMS`;

export default class Parser {
  parsedCache: {[thePath: string]: Cache} = {};
  tables: TableDetail = {};
  tableFetch: tablePromise|undefined;
  includeFileFetch: includeFilePromise|undefined;

  constructor() {
  }

  setTableFetch(promise: tablePromise) {
    this.tableFetch = promise;
  }

  setIncludeFileFetch(promise: includeFilePromise) {
    this.includeFileFetch = promise;
  }

  async fetchTable(name: string, keyVersion = ``, aliases?: boolean): Promise<Declaration[]> {
    if (name === undefined || (name && name.trim() === ``)) return [];
    if (!this.tableFetch) return [];
    const table = name.toUpperCase();
    const existingVersion = table;
    const now = Date.now();

    if (this.tables[existingVersion]) {
      // We use this to make sure we aren't running this all over the place
      if (this.tables[existingVersion].fetching) return [];

      // If we still have a cached version, let's use that
      if (now <= (this.tables[existingVersion].fetched + HALF_HOUR)) {
        return this.tables[existingVersion].recordFormats.map(d => d.clone());
      }
    }

    this.tables[existingVersion] = {
      fetching: true,
      fetched: 0,
      recordFormats: []
    };

    let newDefs: Declaration[];

    try {
      newDefs = await this.tableFetch(table, aliases);

      this.tables[existingVersion] = {
        fetched: now,
        recordFormats: newDefs
      };
    } catch (e) {
      // Failed. Don't fetch it again
      this.tables[existingVersion] = {
        fetched: now,
        recordFormats: []
      };
      newDefs = [];
    }

    this.tables[existingVersion].fetching = false;

    return newDefs.map(d => d.clone());
  }

  /**
   * @param {string} path 
   */
  clearParsedCache(path) {
    this.parsedCache[path] = undefined;
  }

  /**
   * @param {string} path 
   */
  getParsedCache(path) {
    return this.parsedCache[path];
  }

  /**
	 * @param {string} line 
	 * @returns {string|undefined}
	 */
  static getIncludeFromDirective(line: string): string|undefined {
    if (line.includes(`*`)) return; // Likely comment
    if (line.trim().startsWith(`//`)) return; // Likely comment

    const upperLine = line.toUpperCase();
    let comment = -1;
    
    let directivePosition = upperLine.indexOf(`/COPY `);
    // Search comment AFTER the directive
    comment = upperLine.indexOf(`//`, directivePosition);
    let directiveLength = 6;

    if (directivePosition === -1) {
      directivePosition = upperLine.indexOf(`/INCLUDE `);
      // Search comment AFTER the directive
      comment = upperLine.indexOf(`//`, directivePosition);
      directiveLength = 9
    };

    let directiveValue: string|undefined;
    
    if (directivePosition >= 0) {
      if (comment >= 0) {
        directiveValue = line.substring(directivePosition+directiveLength, comment).trim();
      } else {
        directiveValue = line.substring(directivePosition+directiveLength).trim();
      }
    }

    if (directiveValue) {
      const spaceIndex = directiveValue.indexOf(` `);
      if (spaceIndex >= 0) {
        directiveValue = directiveValue.substring(0, spaceIndex);
      }

      return directiveValue;
    }
  }

  async getDocs(workingUri: string, baseContent?: string, options: ParseOptions = {withIncludes: true, collectReferences: true}): Promise<Cache|undefined> {
    const existingCache = this.getParsedCache(workingUri);
    if (options.ignoreCache !== true && existingCache) {
      return existingCache;
    }

    if (!baseContent) return null;

    let scopes: Cache[] = [];

    /** Free format struct scopes. Used for free-format only */
    let dsScopes: Declaration[] = [];

    let globalKeyword: string[] = [];

    // Global scope bits
    scopes.push(new Cache());

    const getObjectName = (objectName: string, keywords: Keywords): string => {
            
      // Check for external object
      const extFile = keywords[`EXTFILE`];
      if (extFile && typeof extFile === `string`) {
        objectName = extFile.toUpperCase();
        if (objectName.startsWith(`'`) && objectName.endsWith(`'`)) {
          objectName = objectName.substring(1, objectName.length - 1);
        }
      }

      if(objectName === `*EXTDESC`){
        // Check for external object
        const extDesc = keywords['EXTDESC'];
        if (extDesc && typeof extDesc === `string`) {
          objectName = extDesc.toUpperCase();

          if (objectName.startsWith(`'`) && objectName.endsWith(`'`)) {
            objectName = objectName.substring(1, objectName.length - 1);
          }
        }
      }

      return objectName;
    };

    let potentialName: Token|undefined;
    let potentialNameUsed = false;

    let currentGroup: "structs"|"procedures"|"constants";

    let definedMacros: string[] = [];

    /**
     * Parse the tokens and add references to the definitions
     * The statement is modified in place and sets tokens undefined when are references
     */
    const collectReferences = (currentUri: string, statement: Token[], currentProcedure?: Declaration, currentDef?: Declaration, isExec = false) => {
      if (statement[0]?.value?.toUpperCase() === `EXEC`) {
        isExec = true;
      }

      const removeCollectedToken = (at: number) => {
        statement[at] = undefined;
      }

      const addReference = (def: Declaration, part: Token, at: number) => {
        def.references.push({
          uri: currentUri,
          offset: part.range,
        });

        removeCollectedToken(at);
      }

      for (let i = 0; i < statement.length; i++) {
        const part = statement[i];
        if (part === undefined) continue;

        if (![`special`, `word`].includes(part.type)) continue;
        if (statement[i - 1] && statement[i - 1].type === `dot`) break;

        if (isExec && statement[i-1]) {
          if (statement[i-1].type !== `seperator`) {
            continue;
          }
        }

        const isSpecial = part.type === `special`;
        const lookupName = (isSpecial ? part.value.substring(1) : part.value).toUpperCase();

        let defRef: Declaration|undefined;

        if (isSpecial) {
          // The only specials that can be looked up at global indicators
          defRef = scopes[0].indicators.find(ind => ind.name.toUpperCase() === lookupName);

        } else {
          if (currentDef) {
            if (currentDef.name.toUpperCase() === lookupName) {
              defRef = currentDef;
            } else if (currentDef.subItems.length > 0) {
              defRef = currentDef.subItems.find(sub => sub.name.toUpperCase() === lookupName);
            }
          }

          if (!defRef && currentProcedure && currentProcedure.scope) {
            defRef = currentProcedure.scope.find(lookupName);

            if (!defRef) {
              defRef = currentProcedure.subItems.find(def => def.name.toUpperCase() === lookupName);
            }
          }

          if (!defRef) {
            defRef = scopes[0].find(lookupName);
          }
        }

        if (defRef) {
          addReference(defRef, part, i);

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
                    addReference(subItemDef, subItemPart, i);
                  }
                }
              }
            }
          }
        }
      }
    }

    //Now the real work
    const parseContent = async (fileUri: string, allContent: string) => {
      const EOL = allContent.includes(`\r\n`) ? `\r\n` : `\n`;
      const LINEEND = ``.padEnd(EOL.length);
      let lines = allContent.split(EOL);

      let postProcessingStatements: {[procedure: string]: Token[][]} = {'GLOBAL': []};

      const addPostProcessingStatements = (procedure = `GLOBAL`, statement: Token[]) => {
        if (!options.collectReferences) return;

        if (!postProcessingStatements[procedure]) {
          postProcessingStatements[procedure] = [];
        }

        postProcessingStatements[procedure].push(statement);
      }

      const scanScopeForReferences = () => {
        for (const procedure in postProcessingStatements) {
          const currentProcedure = scopes[0].procedures.find(proc => proc.name === procedure) ;
          const statements = postProcessingStatements[procedure];
          for (const statement of statements) {
            collectReferences(fileUri, statement, currentProcedure);
          }
        }
      }

      if (lines.length === 0) return;

      let currentTitle = undefined, currentDescription = [];
      let currentTags: {tag: string, content: string}[] = [];

      let currentItem: Declaration|undefined;
      let currentSub: Declaration|undefined;
      let currentProcName: string|undefined;

      let resetDefinition = false; //Set to true when you're done defining a new item
      let docs = false; // If section is for ILEDocs
      let lineNumber = -1;
      let lineIndex = 0;

      let isFullyFree = lines[0].toUpperCase().startsWith(`**FREE`);
      let lineIsFree = false;

      /** Used for handling multiline statements */
      let currentStmtStart: {content?: string, line: number, index: number}|undefined;

      let directIfScope: {condition: boolean}[] = [];

      let lineCanRun = () => {
        return directIfScope.length === 0 || directIfScope.every(scope => scope.condition);
      }

      /**
       * Removes the trailing line comment (//) but keeps the semi-colon if found.
       */
      const stripComment = (inputLine: string) => {
        let comment = -1;
        let inString = false;
        for (let i = inputLine.length - 1; i >= 0; i--) {
          switch (inputLine[i]) {
            case '/':
              if (inputLine[i-1] === `/`) {
                // It's a comment!
                inString = false;
                comment = i-1;
                i--;

                return inputLine.substring(0, comment).trimEnd();
              }
              break;
            case `'`:
              inString = !inString;
              break;
            case ';':
              if (!inString) {
                return inputLine.substring(0, i+1).trimEnd();
              }
              break;
          }
        }
        
        return inputLine;
      }

      /**
       * Removes the trailing comment and optionally removes the semi-colon.
       */
      const getValidStatement = (inputLine: string, withSep?: boolean) => {
        if (!inputLine.includes(`;`)) return inputLine;
        let comment = -1;
        let inString = false;
        for (let i = inputLine.length - 1; i >= 0; i--) {
          switch (inputLine[i]) {
            case `'`:
              inString = !inString;
              break;
            case ';':
              if (!inString) {
                return inputLine.substring(0, i + (withSep ? 1 : 0)).trimEnd();
              }
              break;
          }
        }

        return inputLine;
      }

      const expandDs = async (fileUri: string, fileToken: Token, ds: Declaration): Promise<void> => {
        const tags = [`LIKEDS`, `LIKEREC`, `EXTNAME`];
        const keywords = ds.keyword;
        for (const tag of tags) {
          if (keywords[tag] && typeof keywords[tag] === `string`) {
            let keywordValue = keywords[tag];
            if (keywordValue.includes(`:`)) {
              const parms = keywordValue.split(`:`).filter(part => part.trim().startsWith(`*`) === false);

              if (parms.length > 0) {
                keywordValue = parms[0];
              } else {
                break;
              }
            }

            if (keywordValue.startsWith(`'`) && keywordValue.endsWith(`'`)) {
              keywordValue = keywordValue.substring(1, keywordValue.length - 1);
            }

            if (tag === `EXTNAME`) {
              // Fetch from external definitions
              const keywordLength = Object.keys(ds.keyword);
              const recordFormats = await this.fetchTable(keywordValue, keywordLength.length.toString(), ds.keyword[`ALIAS`] !== undefined);

              if (recordFormats.length > 0) {
                // Got to fix the positions for the defintions to be the declare.
                recordFormats.forEach(recordFormat => {
                  recordFormat.subItems.forEach(subItem => {
                    subItem.position = {
                      path: fileUri,
                      range: fileToken.range
                    };
                  });

                  ds.subItems.push(...recordFormat.subItems);
                });
              }

            } else {
              // We need to add qualified as it is qualified by default.
              if (!ds.keyword[`QUALIFIED`])
                ds.keyword[`QUALIFIED`] = true;

              // Fetch from local definitions
              for (let i = scopes.length - 1; i >= 0; i--) {
                const valuePointer = scopes[i].structs.find(struct => struct.name.toUpperCase() === keywordValue.toUpperCase());
                if (valuePointer) {
                  // Only use same subItems if local definition is from same path
                  if (ds.position.path === valuePointer.position.path) {
                    ds.subItems = valuePointer.subItems;
                  } else {
                    // Clone subitems for correct line assignment
                    valuePointer.subItems.forEach((item) => {
                      const newItem = item.clone();
                      newItem.position.range = ds.position.range;
                      ds.subItems.push(newItem);
                    });
                  }

                  return;
                }
              }
            }
          }
        }
      };

      let fixedExec = false;

      for (let li = 0; li < lines.length; li++) {
        if (li >= 1) {
          lineIndex += lines[li-1].length + EOL.length;
        }
        
        const scope = scopes[scopes.length - 1];

        let baseLine = lines[li];
        let spec: string|undefined;

        lineIsFree = false;
        lineNumber += 1;

        if (baseLine.startsWith(`**`)) {
          // Usually is **FREE
          if (lineNumber === 0) continue;
          // After compile time data, we're done
          else break;
        }

        if (isFullyFree === false && baseLine.length > 6) {
          const comment = baseLine[6];
          spec = baseLine[5].toUpperCase();

          if ([spec, comment].includes(`*`)) {
            if (currentStmtStart && currentStmtStart.content) {
              // Since we're in an extended statement (usually fixed exec), we still need to collect the lengths for the tokeniser
              currentStmtStart.content += ``.padEnd(baseLine.length) + LINEEND;
            }

            continue;
          }

          if (comment === `/`) {
            // Directives can be parsed by the free format parser
            baseLine = ``.padEnd(6) + baseLine.substring(6);
            lineIsFree = true;
          } else if (comment === `+` && fixedExec && currentStmtStart.content) {
            // Fixed format EXEC SQL
            baseLine = ``.padEnd(7) + baseLine.substring(7);
            currentStmtStart.content += baseLine + LINEEND;
            continue;
          } else {
            if (spec === ` `) {
              //Clear out stupid comments
              baseLine = ``.padEnd(7) + baseLine.substring(7);
              lineIsFree = true;

            } else if (![`D`, `P`, `C`, `F`, `H`].includes(spec)) {
              continue;
            }
          }
        } else {
          // Even if the line is useless, we need to capture the characters to be
          // parsed in case it's a statement spread over multiple lines
          if (!isFullyFree && currentStmtStart && currentStmtStart.content) {
            currentStmtStart.content += ``.padEnd(baseLine.length + EOL.length);
          }
        }

        let line = baseLine;
        if (!isFullyFree && line.length > 80) {
          // Remove ending comments
          line = line.substring(0, 80);
        }

        let tokens: Token[] = [];
        let parts: string[];
        let partsLower: string[];
        
        if (isFullyFree || lineIsFree) {
          // Free format!
          if (line.trim() === ``) continue;

          const lineIsComment = line.trim().startsWith(`//`);
          tokens = lineTokens(getValidStatement(line), lineNumber, lineIndex);
          partsLower = tokens.filter(piece => piece.value).map(piece => piece.value);
          parts = partsLower.map(piece => piece.toUpperCase());

          line = line.trim();

          if (!lineIsComment) {

            // First, we need a seperate switch for EXEC statements
            switch (parts[0]) {
              case '/EXEC':
                fixedExec = true;
                baseLine = ``.padEnd(7) + baseLine.substring(7);
                currentStmtStart = {
                  line: lineNumber,
                  index: lineIndex,
                  content: baseLine + LINEEND
                }
                continue;
              case '/END':
                line = `;`;
                baseLine = ``.padEnd(baseLine.length) + `;`;
                fixedExec = false;
                break;
              default:
                // Maybe we're in a fixed exec statement, but a directive is being used.
                // See test case references_21_fixed_exec1
                if (fixedExec && currentStmtStart && currentStmtStart.content) {
                  currentStmtStart.content += ``.padEnd(baseLine.length) + LINEEND;
                }
                break;
            }

            // Then we do regular parsing
            if (parts[0] === `/EOF` && lineCanRun()) {
              // End of parsing for this file
              return;
            } else {
              switch (parts[0]) {
              case `/COPY`:
              case `/INCLUDE`:
                if (options.withIncludes && this.includeFileFetch && lineCanRun()) {
                  const includePath = Parser.getIncludeFromDirective(line);
          
                  if (includePath) {
                    const include = await this.includeFileFetch(workingUri, includePath);
                    if (include.found && include.uri) {
                      if (!scopes[0].includes.some(inc => inc.toPath === include.uri)) {
                        scopes[0].includes.push({
                          toPath: include.uri,
                          line: lineNumber
                        });
                        
                        try {
                          await parseContent(include.uri, include.content);
                        } catch (e) {
                          console.log(`Error parsing include: ${include.uri}`);
                          console.log(e);
                        }
                      }
                    }
                  }
                }
                continue;
              case `/IF`:
                // Not conditions can run
                let condition = false;
                let hasNot = (parts[1] === `NOT`);
                let expr = tokens.slice(hasNot ? 2 : 1);
                let keywords = Parser.expandKeywords(expr);

                if (typeof keywords[`DEFINED`] === `string`) {
                  condition = definedMacros.includes(keywords[`DEFINED`]);
                }

                if (hasNot) condition = !condition;

                directIfScope.push({condition: condition});
                continue;
              case `/ELSE`:
                if (directIfScope.length > 0) {
                  directIfScope[directIfScope.length - 1].condition = !directIfScope[directIfScope.length - 1].condition;
                }
                continue;
              case `/ELSEIF`:
                if (directIfScope.length > 0) {
                  directIfScope.pop();
                  directIfScope.push({condition: false});
                }
                continue;
              case `/ENDIF`:
                if (directIfScope.length > 0) {
                  directIfScope.pop();
                }
                continue;

              case `/DEFINE`:
                if (lineCanRun()) {
                  definedMacros.push(parts[1]);
                }

                continue;
              default:
                if (line.startsWith(`/`)) {
                  continue;
                }
                if (!lineCanRun()) {
                  // Ignore lines inside the IF scope.
                  continue;
                }
                break;
              }
            }
          }


          if (!currentStmtStart || !currentStmtStart.content) {
            currentStmtStart = {line: lineNumber, index: lineIndex};
          }

          if (lineIsComment) {
            // This happens when we put a comment on a line which is part of one long statement.
            // See references_24_comment_in_statement
            if (currentStmtStart.content) {
              currentStmtStart.content += ``.padEnd(baseLine.length) + LINEEND;
            }
          } else {
            if (stripComment(line).endsWith(`;`)) {

              if (currentStmtStart.content) {
                // This means the line is just part of the end of the last statement as well.
                line = currentStmtStart.content + getValidStatement(baseLine);

                tokens = lineTokens(line, currentStmtStart.line, currentStmtStart.index);
                partsLower = tokens.filter(piece => piece.value).map(piece => piece.value);
                parts = partsLower.map(piece => piece.toUpperCase());

                currentStmtStart.content = undefined;
              }

            } else if (!line.endsWith(`;`)) {
              currentStmtStart.content = (currentStmtStart.content || ``) + baseLine;
              
              if (currentStmtStart.content.endsWith(`-`)) 
                currentStmtStart.content = currentStmtStart.content.substring(0, currentStmtStart.content.length - 1) + ` `;

              currentStmtStart.content += LINEEND;

              continue;
            }
          }

          switch (parts[0]) {
          case `CTL-OPT`:
            globalKeyword.push(...parts.slice(1));
            break;

          case `DCL-F`:
            if (currentItem === undefined) {
              if (parts.length > 1) {
                currentItem = new Declaration(`file`);
                currentItem.name = partsLower[1];
                currentItem.keyword = Parser.expandKeywords(tokens.slice(2));
                currentItem.description = currentDescription.join(`\n`);

                currentItem.position = {
                  path: fileUri,
                  range: tokens[1].range
                };

                const objectName = getObjectName(tokens[1].value, currentItem.keyword);
                let prefix = ``;

                const prefixKeyword = currentItem.keyword[`PREFIX`];
                if (prefixKeyword  && typeof prefixKeyword === `string`) {
                  prefix = prefixKeyword.toUpperCase();
                }
          
                const recordFormats = await this.fetchTable(objectName, parts.length.toString(), currentItem.keyword[`ALIAS`] !== undefined);

                if (recordFormats.length > 0) {
                  const qualified = parts.includes(`QUALIFIED`);

                  // Got to fix the positions for the defintions to be the declare.
                  recordFormats.forEach(recordFormat => {
                    recordFormat.keyword = {[parts[1]]: true};
                    if (qualified) recordFormat.keyword[`QUALIFIED`] = true;

                    recordFormat.position = currentItem.position;

                    recordFormat.subItems.forEach(subItem => {
                      // We put the prefix here because in 'fetchTable' we use cached version. So if the user change the prefix, it will not refresh the variable name
                      if(prefix) {
                        subItem.name = prefix + subItem.name;
                      }
                      subItem.position = currentItem.position;
                    });
                  });

                  currentItem.subItems.push(...recordFormats);
                }

                scope.files.push(currentItem);
                resetDefinition = true;
              }
            }
            break;

          case `DCL-C`:
            if (currentItem === undefined) {
              if (parts.length > 1) {
                currentItem = new Declaration(`constant`);
                currentItem.name = partsLower[1];
                currentItem.keyword = Parser.expandKeywords(tokens.slice(2), true);
                currentItem.description = currentDescription.join(`\n`);

                currentItem.position = {
                  path: fileUri,
                  range: tokens[1].range
                };

                scope.constants.push(currentItem);
                resetDefinition = true;
              }
            }
            break;

          case `DCL-S`:
            if (parts.length > 1) {
              if (currentItem === undefined) {
                currentItem = new Declaration(`variable`);
                currentItem.name = partsLower[1];
                currentItem.keyword = Parser.expandKeywords(tokens.slice(2));
                currentItem.description = currentDescription.join(`\n`);
                currentItem.tags = currentTags;

                currentItem.position = {
                  path: fileUri,
                  range: tokens[1].range
                };

                scope.variables.push(currentItem);
                resetDefinition = true;
              }
            }
            break;

          case `DCL-ENUM`:
            if (currentItem === undefined) {
              if (parts.length > 1) {
                currentItem = new Declaration(`constant`);
                currentItem.name = partsLower[1];
                currentItem.keyword = Parser.expandKeywords(tokens.slice(2));
                currentItem.description = currentDescription.join(`\n`);

                currentItem.position = {
                  path: fileUri,
                  range: tokens[1].range
                };

                currentItem.range = {
                  start: currentStmtStart.line,
                  end: currentStmtStart.line
                };

                currentItem.readParms = true;

                currentGroup = `constants`;

                currentDescription = [];
              }
            }
            break;

          case `END-ENUM`:
            if (currentItem && currentItem.type === `constant`) {
              currentItem.range.end = currentStmtStart.line;
              
              scope.constants.push(currentItem);

              resetDefinition = true;
            }
            break;

          case `DCL-DS`:
            if (currentItem === undefined) {
              if (parts.length > 1) {
                currentItem = new Declaration(`struct`);
                currentItem.name = partsLower[1];
                currentItem.keyword = Parser.expandKeywords(tokens.slice(2));
                currentItem.description = currentDescription.join(`\n`);
                currentItem.tags = currentTags;

                currentItem.position = {
                  path: fileUri,
                  range: tokens[1].range
                };

                currentItem.range = {
                  start: currentStmtStart.line,
                  end: currentStmtStart.line
                };

                currentGroup = `structs`;

                // Expand the LIKEDS value if there is one.
                await expandDs(fileUri, tokens[1], currentItem);

                // Does the keywords include a keyword that makes end-ds useless?
                if (Object.keys(currentItem.keyword).some(keyword => oneLineTriggers[`DCL-DS`].some(trigger => keyword.startsWith(trigger)))) {
                  currentItem.range.end = currentStmtStart.line;
                  scope.structs.push(currentItem);
                } else {
                  currentItem.readParms = true;
                  dsScopes.push(currentItem);
                }

                resetDefinition = true;

                currentDescription = [];
              }
            }
            break;

          case `END-DS`:
            if (dsScopes.length > 0) {
              const currentDs = dsScopes[dsScopes.length - 1];
              currentDs.range.end = currentStmtStart.line;
            }

            if (dsScopes.length === 1) {
              scope.structs.push(dsScopes.pop());
            } else
              if (dsScopes.length > 1) {
                dsScopes[dsScopes.length - 2].subItems.push(dsScopes.pop());
              }
            break;
        
          case `DCL-PR`:
            if (currentItem === undefined) {
              if (parts.length > 1) {
                if (!scope.procedures.find(proc => proc.name && proc.name.toUpperCase() === parts[1])) {
                  currentGroup = `procedures`;
                  currentItem = new Declaration(`procedure`);
                  currentItem.name = partsLower[1];
                  currentItem.keyword = Parser.expandKeywords(tokens.slice(2));
                  currentItem.description = currentDescription.join(`\n`);
                  currentItem.tags = currentTags;

                  currentItem.position = {
                    path: fileUri,
                    range: tokens[1].range
                  };

                  currentItem.readParms = true;

                  currentItem.range = {
                    start: currentStmtStart.line,
                    end: currentStmtStart.line
                  };

                  // Does the keywords include a keyword that makes end-ds useless?
                  if (Object.keys(currentItem.keyword).some(keyword => oneLineTriggers[`DCL-PR`].some(trigger => keyword.startsWith(trigger)))) {
                    currentItem.range.end = currentStmtStart.line;
                    scope.procedures.push(currentItem);
                    resetDefinition = true;
                  }

                  currentDescription = [];
                }
              }
            }
            break;

          case `END-PR`:
            if (currentItem && currentItem.type === `procedure`) {
              currentItem.range.end = currentStmtStart.line;

              const isDefinedGlobally = scopes[0].procedures.some(proc => proc.name.toUpperCase() === currentItem.name.toUpperCase());

              // Don't re-add self. This can happens when `END-PR` is used in the wrong place.
              if (!isDefinedGlobally) {
                scope.procedures.push(currentItem);
              }

              resetDefinition = true;
            }
            break;
        
          case `DCL-PROC`:
            if (parts.length > 1) {
            //We can overwrite it.. it might have been a PR before.
            // eslint-disable-next-line no-case-declarations
              const existingProc = scope.procedures.findIndex(proc => proc.name && proc.name.toUpperCase() === parts[1]);

              // We found the PR... so we can overwrite it
              if (existingProc >= 0) scope.procedures.splice(existingProc, 1);

              currentItem = new Declaration(`procedure`);

              currentProcName = partsLower[1];
              currentItem.name = currentProcName;
              currentItem.keyword = Parser.expandKeywords(tokens.slice(2));
              currentItem.description = currentDescription.join(`\n`);
              currentItem.tags = currentTags;

              currentItem.position = {
                path: fileUri,
                range: tokens[1].range
              };

              currentItem.readParms = false;

              currentItem.range = {
                start: currentStmtStart.line,
                end: currentStmtStart.line
              };

              currentItem.scope = new Cache();

              scope.procedures.push(currentItem);
              resetDefinition = true;

              scopes.push(currentItem.scope);
            }
            break;

          case `DCL-PI`:
            //Procedures can only exist in the global scope.
            if (parts.length > 0) {
              if (currentProcName) {
                currentGroup = `procedures`;
                currentItem = scopes[0].procedures.find(proc => proc.name === currentProcName);
              } else {
                currentItem = new Declaration(`struct`);
                currentItem.name = PROGRAMPARMS_NAME;
              }

              if (currentItem) {
                const endInline = tokens.findIndex(part => part.value.toUpperCase() === `END-PI`);

                // Indicates that the PI starts and ends on the same line
                if (endInline >= 0) { 
                  tokens.splice(endInline, 1);
                  currentItem.readParms = false;
                  resetDefinition = true;
                }

                currentItem.keyword = {
                  ...currentItem.keyword,
                  ...Parser.expandKeywords(tokens.slice(2))
                }
                currentItem.readParms = true;

                currentDescription = [];
              }
            }
            break;

          case `END-PI`:
            //Procedures can only exist in the global scope.
            if (currentProcName) {
              currentItem = scopes[0].procedures.find(proc => proc.name === currentProcName);

              if (currentItem && currentItem.type === `procedure`) {
                currentItem.readParms = false;
                resetDefinition = true;
              }
            } else if (currentItem && currentItem.name === PROGRAMPARMS_NAME) {
              // Assign this scopes parameters to the subitems of the program parameters struct
              scopes[0].parameters = currentItem.subItems;
              resetDefinition = true;
            }
            break;

          case `END-PROC`:
            //Procedures can only exist in the global scope.
            if (scopes.length > 1) {
              currentItem = scopes[0].procedures.find(proc => proc.name === currentProcName);

              if (currentItem && currentItem.type === `procedure`) {
                scopes.pop();
                currentItem.range.end = currentStmtStart.line;
                resetDefinition = true;
              }
            }
            break;

          case `BEGSR`:
            if (parts.length > 1) {
              if (!scope.subroutines.find(sub => sub.name && sub.name.toUpperCase() === parts[1])) {
                currentItem = new Declaration(`subroutine`);
                currentItem.name = partsLower[1];
                currentItem.description = currentDescription.join(`\n`);
		            currentItem.keyword = {'Subroutine': true};

                currentItem.position = {
                  path: fileUri,
                  range: tokens[1].range
                };

                currentItem.range = {
                  start: currentStmtStart.line,
                  end: currentStmtStart.line
                };

                currentDescription = [];
              }
            }
            break;
    
          case `ENDSR`:
            if (currentItem && currentItem.type === `subroutine`) {
              currentItem.range.end = currentStmtStart.line;
              scope.subroutines.push(currentItem);
              resetDefinition = true;
            }
            break;

          case `EXEC`:
            const pIncludes = (value) => {
              return parts.some(p => p === value);
            }

            const pIs = (part, value) => {
              return part && part === value;
            }

            if (tokens.length > 2 && !pIncludes(`FETCH`)) {
              // insert into XX.XX
              // delete from xx.xx
              // update xx.xx set
              // select * into :x from xx.xx
              // call xx.xx()
              const preFileWords = [`INTO`, `FROM`, `UPDATE`, `CALL`, `JOIN`];
              const ignoredWords = [`FINAL`, `SET`];

              const cleanupObjectRef = (index) => {
                let nameIndex = index;
                const result = {
                  schema: undefined,
                  name: partsLower[index],
                  length: 1,
                  nameToken: tokens[index]
                }

                const schemaSplit = [`.`, `/`].includes(parts[nameIndex + 1]);
                if (schemaSplit) {
                  result.schema = partsLower[index]
                  result.name = partsLower[index + 2]
                  result.length = 3;
                  result.nameToken = tokens[index + 2];
                  nameIndex += 2;
                }

                return result;
              }

              let isContinued = false;

              let ignoreCtes: string[] = [];

              if (pIncludes(`WITH`)) {
                for (let index = 4; index < tokens.length; index++) {
                  if (pIs(parts[index], `AS`) && pIs(parts[index+1], `(`)) {
                    ignoreCtes.push(parts[index-1].toUpperCase());
                  }
                }
              }

              for (let index = 0; index < parts.length; index++) {
                const part = parts[index];
                let inBlock = preFileWords.includes(part);

                if (
                  (inBlock || isContinued) &&  // If this is true, usually means next word is the object
                  (part === `INTO` ? parts[index-1] === `INSERT` : true) // INTO is special, as it can be used in both SELECT and INSERT
                ) {
                  if (index >= 0 && (index+1) < parts.length && !ignoredWords.includes(parts[index+1])) {

                    const qualifiedObjectPath = cleanupObjectRef(index+1);

                    index += (qualifiedObjectPath.length);

                    isContinued = (parts[index+1] === `,`);

                    if (qualifiedObjectPath.name && !ignoreCtes.includes(qualifiedObjectPath.name.toUpperCase())) {
                      const currentSqlItem = new Declaration(`file`);
                      currentSqlItem.name = qualifiedObjectPath.name;

                      if (currentSqlItem.name)
                        currentSqlItem.keyword = {};
                      
                      currentSqlItem.description = qualifiedObjectPath.schema || ``;
      
                      currentSqlItem.position = {
                        path: fileUri,
                        range: qualifiedObjectPath.nameToken.range
                      };
      
                      scope.sqlReferences.push(currentSqlItem);
                    }
                  }
                }
              };
            }
            break;

          case `///`:
            docs = !docs;
          
            // When enabled
            if (docs === true) {
              currentTitle = undefined;
              currentDescription = [];
              currentTags = [];
            }
            break;

          default:
            if (lineIsComment) {
              if (docs) {
                const content = line.substring(2).trim();
                if (content.length > 0) {
                  if (content.startsWith(`@`)) {
                    const lineData = content.substring(1).split(` `);
                    currentTags.push({
                      tag: lineData[0],
                      content: lineData.slice(1).join(` `)
                    });
                  } else {
                    if (currentTags.length > 0) {
                      currentTags[currentTags.length - 1].content += ` ${content}`;

                    } else {
                      if (currentTitle === undefined) {
                        currentTitle = content;
                      } else {
                        currentDescription.push(content);
                      }
                    }
                  }
                }

              } else {
                //Do nothing because it's a regular comment
              }

            } else {
              if (!currentItem) {
                if (dsScopes.length >= 1) {
                  // We do this as there can be many levels to data structures in free format
                  currentItem = dsScopes[dsScopes.length - 1];
                }
              }

              if (currentItem && [`procedure`, `struct`, `constant`].includes(currentItem.type)) {
                if (currentItem.readParms && parts.length > 0) {
                  if (parts[0].startsWith(`DCL`)) {
                    parts.slice(1);
                    partsLower = partsLower.splice(1);
                    tokens.splice(1);
                  }

                  currentSub = new Declaration(`subitem`);
                  currentSub.name = (parts[0] === `*N` ? `parm${currentItem.subItems.length+1}` : partsLower[0]);
                  currentSub.keyword = Parser.expandKeywords(tokens.slice(1));

                  currentSub.position = {
                    path: fileUri,
                    range: tokens[0].range
                  };

                  // Add comments from the tags
                  if (currentItem.type === `procedure`) {
                    const paramTags = currentItem.tags.filter(tag => tag.tag === `param`);
                    const paramTag = paramTags.length > currentItem.subItems.length ? paramTags[currentItem.subItems.length] : undefined;
                    if (paramTag) {
                      currentSub.description = paramTag.content;
                    }
                  }

                  // If the parameter has likeds, add the subitems to make it a struct.
                  await expandDs(fileUri, tokens[0], currentSub);

                  currentItem.subItems.push(currentSub);
                  currentSub = undefined;

                  if (currentItem.type === `struct` && currentItem.name !== PROGRAMPARMS_NAME) {
                    resetDefinition = true;
                  }
                }
              }
            }
            break;
          }

        } else {
          // Fixed format!
          if (!lineCanRun()) {
            // Ignore lines inside the IF scope.
            continue;
          }

          switch (spec) {
          case `H`:
            globalKeyword.push(line.substring(6));
            break;

          case `F`:
            const fSpec = parseFLine(lineNumber, lineIndex, line);

            if (fSpec.name) {
              currentItem = new Declaration(`file`);
              currentItem.name = fSpec.name.value;
              currentItem.keyword = fSpec.keywords;

              currentItem.position = {
                path: fileUri,
                range: fSpec.name.range
              };
			  
			        let prefix = ``;

              const prefixKeyword = fSpec.keywords[`PREFIX`];
              if (prefixKeyword && typeof prefixKeyword === `string`) {
                prefix = prefixKeyword.toUpperCase();
              }

              const objectName = getObjectName(fSpec.name.value, fSpec.keywords);
              const recordFormats = await this.fetchTable(objectName, line.length.toString(), fSpec.keywords[`ALIAS`] !== undefined);

              if (recordFormats.length > 0) {
                const qualified = fSpec.keywords[`QUALIFIED`] === true;

                // Got to fix the positions for the defintions to be the declare.
                recordFormats.forEach(recordFormat => {
                  recordFormat.keyword = {[objectName]: true};
                  if (qualified) recordFormat.keyword[`QUALIFIED`] = true;;

                  recordFormat.position = currentItem.position;

                  recordFormat.subItems.forEach(subItem => {
					          // We put the prefix here because in 'fetchTable' we use cached version. So if the user change the prefix, it will not refresh the variable name
                    if(prefix) {
                      subItem.name = prefix + subItem.name;
                    }					 
                    subItem.position = currentItem.position;
                  });
                });

                currentGroup = `structs`;
                currentItem.subItems.push(...recordFormats);
              }

              scope.files.push(currentItem);
            } else {
              currentItem = scope.files[scope.files.length-1];
              if (currentItem) {
                currentItem.keyword = {
                  ...fSpec.keywords,
                  ...currentItem.keyword,
                }
              }
            }
            
            resetDefinition = true;
            break;

          case `C`:
            const cSpec = parseCLine(lineNumber, lineIndex, line);

            tokens = [cSpec.indicator, cSpec.ind1, cSpec.ind2, cSpec.ind3];

            const fromToken = (token?: Token) => {
              return token ? lineTokens(token.value, lineNumber, token.range.start) : [];
            };

            if (cSpec.opcode && ALLOWS_EXTENDED.includes(cSpec.opcode.value) && !cSpec.factor1 && cSpec.extended) {
              tokens.push(...fromToken(cSpec.extended));
            } else if (!cSpec.factor1 && !cSpec.opcode && cSpec.extended) {
              tokens.push(...fromToken(cSpec.extended));
            } else {
              tokens.push(
                ...fromToken(cSpec.factor1),
                ...fromToken(cSpec.factor2),
                ...fromToken(cSpec.result),
              );

              if (cSpec.result && cSpec.fieldLength) {
                // This means we need to dynamically define this field
                const fieldName = cSpec.result.value;
                // Don't redefine this field.
                if (!scopes[0].find(fieldName, currentProcName)) {
                  const fieldLength = parseInt(cSpec.fieldLength.value);
                  const decimals = cSpec.fieldDecimals ? parseInt(cSpec.fieldDecimals.value) : undefined;
                  const type = decimals !== undefined ? `PACKED`: `CHAR`;

                  currentItem = new Declaration(`variable`);
                  currentItem.name = fieldName;
                  currentItem.keyword = {[type]: `${fieldLength}${decimals !== undefined ? `:${decimals}` : ``}`};
                  currentItem.position = {
                    path: fileUri,
                    range: cSpec.result.range
                  };
                  currentItem.range = {
                    start: lineNumber,
                    end: lineNumber
                  };

                  scope.variables.push(currentItem);
                }
              }
            }

            potentialName = cSpec.factor1;

            switch (cSpec.opcode && cSpec.opcode.value) {
            case `BEGSR`:
              if (cSpec.factor1 && !scope.subroutines.find(sub => sub.name && sub.name.toUpperCase() === cSpec.factor1.value.toUpperCase())) {
                currentItem = new Declaration(`subroutine`);
                currentItem.name = cSpec.factor1.value;
                currentItem.keyword = {'Subroutine': true};
  
                currentItem.position = {
                  path: fileUri,
                  range: cSpec.factor1.range
                };
  
                currentItem.range = {
                  start: lineNumber,
                  end: lineNumber
                };
  
                currentDescription = [];
              }
              break;

            case `ENDSR`:
              if (currentItem && currentItem.type === `subroutine`) {
                currentItem.range.end = lineNumber;
                scope.subroutines.push(currentItem);
                resetDefinition = true;
              }
              break;
          
            case `CALL`:
              const callItem = new Declaration(`procedure`);
              if (cSpec.factor2) {
                const f2Value = cSpec.factor2.value;
                callItem.name = (f2Value.startsWith(`'`) && f2Value.endsWith(`'`) ? f2Value.substring(1, f2Value.length-1) : f2Value);
                callItem.keyword = {'EXTPGM': true}
                callItem.description = currentDescription.join(`\n`);
                callItem.tags = currentTags;

                callItem.position = {
                  path: fileUri,
                  range: cSpec.factor2.range
                };

                callItem.range = {
                  start: lineNumber,
                  end: lineNumber
                };

                scope.procedures.push(callItem);
              }
              break;

            case `TAG`:
              const tagItem = new Declaration(`tag`);
              if (cSpec.factor1) {
                tagItem.name = cSpec.factor1.value;
                tagItem.position = {
                  path: fileUri,
                  range: cSpec.factor1.range
                };

                tagItem.range = {
                  start: lineNumber,
                  end: lineNumber
                };

                scope.tags.push(tagItem);
              }
              break;
            }

            break;
          case `P`:
            const pSpec = parsePLine(line, lineNumber, lineIndex);

            if (pSpec.potentialName) {
              potentialName = pSpec.potentialName;
              potentialNameUsed = true;
              tokens = [pSpec.potentialName];
            } else {
              if (pSpec.start) {
                tokens = [...pSpec.keywordsRaw, pSpec.name]
                potentialName = pSpec.name && pSpec.name.value.length > 0 ? pSpec.name : potentialName;

                if (potentialName) {
                  //We can overwrite it.. it might have been a PR before.
                  const existingProc = potentialName ? scope.procedures.findIndex(proc => proc.name && proc.name.toUpperCase() === potentialName.value.toUpperCase()) : -1;

                  // We found the PR... so we can overwrite it
                  if (existingProc >= 0) scope.procedures.splice(existingProc, 1);

                  currentItem = new Declaration(`procedure`);

                  currentProcName = potentialName.value;
                  currentItem.name = currentProcName;
                  currentItem.keyword = pSpec.keywords;

                  currentItem.position = {
                    path: fileUri,
                    range: potentialName.range
                  };

                  currentItem.range = {
                    start: potentialName.range.line,
                    end: potentialName.range.line
                  };

                  currentItem.scope = new Cache();

                  scope.procedures.push(currentItem);
                  resetDefinition = true;

                  scopes.push(currentItem.scope);
                }
              } else {
                if (scopes.length > 1) {
                  //Procedures can only exist in the global scope.
                  currentItem = scopes[0].procedures.find(proc => proc.name === currentProcName);

                  if (currentItem && currentItem.type === `procedure`) {
                    scopes.pop();
                    currentItem.range.end = lineNumber;
                    resetDefinition = true;
                  }
                }
              }
            }
            break;

          case `D`:
            const dSpec = parseDLine(lineNumber, lineIndex, line);

            if (dSpec.potentialName && dSpec.potentialName) {
              potentialName = dSpec.potentialName;
              potentialNameUsed = true;
              tokens = [dSpec.potentialName];
              continue;
            } else {
              potentialName = dSpec.name && dSpec.name.value.length > 0 ? dSpec.name : potentialName;
              tokens = [dSpec.field, ...dSpec.keywordsRaw, dSpec.name];

              const useNameToken = potentialName ? potentialName : dSpec.field;

              switch (dSpec.field && dSpec.field.value) {
              case `C`:
                currentItem = new Declaration(`constant`);
                currentItem.name = potentialName ? potentialName.value : `*N`;
                currentItem.keyword = dSpec.keywords || {};
                  
                // TODO: line number might be different with ...?
                currentItem.position = {
                  path: fileUri,
                  range: useNameToken.range
                };
    
                scope.constants.push(currentItem);
                resetDefinition = true;
                break;
              case `S`:
                currentItem = new Declaration(`variable`);
                currentItem.name = potentialName ? potentialName.value : `*N`;
                currentItem.keyword = {
                  ...dSpec.keywords,
                  ...prettyTypeFromToken(dSpec),
                }

                // TODO: line number might be different with ...?
                currentItem.position = {
                  path: fileUri,
                  range: useNameToken.range
                };

                scope.variables.push(currentItem);
                resetDefinition = true;
                break;

              case `DS`:
                currentItem = new Declaration(`struct`);
                currentItem.name = potentialName ? potentialName.value : `*N`;
                currentItem.keyword = dSpec.keywords;

                currentItem.position = {
                  path: fileUri,
                  range: useNameToken.range
                };

                currentItem.range = {
                  start: currentItem.position.range.line,
                  end: currentItem.position.range.line
                };

                expandDs(fileUri, useNameToken, currentItem);

                currentGroup = `structs`;
                scope.structs.push(currentItem);
                resetDefinition = true;
                break;

              case `PR`:
                // Only add a PR if it's not been defined
                if (potentialName && !scope.procedures.find(proc => proc.name && proc.name.toUpperCase() === potentialName.value.toUpperCase())) {
                  currentItem = new Declaration(`procedure`);
                  currentItem.name = potentialName ? potentialName.value : `*N`;
                  currentItem.keyword = {
                    ...prettyTypeFromToken(dSpec),
                    ...dSpec.keywords
                  }
  
                  currentItem.position = {
                    path: fileUri,
                    range: useNameToken.range
                  };
  
                  currentItem.range = {
                    start: currentItem.position.range.line,
                    end: currentItem.position.range.line
                  };
  
                  currentGroup = `procedures`;
                  scope.procedures.push(currentItem);
                  currentDescription = [];
                }
                break;

              case `PI`:
                //Procedures can only exist in the global scope.
                if (currentProcName) {
                  currentItem = scopes[0].procedures.find(proc => proc.name === currentProcName);

                  currentGroup = `procedures`;
                  if (currentItem) {
                    currentItem.keyword = {
                      ...currentItem.keyword,
                      ...prettyTypeFromToken(dSpec),
                      ...dSpec.keywords
                    }
                  }
                }
                break;

              default:
                // No type, must be either a struct subfield OR a parameter
                if (!currentItem) {
                  switch (currentGroup) {
                  case `structs`:
                  case `procedures`:

                    // We have to do this backwards lookup to find the definition
                    // because in fixed format, currentItem is not defined. So
                    // we go find the latest procedure/structure defined
                    let validScope;
                    for (let i = scopes.length - 1; i >= 0; i--) {
                      validScope = scopes[i];
                      if (validScope[currentGroup].length > 0) break;
                    }
                  
                    currentItem = validScope[currentGroup][validScope[currentGroup].length - 1];
                    break;
                  }
                }

                if (currentItem) {

                  // This happens when it's a blank parm.
                    const baseToken = dSpec.type || dSpec.len;
                  if (!potentialName && baseToken) {
                    potentialName = {
                      ...baseToken,
                      value: `parm${currentItem.subItems.length+1}`
                    }
                  }

                  if (potentialName) {
                    currentSub = new Declaration(`subitem`);
                    currentSub.name = potentialName.value;
                    currentSub.keyword = {
                      ...prettyTypeFromToken(dSpec),
                      ...dSpec.keywords
                    }

                    currentSub.position = {
                      path: fileUri,
                      range: potentialName.range
                    };

                    // If the parameter has likeds, add the subitems to make it a struct.
                    await expandDs(fileUri, potentialName, currentSub);

                    currentItem.subItems.push(currentSub);
                    currentSub = undefined;

                    resetDefinition = true;
                  } else {
                    if (currentItem) {
                      if (currentItem.subItems.length > 0) {
                        currentItem.subItems[currentItem.subItems.length - 1].keyword = {
                          ...currentItem.subItems[currentItem.subItems.length - 1].keyword,
                          ...prettyTypeFromToken(dSpec),
                          ...dSpec.keywords
                        }
                      } else {
                        currentItem.keyword = {
                          ...currentItem.keyword,
                          ...dSpec.keywords
                        }
                      }
                    }
                  }

                  currentItem.range.end = lineNumber;
                }
                break;
              }
            
              potentialName = undefined;
            }
            break;
          }
        }

        if (options.collectReferences && tokens.length > 0) {
          const currentProc = scopes[0].procedures.find(proc => proc.name === currentProcName);
          collectReferences(fileUri, tokens, currentProc, currentItem);
          addPostProcessingStatements(currentProcName, tokens);
        }

        if (resetDefinition) {
          potentialName = undefined;
          potentialNameUsed = false;
          
          currentItem = undefined;
          currentTitle = undefined;
          currentDescription = [];
          currentTags = [];
          resetDefinition = false;
        }
      }

      if (options.collectReferences) {
        scanScopeForReferences();
      }
    }

    await parseContent(workingUri, baseContent);

    if (scopes.length > 0) {
      scopes[0].keyword = Parser.expandKeywords(Parser.getTokens(globalKeyword));
    }

    scopes[0].fixProcedures();    

    const parsedData = scopes[0];

    this.parsedCache[workingUri] = parsedData;

    return parsedData;
  }

  static getTokens(content: string|string[]|Token[], lineNumber?: number, baseIndex?: number): Token[] {
    if (Array.isArray(content) && typeof content[0] === `string`) {
      return lineTokens(content.join(` `), lineNumber, baseIndex);
    } else 
      if (typeof content === `string`) {
        return lineTokens(content, lineNumber, baseIndex);
      } else {
        return content as Token[];
      }
  }

  static expandKeywords(tokens: Token[], isConst = false): Keywords {
    const keyvalues: Keywords = {};

    if (tokens.length > 0) {
      const keywordParts = createBlocks(tokens.slice(0));

      for (let i = 0; i < keywordParts.length; i++) {
        if (keywordParts[i].value) {
          if (keywordParts[i+1] && keywordParts[i+1].type === `block`) {
            keyvalues[keywordParts[i].value.toUpperCase()] = keywordParts[i+1].block.map(part => part.value).join(``);
            i++; // Skip one for the block.
          } else {
            if (isConst) {
              keyvalues[`CONST`] = keywordParts[i].value;
            } else {
              keyvalues[keywordParts[i].value.toUpperCase()] = true;
            }
          }
        }
      }
    }

    return keyvalues;
  }
}