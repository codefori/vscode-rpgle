/* eslint-disable no-case-declarations */

import { createBlocks, tokenise } from "./tokens";

import Cache from "./models/cache";
import Declaration from "./models/declaration";

import oneLineTriggers from "./models/oneLineTriggers";
import { parseFLine, parseCLine, parsePLine, parseDLine, getPrettyType } from "./models/fixed";

const HALF_HOUR = (30 * 60 * 1000);

/**
 * @callback tablePromise
 * @param  {string} name Table name
 * @param  {boolean} [aliases] Table name
 * @returns {Promise<Declaration[]>}
 */

/**
 * @callback includeFilePromise
 * @param {string} baseFile
 * @param {string} includeString
 * @returns {Promise<{found: boolean, uri?: string, lines?: string[]}>}
 */

export default class Parser {
  constructor() {
    /** @type {{[path: string]: Cache}} */
    this.parsedCache = {};

    /** @type {{[name: string]: {fetched: number, fetching?: boolean, recordFormats: Declaration[]}}} */
    this.tables = {};

    /** @type {tablePromise} */
    this.tableFetch = undefined;

    /** @type {includeFilePromise} */
    this.includeFileFetch = undefined;
  }

  /**
   * @param {tablePromise} promise 
   */
  setTableFetch(promise) {
    this.tableFetch = promise;
  }

  /**
	 * @param {includeFilePromise} promise 
	 */
  setIncludeFileFetch(promise) {
    this.includeFileFetch = promise;
  }

  /**
   * @param {string} name 
   * @param {string} keyVersion
   * @param {boolean} [aliases]
   * @returns {Promise<Declaration[]>}
   */
  async fetchTable(name, keyVersion = ``, aliases) {
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

    /** @type {Declaration[]} */
    let newDefs;

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
  static getIncludeFromDirective(line) {
    if (line.includes(`*`)) return; // Likely comment
    if (line.includes(`//`)) return; // Likely comment

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

    /** @type {string|undefined} */
    let directiveValue;
    
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

  /**
   * @param {string} workingUri
   * @param {string} [content] 
   * @param {{withIncludes?: boolean, ignoreCache?: boolean}} options
   * @returns {Promise<Cache|undefined>}
   */
  async getDocs(workingUri, content, options = {withIncludes: true}) {
    const existingCache = this.getParsedCache(workingUri);
    if (options.ignoreCache !== true && existingCache) {
      return existingCache;
    }

    if (!content) return null;

    /** @type {{[path: string]: string[]}} */
    const files = {};

    let baseLines = content.replace(new RegExp(`\\\r`, `g`), ``).split(`\n`);

    let currentTitle = undefined, currentDescription = [];
    /** @type {{tag: string, content: string}[]} */
    let currentTags = [];

    /** @type {Declaration} */
    let currentItem;
    /** @type {Declaration} */
    let currentSub;
    let currentProcName;

    let resetDefinition = false; //Set to true when you're done defining a new item
    let docs = false; // If section is for ILEDocs
    let lineNumber, parts, partsLower, pieces;

    /** @type {Cache[]} */
    let scopes = [];

    /** @type {Declaration[]} Free format struct scopes. Used for free-format only */
    let dsScopes = [];

    /** @type {string[]} */
    let keywords = [];

    // Global scope bits
    scopes.push(new Cache());

    /**
     * Gets value of EXTFILE if it exists.
     * @param {string} defaultName 
     * @param {string[]} keywords 
     * @returns {string}
     */
    const getObjectName = (defaultName, keywords) => {
      let objectName = defaultName;
      const extObjKeywords = [`EXTFILE`];
      const extObjKeywordsDesc = [`EXTDESC`];
            
      // Check for external object
      extObjKeywords.forEach(keyword => {
        const keywordValue = keywords.find(part => part.startsWith(`${keyword}(`) && part.endsWith(`)`));
        if (keywordValue) {
          objectName = keywordValue.substring(keyword.length+1, keywordValue.length - 1).toUpperCase();

          if (objectName.startsWith(`'`) && objectName.endsWith(`'`)) {
            objectName = objectName.substring(1, objectName.length - 1);
          }
        }
      });

      if(objectName === `*EXTDESC`){
        // Check for external object
        extObjKeywordsDesc.forEach(keyword => {
          const keywordValue = keywords.find(part => part.startsWith(`${keyword}(`) && part.endsWith(`)`));
          if (keywordValue) {
            objectName = keywordValue.substring(keyword.length+1, keywordValue.length - 1).toUpperCase();

            if (objectName.startsWith(`'`) && objectName.endsWith(`'`)) {
              objectName = objectName.substring(1, objectName.length - 1);
            }
          }
        });
      }

      return objectName;
    };

    /**
     * Expands LIKEDS, LIKEREC and EXTNAME.
     * @param {string} file
     * @param {Declaration} ds 
     */
    const expandDs = async (file, ds) => {
      const tags = [`LIKEDS`, `LIKEREC`, `EXTNAME`];
      for (const tag of tags) {
        const keyword = ds.keywords.find(keyword => keyword.startsWith(`${tag}(`) && keyword.endsWith(`)`));
        if (keyword) {
          let keywordValue = keyword.substring(tag.length+1, keyword.length - 1).toUpperCase();

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

          if ([`EXTNAME`].includes(tag)) {
            // Fetch from external definitions
            const recordFormats = await this.fetchTable(keywordValue, ds.keywords.length.toString(), ds.keywords.includes(`ALIAS`));

            if (recordFormats.length > 0) {

              // Got to fix the positions for the defintions to be the declare.
              recordFormats.forEach(recordFormat => {
                recordFormat.subItems.forEach(subItem => {
                  subItem.position = {
                    path: file,
                    line: lineNumber
                  };
                });

                ds.subItems.push(...recordFormat.subItems);
              });
            }

          } else {
            // We need to add qualified as it is qualified by default.
            if (!ds.keywords.includes(`QUALIFIED`))
              ds.keywords.push(`QUALIFIED`);

            // Fetch from local definitions
            for (let i = scopes.length - 1; i >= 0; i--) {
              const valuePointer = scopes[i].structs.find(struct => struct.name.toUpperCase() === keywordValue);
              if (valuePointer) {
                // Only use same subItems if local definition is from same path
                if (ds.position.path === valuePointer.position.path) {
                  ds.subItems = valuePointer.subItems;
                } else {
                  // Clone subitems for correct line assignment
                  valuePointer.subItems.forEach((item) => {
                    const newItem = item.clone();
                    newItem.position.line = ds.position.line;
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

    if (options.withIncludes && this.includeFileFetch) {
    //First loop is for copy/include statements
      for (let i = baseLines.length - 1; i >= 0; i--) {
        let line = baseLines[i]; //Paths are case insensitive so it's okay
        if (line === ``) continue;

        const includePath = Parser.getIncludeFromDirective(line);

        if (includePath) {
          const include = await this.includeFileFetch(workingUri, includePath);
          if (include.found) {
            files[include.uri] = include.lines;
            scopes[0].includes.push({
              toPath: include.uri,
              line: i
            });
          }
        }
      }
    }

    files[workingUri] = baseLines;

    let potentialName;
    let potentialNameUsed = false;

    /** @type {"structs"|"procedures"|"constants"} */
    let currentGroup;
    let isFullyFree = false;

    //Now the real work
    for (const file of Object.keys(files)) {
      if (files[file].length === 0) continue;
      lineNumber = -1;

      isFullyFree = files[file][0].toUpperCase().startsWith(`**FREE`);
      let lineIsFree = false;

      /** @type {string|undefined} */
      let currentStatement;
      /** @type {number|undefined} */
      let statementStartingLine;

      let directIfScope = 0;

      for (let line of files[file]) {
        const scope = scopes[scopes.length - 1];
        let spec;

        lineIsFree = false;
        lineNumber += 1;

        if (line.startsWith(`**`)) {
          // Usually is **FREE
          if (lineNumber === 0) continue;
          // After compile time data, we're done
          else break;
        }

        if (isFullyFree === false && line.length > 6) {
          const comment = line[6];
          spec = line[5].toUpperCase();

          if (comment === `*`) {
            continue;
          }

          if (comment === `/`) {
            // Directives can be parsed by the free format parser
            line = line.substring(6);
            lineIsFree = true;
          } else {
            if (spec === ` `) {
            //Clear out stupid comments
              line = line.substring(7);

              lineIsFree = true;
            } else if (![`D`, `P`, `C`, `F`, `H`].includes(spec)) {
              continue;
            } else {
              if (spec === `C`) {
                // We don't want to waste precious time parsing all C specs, so we make sure it's got
                // BEGSR or ENDSR in it first.
                const upperLine = line.toUpperCase();
                if ([`BEGSR`, `ENDSR`, `CALL`].some(v => upperLine.includes(v)) === false) {
                  continue;
                }
              }
            }
          }

          if (line.length > 80) {
            // Remove ending comments
            line = line.substring(0, 80);
          }
        }

        pieces = [];
        parts = [];
        
        if (isFullyFree || lineIsFree) {
          // Free format!
          line = line.trim();

          if (line === ``) continue;

          pieces = line.split(`;`);
          parts = pieces[0].toUpperCase().split(` `).filter(piece => piece !== ``);
          partsLower = pieces[0].split(` `).filter(piece => piece !== ``);

          const lineIsComment = line.startsWith(`//`);

          if (!lineIsComment) {
            if (parts[0] === `/EOF` && directIfScope === 0) {
              // End of parsing for this file
              break;
            } else
            if (parts[0] === `/IF`) {
              // Directive IF
              directIfScope += 1;
              continue;
            } else
            if (parts[0] === `/ENDIF`) {
              // Directive ENDIF
              directIfScope -= 1;
              continue;
            } else
            if (directIfScope > 0) {
              // Ignore lines inside the IF scope.
              continue;
            } else
            if (line.startsWith(`/`)) {
              continue;
            }
          }

          if (pieces.length > 1 && pieces[1].includes(`//`)) line = pieces[0] + `;`;

          if (!currentStatement) statementStartingLine = lineNumber;

          if (!lineIsComment) {
            if (line.endsWith(`;`)) {
              if (currentStatement) {
                // This means the line is just part of the end of the last statement as well.
                line = currentStatement + line;
                currentStatement = undefined;

                pieces = line.split(`;`);
                parts = pieces[0].toUpperCase().split(` `).filter(piece => piece !== ``);
                partsLower = pieces[0].split(` `).filter(piece => piece !== ``);
              }

            } else if (!line.endsWith(`;`)) {

              currentStatement = (currentStatement || ``) + line.trim();
              if (currentStatement.endsWith(`-`)) 
                currentStatement = currentStatement.substring(0, currentStatement.length - 1);
              else
                currentStatement += ` `;

              continue;
            }
          }

          switch (parts[0]) {
          case `CTL-OPT`:
            keywords.push(...parts.slice(1));
            break;

          case `DCL-F`:
            if (currentItem === undefined) {
              if (parts.length > 1) {
                currentItem = new Declaration(`file`);
                currentItem.name = partsLower[1];
                currentItem.keywords = parts.slice(2);
                currentItem.description = currentDescription.join(`\n`);

                currentItem.position = {
                  path: file,
                  line: lineNumber
                };

                const objectName = getObjectName(parts[1], parts);
                let prefix = ``;

                parts.find(element => {
                  if (element.toUpperCase().includes(`PREFIX`)) {
                    prefix = element.trim().substring(7, element.indexOf(`)`))
                    return true;
                  }
                });	
          
                const recordFormats = await this.fetchTable(objectName, parts.length.toString(), parts.includes(`ALIAS`));

                if (recordFormats.length > 0) {
                  const qualified = parts.includes(`QUALIFIED`);

                  // Got to fix the positions for the defintions to be the declare.
                  recordFormats.forEach(recordFormat => {
                    recordFormat.keywords = [parts[1]];
                    if (qualified) recordFormat.keywords.push(`QUALIFIED`);

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
                currentItem.keywords = parts.slice(2);
                currentItem.description = currentDescription.join(`\n`);

                currentItem.position = {
                  path: file,
                  line: statementStartingLine
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
                currentItem.keywords = parts.slice(2);
                currentItem.description = currentDescription.join(`\n`);
                currentItem.tags = currentTags;

                currentItem.position = {
                  path: file,
                  line: statementStartingLine
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
                currentItem.keywords = parts.slice(2);
                currentItem.description = currentDescription.join(`\n`);

                currentItem.position = {
                  path: file,
                  line: statementStartingLine
                };

                currentItem.range = {
                  start: statementStartingLine,
                  end: statementStartingLine
                };

                currentItem.readParms = true;

                currentGroup = `constants`;

                currentDescription = [];
              }
            }
            break;

          case `END-ENUM`:
            if (currentItem && currentItem.type === `constant`) {
              currentItem.range.end = statementStartingLine;
              
              scope.constants.push(currentItem);

              resetDefinition = true;
            }
            break;

          case `DCL-DS`:
            if (currentItem === undefined) {
              if (parts.length > 1) {
                currentItem = new Declaration(`struct`);
                currentItem.name = partsLower[1];
                currentItem.keywords = parts.slice(2);
                currentItem.description = currentDescription.join(`\n`);
                currentItem.tags = currentTags;

                currentItem.position = {
                  path: file,
                  line: statementStartingLine
                };

                currentItem.range = {
                  start: statementStartingLine,
                  end: statementStartingLine
                };

                currentGroup = `structs`;

                // Expand the LIKEDS value if there is one.
                await expandDs(file, currentItem);

                // Does the keywords include a keyword that makes end-ds useless?
                if (currentItem.keywords.some(keyword => oneLineTriggers[`DCL-DS`].some(trigger => keyword.startsWith(trigger)))) {
                  currentItem.range.end = statementStartingLine;
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
              currentDs.range.end = statementStartingLine;
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
                  currentItem.keywords = parts.slice(2);
                  currentItem.description = currentDescription.join(`\n`);
                  currentItem.tags = currentTags;

                  currentItem.position = {
                    path: file,
                    line: statementStartingLine
                  };

                  currentItem.readParms = true;

                  currentItem.range = {
                    start: statementStartingLine,
                    end: statementStartingLine
                  };

                  // Does the keywords include a keyword that makes end-ds useless?
                  if (currentItem.keywords.some(keyword => oneLineTriggers[`DCL-PR`].some(trigger => keyword.startsWith(trigger)))) {
                    currentItem.range.end = statementStartingLine;
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
              currentItem.range.end = statementStartingLine;

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
              currentItem.keywords = parts.slice(2);
              currentItem.description = currentDescription.join(`\n`);
              currentItem.tags = currentTags;

              currentItem.position = {
                path: file,
                line: statementStartingLine
              };

              currentItem.readParms = false;

              currentItem.range = {
                start: statementStartingLine,
                end: statementStartingLine
              };

              currentItem.scope = new Cache();

              scope.procedures.push(currentItem);
              resetDefinition = true;

              scopes.push(currentItem.scope);
            }
            break;

          case `DCL-PI`:
            //Procedures can only exist in the global scope.
            if (currentProcName) {
              if (parts.length > 0) {
                currentGroup = `procedures`;
                currentItem = scopes[0].procedures.find(proc => proc.name === currentProcName);

                const endInline = parts.findIndex(part => part === `END-PI`);

                if (currentItem) {

                  // Indicates that the PI starts and ends on the same line
                  if (endInline >= 0) { 
                    parts.splice(endInline, 1);
                    currentItem.readParms = false;
                    resetDefinition = true;
                  }

                  currentItem.keywords.push(...parts.slice(2));
                  currentItem.readParms = true;

                  currentDescription = [];
                }
              }
            }
            break;

          case `END-PI`:
            //Procedures can only exist in the global scope.
            currentItem = scopes[0].procedures.find(proc => proc.name === currentProcName);

            if (currentItem && currentItem.type === `procedure`) {
              currentItem.readParms = false;
              resetDefinition = true;
            }
            break;

          case `END-PROC`:
            //Procedures can only exist in the global scope.
            if (scopes.length > 1) {
              currentItem = scopes[0].procedures.find(proc => proc.name === currentProcName);

              if (currentItem && currentItem.type === `procedure`) {
                scopes.pop();
                currentItem.range.end = statementStartingLine;
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
		            currentItem.keywords = [`Subroutine`];

                currentItem.position = {
                  path: file,
                  line: statementStartingLine
                };

                currentItem.range = {
                  start: statementStartingLine,
                  end: statementStartingLine
                };

                currentDescription = [];
              }
            }
            break;
    
          case `ENDSR`:
            if (currentItem && currentItem.type === `subroutine`) {
              currentItem.range.end = statementStartingLine;
              scope.subroutines.push(currentItem);
              resetDefinition = true;
            }
            break;

          case `EXEC`:
            if (parts.length > 2 && !parts.includes(`FETCH`)) {
              // insert into XX.XX
              // delete from xx.xx
              // update xx.xx set
              // select * into :x from xx.xx
              // call xx.xx()
              const preFileWords = [`INTO`, `FROM`, `UPDATE`, `CALL`, `JOIN`];
              const ignoredWords = [`FINAL`, `SET`];

              const cleanupObjectRef = (content = ``) => {
                const result = {
                  schema: undefined,
                  name: content
                }

                const schemaSplit = Math.max(result.name.indexOf(`.`), result.name.indexOf(`/`));
                if (schemaSplit >= 0) {
                  result.schema = result.name.substring(0, schemaSplit);
                  result.name = result.name.substring(schemaSplit+1);
                }

                // For procedures or functions?
                const openBracket = result.name.indexOf(`(`);
                if (openBracket >= 0) {
                  result.name = result.name.substring(0, openBracket);
                }

                // End bracket for sub-statements
                if (result.name.endsWith(`)`) || result.name.endsWith(`,`)) {
                  result.name = result.name.substring(0, result.name.length - 1);
                }

                return result;
              }

              let isContinued = false;

              /** @type {string[]} */
              let ignoreCtes = [];

              if (parts.includes(`WITH`)) {
                for (let index = 4; index < parts.length; index++) {
                  if (parts[index].startsWith(`AS`) && (parts[index].endsWith(`(`) || parts[index+1] === `(`)) {
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
                    const possibleFileName = partsLower[index+1];
                    isContinued = (possibleFileName.endsWith(`,`) || (parts[index+2] === `,`));

                    const qualifiedObjectPath = cleanupObjectRef(possibleFileName);

                    if (qualifiedObjectPath.name && !qualifiedObjectPath.name.startsWith(`:`) && !ignoreCtes.includes(qualifiedObjectPath.name.toUpperCase())) {
                      const currentSqlItem = new Declaration(`file`);
                      currentSqlItem.name = qualifiedObjectPath.name;

                      if (currentSqlItem.name)

                      currentSqlItem.keywords = [];
                      currentSqlItem.description = qualifiedObjectPath.schema || ``;
      
                      currentSqlItem.position = {
                        path: file,
                        line: statementStartingLine
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
                  }

                  currentSub = new Declaration(`subitem`);
                  currentSub.name = (parts[0] === `*N` ? `parm${currentItem.subItems.length+1}` : partsLower[0]) ;
                  currentSub.keywords = parts.slice(1);

                  currentSub.position = {
                    path: file,
                    line: statementStartingLine
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
                  await expandDs(file, currentSub);
                  currentSub.keyword = Parser.expandKeywords(currentSub.keywords);

                  currentItem.subItems.push(currentSub);
                  currentSub = undefined;

                  if (currentItem.type === `struct`) {
                    resetDefinition = true;
                  }
                }
              }
            }
            break;
          }

        } else {
          // Fixed format!
          if (directIfScope > 0) {
            // Ignore lines inside the IF scope.
            continue;
          }

          switch (spec) {
          case `H`:
            keywords.push(line.substring(6));
            break;

          case `F`:
            const fSpec = parseFLine(line);
            potentialName = getObjectName(fSpec.name, fSpec.keywords);

            if (fSpec.name) {
              currentItem = new Declaration(`file`);
              currentItem.name = potentialName;
              currentItem.keywords = fSpec.keywords;

              currentItem.position = {
                path: file,
                line: lineNumber
              };
			  
			        let prefix = ``;

              fSpec.keywords.find(element => {
                if (element.toUpperCase().includes(`PREFIX`)) {
                  prefix = element.substring(7, element.indexOf(`)`))
                  return true;
                }
              });

              const recordFormats = await this.fetchTable(potentialName, line.length.toString(), fSpec.keywords.includes(`ALIAS`));

              if (recordFormats.length > 0) {
                const qualified = fSpec.keywords.includes(`QUALIFIED`);

                // Got to fix the positions for the defintions to be the declare.
                recordFormats.forEach(recordFormat => {
                  recordFormat.keywords = [potentialName];
                  if (qualified) recordFormat.keywords.push(`QUALIFIED`);

                  recordFormat.position = currentItem.position;

                  recordFormat.subItems.forEach(subItem => {
					          // We put the prefix here because in 'fetchTable' we use cached version. So if the user change the prefix, it will not refresh the variable name
                    if(prefix) {
                      subItem.name = prefix.toUpperCase() + subItem.name;
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
                currentItem.keywords = [
                  ...(currentItem.keywords ? currentItem.keywords : []),
                  ...fSpec.keywords
                ]
              }
            }
            
            resetDefinition = true;
            break;

          case `C`:
            const cSpec = parseCLine(line);

            potentialName = cSpec.factor1;

            switch (cSpec.opcode) {
            case `BEGSR`:
              if (!scope.subroutines.find(sub => sub.name && sub.name.toUpperCase() === potentialName)) {
                currentItem = new Declaration(`subroutine`);
                currentItem.name = potentialName;
                currentItem.keywords = [`Subroutine`];
  
                currentItem.position = {
                  path: file,
                  line: lineNumber
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
              callItem.name = (cSpec.factor2.startsWith(`'`) && cSpec.factor2.endsWith(`'`) ? cSpec.factor2.substring(1, cSpec.factor2.length-1) : cSpec.factor2);
              callItem.keywords = [`EXTPGM`];
              callItem.description = currentDescription.join(`\n`);
              callItem.tags = currentTags;

              callItem.position = {
                path: file,
                line: lineNumber
              };

              callItem.range = {
                start: lineNumber,
                end: lineNumber
              };

              callItem.keyword = Parser.expandKeywords(callItem.keywords);

              scope.procedures.push(callItem);
              break;
            }

            break;
          case `P`:
            const pSpec = parsePLine(line);

            if (pSpec.potentialName === ``) continue;

            if (pSpec.potentialName.endsWith(`...`)) {
              potentialName = pSpec.potentialName.substring(0, pSpec.potentialName.length - 3);
              potentialNameUsed = true;
            } else {
              if (pSpec.start) {
                potentialName = pSpec.name.length > 0 ? pSpec.name : potentialName;

                if (potentialName) {
                  //We can overwrite it.. it might have been a PR before.
                  const existingProc = scope.procedures.findIndex(proc => proc.name && proc.name.toUpperCase() === potentialName.toUpperCase());

                  // We found the PR... so we can overwrite it
                  if (existingProc >= 0) scope.procedures.splice(existingProc, 1);

                  currentItem = new Declaration(`procedure`);

                  currentProcName = potentialName;
                  currentItem.name = currentProcName;
                  currentItem.keywords = pSpec.keywords;

                  currentItem.position = {
                    path: file,
                    line: lineNumber - (potentialNameUsed ? 1 : 0) // Account that name is on line before
                  };

                  currentItem.range = {
                    start: currentItem.position.line,
                    end: currentItem.position.line
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
            const dSpec = parseDLine(line);

            if (dSpec.potentialName === ``) continue;

            if (dSpec.potentialName.endsWith(`...`)) {
              potentialName = dSpec.potentialName.substring(0, dSpec.potentialName.length - 3);
              potentialNameUsed = true;
              continue;
            } else {
              potentialName = dSpec.name.length > 0 ? dSpec.name : potentialName ? potentialName : ``;

              switch (dSpec.field) {
              case `C`:
                currentItem = new Declaration(`constant`);
                currentItem.name = potentialName || `*N`;
                currentItem.keywords = [...dSpec.keywords];
                  
                // TODO: line number might be different with ...?
                currentItem.position = {
                  path: file,
                  line: lineNumber - (potentialNameUsed ? 1 : 0) // Account that name is on line before
                };
    
                scope.constants.push(currentItem);
                resetDefinition = true;
                break;
              case `S`:
                currentItem = new Declaration(`variable`);
                currentItem.name = potentialName || `*N`;
                currentItem.keywords = [getPrettyType(dSpec), ...dSpec.keywords];

                // TODO: line number might be different with ...?
                currentItem.position = {
                  path: file,
                  line: lineNumber - (potentialNameUsed ? 1 : 0) // Account that name is on line before
                };

                scope.variables.push(currentItem);
                resetDefinition = true;
                break;

              case `DS`:
                currentItem = new Declaration(`struct`);
                currentItem.name = potentialName || `*N`;
                currentItem.keywords = dSpec.keywords;

                currentItem.position = {
                  path: file,
                  line: lineNumber - (potentialNameUsed ? 1 : 0) // Account that name is on line before
                };

                currentItem.range = {
                  start: currentItem.position.line,
                  end: currentItem.position.line
                };

                expandDs(file, currentItem);

                currentGroup = `structs`;
                scope.structs.push(currentItem);
                resetDefinition = true;
                break;

              case `PR`:
                // Only add a PR if it's not been defined
                if (!scope.procedures.find(proc => proc.name && proc.name.toUpperCase() === potentialName.toUpperCase())) {
                  currentItem = new Declaration(`procedure`);
                  currentItem.name = potentialName || `*N`;
                  currentItem.keywords = [getPrettyType(dSpec), ...dSpec.keywords];
  
                  currentItem.position = {
                    path: file,
                    line: lineNumber - (potentialNameUsed ? 1 : 0) // Account that name is on line before
                  };

                  currentItem.range = {
                    start: currentItem.position.line,
                    end: currentItem.position.line
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
                    currentItem.keywords.push(getPrettyType(dSpec), ...dSpec.keywords);
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
                  if (potentialName === `` && (dSpec.type || dSpec.len))
                    potentialName = (potentialName === `` ? `parm${currentItem.subItems.length+1}` : potentialName);

                  if (potentialName) {
                    currentSub = new Declaration(`subitem`);
                    currentSub.name = potentialName;
                    currentSub.keywords = [getPrettyType(dSpec), ...dSpec.keywords];

                    currentSub.position = {
                      path: file,
                      line: lineNumber
                    };

                    // If the parameter has likeds, add the subitems to make it a struct.
                    await expandDs(file, currentSub);
                    currentSub.keyword = Parser.expandKeywords(currentSub.keywords);

                    currentItem.subItems.push(currentSub);
                    currentSub = undefined;

                    resetDefinition = true;
                  } else {
                    if (currentItem) {
                      if (currentItem.subItems.length > 0)
                        currentItem.subItems[currentItem.subItems.length - 1].keywords.push(getPrettyType(dSpec), ...dSpec.keywords);
                      else
                        currentItem.keywords.push(...dSpec.keywords);
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

        if (resetDefinition) {
          // Parse keywords to make it easier to use later
          if (currentItem) {
            currentItem.keyword = Parser.expandKeywords(currentItem.keywords);
          }

          potentialName = undefined;
          potentialNameUsed = false;
          
          currentItem = undefined;
          currentTitle = undefined;
          currentDescription = [];
          currentTags = [];
          resetDefinition = false;
        }
      }
    }

    if (scopes.length > 0) {
      scopes[0].keyword = Parser.expandKeywords(keywords);
    }

    scopes[0].fixProcedures();    

    const parsedData = scopes[0];

    this.parsedCache[workingUri] = parsedData;

    return parsedData;
  }

  /**
   * @param {string[]} parts 
   */
  static expandKeywords(parts) {
    /** @type {import("./parserTypes").Keywords} */
    const keyvalues = {};

    if (parts.length > 0) {
      const keywordParts = createBlocks(tokenise(parts.join(` `)));

      for (let i = 0; i < keywordParts.length; i++) {
        if (keywordParts[i].value) {
          if (keywordParts[i+1] && keywordParts[i+1].type === `block`) {
            keyvalues[keywordParts[i].value.toUpperCase()] = keywordParts[i+1].block.map(part => part.value).join(``);
            i++; // Skip one for the block.
          } else {
            keyvalues[keywordParts[i].value.toUpperCase()] = true;
          }
        }
      }
    }

    return keyvalues;
  }
}
