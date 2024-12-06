/* eslint-disable no-case-declarations */

import { createBlocks, tokenise } from "./tokens";

import Cache from "./models/cache";
import Declaration from "./models/declaration";

import oneLineTriggers from "./models/oneLineTriggers";
import { parseFLine, parseCLine, parsePLine, parseDLine, getPrettyType, prettyTypeFromToken } from "./models/fixed";

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
 * @returns {Promise<{found: boolean, uri?: string, content?: string}>}
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
   * @param {string} [baseContent] 
   * @param {{withIncludes?: boolean, ignoreCache?: boolean, collectReferences?: boolean}} options
   * @returns {Promise<Cache|undefined>}
   */
  async getDocs(workingUri, baseContent, options = {withIncludes: true, collectReferences: false}) {
    const existingCache = this.getParsedCache(workingUri);
    if (options.ignoreCache !== true && existingCache) {
      return existingCache;
    }

    if (!baseContent) return null;

    /** @type {Cache[]} */
    let scopes = [];

    /** @type {Declaration[]} Free format struct scopes. Used for free-format only */
    let dsScopes = [];

    /** @type {string[]} */
    let globalKeyword = [];

    // Global scope bits
    scopes.push(new Cache());

    /**
     * Gets value of EXTFILE if it exists.
     * @param {string} defaultName 
     * @param {import("./parserTypes").Keywords} keywords 
     * @returns {string}
     */
    const getObjectName = (defaultName, keywords) => {
      let objectName = defaultName;
      const extObjKeywords = [`EXTFILE`];
      const extObjKeywordsDesc = [`EXTDESC`];
            
      // Check for external object
      extObjKeywords.forEach(keyword => {
        const keywordValue = keywords[keyword];
        if (keywordValue && typeof keywordValue === `string`) {
          objectName = keywordValue.substring(keyword.length+1, keywordValue.length - 1).toUpperCase();

          if (objectName.startsWith(`'`) && objectName.endsWith(`'`)) {
            objectName = objectName.substring(1, objectName.length - 1);
          }
        }
      });

      if(objectName === `*EXTDESC`){
        // Check for external object
        extObjKeywordsDesc.forEach(keyword => {
          const keywordValue = keywords[keyword];
          if (keywordValue && typeof keywordValue === `string`) {
            objectName = keywordValue.substring(keyword.length+1, keywordValue.length - 1).toUpperCase();

            if (objectName.startsWith(`'`) && objectName.endsWith(`'`)) {
              objectName = objectName.substring(1, objectName.length - 1);
            }
          }
        });
      }

      return objectName;
    };

    /** @type {string} */
    let potentialName;
    let potentialNameUsed = false;

    /** @type {"structs"|"procedures"|"constants"} */
    let currentGroup;

    /** @type {string[]} */
    let definedMacros = [];

    /**
     * 
     * @param {import("./types").Token[]} statement
     * @param {Declaration} [currentProcedure]
     * @param {Declaration} [currentDef]
     */
    const collectReferences = (statement, currentProcedure, currentDef) => {
      for (let i = 0; i < statement.length; i++) {
        const part = statement[i];
        if (part === undefined) continue;

        if (![`special`, `word`].includes(part.type)) continue;
        if (statement[i - 1] && statement[i - 1].type === `dot`) break;
        const lookupName = (part.type === `special` ? part.value.substring(1) : part.value).toUpperCase();

        /**
         * @type {Declaration|undefined}
         */
        let defRef;

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

        if (defRef) {
          if (!defRef.references.some(ref => ref.offset.position === part.range.start && ref.offset.end === part.range.end)) {
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
                    if (!subItemDef.references.some(ref => ref.offset.position === subItemPart.range.start && ref.offset.end === subItemPart.range.end)) {
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
    }

    //Now the real work
    /**
     * @param {string} file 
     * @param {string} allContent 
     */
    const parseContent = async (file, allContent) => {
      let eol = allContent.includes(`\r\n`) ? `\r\n` : `\n`;
      let lines = allContent.split(eol);

      /** @type {{[procedure: string]: import("./types").Token[][]}} */
      let postProcessingStatements = {'GLOBAL': []};

      const addPostProcessingStatements = (procedure, statement) => {
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
            collectReferences(statement, currentProcedure);
          }
        }
      }

      if (lines.length === 0) return;

      let currentTitle = undefined, currentDescription = [];
      /** @type {{tag: string, content: string}[]} */
      let currentTags = [];

      /** @type {Declaration} */
      let currentItem;
      /** @type {Declaration} */
      let currentSub;

      /** @type {string|undefined} */
      let currentProcName;

      let resetDefinition = false; //Set to true when you're done defining a new item
      let docs = false; // If section is for ILEDocs
      let lineNumber = -1;
      let lineIndex = 0;

      /**
       * @type {string[]}
       */
      let parts;

      /**
       * @type {string[]}
       */
      let partsLower;

      /**
       * @type {string[]}
       */
      let pieces;

      let isFullyFree = lines[0].toUpperCase().startsWith(`**FREE`);
      let lineIsFree = false;

      /** @type {string|undefined} */
      let currentStatement;
      /** @type {number|undefined} */
      let statementStartingLine;

      /** @type {{condition: boolean}[]} */
      let directIfScope = [];

      let lineCanRun = () => {
        return directIfScope.length === 0 || directIfScope.every(scope => scope.condition);
      }

      /**
       * Expands LIKEDS, LIKEREC and EXTNAME.
       * @param {string} file
       * @param {Declaration} ds 
       */
      const expandDs = async (file, ds) => {
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

            if ([`EXTNAME`].includes(tag)) {
              // Fetch from external definitions
              const keywordLength = Object.keys(ds.keyword);
              const recordFormats = await this.fetchTable(keywordValue, keywordLength.length.toString(), ds.keyword[`ALIAS`] !== undefined);

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
              if (!ds.keyword[`QUALIFIED`])
                ds.keyword[`QUALIFIED`];

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

      for (let li = 0; li < lines.length; li++) {
        if (li >= 1) {
          lineIndex += lines[li-1].length + (eol === `\r\n` ? 2 : 1);
          // assert.strictEqual(allContent.substring(lineIndex, lineIndex + lines[li].length), lines[li]);
        }
        
        const scope = scopes[scopes.length - 1];

        let line = lines[li];
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

          if ([spec, comment].includes(`*`)) {
            continue;
          }

          if (comment === `/`) {
            // Directives can be parsed by the free format parser
            line = ``.padEnd(6) + line.substring(6);
            lineIsFree = true;
          } else {
            if (spec === ` `) {
              //Clear out stupid comments
              line = ``.padEnd(7) + line.substring(7);
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

        /**
         * @type {import("./types").Token[]}
         */
        let tokens = [];
        pieces = [];
        parts = [];
        
        if (isFullyFree || lineIsFree) {
          // Free format!
          if (line.trim() === ``) continue;

          pieces = line.split(`;`);
          tokens = tokenise(pieces[0], lineNumber, lineIndex);
          partsLower = tokens.filter(piece => piece.value).map(piece => piece.value);
          parts = partsLower.map(piece => piece.toUpperCase());

          line = line.trim();

          const lineIsComment = line.startsWith(`//`);

          if (!lineIsComment) {
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
                    if (include.found) {
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
                continue;
              case `/IF`:
                // Not conditions can run
                let condition = false;
                let hasNot = (parts[1] === `NOT`);
                let expr = tokens.slice(hasNot ? 2 : 1);
                let keywords = Parser.expandKeywords(Parser.getTokens(expr));

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

          if (pieces.length > 1 && pieces[1].includes(`//`)) line = pieces[0] + `;`;

          if (!currentStatement) statementStartingLine = lineNumber;

          if (!lineIsComment) {
            if (line.endsWith(`;`)) {
              if (currentStatement) {
                // This means the line is just part of the end of the last statement as well.
                line = currentStatement + line;
                currentStatement = undefined;

                pieces = line.split(`;`);
                tokens = tokenise(pieces[0], lineNumber);
                partsLower = tokens.filter(piece => piece.value).map(piece => piece.value);
                parts = partsLower.map(piece => piece.toUpperCase());
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
                  path: file,
                  line: lineNumber
                };

                const objectName = getObjectName(parts[1], currentItem.keyword);
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
                currentItem.keyword = Parser.expandKeywords(tokens.slice(2));
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
                currentItem.keyword = Parser.expandKeywords(tokens.slice(2));
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
                currentItem.keyword = Parser.expandKeywords(tokens.slice(2));
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
                currentItem.keyword = Parser.expandKeywords(tokens.slice(2));
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
                if (Object.keys(currentItem.keyword).some(keyword => oneLineTriggers[`DCL-DS`].some(trigger => keyword.startsWith(trigger)))) {
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
                  currentItem.keyword = Parser.expandKeywords(tokens.slice(2));
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
                  if (Object.keys(currentItem.keyword).some(keyword => oneLineTriggers[`DCL-PR`].some(trigger => keyword.startsWith(trigger)))) {
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
              currentItem.keyword = Parser.expandKeywords(tokens.slice(2));
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

                const endInline = tokens.findIndex(part => part.value.toUpperCase() === `END-PI`);

                if (currentItem) {

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
		            currentItem.keyword = {'Subroutine': true};

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
                  length: 1
                }

                const schemaSplit = [`.`, `/`].includes(parts[nameIndex + 1]);
                if (schemaSplit) {
                  result.schema = partsLower[index]
                  result.name = partsLower[index + 2]
                  result.length = 3;
                  nameIndex += 2;
                }

                return result;
              }

              let isContinued = false;

              /** @type {string[]} */
              let ignoreCtes = [];

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
                  currentSub.name = (parts[0] === `*N` ? `parm${currentItem.subItems.length+1}` : partsLower[0]);
                  currentSub.keyword = Parser.expandKeywords(tokens.slice(1));

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
          if (!lineCanRun()) {
            // Ignore lines inside the IF scope.
            continue;
          }

          switch (spec) {
          case `H`:
            globalKeyword.push(line.substring(6));
            break;

          case `F`:
            const fSpec = parseFLine(line);
            potentialName = getObjectName(fSpec.name, fSpec.keywords);

            if (fSpec.name) {
              currentItem = new Declaration(`file`);
              currentItem.name = potentialName;
              currentItem.keyword = fSpec.keywords;

              currentItem.position = {
                path: file,
                line: lineNumber
              };
			  
			        let prefix = ``;

              const prefixKeyword = fSpec.keywords[`PREFIX`];
              if (prefixKeyword && typeof prefixKeyword === `string`) {
                prefix = prefixKeyword.toUpperCase();
              }

              const recordFormats = await this.fetchTable(potentialName, line.length.toString(), fSpec.keywords[`ALIAS`] !== undefined);

              if (recordFormats.length > 0) {
                const qualified = fSpec.keywords[`QUALIFIED`] === true;

                // Got to fix the positions for the defintions to be the declare.
                recordFormats.forEach(recordFormat => {
                  recordFormat.keyword = {[potentialName]: true};
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

            tokens = [cSpec.ind1, cSpec.ind2, cSpec.ind3];

            if (cSpec.opcode && [`EVAL`, `EVALR`].includes(cSpec.opcode.value) && cSpec.factor1 === undefined) {
              if (cSpec.extended) {
                tokens.push(...tokenise(cSpec.extended.value, lineNumber, lineIndex));
              }
            } else {
              tokens.push(cSpec.factor1, cSpec.factor2, cSpec.result);
            }

            potentialName = cSpec.factor1 ? cSpec.factor1.value : ``;

            switch (cSpec.opcode.value) {
            case `BEGSR`:
              if (!scope.subroutines.find(sub => sub.name && sub.name.toUpperCase() === potentialName)) {
                currentItem = new Declaration(`subroutine`);
                currentItem.name = potentialName;
                currentItem.keyword = {'Subroutine': true};
  
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
              if (cSpec.factor2) {
                const f2Value = cSpec.factor2.value;
                callItem.name = (f2Value.startsWith(`'`) && f2Value.endsWith(`'`) ? f2Value.substring(1, f2Value.length-1) : f2Value);
                callItem.keyword = {'EXTPGM': true}
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

                scope.procedures.push(callItem);
              }
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
                  currentItem.keyword = pSpec.keywords;

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
            const dSpec = parseDLine(lineNumber, lineIndex, line);

            if (dSpec.potentialName && dSpec.potentialName) {
              potentialName = dSpec.potentialName.value;
              potentialNameUsed = true;
              tokens = [dSpec.potentialName];
              continue;
            } else {
              potentialName = dSpec.name && dSpec.name.value.length > 0 ? dSpec.name.value : (potentialName ? potentialName : ``);
              tokens = [dSpec.field, ...dSpec.keywordsRaw, dSpec.name]

              switch (dSpec.field && dSpec.field.value) {
              case `C`:
                currentItem = new Declaration(`constant`);
                currentItem.name = potentialName || `*N`;
                currentItem.keyword = dSpec.keyword;
                  
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
                currentItem.keyword = {
                  ...dSpec.keywords,
                  ...prettyTypeFromToken(dSpec),
                }

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
                currentItem.keyword = dSpec.keywords;

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
                  currentItem.keyword = {
                    ...prettyTypeFromToken(dSpec),
                    ...dSpec.keywords
                  }
  
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
                  if (potentialName === `` && (dSpec.type || dSpec.len))
                    potentialName = (potentialName === `` ? `parm${currentItem.subItems.length+1}` : potentialName);

                  if (potentialName) {
                    currentSub = new Declaration(`subitem`);
                    currentSub.name = potentialName;
                    currentSub.keyword = {
                      ...prettyTypeFromToken(dSpec),
                      ...dSpec.keywords
                    }

                    currentSub.position = {
                      path: file,
                      line: lineNumber
                    };

                    // If the parameter has likeds, add the subitems to make it a struct.
                    await expandDs(file, currentSub);

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
          collectReferences(tokens, currentProc, currentItem);
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

  /**
   * @param {import("./types").Token[]|string|string[]} content 
   * @returns {import("./types").Token[]}
   */
  static getTokens(content) {
    if (Array.isArray(content) && typeof content[0] === `string`) {
      return tokenise(content.join(` `));
    } else 
      if (typeof content === `string`) {
        return tokenise(content);
      } else {
        // @ts-ignore
        return content;
      }
  

  }

  /**
   * @param {import("./types").Token[]} tokens 
   */
  static expandKeywords(tokens) {
    /** @type {import("./parserTypes").Keywords} */
    const keyvalues = {};

    if (tokens.length > 0) {
      const keywordParts = createBlocks(tokens.slice(0));

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