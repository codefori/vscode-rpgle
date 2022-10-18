
const path = require(`path`);
const vscode = require(`vscode`);

const Generic = require(`./generic`);
const Statement = require(`./statement`);

const Cache = require(`./models/cache`);
const Declaration = require(`./models/declaration`);

const oneLineTriggers = require(`./models/oneLineTriggers`);
const Fixed = require(`./models/fixed`);

const HALF_HOUR = (30 * 60 * 1000);

/**
 * @callback tablePromise
 * @param  {string} name Table name
 * @param  {boolean} [aliases] Table name
 * @returns {Promise<Declaration[]>}
 */
module.exports = class Parser {
  constructor() {
    /** @type {{[path: string]: Cache}} */
    this.parsedCache = {};

    /** @type {{[name: string]: {fetched: number, fetching?: boolean, recordFormats: Declaration[]}}} */
    this.tables = {};

    /** @type {tablePromise} */
    this.tableFetch = undefined;

    /** @type {{[path: string]: vscode.Uri}} */
    this.localUris = {};
  }

  /**
   * @param {tablePromise} promise 
   */
  setTableFetch(promise) {
    this.tableFetch = promise;
  }

  /**
   * @param {string} name 
   * @param {string} keyVersion
   * @param {boolean} [aliases]
   * @returns {Promise<Declaration[]>}
   */
  async fetchTable(name, keyVersion = ``, aliases) {
    if (name.trim() === ``) return [];
    if (!this.tableFetch) return [];
    const table = name.toUpperCase();
    const existingVersion = table + keyVersion;
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
    }

    /** @type {Declaration[]} */
    let newDefs;

    try {
      newDefs = await this.tableFetch(table, aliases);

      this.tables[existingVersion] = {
        fetched: now,
        recordFormats: newDefs
      }
    } catch (e) {
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
   * Used to tell getContent to search for the local file again
   * Useful if the file didn't exist previously, but now does
   * @param {string} basePath 
   */
  clearUris(basePath) {
    const basename = path.parse(basePath).name.toUpperCase();

    const localPaths = Object.keys(this.localUris).filter(p => p.toUpperCase().includes(basename));

    localPaths.forEach(p => {
      this.localUris[p] = undefined;
    });
  }

  /**
   * @param {vscode.Uri} workingUri Path being worked with
   * @param {string} getPath IFS or member path to fetch
   * @returns {Promise<{lines: string[], found: boolean, uri: vscode.Uri, path: string, type: string}>}
   */
  async getContent(workingUri, getPath) {
    //const hrstart = process.hrtime();

    let content;
    let lines = [];

    let uri;
    let found = true;
    let attemptedPath;
  
    const type = workingUri.scheme;
    const {finishedPath} = Generic.getPathInfo(workingUri, getPath);
  
    try {
      let doc;
      let eol;
      switch (type) {
      case `member`:
        attemptedPath = finishedPath;

        // We have to be agnostic for the type. First we check if we opened a copybook before.
        const openDoc = vscode.workspace.textDocuments.find(doc => 
          doc.uri.path.startsWith(finishedPath) && !doc.uri.path.endsWith(`.MBR`)
        );

        if (openDoc) {
          // If we've opened it before, we can just use the content
          doc = openDoc;
        } else {
          // Otherwise, we go and fetch the correct content
          doc = await vscode.workspace.openTextDocument(vscode.Uri.from({
            scheme: type,
            path: finishedPath + `.RPGLE`
          }));
        }

        eol = doc.eol === vscode.EndOfLine.CRLF ? `\r\n` : `\n`;

        uri = doc.uri;
        lines = doc.getText().split(eol);
        break;
  
      case `streamfile`:
        attemptedPath = finishedPath;

        doc = await vscode.workspace.openTextDocument(vscode.Uri.from({
          scheme: type,
          path: finishedPath
        }));
        eol = doc.eol === vscode.EndOfLine.CRLF ? `\r\n` : `\n`;

        uri = doc.uri;
        lines = doc.getText().split(eol);
        break;

      case `file`:
      case `vscode-vfs`:
        /** @type {vscode.Uri} */
        let possibleFile;

        if (getPath.startsWith(`/`)) {
          possibleFile = vscode.Uri.from({
            scheme: type,
            path: getPath
          });
          
        } else {
          if (getPath.startsWith(`'`)) getPath = getPath.substring(1);
          if (getPath.endsWith(`'`)) getPath = getPath.substring(0, getPath.length - 1);
          if (getPath.startsWith(`./`)) getPath = getPath.substring(2);
  
          // Member styled include
          if (!getPath.includes(`/`)) {
            let memberParts = getPath.split(`,`);
  
            if (memberParts.length === 1) {
              memberParts = [`qrpgleref`, memberParts[0]];
            }
            getPath = memberParts.join(path.sep) + `*`
          }
  
          attemptedPath = getPath;

          if (this.localUris[getPath]) possibleFile = this.localUris[getPath];
          else {
            const fileSearch = await vscode.workspace.findFiles(`**/${getPath}`, null, 1);
            if (fileSearch.length > 0) { 
              possibleFile = fileSearch[0];
            }
          }
        }


        if (possibleFile) {
          doc = await vscode.workspace.openTextDocument(possibleFile);
          this.localUris[getPath] = possibleFile;

          eol = doc.eol === vscode.EndOfLine.CRLF ? `\r\n` : `\n`;
          uri = doc.uri;
          lines = doc.getText().split(eol);
        } else {
          found = false;
        }
        break;

      default:
        found = false;
        break;
      }
    } catch (e) {
      found = false;
    }

    //const hrend = process.hrtime(hrstart);
    //console.info(`getContent() took ${hrend[0]}s and ${hrend[1] / 1000000}ms: ${getPath} (${lines.length})`);
  
    return {
      lines,
      path: attemptedPath,
      uri,
      found,
      type
    };
  }

  /**
   * @param {vscode.Uri} workingUri
   * @param {string} [content] 
   * @param {{withIncludes?: boolean, ignoreCache?: boolean}} options
   * @returns {Promise<Cache|null>}
   */
  async getDocs(workingUri, content, options = {withIncludes: true}) {
    const existingCache = this.getParsedCache(workingUri.path);
    if (options.ignoreCache !== true && existingCache) {
      return existingCache;
    };

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
    let currentSub
    let currentProcName;

    let resetDefinition = false; //Set to true when you're done defining a new item
    let docs = false; // If section is for ILEDocs
    let lineNumber, parts, partsLower, pieces;

    /** @type {Cache[]} */
    let scopes = [];

    /** @type {Declaration[]} Free format struct scopes. Used for free-format only */
    let dsScopes = [];

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
    }

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
            if (!ds.keywords.includes(`QUALIFIED`))
              ds.keywords.push(`QUALIFIED`);

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
            // Fetch from local definitions
            for (let i = scopes.length - 1; i >= 0; i--) {
              const valuePointer = scopes[i].structs.find(struct => struct.name.toUpperCase() === keywordValue);
              if (valuePointer) {
                ds.subItems = valuePointer.subItems;
    
                // We need to add qualified as it is qualified by default.
                if (!ds.keywords.includes(`QUALIFIED`))
                  ds.keywords.push(`QUALIFIED`);
                return;
              }
            }
          }
        }
      };
    }

    if (options.withIncludes) {
    //First loop is for copy/include statements
      for (let i = baseLines.length - 1; i >= 0; i--) {
        let line = baseLines[i].trim(); //Paths are case insensitive so it's okay
        if (line === ``) continue;

        pieces = line.split(` `).filter(piece => piece !== ``);

        this.brokenPaths = [];

        const copyIndex = pieces.findIndex(piece => {
          if (piece.includes(`*`)) return false; // Comment
          const pieceUpper = piece.toUpperCase();
          return (pieceUpper.includes(`/COPY`) || pieceUpper.includes(`/INCLUDE`));
        });

        if (copyIndex >= 0 && pieces[copyIndex+1]) {
          const include = (await this.getContent(workingUri, pieces[copyIndex+1]));
          if (include.found) {
            files[include.uri.fsPath] = include.lines;
          }
        }
      }
    }

    files[workingUri.path] = baseLines;

    let potentialName;
    let potentialNameUsed = false;

    /** @type {"structs"|"procedures"} */
    let currentGroup;
    let isFullyFree = false;

    //Now the real work
    for (const file of Object.keys(files)) {
      if (files[file].length === 0) continue;
      lineNumber = -1;

      isFullyFree = files[file][0].toUpperCase().startsWith(`**FREE`);
      let lineIsFree = false;

      let directIfScope = 0;

      for (let line of files[file]) {
        const scope = scopes[scopes.length - 1];
        let spec;

        lineIsFree = false;
        lineNumber += 1;

        // After compile time data, we're done
        if (lineNumber > 0 && line.startsWith(`**`)) {
          break;
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
            } else if (![`D`, `P`, `C`, `F`].includes(spec)) {
              continue;
            } else {
              if (spec === `C`) {
                // We don't want to waste precious time parsing all C specs, so we make sure it's got
                // BEGSR or ENDSR in it first.
                const upperLine = line.toUpperCase();
                if ([`BEGSR`, `ENDSR`].some(v => upperLine.includes(v)) === false) {
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

        if (isFullyFree || lineIsFree) {
          // Free format!
          line = line.trim();

          if (line === ``) continue;

          pieces = line.split(`;`);
          parts = pieces[0].toUpperCase().split(` `).filter(piece => piece !== ``);
          partsLower = pieces[0].split(` `).filter(piece => piece !== ``);

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
          }

          switch (parts[0]) {
          case `DCL-F`:
            if (currentItem === undefined) {
              currentItem = new Declaration(`file`);
              currentItem.name = partsLower[1];
              currentItem.keywords = parts.slice(2);
              currentItem.description = currentDescription.join(` `);

              currentItem.position = {
                path: file,
                line: lineNumber
              }

              const objectName = getObjectName(parts[1], parts);

              const recordFormats = await this.fetchTable(objectName, parts.length.toString(), parts.includes(`ALIAS`));

              if (recordFormats.length > 0) {
                const qualified = parts.includes(`QUALIFIED`);

                // Got to fix the positions for the defintions to be the declare.
                recordFormats.forEach(recordFormat => {
                  recordFormat.keywords = [parts[1]];
                  if (qualified) recordFormat.keywords.push(`QUALIFIED`);

                  recordFormat.position = currentItem.position;

                  recordFormat.subItems.forEach(subItem => {
                    subItem.position = currentItem.position;
                  });
                });

                currentItem.subItems.push(...recordFormats);
              }

              scope.files.push(currentItem);
              resetDefinition = true;
            }
            break;

          case `DCL-C`:
            if (currentItem === undefined) {
              currentItem = new Declaration(`constant`);
              currentItem.name = partsLower[1];
              currentItem.keywords = parts.slice(2);
              currentItem.description = currentDescription.join(` `);

              currentItem.position = {
                path: file,
                line: lineNumber
              }

              scope.constants.push(currentItem);
              resetDefinition = true;
            }
            break;

          case `DCL-S`:
            if (currentItem === undefined) {
              currentItem = new Declaration(`variable`);
              currentItem.name = partsLower[1];
              currentItem.keywords = parts.slice(2);
              currentItem.description = currentDescription.join(` `);
              currentItem.tags = currentTags;

              currentItem.position = {
                path: file,
                line: lineNumber
              }

              scope.variables.push(currentItem);
              resetDefinition = true;
            }
            break;

          case `DCL-DS`:
            if (currentItem === undefined) {
              currentItem = new Declaration(`struct`);
              currentItem.name = partsLower[1];
              currentItem.keywords = parts.slice(2);
              currentItem.description = currentDescription.join(` `);
              currentItem.tags = currentTags;

              currentItem.position = {
                path: file,
                line: lineNumber
              }

              currentItem.range = {
                start: lineNumber,
                end: lineNumber
              };

              currentGroup = `structs`;

              // Expand the LIKEDS value if there is one.
              await expandDs(file, currentItem);

              // Does the keywords include a keyword that makes end-ds useless?
              if (currentItem.keywords.some(keyword => oneLineTriggers[`DCL-DS`].some(trigger => keyword.startsWith(trigger)))) {
                currentItem.range.end = lineNumber;
                scope.structs.push(currentItem);
              } else {
                currentItem.readParms = true;
                dsScopes.push(currentItem);
              }

              resetDefinition = true;

              currentDescription = [];
            } else {

            }
            break;

          case `END-DS`:
            if (dsScopes.length > 0) {
              const currentDs = dsScopes[dsScopes.length - 1];
              currentDs.range.end = lineNumber;
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
              if (!scope.procedures.find(proc => proc.name.toUpperCase() === parts[1])) {
                currentGroup = `procedures`;
                currentItem = new Declaration(`procedure`);
                currentItem.name = partsLower[1];
                currentItem.keywords = parts.slice(2);
                currentItem.description = currentDescription.join(` `);
                currentItem.tags = currentTags;

                currentItem.position = {
                  path: file,
                  line: lineNumber
                }

                currentItem.readParms = true;

                currentItem.range = {
                  start: lineNumber,
                  end: lineNumber
                };

                // Does the keywords include a keyword that makes end-ds useless?
                if (currentItem.keywords.some(keyword => oneLineTriggers[`DCL-PR`].some(trigger => keyword.startsWith(trigger)))) {
                  currentItem.range.end = lineNumber;
                  scope.procedures.push(currentItem);
                  resetDefinition = true;
                }

                currentDescription = [];
              }
            }
            break;

          case `END-PR`:
            if (currentItem && currentItem.type === `procedure`) {
              currentItem.range.end = lineNumber;
              scope.procedures.push(currentItem);
              resetDefinition = true;
            }
            break;
        
          case `DCL-PROC`:
            //We can overwrite it.. it might have been a PR before.
            const existingProc = scope.procedures.findIndex(proc => proc.name.toUpperCase() === parts[1]);

            // We found the PR... so we can overwrite it
            if (existingProc >= 0) scope.procedures.splice(existingProc, 1);

            currentItem = new Declaration(`procedure`);

            currentProcName = partsLower[1];
            currentItem.name = currentProcName;
            currentItem.keywords = parts.slice(2);
            currentItem.description = currentDescription.join(` `);
            currentItem.tags = currentTags;

            currentItem.position = {
              path: file,
              line: lineNumber
            }

            currentItem.readParms = false;

            currentItem.range = {
              start: lineNumber,
              end: lineNumber
            };

            scope.procedures.push(currentItem);
            resetDefinition = true;

            scopes.push(new Cache());
            break;

          case `DCL-PI`:
            //Procedures can only exist in the global scope.
            if (currentProcName) {
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
            currentItem = scopes[0].procedures.find(proc => proc.name === currentProcName);

            if (currentItem && currentItem.type === `procedure`) {
              currentItem.scope = scopes.pop();
              currentItem.range.end = lineNumber;
              resetDefinition = true;
            }
            break;

          case `BEGSR`:
            if (!scope.subroutines.find(sub => sub.name.toUpperCase() === parts[1])) {
              currentItem = new Declaration(`subroutine`);
              currentItem.name = partsLower[1];
              currentItem.description = currentDescription.join(` `);

              currentItem.position = {
                path: file,
                line: lineNumber
              }

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
            if (line.startsWith(`//`)) {
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

              if (currentItem && [`procedure`, `struct`].includes(currentItem.type)) {
                if (currentItem.readParms && parts.length > 0) {
                  if (parts[0].startsWith(`DCL`))
                    parts.slice(1);

                  currentSub = new Declaration(`subitem`);
                  currentSub.name = (parts[0] === `*N` ? `parm${currentItem.subItems.length+1}` : partsLower[0]) ;
                  currentSub.keywords = parts.slice(1);

                  currentSub.position = {
                    path: file,
                    line: lineNumber
                  }

                  // Add comments from the tags
                  if (currentDescription.length > 0) {
                    currentSub.description = currentDescription.join(` `);
                  }

                  currentSub.tags = currentTags;

                  // If the parameter has likeds, add the subitems to make it a struct.
                  await expandDs(file, currentSub);
                  currentSub.keyword = Parser.expandKeywords(currentSub.keywords);

                  currentItem.subItems.push(currentSub);
                  currentSub = undefined;
                  currentTags = [];

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
          case `F`:
            const fSpec = Fixed.parseFLine(line);
            potentialName = getObjectName(fSpec.name, fSpec.keywords);

            if (potentialName) {
              currentItem = new Declaration(`file`);
              currentItem.name = potentialName;
              currentItem.keywords = fSpec.keywords;

              currentItem.position = {
                path: file,
                line: lineNumber
              }

              const recordFormats = await this.fetchTable(potentialName, line.length.toString(), fSpec.keywords.includes(`ALIAS`));

              if (recordFormats.length > 0) {
                const qualified = fSpec.keywords.includes(`QUALIFIED`);

                // Got to fix the positions for the defintions to be the declare.
                recordFormats.forEach(recordFormat => {
                  recordFormat.keywords = [potentialName];
                  if (qualified) recordFormat.keywords.push(`QUALIFIED`);

                  recordFormat.position = currentItem.position;

                  recordFormat.subItems.forEach(subItem => {
                    subItem.position = currentItem.position;
                  });
                });

                currentGroup = `structs`
                currentItem.subItems.push(...recordFormats);
              }

              scope.files.push(currentItem);
            }
            
            resetDefinition = true;
            break;

          case `C`:
            const cSpec = Fixed.parseCLine(line);

            potentialName = cSpec.factor1;

            switch (cSpec.opcode) {
            case `BEGSR`:
              if (!scope.subroutines.find(sub => sub.name.toUpperCase() === potentialName)) {
                currentItem = new Declaration(`subroutine`);
                currentItem.name = potentialName;
                currentItem.keywords = [`Subroutine`];
  
                currentItem.position = {
                  path: file,
                  line: lineNumber
                }
  
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
            }

            break;
          case `P`:
            const pSpec = Fixed.parsePLine(line);

            if (pSpec.potentialName === ``) continue;

            if (pSpec.potentialName.endsWith(`...`)) {
              potentialName = pSpec.potentialName.substring(0, pSpec.potentialName.length - 3);
              potentialNameUsed = true;
            } else {
              if (pSpec.start) {
                potentialName = pSpec.name.length > 0 ? pSpec.name : potentialName;

                if (potentialName) {
                  //We can overwrite it.. it might have been a PR before.
                  const existingProc = scope.procedures.findIndex(proc => proc.name.toUpperCase() === parts[1]);

                  // We found the PR... so we can overwrite it
                  if (existingProc >= 0) scope.procedures.splice(existingProc, 1);

                  currentItem = new Declaration(`procedure`);

                  currentProcName = potentialName;
                  currentItem.name = currentProcName;
                  currentItem.keywords = pSpec.keywords;

                  currentItem.position = {
                    path: file,
                    line: lineNumber - (potentialNameUsed ? 1 : 0) // Account that name is on line before
                  }

                  currentItem.range = {
                    start: currentItem.position.line,
                    end: currentItem.position.line
                  };

                  scope.procedures.push(currentItem);
                  resetDefinition = true;

                  scopes.push(new Cache());
                }
              } else {
                //Procedures can only exist in the global scope.
                currentItem = scopes[0].procedures.find(proc => proc.name === currentProcName);

                if (currentItem && currentItem.type === `procedure`) {
                  currentItem.scope = scopes.pop();
                  currentItem.range.end = lineNumber;
                  resetDefinition = true;
                }
              }
            }
            break;

          case `D`:
            const dSpec = Fixed.parseDLine(line);

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
                }
    
                scope.constants.push(currentItem);
                resetDefinition = true;
                break;
              case `S`:
                currentItem = new Declaration(`variable`);
                currentItem.name = potentialName || `*N`;
                currentItem.keywords = [Fixed.getPrettyType(dSpec), ...dSpec.keywords];

                // TODO: line number might be different with ...?
                currentItem.position = {
                  path: file,
                  line: lineNumber - (potentialNameUsed ? 1 : 0) // Account that name is on line before
                }

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
                }

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
                if (!scope.procedures.find(proc => proc.name.toUpperCase() === potentialName.toUpperCase())) {
                  currentItem = new Declaration(`procedure`);
                  currentItem.name = potentialName || `*N`;
                  currentItem.keywords = [Fixed.getPrettyType(dSpec), ...dSpec.keywords];
  
                  currentItem.position = {
                    path: file,
                    line: lineNumber - (potentialNameUsed ? 1 : 0) // Account that name is on line before
                  }

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
                    currentItem.keywords.push(Fixed.getPrettyType(dSpec), ...dSpec.keywords);
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
                    currentSub.keywords = [Fixed.getPrettyType(dSpec), ...dSpec.keywords];

                    currentSub.position = {
                      path: file,
                      line: lineNumber
                    }

                    // If the parameter has likeds, add the subitems to make it a struct.
                    await expandDs(file, currentSub);
                    currentSub.keyword = Parser.expandKeywords(currentSub.keywords);

                    currentItem.subItems.push(currentSub);
                    currentSub = undefined;

                    resetDefinition = true;
                  } else {
                    if (currentItem) {
                      if (currentItem.subItems.length > 0)
                        currentItem.subItems[currentItem.subItems.length - 1].keywords.push(Fixed.getPrettyType(dSpec), ...dSpec.keywords);
                      else
                        currentItem.keywords.push(...dSpec.keywords)
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

    const parsedData = scopes[0];

    this.parsedCache[workingUri.path] = parsedData;

    return parsedData;
  }

  /**
   * @param {string[]} parts 
   * @returns {Keywords}
   */
  static expandKeywords(parts) {
    const keyvalues = {};

    if (parts.length > 0) {
      const keywordParts = Statement.createBlocks(Statement.parseStatement(parts.join(` `)));

      for (let i = 0; i < keywordParts.length; i++) {
        if (keywordParts[i+1] && keywordParts[i+1].type === `block`) {
          keyvalues[keywordParts[i].value.toUpperCase()] = keywordParts[i+1].block.map(part => part.value).join(``);
          i++; // Skip one for the block.
        } else {
          keyvalues[keywordParts[i].value.toUpperCase()] = true;
        }
      }
    }

    return keyvalues;
  }
}
