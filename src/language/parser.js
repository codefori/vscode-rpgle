
const path = require(`path`);
const vscode = require(`vscode`);

const Generic = require(`./generic`);

const Cache = require(`./models/cache`);
const Declaration = require(`./models/declaration`);

const oneLineTriggers = require(`./models/oneLineTriggers`);
const Fixed = require(`./models/fixed`);

const HALF_HOUR = (30 * 60 * 1000);

/**
 * @callback tablePromise
 * @param  {string} name Table name
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
   * @returns {Promise<Declaration[]>}
   */
  async fetchTable(name) {
    if (name.trim() === ``) return [];
    if (!this.tableFetch) return [];
    const table = name.toUpperCase();
    const now = Date.now();

    if (this.tables[table]) {
      // We use this to make sure we aren't running this all over the place
      if (this.tables[table].fetching) return [];

      // If we still have a cached version, let's use that
      if (now <= (this.tables[table].fetched + HALF_HOUR)) {
        return this.tables[table].recordFormats;
      }
    }

    this.tables[table] = {
      fetching: true,
      fetched: 0,
      recordFormats: []
    }

    let newDefs;

    try {
      newDefs = await this.tableFetch(table);

      this.tables[table] = {
        fetched: now,
        recordFormats: newDefs
      }
    } catch (e) {
      newDefs = [];
    }

    this.tables[table].fetching = false;

    return newDefs;
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
   * @returns {Promise<string[]>}
   */
  async getContent(workingUri, getPath) {
    //const hrstart = process.hrtime();

    let content;
    let lines = undefined;
  
    let {type, memberPath, finishedPath} = Generic.getPathInfo(workingUri, getPath);
  
    try {
      let doc;
      let eol;
      switch (type) {
      case `member`:

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
            path: finishedPath + `.MBR`
          }));
        }

        eol = doc.eol === vscode.EndOfLine.CRLF ? `\r\n` : `\n`;

        lines = doc.getText().split(eol);
        break;
  
      case `streamfile`:
        doc = await vscode.workspace.openTextDocument(vscode.Uri.from({
          scheme: type,
          path: finishedPath
        }));
        eol = doc.eol === vscode.EndOfLine.CRLF ? `\r\n` : `\n`;

        lines = doc.getText().split(eol);
        break;

      case `file`:
        // We have to find the file because of the case insensitivity
        if (getPath.startsWith(`'`)) getPath = getPath.substring(1);
        if (getPath.endsWith(`'`)) getPath = getPath.substring(0, getPath.length - 1);
        if (getPath.startsWith(`./`)) getPath = getPath.substring(2);

        /** @type {vscode.Uri} */
        let possibleFile;

        if (this.localUris[getPath] !== undefined) possibleFile = this.localUris[getPath];
        else {
          const fileSearch = await vscode.workspace.findFiles(`**/${getPath}`, null, 1);
          if (fileSearch.length > 0) { 
            possibleFile = fileSearch[0];
            this.localUris[getPath] = possibleFile;
          } else {
            this.localUris[getPath] = null;
          }
        }

        if (possibleFile) {
          content = (await vscode.workspace.fs.readFile(possibleFile)).toString();
          lines = content.replace(new RegExp(`\\\r`, `g`), ``).split(`\n`);
        } else {
          lines = [];
        }
        break;
      }
    } catch (e) {
      lines = [];
    }

    //const hrend = process.hrtime(hrstart);
    //console.info(`getContent() took ${hrend[0]}s and ${hrend[1] / 1000000}ms: ${getPath} (${lines.length})`);
  
    return lines;
  }

  /**
   * @param {vscode.Uri} workingUri
   * @param {string} [content] 
   * @param {boolean} [withIncludes] To make sure include statements are parsed
   * @returns {Promise<Cache|null>}
   */
  async getDocs(workingUri, content, withIncludes = true) {
    const existingCache = this.getParsedCache(workingUri.path);
    if (existingCache) {
      return existingCache;
    };

    if (!content) return null;

    /** @type {{[path: string]: string[]}} */
    const files = {};

    /** @type {string[]} Order which files should be scanned */
    const fileList = [];

    let baseLines = content.replace(new RegExp(`\\\r`, `g`), ``).split(`\n`);

    let currentTitle = undefined, currentDescription = [];
    /** @type {{tag: string, content: string}[]} */
    let currentTags = [];

    let currentItem, currentSub, currentProcName;

    let resetDefinition = false; //Set to true when you're done defining a new item
    let docs = false; // If section is for ILEDocs
    let lineNumber, parts, partsLower, pieces;

    /** @type {Cache[]} */
    let scopes = [];

    /** @type {Declaration[]} Free format struct scopes. Used for free-format only */
    let dsScopes = [];

    // Global scope bits
    scopes.push(new Cache());

    files[workingUri.path] = baseLines;

    if (withIncludes) {
    //First loop is for copy/include statements
      for (let i = baseLines.length - 1; i >= 0; i--) {
        let line = baseLines[i].trim(); //Paths are case insensitive so it's okay
        if (line === ``) continue;

        pieces = line.split(` `).filter(piece => piece !== ``);

        if ([`/COPY`, `/INCLUDE`].includes(pieces[0].toUpperCase())) {
          files[pieces[1]] = (await this.getContent(workingUri, pieces[1]));
          fileList.push(pieces[1]);
        }
      }
    }

    // Scan the base file last. Handle copy books first
    fileList.push(workingUri.path);

    let potentialName;
    /** @type {"structs"|"procedures"} */
    let currentGroup;
    let isFullyFree = false;

    //Now the real work
    for (const file of fileList) {
      if (files[file].length === 0) continue;
      lineNumber = -1;

      isFullyFree = files[file][0].toUpperCase().startsWith(`**FREE`);
      let lineIsFree = false;

      for (let line of files[file]) {
        const scope = scopes[scopes.length - 1];
        let spec;

        lineIsFree = false;
        lineNumber += 1;

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

          if (parts[0] === `/EOF`) {
            // End of parsing for this file
            break;
          }

          switch (parts[0]) {
          case `DCL-F`:
            const recordFormats = await this.fetchTable(parts[1]);

            if (recordFormats.length > 0) {
              const qualified = parts.includes(`QUALIFIED`);

              // Got to fix the positions for the defintions to be the declare.
              recordFormats.forEach(recordFormat => {
                recordFormat.description = `Table ${parts[1]}`;

                recordFormat.keywords = [parts[1]];
                if (qualified) recordFormat.keywords.push(`QUALIFIED`);

                recordFormat.position = {
                  path: file,
                  line: lineNumber
                };

                recordFormat.subItems.forEach(subItem => {
                  subItem.position = {
                    path: file,
                    line: lineNumber
                  };
                });
              });

              scope.structs.push(...recordFormats);
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

              currentGroup = `structs`;
              // Does the keywords include a keyword that makes end-ds useless?
              if (currentItem.keywords.some(keyword => oneLineTriggers[`DCL-DS`].some(trigger => keyword.startsWith(trigger)))) {

                // Expand the LIKEDS value if there is one.
                const likeDS = currentItem.keywords.find(keyword => keyword.startsWith(`LIKEDS(`) && keyword.endsWith(`)`));
                if (likeDS) {
                  const likeDSValue = likeDS.substring(7, likeDS.length - 1).toUpperCase();

                  for (let i = scopes.length - 1; i >= 0; i--) {
                    const likeDSItem = scopes[i].structs.find(struct => struct.name.toUpperCase() === likeDSValue);
                    if (likeDSItem) {
                      currentItem.subItems = likeDSItem.subItems;

                      // We need to add qualified as it is qualified by default.
                      if (!currentItem.keywords.includes(`QUALIFIED`))
                        currentItem.keywords.push(`QUALIFIED`);
                      break;
                    }
                  }
                }

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

                currentDescription = [];
              }
            }
            break;

          case `END-PR`:
            if (currentItem && currentItem.type === `procedure`) {
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
              end: null
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

              if (currentItem) {
                currentItem.keywords = parts.slice(2);
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
                end: null
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
                if (currentItem.readParms) {
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
                  const paramTags = currentTags.filter(tag => tag.tag === `param`);
                  const paramTag = paramTags.length > currentItem.subItems.length ? paramTags[currentItem.subItems.length] : undefined;
                  if (paramTag) {
                    currentSub.description = paramTag.content;
                  }

                  // If the parameter has likeds, add the subitems to make it a struct.
                  const likeDS = currentSub.keywords.find(keyword => keyword.startsWith(`LIKEDS(`) && keyword.endsWith(`)`));
                  if (likeDS) {
                    const likeDSValue = likeDS.substring(7, likeDS.length - 1).toUpperCase();
                    
                    for (let i = scopes.length - 1; i >= 0; i--) {
                      const likeDSItem = scopes[i].structs.find(struct => struct.name.toUpperCase() === likeDSValue);
                      if (likeDSItem) {
                        currentSub.subItems = likeDSItem.subItems;
                    
                        // We need to add qualified as it is qualified by default.
                        if (!currentSub.keywords.includes(`QUALIFIED`))
                          currentSub.keywords.push(`QUALIFIED`);
                        break;
                      }
                    }
                  }

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

          switch (spec) {
          case `F`:
            const fSpec = Fixed.parseFLine(line);
            potentialName = fSpec.name;

            const recordFormats = await this.fetchTable(potentialName);

            if (recordFormats.length > 0) {
              const qualified = fSpec.keywords.includes(`QUALIFIED`);

              // Got to fix the positions for the defintions to be the declare.
              recordFormats.forEach(recordFormat => {
                recordFormat.description = `Table ${potentialName}`;

                recordFormat.keywords = [potentialName];
                if (qualified) recordFormat.keywords.push(`QUALIFIED`);

                recordFormat.position = {
                  path: file,
                  line: lineNumber
                };

                recordFormat.subItems.forEach(subItem => {
                  subItem.position = {
                    path: file,
                    line: lineNumber
                  };
                });
              });

              currentGroup = `structs`
              scope.structs.push(...recordFormats);
            }
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
                  end: null
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
                    line: lineNumber
                  }

                  currentItem.range = {
                    start: lineNumber,
                    end: null
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
              continue;
            } else {
              potentialName = dSpec.name.length > 0 ? dSpec.name : potentialName ? potentialName : `*N`;

              switch (dSpec.field) {
              case `C`:
                currentItem = new Declaration(`constant`);
                currentItem.name = potentialName;
                currentItem.keywords = [...dSpec.keywords];
                  
                // TODO: line number might be different with ...?
                currentItem.position = {
                  path: file,
                  line: lineNumber
                }
    
                scope.constants.push(currentItem);
                resetDefinition = true;
                break;
              case `S`:
                currentItem = new Declaration(`variable`);
                currentItem.name = potentialName;
                currentItem.keywords = [Fixed.getPrettyType(dSpec), ...dSpec.keywords];

                // TODO: line number might be different with ...?
                currentItem.position = {
                  path: file,
                  line: lineNumber
                }

                scope.variables.push(currentItem);
                resetDefinition = true;
                break;

              case `DS`:
                currentItem = new Declaration(`struct`);
                currentItem.name = potentialName;
                currentItem.keywords = dSpec.keywords;

                currentItem.position = {
                  path: file,
                  line: lineNumber
                }

                currentGroup = `structs`;
                scope.structs.push(currentItem);
                resetDefinition = true;
                break;

              case `PR`:
                // Only add a PR if it's not been defined
                if (!scope.procedures.find(proc => proc.name.toUpperCase() === potentialName.toUpperCase())) {
                  currentItem = new Declaration(`procedure`);
                  currentItem.name = potentialName;
                  currentItem.keywords = [Fixed.getPrettyType(dSpec), ...dSpec.keywords];
  
                  currentItem.position = {
                    path: file,
                    line: lineNumber
                  }
  
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
                switch (currentGroup) {
                case `procedures`:
                  // Get from the parent
                  currentItem = scopes[scopes[0].procedures.length - 1].procedures[scopes[0].procedures.length - 1];
                  break;

                case `structs`:
                  currentItem = scope[currentGroup][scope[currentGroup].length - 1];
                  break;
                }

                if (currentItem) {
                  currentSub = new Declaration(`subitem`);
                  currentSub.name = (potentialName === `*N` ? `parm${currentItem.subItems.length+1}` : potentialName) ;
                  currentSub.keywords = [Fixed.getPrettyType(dSpec), ...dSpec.keywords];

                  currentSub.position = {
                    path: file,
                    line: lineNumber
                  }

                  // If the parameter has likeds, add the subitems to make it a struct.
                  const likeDS = currentSub.keywords.find(keyword => keyword.startsWith(`LIKEDS(`) && keyword.endsWith(`)`));
                  if (likeDS) {
                    const likeDSValue = likeDS.substring(7, likeDS.length - 1).toUpperCase();
  
                    for (let i = scopes.length - 1; i >= 0; i--) {
                      const likeDSItem = scopes[i].structs.find(struct => struct.name.toUpperCase() === likeDSValue);
                      if (likeDSItem) {
                        currentSub.subItems = likeDSItem.subItems;
  
                        // We need to add qualified as it is qualified by default.
                        if (!currentSub.keywords.includes(`QUALIFIED`))
                          currentSub.keywords.push(`QUALIFIED`);
                        break;
                      }
                    }
                  }

                  currentItem.subItems.push(currentSub);
                  currentSub = undefined;

                  resetDefinition = true;
                }
                break;
              }
            
              potentialName = undefined;
            }
            break;
          }
        }

        if (resetDefinition) {
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
}