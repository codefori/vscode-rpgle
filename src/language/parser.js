
const vscode = require(`vscode`);

const Generic = require(`./generic`);

const Cache = require(`./models/cache`);
const Declaration = require(`./models/declaration`);

const oneLineTriggers = require(`./models/oneLineTriggers`);

const getInstance = require(`../base`);

module.exports = class Parser {
  constructor() {
    /** @type {{[path: string]: string[]}} */
    this.copyBooks = {};

    /** @type {{[path: string]: Cache}} */
    this.parsedCache = {};
  }

  clearParsedCache(path) {
    this.parsedCache[path] = undefined;
  }

  getParsedCache(path) {
    return this.parsedCache[path];
  }

  getCopybook(path) {
    return this.copyBooks[path];
  }

  /** 
   * @param {string} path
   * @param {string|string[]} content
   */
  setCopybook(path, content) {
    this.copyBooks[path] = (typeof content === `string` ? [content] : content);
  }

  /**
   * @param {vscode.Uri} workingUri Path being worked with
   * @param {string} getPath IFS or member path to fetch
   * @returns {Promise<string[]>}
   */
  async getContent(workingUri, getPath) {
    let contentApi;
    let content;
    let lines = undefined;
  
    let instance;
    let {type, memberPath, finishedPath} = Generic.getPathInfo(workingUri, getPath);
  
    try {
      switch (type) {
      case `member`:
        instance = getInstance();
        if (!instance) throw new Error(`Connection instance not found`);
        contentApi = instance.getContent();
        
        lines = this.getCopybook(finishedPath);
        if (!lines) {  
          content = await contentApi.downloadMemberContent(memberPath[0], memberPath[1], memberPath[2], memberPath[3]);
          lines = content.replace(new RegExp(`\\\r`, `g`), ``).split(`\n`);
          this.setCopybook(finishedPath, lines);
        }
        break;
  
      case `streamfile`:
        instance = getInstance();
        if (!instance) throw new Error(`Connection instance not found`);
        contentApi = instance.getContent();

        lines = this.getCopybook(finishedPath);
        if (!lines) {
          content = await contentApi.downloadStreamfile(finishedPath);
          lines = content.replace(new RegExp(`\\\r`, `g`), ``).split(`\n`);
          this.setCopybook(finishedPath, lines);
        }
        break;

      case `file`:
        lines = this.getCopybook(finishedPath);
        if (!lines) {
          // We have to find the file because of the case insensitivity
          if (getPath.startsWith(`'`)) getPath = getPath.substring(1);
          if (getPath.endsWith(`'`)) getPath = getPath.substring(0, getPath.length - 1);
          if (getPath.startsWith(`./`)) getPath = getPath.substring(2);

          const possibleFile = await vscode.workspace.findFiles(`**/${getPath}`, null, 1);
          if (possibleFile.length > 0) {
            content = await (await vscode.workspace.fs.readFile(possibleFile[0])).toString();
            lines = content.replace(new RegExp(`\\\r`, `g`), ``).split(`\n`);
            this.setCopybook(finishedPath, lines);
          } else {
            lines = [`// NOT FOUND: ${getPath}`];
            this.setCopybook(finishedPath, lines);
          }
        }
        break;
      }
    } catch (e) {
      lines = [`// ERROR: ${getPath}`];
      this.setCopybook(finishedPath, lines);
    }
  
    return lines;
  }

  /**
   * @param {vscode.Uri} workingUri
   * @param {string} content 
   */
  async updateCopybookCache(workingUri, content) {
    if (this.getParsedCache(workingUri.path)) {
      this.clearParsedCache(workingUri.path); //Clear parsed data

      let baseLines = content.replace(new RegExp(`\\\r`, `g`), ``).split(`\n`);

      //First loop is for copy/include statements
      for (let i = baseLines.length - 1; i >= 0; i--) {
        const line = baseLines[i].trim(); //Paths are case insensitive so it's okay
        if (line === ``) continue;

        const pieces = line.split(` `).filter(piece => piece !== ``);

        if ([`/COPY`, `/INCLUDE`].includes(pieces[0].toUpperCase())) {
          await this.getContent(workingUri, pieces[1]);
        }
      }
    }
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

    let files = {};
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
        }
      }
    }

    let isFullyFree = false;

    //Now the real work
    for (const file in files) {
      lineNumber = -1;

      isFullyFree = files[file][0].toUpperCase().startsWith(`**FREE`);
      let lineIsFree = false;

      for (let line of files[file]) {
        const scope = scopes[scopes.length - 1];

        lineNumber += 1;

        if (isFullyFree === false && line.length > 6) {

          const spec = line[5].toUpperCase();
          const comment = line[6];

          if (comment !== ` `) {
            continue;
          }

          if (spec === ` `) {
            //Clear out stupid comments
            line = ``.padEnd(4) + line.substring(4);

            lineIsFree = true;
          } else if (spec !== `D`) {
            continue;
          }
        }

        if (isFullyFree || lineIsFree) {
          // Free format!
          line = line.trim();

          if (line === ``) continue;

          pieces = line.split(`;`);
          parts = pieces[0].toUpperCase().split(` `).filter(piece => piece !== ``);
          partsLower = pieces[0].split(` `).filter(piece => piece !== ``);

          switch (parts[0]) {
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
              if (!parts.includes(`TEMPLATE`)) {
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
            }
            break;

          case `DCL-DS`:
            if (currentItem === undefined) {
              if (!parts.includes(`TEMPLATE`)) {
                currentItem = new Declaration(`struct`);
                currentItem.name = partsLower[1];
                currentItem.keywords = parts.slice(2);
                currentItem.description = currentDescription.join(` `);
                currentItem.tags = currentTags;

                currentItem.position = {
                  path: file,
                  line: lineNumber
                }

                // Does the keywords include a keyword that makes end-ds useless?
                if (currentItem.keywords.some(keyword => oneLineTriggers[`DCL-DS`].some(trigger => keyword.startsWith(trigger)))) {
                  scope.structs.push(currentItem);
                  resetDefinition = true;
                } else {
                  currentItem.readParms = true;
                }

                currentDescription = [];
              }
            }
            break;

          case `END-DS`:
            if (currentItem && currentItem.type === `struct`) {
              scope.structs.push(currentItem);
              resetDefinition = true;
            }
            break;
        
          case `DCL-PR`:
            if (currentItem === undefined) {
              if (!scope.procedures.find(proc => proc.name.toUpperCase() === parts[1])) {
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
            currentItem = scopes[0].procedures.find(proc => proc.name === currentProcName);

            if (currentItem) {
              currentItem.keywords = parts.slice(2);
              currentItem.readParms = true;

              currentDescription = [];
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
              if (currentItem && [`procedure`, `struct`].includes(currentItem.type)) {
                if (currentItem.readParms) {
                  if (parts[0].startsWith(`DCL`))
                    parts.slice(1);

                  currentSub = new Declaration(`subitem`);
                  currentSub.name = (parts[0] === `*N` ? `parm${currentItem.subItems.length+1}` : partsLower[0]) ;
                  currentSub.keywords = parts.slice(1);

                  const paramTags = currentTags.filter(tag => tag.tag === `param`);
                  const paramTag = paramTags.length > currentItem.subItems.length ? paramTags[currentItem.subItems.length] : undefined;
                  if (paramTag) {
                    currentSub.description = paramTag.content;
                  }

                  currentItem.subItems.push(currentSub);
                  currentSub = undefined;
                }
              }
            }
            break;
          }

          if (resetDefinition) {
            currentItem = undefined;
            currentTitle = undefined;
            currentDescription = [];
            currentTags = [];
            resetDefinition = false;
          }
        } else {
          // Fixed format!
        }
      }
    }

    const parsedData = scopes[0];

    this.parsedCache[workingUri.path] = parsedData;

    return parsedData;
  }
}