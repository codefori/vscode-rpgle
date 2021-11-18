
const vscode = require(`vscode`);
const { instance } = vscode.extensions.getExtension(`halcyontechltd.code-for-ibmi`).exports;

const Generic = require(`./generic`);

const Cache = require(`./models/cache`);
const Declaration = require(`./models/declaration`);

const oneLineTriggers = require(`./models/oneLineTriggers`);

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
    const contentApi = instance.getContent();
  
    let content;
    let lines = undefined;
  
    let {type, memberPath, finishedPath} = Generic.getPathInfo(workingUri, getPath);
  
    try {
      switch (type) {
      case `member`:
        lines = this.getCopybook(finishedPath);
        if (!lines) {  
          content = await contentApi.downloadMemberContent(memberPath[0], memberPath[1], memberPath[2], memberPath[3]);
          lines = content.replace(new RegExp(`\\\r`, `g`), ``).split(`\n`);
          this.setCopybook(finishedPath, lines);
        }
        break;
  
      case `streamfile`:
        lines = this.getCopybook(finishedPath);
        if (!lines) {
          content = await contentApi.downloadStreamfile(finishedPath);
          lines = content.replace(new RegExp(`\\\r`, `g`), ``).split(`\n`);
          this.setCopybook(finishedPath, lines);
        }
        break;
      }
    } catch (e) {
      lines = [];
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

    let currentItem, currentSub;

    let resetDefinition = false; //Set to true when you're done defining a new item
    let docs = false; // If section is for ILEDocs
    let lineNumber, parts, partsLower, pieces;

    const constants = [];
    const variables = [];
    const structs = [];
    const procedures = [];
    const subroutines = [];

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

    //Now the real work
    for (const file in files) {
      lineNumber = -1;
      for (let line of files[file]) {
        lineNumber += 1;

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

            constants.push(currentItem);
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

              variables.push(currentItem);
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
                structs.push(currentItem);
                resetDefinition = true;
              }

              currentDescription = [];
            }
          }
          break;

        case `END-DS`:
          if (currentItem && currentItem.type === `struct`) {
            structs.push(currentItem);
            resetDefinition = true;
          }
          break;
        
        case `DCL-PR`:
          if (currentItem === undefined) {
            if (!procedures.find(proc => proc.name.toUpperCase() === parts[1])) {
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
            procedures.push(currentItem);
            resetDefinition = true;
          }
          break;
        
        case `DCL-PROC`:
          //We can overwrite it.. it might have been a PR before.
          currentItem = procedures.find(proc => proc.name.toUpperCase() === parts[1]) || new Declaration(`procedure`);

          currentItem.name = partsLower[1];
          currentItem.keywords = parts.slice(2);
          currentItem.description = currentDescription.join(` `);
          currentItem.tags = currentTags;

          currentItem.position = {
            path: file,
            line: lineNumber
          }

          currentItem.readParms = false;

          currentDescription = [];
          break;

        case `DCL-PI`:
          if (currentItem) {
            currentItem.keywords = parts.slice(2);
            currentItem.readParms = true;

            currentDescription = [];
          }
          break;

        case `END-PI`:
          if (currentItem && currentItem.type === `procedure`) {
            currentItem.readParms = false;
          }
          break;

        case `END-PROC`:
          if (currentItem && currentItem.type === `procedure`) {
            procedures.push(currentItem);
            resetDefinition = true;
          }
          break;

        case `BEGSR`:
          if (!subroutines.find(sub => sub.name.toUpperCase() === parts[1])) {
            currentItem = new Declaration(`subroutine`);
            currentItem.name = partsLower[1];
            currentItem.description = currentDescription.join(` `);

            currentItem.position = {
              path: file,
              line: lineNumber
            }

            currentDescription = [];
          }
          break;
    
        case `ENDSR`:
          if (currentItem && currentItem.type === `subroutine`) {
            subroutines.push(currentItem);
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
            if (currentItem && currentItem.type === `procedure`) {
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
      
      }
    }

    const parsedData = new Cache({
      procedures,
      structs,
      subroutines,
      variables,
      constants
    });

    this.parsedCache[workingUri.path] = parsedData;

    return parsedData;
  }
}