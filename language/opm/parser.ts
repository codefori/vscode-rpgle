import Cache from '../models/cache';
import Declaration, { DeclarationType } from '../models/declaration';
import { Keywords } from '../ile/parserTypes';
import { EmbeddedSqlSpecification, InputConstantEntry, parseSpecification } from './specs';

export type tablePromise = (name: string, aliases?: boolean) => Promise<Declaration[]>;
export type includeFilePromise = (baseFile: string, includeString: string) => Promise<{found: boolean, uri?: string, content?: string}>;

export interface ParseOptions {
  withIncludes?: boolean;
  keepTree?: boolean;
  keepSqlInTree?: boolean;
}

/**
 * Helper function to map OPM internal data format to RPGLE data type
 */
function getRpgDataType(type: string): string {
  switch (type) {
    case "B":
      return "char";
    case "P":
      return "packed";
    default:
      return "char";
  }
}

/**
 * Helper function to find the most recently added symbol of a given type
 */
function findPriorType(
  cache: Cache, 
  type: DeclarationType | DeclarationType[]
): Declaration | undefined {
  const symbols = cache.symbols;
  for (let i = symbols.length - 1; i >= 0; i--) {
    const symbol = symbols[i];
    const isMatch = Array.isArray(type) 
      ? type.includes(symbol.type) 
      : symbol.type === type;
    if (isMatch) {
      return symbol;
    }
  }
  return undefined;
}

/**
 * Helper function to create a properly initialized Declaration
 */
function createDeclaration(
  type: DeclarationType,
  name: string,
  fileUri: string,
  lineI: number,
  index: number,
  lineLength: number,
  keywords: Keywords = {}
): Declaration {
  const declaration = new Declaration(type);
  declaration.name = name;
  declaration.keyword = keywords;
  declaration.position = {
    path: fileUri,
    range: {
      line: lineI,
      start: index,
      end: index + lineLength
    }
  };
  declaration.range = {
    start: lineI,
    end: lineI
  };
  declaration.subItems = [];
  declaration.references = [];
  declaration.tags = [];
  declaration.readParms = false;
  
  return declaration;
}

/**
 * Helper function to remove quotes from strings
 */
function trimQuotes(input: string, value = '\'\''): string {
  const quote = value[0];
  
  if (input.startsWith(quote)) {
    input = input.substring(1);
  }

  if (input.endsWith(quote)) {
    input = input.substring(0, input.length - 1);
  }

  return input;
}

/**
 * OPM RPG Parser - Returns Cache directly (compatible with ILE parser)
 */
export class OpmParser {
  private tableFetch: tablePromise | undefined;
  private includeFileFetch: includeFilePromise | undefined;

  setTableFetch(promise: tablePromise) {
    this.tableFetch = promise;
  }

  setIncludeFileFetch(promise: includeFilePromise) {
    this.includeFileFetch = promise;
  }

  /**
   * Parse OPM RPG source and return Cache (same as ILE parser)
   */
  async getDocs(fileUri: string, baseContent: string, options: ParseOptions = {}): Promise<Cache> {
    const cache = new Cache({}, true); // Don't add default indicators for OPM
    
    const parseContent = async (fileUri: string, content: string) => {
      let index = 0;
      const EOL = content.includes(`\r\n`) ? `\r\n` : `\n`;
      const lines = content.split(EOL);

      if (options.keepTree || options.keepSqlInTree) {
        if (!cache.parseTree) {
          cache.parseTree = {};
        }
        if (!cache.parseTree[fileUri]) {
          cache.parseTree[fileUri] = [];
        }
      }

      let currentSqlSpec: EmbeddedSqlSpecification | undefined;

      for (let lineI = 0; lineI < lines.length; lineI++) {
        const line = lines[lineI];
        
        // Break when we find Local Data Area(LDA) or compile time array
        if (line.startsWith("**")) {
          break;
        }

        const spec = parseSpecification(line, index);

        if (spec) {
          if (options.keepTree && spec.type !== `sql`) {
            cache.parseTree![fileUri].push(spec);
          }

          switch (spec.type) {
            case `sql`:
              // Handle SQL (aggregate multi-line SQL statements)
              if (currentSqlSpec && currentSqlSpec.contents !== undefined && !spec.end) {
                currentSqlSpec.contents += ` ` + spec.contents;
                currentSqlSpec.specs.push(spec);
              } else if (currentSqlSpec?.contents && spec.end) {
                currentSqlSpec.contents = currentSqlSpec.contents.trim();
                currentSqlSpec.specs.push(spec);

                if (options.keepTree || options.keepSqlInTree) {
                  cache.parseTree![fileUri].push({
                    type: `sql`,
                    rawLine: currentSqlSpec.contents.trim(),
                    specs: currentSqlSpec.specs
                  });
                }

                currentSqlSpec = undefined;
              } else if (!currentSqlSpec && !spec.end) {
                currentSqlSpec = {
                  type: `sql`,
                  rawLine: ``,
                  contents: spec.contents,
                  specs: [spec]
                };
              }
              break;

            case `directive`:
              if (spec.directiveName.value?.toUpperCase() === `COPY` && this.includeFileFetch && spec.value) {
                const includeResult = await this.includeFileFetch(fileUri, String(spec.value.value));
                if (includeResult?.found && includeResult.uri && includeResult.content) {
                  await parseContent(includeResult.uri, includeResult.content);
                }
              }
              break;

            case `file`:
              if (spec.fileName) {
                const fileName = String(spec.fileName.value);
                
                const declaration = createDeclaration(
                  `file`,
                  fileName,
                  fileUri,
                  lineI,
                  index,
                  line.length
                );

                if (this.tableFetch) {
                  const recordFormats = await this.tableFetch(fileName);
                  
                  // Update positions for record formats and fields
                  for (const recordFormat of recordFormats) {
                    recordFormat.position = {
                      path: fileUri,
                      range: {
                        line: lineI,
                        start: index,
                        end: index + line.length
                      }
                    };

                    for (const field of recordFormat.subItems) {
                      field.position = {
                        path: fileUri,
                        range: {
                          line: lineI,
                          start: index,
                          end: index + line.length
                        }
                      };
                    }
                    
                    declaration.subItems.push(recordFormat);
                  }
                }

                cache.addSymbol(declaration);
              }
              break;

            case `calculation`:
              let defined: Declaration | undefined;
              if (spec.fieldLength) {
                let dataType: string = `char`;
                if (spec.decimalPositions) {
                  dataType = `packed`;
                }

                const length = Number(spec.fieldLength.value);

                defined = createDeclaration(
                  `variable`,
                  String(spec.resultField.value),
                  fileUri,
                  lineI,
                  index,
                  line.length,
                  { [dataType]: String(length) }
                );

                if (spec.decimalPositions) {
                  defined.keyword.decimals = String(spec.decimalPositions.value);
                }

                cache.addSymbol(defined);
              }

              // Handle operation-based symbols
              if (spec.operation) {
                const operation = spec.operation.value.toString().toUpperCase();
                
                const operationTypeMap: { [op: string]: DeclarationType } = {
                  'PLIST': 'plist',
                  'KLIST': 'klist',
                  'BEGSR': 'subroutine'
                };

                if (operationTypeMap[operation] && spec.factor1) {
                  const declaration = createDeclaration(
                    operationTypeMap[operation],
                    spec.factor1.value as string,
                    fileUri,
                    lineI,
                    index,
                    line.length
                  );
                  
                  cache.addSymbol(declaration);
                } else if (operation === `ENDSR`) {
                  const lastSubroutine = findPriorType(cache, `subroutine`);
                  if (lastSubroutine) {
                    lastSubroutine.range.end = lineI;
                  }
                } else if (operation === `CALL` && spec.factor2) {
                  const declaration = createDeclaration(
                    `call`,
                    trimQuotes(spec.factor2.value as string),
                    fileUri,
                    lineI,
                    index,
                    line.length
                  );
                  
                  cache.addSymbol(declaration);
                } else if ((operation === `PARM` || operation === `KFLD`) && spec.resultField) {
                  if (!defined) {
                    defined = cache.find(spec.resultField.value as string);
                  }

                  if (!defined) {
                    defined = createDeclaration(
                      `variable`,
                      String(spec.resultField.value),
                      fileUri,
                      lineI,
                      index,
                      line.length,
                      { unresolved: true }
                    );
                    cache.addSymbol(defined);
                  }

                  if (defined) {
                    const lastSymbol = findPriorType(cache, [`call`, `plist`, `klist`]);
                    if (lastSymbol && spec.resultField) {
                      lastSymbol.subItems.push(defined);
                      lastSymbol.range.end = lineI;
                    }
                  }
                }
              }
              break;

            case `input`:
              if (spec.subtype === `field`) {
                const lastStruct = findPriorType(cache, `struct`);

                if (lastStruct && spec.name) {
                  let length: number = 0;
                  let type: string = `char`;

                  const inputField = createDeclaration(
                    `variable`,
                    String(spec.name.value),
                    fileUri,
                    lineI,
                    index,
                    line.length
                  );

                  if (spec.keywords && spec.keywords.value.toString().includes(`*`)) {
                    const keyword = spec.keywords.value as string;
                    inputField.keyword[keyword] = true;
                  } else {
                    length = spec.from && spec.to ? 
                      (Number(spec.to.value) - Number(spec.from.value) + 1) : 0;
                    type = spec.internalDataFormat ? 
                      getRpgDataType(spec.internalDataFormat.value.toString()) : `char`;
                    inputField.keyword[type] = String(length);
                  }

                  // If there are decimal numbers, it's a number
                  if (spec.decimalPositions) {
                    delete inputField.keyword[type];
                    inputField.keyword.decimals = String(spec.decimalPositions.value);
                    inputField.keyword[`packed`] = String(length);
                  }

                  lastStruct.subItems.push(inputField);
                  lastStruct.range.end = lineI;
                }
              } else if (spec.subtype === `record`) {
                if (spec.described === `structure` && spec.name) {
                  const inputSpec = createDeclaration(
                    `struct`,
                    String(spec.name.value),
                    fileUri,
                    lineI,
                    index,
                    line.length
                  );

                  cache.addSymbol(inputSpec);
                } else if (spec.described === `constant`) {
                  const constantEntry = spec as InputConstantEntry;
                  if (constantEntry.constantName) {
                    const constantSpec = createDeclaration(
                      `constant`,
                      constantEntry.constantName.value as string,
                      fileUri,
                      lineI,
                      index,
                      line.length
                    );
                    
                    cache.addSymbol(constantSpec);
                  }
                }
              }
              break;
          }
        }

        index += line.length + EOL.length;
      }
    };

    await parseContent(fileUri, baseContent);
    
    return cache;
  }

  /**
   * Clear table cache (optional - for compatibility with ILE parser interface)
   */
  clearTableCache?(): void {
    // OPM parser doesn't cache tables internally
  }

  /**
   * Clear parsed cache (optional - for compatibility with ILE parser interface)
   */
  clearParsedCache?(path: string): void {
    // OPM parser doesn't maintain a parse cache
  }
}
