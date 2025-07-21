import { CacheProps, IncludeStatement, Keywords } from "../parserTypes";
import { IRange } from "../types";
import Declaration, { DeclarationType } from "./declaration";

const DEFAULT_INDICATORS = [
  ...Array(98).keys(), 
  `LR`, `KL`, `MR`,
  `L1`, `L2`, `L3`, `L4`, `L5`, `L6`, `L7`, `L8`, `L9`,
  `U1`, `U2`, `U3`, `U4`, `U5`, `U6`, `U7`, `U8`,
  `OA`, `OB`, `OC`, `OD`, `OE`, `OF`, `OG`, `OV`,
  `KA`, `KB`, `KC`, `KD`, `KE`, `KF`, `KG`, `KH`, `KI`, `KJ`, `KK`, `KL`, `KM`, `KN`,
  `KP`, `KQ`, `KR`, `KS`, `KT`, `KU`, `KV`, `KW`, `KX`, `KY`,
  `H1`, `H2`, `H3`, `H4`, `H5`, `H6`, `H7`, `H8`, `H9`
];

const newInds = () => {
  return DEFAULT_INDICATORS.map(val => `IN${val.toString().padStart(2, `0`)}`).map(ind => {
    const indDef = new Declaration(`indicator`);
    indDef.name = ind;
    indDef.keyword = { IND: true };
    return indDef;
  })
};

export type SymbolRegister = Map<string, Declaration[]>;

export type RpgleVariableType = `char` | `varchar` | `ucs2` | `varucs2` | `int` | `uns` | `packed` | `zoned`  | `float` | `ind` | `date` | `time` | `timestamp` | `pointer` | `graph` | `vargraph`;
const validTypes: RpgleVariableType[] = [`char`, `varchar`, `ucs2`, `varucs2`, `int`, `uns`, `packed`, `zoned`, `float`, `ind`, `date`, `time`, `timestamp`, `pointer`, `graph`, `vargraph`];

export interface RpgleTypeDetail {
  type?: { name: RpgleVariableType, value?: string };
  reference?: Declaration;
}

export default class Cache {
  keyword: Keywords;
  sqlReferences: Declaration[];
  includes: IncludeStatement[];
  private symbolRegister: SymbolRegister;

  constructor(cache: CacheProps = {}, isProcedure: boolean = false) {
    this.keyword = {};
    // this.symbols = cache.symbols || [...newInds()];

    if (isProcedure) {
      this.symbolRegister = cache.symbolRegister || new Map();
    } else {
      this.symbolRegister = cache.symbolRegister || new Map();

      newInds().forEach(ind => {
        this.addSymbol(ind);
      });
    }


    this.sqlReferences = cache.sqlReferences || [];
    this.includes = cache.includes || [];
  }

  private symbolCache: Declaration[] | undefined;

  get symbols() {
    if (this.symbolCache) return this.symbolCache;

    this.symbolCache = Array.from(this.symbolRegister.values()).flat(1);

    return this.symbolCache;
  }

  get subroutines() {
    return this.symbols.filter(s => s.type === `subroutine`);
  }

  get procedures() {
    return this.symbols.filter(s => s.type === `procedure`);
  }

  get files() {
    return this.symbols.filter(s => s.type === `file`);
  }

  get constants() {
    return this.symbols.filter(s => s.type === `constant`);
  }

  get variables() {
    return this.symbols.filter(s => s.type === `variable`);
  }

  get structs() {
    return this.symbols.filter(s => s.type === `struct`);
  }

  get indicators() {
    return this.symbols.filter(s => s.type === `indicator`);
  }

  get tags() {
    return this.symbols.filter(s => s.type === `tag`);
  }

  get parameters() {
    return this.symbols.filter(s => s.type === `parameter`);
  }
  
  addSymbol(symbol: Declaration) {
    const name = symbol.name.toUpperCase();
    if (this.symbolRegister.has(name)) {
      // If the symbol already exists, we can merge it
      const existing = this.symbolRegister.get(name);
      existing.push(symbol);
    } else {
      this.symbolRegister.set(name, [symbol]);
    }

    this.symbolCache = undefined;
  }

  /**
   * Returns 0-indexed line number where definition block starts for current scope
   * @param {string} fsPath Path to check
   * @returns {number} Line number
   */
  getDefinitionBlockEnd(fsPath: string) {
    const checkTypes: DeclarationType[] = [`procedure`, `struct`, `file`, `variable`, `constant`];
    const lasts = [
      this.symbols.filter(d => checkTypes.includes(d.type) && d.position.path === fsPath && d.keyword[`EXTPROC`] !== undefined).pop(),
    ].filter(d => d !== undefined);

    const lines = lasts.map(d => d.range && d.range.end ? d.range.end : d.position.range.line).sort((a, b) => b - a);

    return (lines.length >= 1 ? lines[0] : 0);
  }

  find(name: string, specificType?: DeclarationType): Declaration | undefined {
    name = name.toUpperCase();

    const existing = this.symbolRegister.get(name);
    if (existing) {
      const symbols = Array.isArray(existing) ? existing : [existing];
      // Loop through them all in case of duplicate names with different types
      for (let i = symbols.length - 1; i >= 0; i--) {
        // Scan symbols in reverse to determine the most recently defined
        const symbol = symbols[i];
        if (specificType && symbol.type !== specificType) {
          return undefined;
        }

        if (symbol.name.toUpperCase() === name) {
          return symbol;
        }
      }
    }

    // Additional logic to check for subItems in symbols
    const symbolsWithSubs = [...this.structs, ...this.files];

    for (const struct of symbolsWithSubs) {
      if (struct.keyword[`QUALIFIED`] !== true) {
        // If the symbol is qualified, we need to check the subItems
        const subItem = struct.subItems.find(sub => sub.name.toUpperCase() === name);
        if (subItem) return subItem;

        if (struct.type === `file`) {
          // If it's a file, we also need to check the subItems of the file's recordformats
          for (const subFile of struct.subItems) {
            const subSubItem = subFile.subItems.find(sub => sub.name.toUpperCase() === name);
            if (subSubItem) return subSubItem;
          }
        }
      }
    }

    return;
  }

  findAll(name: string): Declaration[] {
    name = name.toUpperCase();
    const symbols = this.symbolRegister.get(name);
    if (symbols) {
      return Array.isArray(symbols) ? symbols : [symbols];
    }

    return [];
  }

  public findProcedurebyLine(lineNumber: number): Declaration | undefined {
    return this.procedures.find(proc => proc.scope && lineNumber >= proc.range.start && lineNumber <= proc.range.end);
  }

  findDefinition(lineNumber: number, word: string) {
    // If they're typing inside of a procedure, let's get the stuff from there too
    const currentProcedure = this.findProcedurebyLine(lineNumber);

    if (currentProcedure) {
      const localDef = currentProcedure.scope.find(word);

      if (localDef) {
        return localDef;
      }
    }

    const globalDef = this.find(word);

    if (globalDef) {
      return globalDef;
    }
  }

  findConstByValue(lineNumber: number, value: string) {
    // If they're typing inside of a procedure, let's get the stuff from there too
    const currentProcedure = this.findProcedurebyLine(lineNumber);

    if (currentProcedure) {
      const localDef = currentProcedure.scope.symbols.find(def => def.keyword[`CONST`] === value);

      if (localDef) {
        return localDef;
      }
    }

    const globalDef = this.symbols.find(def => def.keyword[`CONST`] === value);

    if (globalDef) {
      return globalDef;
    }
  }

  referencesInRange(baseUri: string, range: IRange): { dec: Declaration, refs: IRange[] }[] {
    let list: { dec: Declaration, refs: IRange[] }[] = [];

    for (let i = range.start; i <= range.end; i++) {
      const ref = Cache.referenceByOffset(baseUri, this, i);
      if (ref) {
        // No duplicates allowed
        if (list.some(item => item.dec.name === ref.name)) continue;

        list.push({
          dec: ref,
          refs: ref.references.filter(r => r.offset.start >= range.start && r.offset.end <= range.end).map(r => r.offset)
        })
      };
    }

    return list;
  }

  /**
   * Typically used to resolve the type of a procedure correctly.
   */
  resolveType(def: Declaration): RpgleTypeDetail {
    const keywords = def.keyword;
    let refName: string;
    let reference: Declaration | undefined;

    if (typeof keywords[`LIKEDS`] === `string`) {
      refName = (keywords[`LIKEDS`] as string).toUpperCase();
      reference = this.symbols.find(s => s.name.toUpperCase() === refName);

      return { reference };
    } else if (typeof keywords[`LIKE`] === `string`) {
      refName = (keywords[`LIKE`] as string).toUpperCase();
      reference = this.symbols.find(s => s.name.toUpperCase() === refName);

      if (reference && reference.type === `procedure`) {
        // If the LIKE is a procedure, we need to resolve the return type of the procedure
        return this.resolveType(reference);
      }

      return { reference };
    } else {
      const type = Object.keys(keywords).find(key => validTypes.includes(key.toLowerCase() as RpgleVariableType));
      if (type) {
        return { type: { name: (type.toLowerCase() as RpgleVariableType), value: keywords[type] as string } };
      }
    }

    return {};
  }

  static referenceByOffset(baseUri: string, scope: Cache, offset: number): Declaration | undefined {  
    for (const def of scope.symbols) {
      let possibleRef: boolean;

      // Search top level
      possibleRef = def.references.some(r => r.uri === baseUri && offset >= r.offset.start && offset <= r.offset.end);
      if (possibleRef) return def;

      // Search any subitems
      if (def.subItems.length > 0) {
        for (const subItem of def.subItems) {
          possibleRef = subItem.references.some(r => r.uri === baseUri && offset >= r.offset.start && offset <= r.offset.end);
          if (possibleRef) return subItem;
        }
      }

      // Search scope if any
      if (def.scope) {
        const inScope = Cache.referenceByOffset(baseUri, def.scope, offset);
        if (inScope) return inScope;
      }
    }
  }
}