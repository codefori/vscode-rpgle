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

export type RpgleVariableType = `char` | `varchar` | `ucs2` | `varucs2` | `int` | `uns` | `packed` | `zoned`  | `float` | `ind` | `date` | `time` | `timestamp` | `pointer` | `graph` | `vargraph`;
const validTypes: RpgleVariableType[] = [`char`, `varchar`, `ucs2`, `varucs2`, `int`, `uns`, `packed`, `zoned`, `float`, `ind`, `date`, `time`, `timestamp`, `pointer`, `graph`, `vargraph`];

export interface RpgleTypeDetail {
  type?: { name: RpgleVariableType, value?: string };
  reference?: Declaration;
}

export default class Cache {
  keyword: Keywords;
  parameters: Declaration[];
  symbols: Declaration[];
  sqlReferences: Declaration[];
  includes: IncludeStatement[];

  constructor(cache: CacheProps = {}) {
    this.keyword = {};
    this.parameters = cache.parameters || [];
    this.symbols = cache.symbols || [...newInds()];
    this.sqlReferences = cache.sqlReferences || [];
    this.includes = cache.includes || [];
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

  /**
   * @param {Cache} [second] 
   * @returns {Cache} merged caches
   */
  merge(second) {
    if (second) {
      return new Cache({
        parameters: [...this.parameters, ...second.parameters],
        symbols: [...this.symbols, ...second.symbols],
        sqlReferences: [...this.sqlReferences, ...second.sqlReferences],
      });
    } else {
      return this;
    }
  }

  /**
   * @returns {String[]}
   */
  getNames() {
    const names = new Set<string>();

    this.parameters.forEach(def => names.add(def.name));
    this.symbols.forEach(def => names.add(def.name));

    return Array.from(names);
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

  find(name: string, includeProcedure?: string): Declaration | undefined {
    name = name.toUpperCase();

    const fileStructs = this.symbols.filter(d => d.type === `file`).flatMap(file => file.subItems);

    const searchIn = [
      this.parameters,
      this.symbols.filter(d => d.type !== `procedure`),
      this.symbols.filter(d => d.type === `procedure` && !d.prototype),
      this.symbols.filter(d => d.type === `procedure` && d.prototype),
    ];

    for (const list of searchIn) {
      const found = list.find(def => def.name.toUpperCase() === name);
      if (found) return found;
    }

    if (includeProcedure) {
      const procedureScope = this.symbols.find(proc => proc.scope && proc.name.toUpperCase() === includeProcedure);
      if (procedureScope) {
        const found = procedureScope.scope.find(name);
        if (found) return found;
      }
    }

    const allStructs = [...fileStructs, ...this.symbols.filter(d => d.type === `struct`)];

    if (allStructs.length > 0) {
      for (const def of allStructs) {
        if (def.keyword[`QUALIFIED`] !== true) {
          const subItem = def.subItems.find(sub => sub.name.toUpperCase() === name);
          if (subItem) return subItem;
        }
      }
    }

    return;
  }

  public findProcedurebyLine(lineNumber: number): Declaration | undefined {
    return this.symbols.find(proc => proc.scope && lineNumber >= proc.range.start && lineNumber <= proc.range.end);
  }

  findDefinition(lineNumber, word) {
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

  /**
   * Move all procedure subItems (the paramaters) into the cache
   */
  fixProcedures() {
    const procedures = this.symbols.filter(d => d.type === `procedure` && d.scope && d.subItems.length > 0);
    if (procedures.length > 0) {
      procedures.forEach(proc => {
        if (proc.scope && proc.subItems.length > 0) {
          proc.scope.parameters = [...proc.subItems];
          proc.scope.fixProcedures();
        }
      })
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