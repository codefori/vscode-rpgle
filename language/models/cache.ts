import { re } from "../../out/extension";
import { CacheProps, IncludeStatement, Keywords } from "../parserTypes";
import { IRange } from "../types";
import Declaration from "./declaration";

const newInds = () => {
  return [...Array(98).keys(), `LR`, `KL`].map(val => `IN${val.toString().padStart(2, `0`)}`).map(ind => {
    const indDef = new Declaration(`variable`);
    indDef.name = ind;
    indDef.keyword = { IND: true };
    return indDef;
  })
};

export type RpgleVariableType = `char` | `varchar` | `int` | `uns` | `packed` | `zoned` | `ind` | `date` | `time` | `timestamp` | `pointer` | `float` | `graph`;
const validTypes: RpgleVariableType[] = [`char`, `varchar`, `int`, `uns`, `packed`, `zoned`, `ind`, `date`, `time`, `timestamp`, `pointer`, `float`, `graph`];

export interface RpgleTypeDetail {
  type?: { name: RpgleVariableType, value?: string };
  reference?: Declaration;
}

export default class Cache {
  keyword: Keywords;
  parameters: Declaration[];
  subroutines: Declaration[];
  procedures: Declaration[];
  files: Declaration[];
  variables: Declaration[];
  structs: Declaration[];
  constants: Declaration[];
  sqlReferences: Declaration[];
  indicators: Declaration[];
  tags: Declaration[];
  includes: IncludeStatement[];

  constructor(cache: CacheProps = {}) {
    this.keyword = {};
    this.parameters = cache.parameters || [];
    this.subroutines = cache.subroutines || [];
    this.procedures = cache.procedures || [];
    this.files = cache.files || [];
    this.variables = cache.variables || [];
    this.structs = cache.structs || [];
    this.constants = cache.constants || [];
    this.sqlReferences = cache.sqlReferences || [];
    this.indicators = cache.indicators || [...newInds()];
    this.includes = cache.includes || [];
    this.tags = cache.tags || [];
  }

  /**
   * @param {Cache} [second] 
   * @returns {Cache} merged caches
   */
  merge(second) {
    if (second) {
      return new Cache({
        parameters: [...this.parameters, ...second.parameters],
        subroutines: [...this.subroutines, ...second.subroutines],
        procedures: [...this.procedures, ...second.procedures],
        variables: [...this.variables, ...second.variables],
        files: [...this.files, ...second.files],
        structs: [...this.structs, ...second.structs],
        constants: [...this.constants, ...second.constants],
        sqlReferences: [...this.sqlReferences, ...second.sqlReferences],
        indicators: [...this.indicators, ...second.indicators],
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
    this.constants.forEach(def => names.add(def.name));
    this.procedures.forEach(def => names.add(def.name));
    this.files.forEach(def => names.add(def.name));
    this.files.forEach(file => file.subItems.forEach(sub => names.add(sub.name)));
    this.subroutines.forEach(def => names.add(def.name));
    this.variables.forEach(def => names.add(def.name));
    this.structs.forEach(def => names.add(def.name));
    this.tags.forEach(def => names.add(def.name));

    return Array.from(names);
  }

  /**
   * Returns 0-indexed line number where definition block starts for current scope
   * @param {string} fsPath Path to check
   * @returns {number} Line number
   */
  getDefinitionBlockEnd(fsPath) {
    const lasts = [
      this.procedures.filter(d => d.position.path === fsPath && d.keyword[`EXTPROC`] !== undefined).pop(),
      this.structs.filter(d => d.position.path === fsPath).pop(),
      this.variables.filter(d => d.position.path === fsPath).pop(),
      this.constants.filter(d => d.position.path === fsPath).pop(),
      this.files.filter(d => d.position.path === fsPath).pop()
    ].filter(d => d !== undefined);

    const lines = lasts.map(d => d.range && d.range.end ? d.range.end : d.position.range.line).sort((a, b) => b - a);

    return (lines.length >= 1 ? lines[0] : 0);
  }

  find(name: string, includeProcedure?: string): Declaration | undefined {
    name = name.toUpperCase();

    const fileStructs = this.files.flatMap(file => file.subItems);
    const allStructs = [...fileStructs, ...this.structs];

    const searchIn = [
      this.parameters,
      this.constants,
      this.procedures,
      this.files,
      allStructs,
      this.subroutines,
      this.variables,
      this.indicators,
      this.tags
    ];

    for (const list of searchIn) {
      const found = list.find(def => def.name.toUpperCase() === name);
      if (found) return found;
    }

    if (includeProcedure) {
      const procedureScope = this.procedures.find(proc => proc.name.toUpperCase() === includeProcedure);
      if (procedureScope) {
        const found = procedureScope.scope.find(name);
        if (found) return found;
      }
    }

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

  findDefinition(lineNumber, word) {
    // If they're typing inside of a procedure, let's get the stuff from there too
    const currentProcedure = this.procedures.find(proc => lineNumber >= proc.range.start && lineNumber <= proc.range.end);

    if (currentProcedure && currentProcedure.scope) {
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
    const currentProcedure = this.procedures.find(proc => lineNumber >= proc.range.start && lineNumber <= proc.range.end);

    if (currentProcedure && currentProcedure.scope) {
      const localDef = currentProcedure.scope.constants.find(def => def.keyword[`CONST`] === value);

      if (localDef) {
        return localDef;
      }
    }

    const globalDef = this.constants.find(def => def.keyword[`CONST`] === value);

    if (globalDef) {
      return globalDef;
    }
  }

  /**
   * Move all procedure subItems (the paramaters) into the cache
   */
  fixProcedures() {
    if (this.procedures.length > 0) {
      this.procedures.forEach(proc => {
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
      reference = this.structs.find(s => s.name.toUpperCase() === refName);

      return { reference };
    } else if (typeof keywords[`LIKE`] === `string`) {
      refName = (keywords[`LIKE`] as string).toUpperCase();
      reference = this.variables.find(s => s.name.toUpperCase() === refName);

      if (!reference) {
        // Like does technically work on structs too, so let's check those
        // Though it's recommend to use LIKEDS in modern code
        reference = this.structs.find(s => s.name.toUpperCase() === refName);
      }

      if (!reference) {
        // LIKE can also be used on procedures, and it will return the return type of the procedure
        reference = this.procedures.find(s => s.name.toUpperCase() === refName);
        if (reference) {
          return this.resolveType(reference);
        }
      }

      return { reference }
    } else {
      const type = Object.keys(keywords).find(key => validTypes.includes(key.toLowerCase() as RpgleVariableType));
      if (type) {
        return { type: { name: (type.toLowerCase() as RpgleVariableType), value: keywords[type] as string } };
      }
    }

    return {};
  }

  static referenceByOffset(baseUri: string, scope: Cache, offset: number): Declaration | undefined {
    const props: (keyof Cache)[] = [`parameters`, `subroutines`, `procedures`, `files`, `variables`, `structs`, `constants`, `indicators`, `tags`];
    for (const prop of props) {
      const list = scope[prop] as unknown as Declaration[];
      for (const def of list) {
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
}