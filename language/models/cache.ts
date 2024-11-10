import { CacheProps, IncludeStatement, Keywords, Offset } from "../parserTypes";
import Declaration from "./declaration";

const newInds = () => {
  return [...Array(98).keys(), `LR`, `KL`].map(val => `IN${val.toString().padStart(2, `0`)}`).map(ind => {
    const indDef = new Declaration(`variable`);
    indDef.name = ind;
    indDef.keywords = [`IND`];
    return indDef;
  })
};

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
  includes: IncludeStatement[];

  constructor(cache: CacheProps = {}) {
    /** @type {import("../parserTypes").Keywords} */
    this.keyword = {};

    /** @type {Declaration[]} */
    this.parameters = cache.parameters || [];

    /** @type {Declaration[]} */
    this.subroutines = cache.subroutines || [];

    /** @type {Declaration[]} */
    this.procedures = cache.procedures || [];

    /** @type {Declaration[]} */
    this.files = cache.files || [];

    /** @type {Declaration[]} */
    this.variables = cache.variables || [];

    /** @type {Declaration[]} */
    this.structs = cache.structs || [];

    /** @type {Declaration[]} */
    this.constants = cache.constants || [];

    /** @type {Declaration[]} */
    this.sqlReferences = cache.sqlReferences || [];

    /** @type {Declaration[]} */
    this.indicators = cache.indicators || [...newInds()];

    /** @type {import("../parserTypes").IncludeStatement[]} */
    this.includes = cache.includes || [];
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
    const fileStructNames = this.files.map(file => file.subItems.map(sub => sub.name)).flat();
    return [
      ...this.parameters.map(def => def.name),
      ...this.constants.map(def => def.name),
      ...this.procedures.map(def => def.name),
      ...this.files.map(def => def.name),
      ...fileStructNames,
      ...this.subroutines.map(def => def.name),
      ...this.variables.map(def => def.name),
      ...this.structs.map(def => def.name),
    ].filter(name => name);
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

    const lines = lasts.map(d => d.range && d.range.end ? d.range.end : d.position.line).sort((a, b) => b - a);

    return (lines.length >= 1 ? lines[0] : 0);
  }

  /**
   * 
   * @param {string} name 
   * @returns {Declaration}
   */
  find(name) {
    name = name.toUpperCase();
    const fileStructs = this.files.map(file => file.subItems).flat();
    const allStructs = [...fileStructs, ...this.structs];

    const possibles = [
      ...this.parameters.filter(def => def.name.toUpperCase() === name),
      ...this.constants.filter(def => def.name.toUpperCase() === name),
      ...this.procedures.filter(def => def.name.toUpperCase() === name),
      ...this.files.filter(def => def.name.toUpperCase() === name),
      ...allStructs.filter(def => def.name.toUpperCase() === name),
      ...this.subroutines.filter(def => def.name.toUpperCase() === name),
      ...this.variables.filter(def => def.name.toUpperCase() === name),
      ...this.indicators.filter(def => def.name.toUpperCase() === name),
    ];

    if (allStructs.length > 0 && possibles.length === 0) {
      allStructs.filter(def => def.keyword[`QUALIFIED`] !== true).forEach(def => {
        possibles.push(...def.subItems.filter(sub => sub.name.toUpperCase() === name));
      });
    }

    if (possibles.length > 0) {
      return possibles[0];
    } else {
      return null;
    }
  }

  clearReferences() {
    const fileStructs = this.files.map(file => file.subItems).flat();

    [...fileStructs, ...this.parameters, ...this.constants, ...this.files, ...this.procedures, ...this.subroutines, ...this.variables, ...this.structs].forEach(def => {
      def.references = [];
      def.subItems.forEach(sub => sub.references = []);
    });

    this.procedures.forEach(proc => {
      if (proc.scope) {
        proc.scope.clearReferences();
      }
    });
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
    const upperValue = value.toUpperCase(); // Keywords are stored in uppercase

    // If they're typing inside of a procedure, let's get the stuff from there too
    const currentProcedure = this.procedures.find(proc => lineNumber >= proc.range.start && lineNumber <= proc.range.end);

    if (currentProcedure && currentProcedure.scope) {
      const localDef = currentProcedure.scope.constants.find(def => def.keyword[upperValue] === true);

      if (localDef) {
        return localDef;
      }
    }

    const globalDef = this.constants.find(def => def.keyword[upperValue] === true);

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

  referencesInRange(range: Offset): { dec: Declaration, refs: Offset[] }[] {
    let list: { dec: Declaration, refs: Offset[] }[] = [];

    for (let i = range.position; i <= range.end; i++) {
      const ref = Cache.referenceByOffset(this, i);
      if (ref) {
        // No duplicates allowed
        if (list.some(item => item.dec.name === ref.name)) continue;

        list.push({
          dec: ref,
          refs: ref.references.filter(r => r.offset.position >= range.position && r.offset.end <= range.end).map(r => r.offset)
        })
      };
    }

    return list;
  }

  static referenceByOffset(scope: Cache, offset: number): Declaration | undefined {
    const props: (keyof Cache)[] = [`parameters`, `subroutines`, `procedures`, `files`, `variables`, `structs`, `constants`, `indicators`];

    for (const prop of props) {
      const list = scope[prop] as unknown as Declaration[];
      for (const def of list) {
        let possibleRef: boolean;

        // Search top level
        possibleRef = def.references.some(r => offset >= r.offset.position && offset <= r.offset.end);
        if (possibleRef) return def;

        // Search any subitems
        if (def.subItems.length > 0) {
          for (const subItem of def.subItems) {
            possibleRef = subItem.references.some(r => offset >= r.offset.position && offset <= r.offset.end);
            if (possibleRef) return subItem;
          }
        }

        // Search scope if any
        if (def.scope) {
          const inScope = Cache.referenceByOffset(def.scope, offset);
          if (inScope) return inScope;
        }
      }
    }
  }
}