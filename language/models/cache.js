import Declaration from "./declaration";

const inds = [...Array(98).keys(), `LR`, `KL`].map(val => `IN${val.toString().padStart(2, `0`)}`).map(ind => {
  const indDef = new Declaration(`variable`);
  indDef.name = ind;
  indDef.keywords = [`IND`];
  return indDef;
});

export default class Cache {
  /**
   * 
   * @param {import("../parserTypes").CacheProps} cache 
   */
  constructor(cache = {}) {
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
    this.indicators = cache.indicators || [...inds];

    /** @type {import("../parserTypes").IncludeStatement[]} */
    this.includes = cache.includes || [];

    /** @type {Declaration[]} */
    this.cursors = cache.cursors || [];

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
        cursors: [...this.cursors, ...second.cursors],
        sqlReferences: [...this.sqlReferences, ...second.sqlReferences],
        indicators: [...this.indicators, ...second.indicators]
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
      ...this.cursors.map(def => def.name),
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
      this.files.filter(d => d.position.path === fsPath).pop(),
      this.cursors.filter(d => d.position.path === fsPath).pop()
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
      ...this.cursors.filter(def => def.name.toUpperCase() === name),
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
    [...this.parameters, ...this.constants, ...this.files, ...this.procedures, ...this.subroutines, ...this.variables, ...this.structs, ...this.cursors].forEach(def => {
      def.references = [];
    });

    this.procedures.forEach(proc => {
      if (proc.scope) {
        proc.scope.clearReferences();
      }
    });

    const fileStructs = this.files.map(file => file.subItems).flat();
    const allStructs = [...fileStructs, ...this.structs];

    allStructs.forEach(struct => {
      struct.subItems.forEach(sub => sub.references = []);
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
}