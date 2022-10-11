const Declaration = require(`./declaration`);

const inds = [...Array(98).keys(), `LR`, `KL`].map(val => `IN${val.toString().padStart(2, `0`)}`).map(ind => {
  const indDef = new Declaration(`variable`);
  indDef.name = ind;
  indDef.keywords = [`IND`];
  return indDef;
});

module.exports = class Cache {
  /**
   * 
   * @param {{subroutines?: Declaration[], procedures?: Declaration[], files?: Declaration[], variables?: Declaration[], structs?: Declaration[], constants?: Declaration[], indicators?: Declaration[]}} cache 
   */
  constructor(cache = {}) {
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
    this.indicators = cache.indicators || [...inds];
  }

  /**
   * @param {Cache} [second] 
   * @returns {Cache} merged caches
   */
  merge(second) {
    if (second) {
      return new Cache({
        subroutines: [...this.subroutines, ...second.subroutines],
        procedures: [...this.procedures, ...second.procedures],
        variables: [...this.variables, ...second.variables],
        files: [...this.files, ...second.files],
        structs: [...this.structs, ...second.structs],
        constants: [...this.constants, ...second.constants],
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
   * 
   * @param {string} name 
   * @returns {Declaration}
   */
  find(name) {
    name = name.toUpperCase();
    const fileStructs = this.files.map(file => file.subItems).flat();
    const allStructs = [...fileStructs, ...this.structs];

    const possibles = [
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
    [...this.constants, ...this.files, ...this.procedures, ...this.subroutines, ...this.variables, ...this.structs].forEach(def => {
      def.references = [];
    });

    this.procedures.forEach(proc => {
      if (proc.scope) {
        proc.scope.clearReferences()
      }
    });

    const fileStructs = this.files.map(file => file.subItems).flat();
    const allStructs = [...fileStructs, ...this.structs];

    allStructs.forEach(struct => {
      struct.subItems.forEach(sub => sub.references = [])
    });
  }
}