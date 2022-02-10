
const Cache = require(`./cache`);
const ContentRange = require(`./ContentRange`);

module.exports = class Declaration {
  /**
   * 
   * @param {"procedure"|"subroutine"|"struct"|"subitem"|"variable"|"constant"} type 
   */
  constructor(type) {
    this.type = type;
    this.name = ``;

    /** @type {string[]} */
    this.keywords = [];
    
    this.description = ``;

    /** @type {{tag: string, content: string}[]} */
    this.tags = [];

    // Used for everything but procedures and subroutines
    /** @type {{path: string, line: number}} */
    this.position = undefined;

    /** @type {ContentRange[]} */
    this.references = [];

    // Not used in subitem:
    /** @type {Declaration[]} */
    this.subItems = [];

    // Only used in procedure
    this.readParms = false;

    // Used for subroutines and procedures
    /** @type {{start?: number, end?: number}} */
    this.range = {
      start: null,
      end: null
    }

    // Only used in procedures
    /** @type {Cache|undefined} */
    this.scope = undefined;
  }
}