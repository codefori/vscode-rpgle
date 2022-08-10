
const Cache = require(`./cache`);

module.exports = class Declaration {
  /**
   * 
   * @param {"procedure"|"subroutine"|"file"|"struct"|"subitem"|"variable"|"constant"} type 
   */
  constructor(type) {
    this.type = type;
    this.name = ``;

    /** @type {string[]} */
    this.keywords = [];

    /** @type {Keywords} */
    this.keyword = {};
    
    this.description = ``;

    /** @type {{tag: string, content: string}[]} */
    this.tags = [];

    // Used for everything but procedures and subroutines
    /** @type {{path: string, line: number}} */
    this.position = undefined;

    /** @type {IssueRange[]} */
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

  clone() {
    const clone = new Declaration(this.type);
    clone.name = this.name;
    clone.keywords = [...this.keywords];
    clone.description = this.description;
    clone.tags = this.tags;

    if (this.position) {
      clone.position = {
        path: this.position.path,
        line: this.position.line
      };
    }

    clone.subItems = this.subItems.map(subItem => subItem.clone());
    
    clone.range = {
      start: this.range.start,
      end: this.range.end
    };

    //clone.references = this.references;
    //clone.readParms = this.readParms;
    //clone.scope = this.scope;
    return clone;
  }
}