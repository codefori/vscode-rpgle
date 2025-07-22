
import { Keywords, Reference } from "../parserTypes";
import { IRangeWithLine } from "../types";
import Cache from "./cache";

export type DeclarationType = "parameter"|"procedure"|"subroutine"|"file"|"struct"|"subitem"|"variable"|"constant"|"tag"|"indicator";

export default class Declaration {
  name: string = ``;
  keyword: Keywords = {};
  tags: {tag: string, content: string}[] = [];
  /**
   * Position is the location of the declaration in the source file, by offset and line number.
   */
  position: {path: string, range: IRangeWithLine};
  references: Reference[] = [];
  subItems: Declaration[] = [];
  readParms: boolean = false;
  /**
   * This range property is solely line numbers.
   */
  range: {start: number|null, end: number|null} = {start: null, end: null};
  scope?: Cache;
  constructor(public type: DeclarationType) {}

  clone() {
    const clone = new Declaration(this.type);
    clone.name = this.name;
    clone.keyword = this.keyword;
    clone.tags = this.tags;

    if (this.position) {
      clone.position = {...this.position};
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

  get description() {
    return this.tags.find(tag => tag.tag === `description`)?.content || ``;
  }

  get prototype() {
    return this.scope === undefined;
  }
}