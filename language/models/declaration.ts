
import { Keywords, Reference } from "../parserTypes";
import { IRangeWithLine } from "../types";
import Cache from "./cache";

type DeclarationType = "procedure"|"subroutine"|"file"|"struct"|"subitem"|"variable"|"constant"|"tag";

export default class Declaration {
  name: string = ``;
  keyword: Keywords = {};
  tags: {tag: string, content: string}[] = [];
  position: {path: string, range: IRangeWithLine};
  references: Reference[] = [];
  subItems: Declaration[] = [];
  readParms: boolean = false;
  range: {start: number|null, end: number|null} = {start: null, end: null};
  scope: Cache|undefined;
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
}