
import { Keywords, Reference } from "../parserTypes";
import { IRangeWithLine } from "../ile/types";
import Cache from "./cache";

export type DeclarationType = "parameter"|"procedure"|"subroutine"|"file"|"struct"|"subitem"|"variable"|"constant"|"tag"|"indicator"|"input"|"call"|"plist"|"klist";

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
   * Inclusive, zero-based line numbers in position.path.
   */
  range: {start: number|null, end: number|null} = {start: null, end: null};
  /** File-specific portions of a declaration that can span /COPY boundaries. */
  sourceRanges: {path: string, start: number, end: number}[] = [];
  scope?: Cache;
  constructor(public type: DeclarationType) {}

  addSourceRange(path: string, start: number, end = start) {
    let sourceRange = this.sourceRanges.find(range => range.path === path);
    if (sourceRange) {
      sourceRange.start = Math.min(sourceRange.start, start);
      sourceRange.end = Math.max(sourceRange.end, end);
    } else {
      sourceRange = {path, start, end};
      this.sourceRanges.push(sourceRange);
    }

    if (this.position?.path === path) {
      this.range.start = sourceRange.start;
      this.range.end = sourceRange.end;
    }
  }

  containsLine(path: string, line: number): boolean {
    if (this.sourceRanges.length > 0) {
      return this.sourceRanges.some(range => range.path === path && line >= range.start && line <= range.end);
    }

    return this.position?.path === path && this.range.start !== null && this.range.end !== null
      && line >= this.range.start && line <= this.range.end;
  }

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
    clone.sourceRanges = this.sourceRanges.map(range => ({...range}));

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