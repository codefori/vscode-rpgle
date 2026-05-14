
export interface IRange {
  start: number;
  end: number;
}

export interface IRangeWithLine extends IRange {
  line: number;
}

export interface Token {
  value?: string;
  block?: Token[];
  type: string;
  range: IRangeWithLine;
}