export class Position {
	line: number;
	character: number;
  constructor(line: number, character: number) {
    this.line = line;
    this.character = character;
  }
}

export class Range {
	constructor(public start: Position, public end: Position) {}

	static create(startLine: number, startChar: number, endLine: number, endChar: number) {
		return new this(
			new Position(startLine, startChar),
			new Position(endLine, endChar)
		);
	}
}