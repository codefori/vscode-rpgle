"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.Range = exports.Position = void 0;
class Position {
    constructor(line, character) {
        this.line = line;
        this.character = character;
    }
}
exports.Position = Position;
class Range {
    constructor(start, end) {
        this.start = start;
        this.end = end;
    }
    static create(startLine, startChar, endLine, endChar) {
        return new this(new Position(startLine, startChar), new Position(endLine, endChar));
    }
}
exports.Range = Range;
//# sourceMappingURL=DataPoints.js.map