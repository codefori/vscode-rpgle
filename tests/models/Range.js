const Position = require(`./Position`);

module.exports = class Range {
  constructor() {
    switch (arguments.length) {
    case 4:
      this.start = new Position(arguments[0], arguments[1]);
      this.end = new Position(arguments[2], arguments[3]);   
      break;
    case 2:
      this.start = arguments[0];
      this.end = arguments[1];
      break;
    }
  }
}