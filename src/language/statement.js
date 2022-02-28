
/** @type {{name: string, match: {type: string, match?: function}[], becomes: {type: string}}[]} */
const commonMatchers = [
  {
    name: `FORMAT_STATEMEMT`,
    match: [
      { type: `asterisk` },
      { type: `asterisk` },
      { type: `word` },
    ],
    becomes: {
      type: `format`,
    }
  },
  {
    name: `DIRECTIVE`,
    match: [
      { type: `divide` },
      { 
        type: `word`, 
        match: (word) => [`TITLE`, `EJECT`, `SPACE`, `COPY`, `INCLUDE`, `SET`, `RESTORE`, `OVERLOAD`, `DEFINE`, `UNDEFINED`, `IF`, `ELSE`, `ELSEIF`, `ENDIF`, `EOF`].includes(word.toUpperCase())
      },
    ],
    becomes: {
      type: `directive`
    }
  },
  {
    name: `IS_NUMBER`,
    match: [
      { type: `number` },
      { type: `dot` },
      { type: `number` }
    ],
    becomes: {
      type: `number`
    }
  },
  {
    name: `IS_SPECIAL`,
    match: [
      { type: `asterisk` },
      { type: `word`, match: (word) => 
        [ `CTDATA`, `BLANK`, `BLANKS`, `ZERO`, `ZEROS`, `ON`, `OFF`, `NULL`, `ISO`, `MDY`, `DMY`, `EUR`, `YMD`, `USA`, `SECONDS`, `S`, `MINUTES`, `MN`, `HOURS`, `H`, `DAYS`, `D`, `MONTHS`, `M`, `YEARS`, `Y`, `HIVAL`, `END`, `LOVAL`, `START`, `N`, `OMIT`, `STRING`].includes(word.toUpperCase())
      }
    ],
    becomes: {
      type: `special`
    }
  },
  {
    name: `HEX`,
    match: [
      { type: `word`, match: (word) => word.toUpperCase() === `x` },
      { type: `string` }
    ],
    becomes: {
      type: `hex`
    }
  },
  {
    name: `TIME`,
    match: [
      { type: `word`, match: (word) => word.toUpperCase() === `t` },
      { type: `string` }
    ],
    becomes: {
      type: `hex`
    }
  },
  {
    name: `DATE`,
    match: [
      { type: `word`, match: (word) => word.toUpperCase() === `d` },
      { type: `string` }
    ],
    becomes: {
      type: `hex`
    }
  },
  {
    name: `DECLARE`,
    match: [
      { type: `word`, match: (word) => word.toUpperCase() === `DCL` },
      { type: `minus` },
      { type: `word` },
    ],
    becomes: {
      type: `declare`
    }
  },
  {
    name: `END`,
    match: [
      { type: `word`, match: (word) => word.toUpperCase() === `END` },
      { type: `minus` },
      { type: `word` },
    ],
    becomes: {
      type: `end`
    }
  },
  {
    name: `DECLARE_SUB`,
    match: [
      { type: `word`, match: (word) => word.toUpperCase() === `BEGSR` },
    ],
    becomes: {
      type: `declare`
    }
  },
  {
    name: `END_SUB`,
    match: [
      { type: `word`, match: (word) => word.toUpperCase() === `ENDSR` },
    ],
    becomes: {
      type: `end`
    }
  },
  {
    name: `FOR-EACH`,
    match: [
      { type: `word`, match: (word) => word.toUpperCase() === `FOR` },
      { type: `minus` },
      { type: `word`, match: (word) => word.toUpperCase() === `EACH` },
    ],
    becomes: {
      type: `word`
    }
  },
  {
    name: `EVAL-CORR`,
    match: [
      { type: `word`, match: (word) => word.toUpperCase() === `EVAL` },
      { type: `minus` },
      { type: `word`, match: (word) => word.toUpperCase() === `CORR` },
    ],
    becomes: {
      type: `word`
    }
  },
  {
    name: `CTL-OPT`,
    match: [
      { type: `word`, match: (word) => word.toUpperCase() === `CTL` },
      { type: `minus` },
      { type: `word`, match: (word) => word.toUpperCase() === `OPT` },
    ],
    becomes: {
      type: `declare`
    }
  },
  {
    name: `BIF`,
    match: [
      { type: `percent` },
      { type: `word` }
    ],
    becomes: {
      type: `builtin`
    }
  }
]

const splitParts = [`%`, `.`, `(`, `)`, `+`, `-`, `*`, `/`, `=`, `:`, ` `];
const types = {
  '%': `percent`,
  '.': `dot`,
  '(': `openbracket`,
  ')': `closebracket`,
  '+': `plus`,
  '-': `minus`,
  '/': `divide`,
  '*': `asterisk`,
  '=': `equal`,
  ':': `seperator`,
  ';': `end`
}

module.exports = class Statement {

  /**
   * @param {string} statement 
   * @returns {{value?: string, block?: object[], type: string, position: number}[]}
   */
  static parseStatement(statement) {
    let inString = false;

    let result = [];

    let startsAt = 0;
    let currentText = ``;

    for (let i = 0; i < statement.length; i++) {
      if (inString && statement[i] !== `'`) {
        currentText += statement[i];
      } else {
        switch (statement[i]) {
        case `'`:
          if (inString) {
            currentText += statement[i];
            result.push({value: currentText, type: `string`, position: startsAt});
            currentText = ``;
          } else {
            startsAt = i;
            currentText += statement[i];
          }

          inString = !inString;
          break;
        default:
          if (splitParts.includes(statement[i]) && inString === false) {
            if (currentText.trim() !== ``) {
              result.push({value: currentText, type: `word`, position: startsAt});
              currentText = ``;
            }

            if (statement[i] !== ` `) {
              result.push({value: statement[i], type: types[statement[i]], position: i});
            }

            startsAt = i + 1;

          } else {
            currentText += statement[i];
          }
          break;
        }
      }
    }

    if (currentText.trim() !== ``) {
      result.push({value: currentText, type: `word`, position: startsAt});
      currentText = ``;
    }

    result = this.fixStatement(result);
    //result = this.createBlocks(result);

    return result;
  }

  /**
   * @param {{value: string, type: string, position: number}[]} statement 
   */
  static fixStatement(statement) {
    for (let i = 0; i < statement.length; i++) {
      for (let y = 0; y < commonMatchers.length; y++) {
        const type = commonMatchers[y];
        let goodMatch = true;

        for (let x = 0; x < type.match.length; x++) {
          const match = type.match[x];
          
          if (statement[i+x]) {
            if (statement[i+x].type === match.type) {
              if (match.match) {
                if (match.match(statement[i+x].value)) {
                  goodMatch = true;
                } else {
                  goodMatch = false;
                  break;
                }
              } else {
                goodMatch = true;
              }
            } else {
              goodMatch = false;
              break;
            }
          } else {
            goodMatch = false;
          }
        }

        if (goodMatch) {
          const value = statement.slice(i, i + type.match.length).map(x => x.value).join(``);
          statement.splice(i, type.match.length, {
            ...type.becomes,
            value,
            position: statement[i].position
          });

          break;
        }
      }
    }

    return statement;
  }

  static createBlocks(statement) {
    let start = 0;
    let level = 0;

    for (let i = 0; i < statement.length; i++) {
      switch (statement[i].type) {
      case `openbracket`:
        if (level === 0) {
          start = i;
        }
        level++;
        break;
      case `closebracket`:
        level--;

        if (level === 0) {
          statement.splice(start, i - start + 1, {
            type: `block`,
            block: this.createBlocks(statement.slice(start+1, i)),
            position: statement[start].position
          });
        }
        break;
      }
    }

    return statement;
  }
}