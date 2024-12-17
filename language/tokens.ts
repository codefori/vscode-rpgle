import { Token } from "./types";

interface Matcher {
  name: string;
  match: {
    type: string;
    match?: Function;
  }[];
  becomes: {
    type: string
  };
};

enum ReadState {
  NORMAL = "Normal",
  IN_STRING = "String",
  IN_COMMENT = "Comment"
}

const commonMatchers: Matcher[] = [
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
        match: (word) => [`TITLE`, `EJECT`, `SPACE`, `COPY`, `INCLUDE`, `SET`, `RESTORE`, `OVERLOAD`, `DEFINE`, `UNDEFINE`, `IF`, `ELSE`, `ELSEIF`, `ENDIF`, `EOF`, `CHARCOUNT`, `EXEC`, `END`].includes(word.toUpperCase())
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
      {
        type: `word`, match: (word) =>
          [`PSSR`, `CTDATA`, `BLANK`, `BLANKS`, `ZERO`, `ZEROS`, `ON`, `OFF`, `NULL`, `ISO`, `MDY`, `DMY`, `EUR`, `YMD`, `USA`, `SECONDS`, `S`, `MINUTES`, `MN`, `HOURS`, `H`, `DAYS`, `D`, `MONTHS`, `M`, `YEARS`, `Y`, `HIVAL`, `END`, `LOVAL`, `START`, `N`, `OMIT`, `STRING`, `CWIDEN`, `CONVERT`].includes(word.toUpperCase()) || word.toUpperCase().startsWith(`IN`)
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
    name: `SND-MSG`,
    match: [
      { type: `word`, match: (word) => word.toUpperCase() === `SND` },
      { type: `minus` },
      { type: `word`, match: (word) => word.toUpperCase() === `MSG` },
    ],
    becomes: {
      type: `word`
    }
  },
  {
    name: `WHEN-GROUP`,
    match: [
      { type: `word`, match: (word) => word.toUpperCase() === `WHEN` },
      { type: `minus` },
      { type: `word`, match: (word) => [`IN`, `IS`].includes(word.toUpperCase())},
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
    name: `ON-EXIT`,
    match: [
      { type: `word`, match: (word) => word.toUpperCase() === `ON` },
      { type: `minus` },
      { type: `word`, match: (word) => word.toUpperCase() === `EXIT` },
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
  },
  {
    name: `ON-ERROR`,
    match: [
      { type: `word`, match: (word) => word.toUpperCase() === `ON` },
      { type: `minus` },
      { type: `word`, match: (word) => [`ERROR`, `EXCP`].includes(word.toUpperCase())},
    ],
    becomes: {
      type: `word`
    }
  },
  {
    name: `NEWLINE`,
    match: [{ type: `newliner` }, { type: `newline` }],
    becomes: {
      type: `newline`
    },
  },
];

const splitParts = [`%`, `.`, `(`, `)`, `+`, `-`, `*`, `/`, `=`, `:`, `,`, `;`, `\n`, `\r`, `\t`, ` `];
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
  ';': `semicolon`,
  ',': `comma`,
  '\n': `newline`,
  '\r': `newliner`,
  '\t': `tab`,
};

const stringChar: string = `'`;
const startCommentString = `//`;
const endCommentString = `\n`;

export const ALLOWS_EXTENDED = [
  `CALLP`,
  `DATA-GEN`,
  `DATA-INTO`,
  `DOU`,
  `DOW`,
  `ELSEIF`,
  `EVAL`,
  `EVAL-CORR`,
  `EVALR`,
  `FOR`,
  `FOR-EACH`,
  `IF`,
  `ON-ERROR`,
  `ON-EXCP`,
  `ON-EXIT`,
  `RETURN`,
  `SND-MSG`,
  `SORTA`,
  `WHEN`,
  `XML-INTO`,
  `XML-SAX`
]

export type TokeniseOptions = {lineNumber?: number, baseIndex?: number, ignoreTypes?: string[]};

/**
 * @returns {{value?: string, block?: object[], type: string, position: number}[]}
 */
export function tokenise(statement: string, options: TokeniseOptions = {}): Token[] {
  let lineNumber = options.lineNumber || 0;
  let baseIndex = options.baseIndex || 0;

  let commentStart = -1;
  let state: ReadState = ReadState.NORMAL;

  let result: Token[] = [];

  let startsAt = 0;
  let currentText = ``;

  for (let i = 0; i < statement.length; i++) {
    // Handle when the comment character is found
    if (state === ReadState.NORMAL && statement[i] && statement[i + 1] && statement.substring(i, i + 2) === startCommentString) {
      commentStart = i;
      currentText = startCommentString;
      state = ReadState.IN_COMMENT;

      i++

      // Handle when the end of line is there and we're in a comment
    } else if (state === ReadState.IN_COMMENT && statement[i] === endCommentString) {
      state = ReadState.NORMAL;

      // We're at the new line character so we add the newline
      result.push(
        { value: currentText, type: `comment`, range: { start: commentStart, end: i-1, line: lineNumber } },
        { value: statement[i], type: `newline`, range: { start: i, end: i + statement[i].length, line: lineNumber } }
      );

      currentText = ``;
      startsAt = i+1;

      // Ignore characters when we're in a string
    } else if (state === ReadState.IN_COMMENT) {
      currentText += statement[i];
    
    } else if (state === ReadState.IN_STRING && statement[i] !== stringChar) {
      currentText += statement[i];

    } else {
      switch (statement[i]) {
      // When it's the string character..
      case stringChar:
        const possibleEscape = statement[i+1] === stringChar && statement[i-1] !== stringChar;
        if (state === ReadState.IN_STRING) {
          if (possibleEscape) {
            currentText += `''`;
            i++;
          } else {
            currentText += statement[i];
            result.push({ value: currentText, type: `string`, range: { start: startsAt, end: startsAt + currentText.length, line: lineNumber } });
            currentText = ``;
          }
        } else {
          startsAt = i;
          currentText += statement[i];
        }

        // @ts-ignore
        state = state === ReadState.IN_STRING && !possibleEscape ? ReadState.NORMAL : ReadState.IN_STRING;
        break;

      // When it's any other character...
      default:
        if (splitParts.includes(statement[i]) && state === ReadState.NORMAL) {
          if (currentText.trim() !== ``) {
            result.push({ value: currentText, type: `word`, range: { start: startsAt, end: startsAt + currentText.length, line: lineNumber }  });
            currentText = ``;
          }

          if (statement[i] !== ` `) {
            const type = types[statement[i]];

            if (options.ignoreTypes && options.ignoreTypes.includes(type)) {
              continue;
            }

            result.push({ value: statement[i], type, range: { start: i, end: i + statement[i].length, line: lineNumber } });
          }

          startsAt = i + 1;

        } else {
          currentText += statement[i];
        }
        break;
      }
    }

    if (statement[i] === `\n`) lineNumber++;
  }

  if (currentText.trim() !== `` && state !== ReadState.IN_COMMENT) {
    result.push({ value: currentText, type: state === ReadState.NORMAL ? `word` : `string`, range: { start: startsAt, end: startsAt + currentText.length, line: lineNumber } });
    currentText = ``;
  } else {
    if (currentText.trim().length > 0) {
      result.push({ value: currentText, type: `comment`, range: { start: startsAt, end: startsAt + currentText.length, line: lineNumber } });
    }
  }

  result = fixStatement(result);
  //result = createBlocks(result);

  if (baseIndex) {
    for (let i = 0; i < result.length; i++) {
      result[i].range.start += baseIndex;
      result[i].range.end += baseIndex;
    }
  }

  return result;
}

export function fixStatement(tokens: Token[]) {
  for (let i = 0; i < tokens.length; i++) {
    for (let y = 0; y < commonMatchers.length; y++) {
      const type = commonMatchers[y];
      let goodMatch = true;

      for (let x = 0; x < type.match.length; x++) {
        const match = type.match[x];

        if (tokens[i + x]) {
          if (tokens[i + x].type === match.type) {
            if (match.match) {
              if (match.match(tokens[i + x].value)) {
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
        const newTokens = tokens.slice(i, i + type.match.length);
        tokens.splice(i, type.match.length, {
          ...type.becomes,
          value: newTokens.map(x => x.value).join(``),
          range: {
            start: newTokens[0].range.start,
            end: newTokens[newTokens.length-1].range.end,
            line: newTokens[0].range.line
          }
        });

        break;
      }
    }
  }

  return tokens;
}

export function createBlocks(tokens: Token[]) {
  let start = 0;
  let level = 0;

  for (let i = 0; i < tokens.length; i++) {
    switch (tokens[i].type) {
    case `openbracket`:
      if (level === 0) {
        start = i;
      }
      level++;
      break;
    case `closebracket`:
      level--;

      if (level === 0) {
        const newTokens = tokens.slice(start + 1, i);
        tokens.splice(start, i - start + 1, {
          type: `block`,
          block: createBlocks(newTokens),
          range: {
            line: tokens[start].range.line,
            start: tokens[i].range.start,
            end: tokens[i].range.end,
          }
        });
        i = start;
      }
      break;
    }
  }

  return tokens;
}