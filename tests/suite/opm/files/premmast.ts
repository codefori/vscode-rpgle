import Declaration from "../../../../language/models/declaration";

export const PREMMAST: Declaration[] = [
  {
    name: `PREMASTR`,
    type: `struct`,
    keyword: {},
    position: {path: ``, range: {start: 0, end: 0, line: 0}},
    range: {start: 0, end: 0},
    subItems: [
      {
        name: `XXCNO`,
        type: `variable`,
        keyword: { char: `10` },
        position: {path: ``, range: {start: 0, end: 0, line: 0}},
        range: {start: 0, end: 0},
        subItems: [],
        references: [],
        tags: [],
        readParms: false
      } as Declaration,
      {
        name: `XXCROP`,
        type: `variable`,
        keyword: { char: `10` },
        position: {path: ``, range: {start: 0, end: 0, line: 0}},
        range: {start: 0, end: 0},
        subItems: [],
        references: [],
        tags: [],
        readParms: false
      } as Declaration
    ],
    references: [],
    tags: [],
    readParms: false
  } as Declaration
];
