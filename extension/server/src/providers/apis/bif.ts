import { RpglePrimitiveType, RpgleType, RpgleVariableType, typeToPrimitive } from "../../../../../language/models/cache";

const RpgleDateFormats = [`*YMD`,`*DMY`,`*JUL`,`*JOBRUN`,`*CYMD`,`*CDMY`,`*CMDY`,`*ISO`,`*USA`,`*EUR`,`*JIS`,`*LONGJUL`,`*LONGJOBRUN`];
const RpgleTimeUnits = [`*MSECONDS`, `*SECONDS`, `*MINUTES`, `*HOURS`, `*DAYS`, `*MONTHS`, `*YEARS`]; // TODO: add shorthard

export interface IleFunctionParameter {
  name: string;
  type: RpgleType[];
  isArray?: boolean;
  optional?: boolean;
  detail?: string;
  continuous?: boolean;

  /** True if the parameter should be used to prompt against a specific type */
  base?: boolean;
}

export interface IleFunction {
  name: string;
  parameters: IleFunctionParameter[]
  returnType?: RpgleType;
}

export function getBuiltInsForType(type: RpgleType, isArray: boolean): IleFunction[] {
  const primitiveType = typeToPrimitive(type);
  return BuiltInFunctions.filter(func => func.parameters.some(param => 
    param.base && 
    (param.type.includes(`any`) || param.type.includes(type) || (primitiveType && param.type.includes(primitiveType))) &&
    (isArray ? param.isArray === isArray : !param.isArray)
  ));
}

export function getBuiltIn(name: string): IleFunction | undefined {
  return BuiltInFunctions.find(func => func.name.toLowerCase() === name.toLowerCase());
}

export function getBuiltIns() {
  return BuiltInFunctions;
}

const StringOptions: IleFunctionParameter = {name: `option`, type: [`special`], optional: true, detail: `*NATURAL, *STDCHARSIZE`};
const LookupParameters: IleFunctionParameter[] = [
  {name: `argument`, type: [`any`]},
  {name: `array`, type: [`any`], isArray: true, base: true},
  {name: `startIndex`, type: [`number`], optional: true},
  {name: `length`, type: [`number`], optional: true}
]

const BuiltInFunctions: IleFunction[] = [
  {
    name: `%alloc`,
    returnType: `pointer`,
    parameters: [
      {name: `bytes`, type: [`number`]}
    ]
  },
  {name: `%subst`, returnType: `char`, parameters: [
    { name: `value`, type: [`string`], base: true },
    { name: `start`, type: [`number`] },
    { name: `length`, type: [`number`], optional: true }
  ]},
  {name: `%trim`, returnType: `char`, parameters: [
    { name: `value`, type: [`string`], base: true },
  ]},
  {name: `%trimr`, returnType: `char`, parameters: [
    { name: `value`, type: [`string`], base: true },
  ]},
  {name: `%triml`, returnType: `char`, parameters: [
    { name: `value`, type: [`string`], base: true },
  ]},
  {
    name: `%len`,
    returnType: `int`,
    parameters: [
      { name: `value`, type: [`string`], base: true }
    ]
  },
  {
    name: `%upper`,
    returnType: `char`,
    parameters: [
      { name: `value`, type: [`string`], base: true },
    ]
  },
  {
    name: `%lower`,
    returnType: `char`,
    parameters: [
      { name: `value`, type: [`string`], base: true },
    ]
  },
  {
    name: `%split`,
    returnType: `char`,
    parameters: [
      { name: `value`, type: [`string`], base: true },
      { name: `delimiter`, type: [`string`] }
    ]
  },
  {
    name: `%scanrpl`,
    returnType: `char`,
    parameters: [
      { name: `search`, type: [`string`] },
      { name: `replace`, type: [`string`] },
      { name: `string`, type: [`string`], base: true },
    ]
  },
  {
    name: `%scan`,
    returnType: `int`,
    parameters: [
      { name: `search`, type: [`string`] },
      { name: `value`, type: [`string`], base: true },
      { name: `startPosition`, type: [`number`], optional: true },
      { name: `length`, type: [`number`], optional: true }
    ]
  },
  {
    name: `%scanr`,
    returnType: `int`,
    parameters: [
      { name: `search`, type: [`string`] },
      { name: `value`, type: [`string`], base: true },
      { name: `startPosition`, type: [`number`], optional: true },
      { name: `length`, type: [`number`], optional: true }
    ]
  },
  {
    name: `%abs`,
    returnType: `number`,
    parameters: [
      { name: `value`, type: [`number`], base: true }
    ]
  },
  {
    name: `%addr`,
    returnType: `pointer`,
    parameters: [
      { name: `value`, type: [`any`], base: true },
      { name: `option`, type: [`special`], detail: `*DATA`, optional: true}
    ]
  },
  {
    name: `%bitadd`,
    returnType: `ind`,
    parameters: [
      {name: `expr`, type: [`any`], continuous: true}
    ]
  },
  {
    name: `%bitnot`,
    returnType: `ind`,
    parameters: [
      {name: `expr`, type: [`any`]}
    ]
  },
  {
    name: `%bitor`,
    returnType: `ind`,
    parameters: [
      {name: `expr`, type: [`any`], continuous: true}
    ]
  },
  {
    name: `%bitxor`,
    returnType: `ind`,
    parameters: [
      {name: `expr`, type: [`any`]},
      {name: `expr`, type: [`any`]}
    ]
  },
  {
    name: `%char`,
    returnType: `string`,
    parameters: [
      {name: `expr`, type: [`any`], base: true},
      {name: `formatOrCcsid`, type: [`number`, `special`], optional: true, detail: `If number, treated as CCSID. Use format when converting datetime types.`}
    ]
  },
  {
    name: `%charcount`,
    returnType: `number`,
    parameters: [
      {name: `value`, type: [`string`], base: true}
    ]
  },
  {
    name: `%check`,
    returnType: `number`,
    parameters: [
      {name: `comparator`, type: [`string`]},
      {name: `base`, type: [`string`]},
      {name: `start`, type: [`number`], optional: true},
      StringOptions,
    ]
  },
  {
    name: `%checkr`,
    returnType: `number`,
    parameters: [
      {name: `comparator`, type: [`string`]},
      {name: `base`, type: [`string`]},
      {name: `start`, type: [`number`], optional: true},
      StringOptions,
    ]
  },
  {
    name: `%concat`,
    returnType: `string`,
    parameters: [
      {name: `separator`, type: [`string`]},
      {name: `value`, type: [`any`], continuous: true}
    ]
  },
  {
    name: `%concatarr`,
    returnType: `string`,
    parameters: [
      {name: `comparator`, type: [`string`]},
      {name: `array`, type: [`any`], isArray: true},
      StringOptions,
    ]
  },
  {
    name: `%date`,
    returnType: `date`,
    parameters: [
      { name: `value`, type: [`string`, `datetime`, `number`], base: true },
      { name: `format`, type: [`special`], detail: RpgleDateFormats.join(`, `) }
    ]
  },
  {
    name: `%days`,
    returnType: `datetime`,
    parameters: [
      {name: `duration`, type: [`number`], base: true}
    ]
  },
  {
    name: `%dec`,
    returnType: `packed`,
    parameters: [
      { name: `value`, type: [`number`, `string`, `datetime`], base: true },
      { name: `precision`, type: [`number`], optional: true },
      { name: `decimals`, type: [`number`], optional: true }
    ]
  },
  {
    name: `%dech`,
    returnType: `packed`,
    parameters: [
      { name: `value`, type: [`number`, `string`, `datetime`] },
      { name: `precision`, type: [`number`], optional: true },
      { name: `decimals`, type: [`number`], optional: true }
    ]
  },
  {
    name: `%decpos`,
    returnType: `number`,
    parameters: [
      { name: `value`, type: [`number`], base: true },
    ]
  },
  {
    name: `%diff`,
    returnType: `any`,
    parameters: [
      { name: `start`, type: [`datetime`], base: true, detail: `Returns a datetime by default.` },
      { name: `end`, type: [`datetime`] },
      { name: `unit`, type: [`string`], detail: RpgleTimeUnits.join(`, `) },
      { name: `frac`, type: [`number`], optional: true, detail: `Returns number when provided.` }
    ],
  },
  {
    name: `%div`,
    returnType: `int`,
    parameters: [
      { name: `n`, type: [`number`] },
      { name: `m`, type: [`number`] },
    ],
  },
  {
    name: `%elem`,
    returnType: `int`,
    parameters: [
      { name: `array`, type: [`any`], isArray: true, base: true },
    ]
  },
  {
    name: `%eof`,
    returnType: `ind`,
    parameters: [
      {name: `fileName`, type: [`file`], base: true, optional: true}
    ]
  },
  {
    name: `%equal`,
    returnType: `ind`,
    parameters: [
      {name: `fileName`, type: [`file`], base: true, optional: true}
    ]
  },
  {
    name: `%error`,
    returnType: `ind`,
    parameters: []
  },
  {
    name: `%float`,
    returnType: `float`,
    parameters: [
      { name: `value`, type: [`number`, `string`], base: true },
    ]
  },
  {
    name: `%fields`,
    parameters: [
      {name: `name`, type: [`any`], continuous: true}
    ]
  },
  {
    name: `%found`,
    returnType: `ind`,
    parameters: [
      {name: `fileName`, type: [`file`], base: true, optional: true}
    ]
  },
  {
    name: `%graph`,
    returnType: `graph`,
    parameters: [
      {name: `expression`, type: [`string`]}
    ]
  },
  {
    name: `%hival`,
    returnType: `any`,
    parameters: [
      {name: `variable`, type: [`any`], base: true}
    ]
  },
  {
    name: `%hours`,
    returnType: `datetime`,
    parameters: [
      {name: `duration`, type: [`number`], base: true}
    ]
  },
  {
    name: `%int`,
    returnType: `int`,
    parameters: [
      { name: `value`, type: [`number`, `string`], base: true },
    ]
  },
  {
    name: `%inth`,
    returnType: `int`,
    parameters: [
      { name: `value`, type: [`number`, `string`], base: true },
    ]
  },
  {
    name: `%kds`,
    parameters: [
      { name: `structure`, type: [`any`] },
      { name: `numberOfKeys`, type: [`number`], optional: true }
    ]
  },
  {
    name: `%left`,
    returnType: `string`,
    parameters: [
      { name: `value`, type: [`string`], base: true },
      { name: `length`, type: [`number`], optional: true },
      StringOptions
    ]
  },
  {
    name: `%loval`,
    returnType: `any`,
    parameters: [
      {name: `variable`, type: [`any`], base: true}
    ]
  },
  {
    name: `%list`,
    returnType: `any`,
    parameters: [
      { name: `value`, type: [`any`], continuous: true },
    ]
  },
  {
    name: `%lookup`,
    returnType: `int`,
    parameters: LookupParameters
  },
  {
    name: `%lookuplt`,
    returnType: `int`,
    parameters: LookupParameters
  },
  {
    name: `%lookuple`,
    returnType: `int`,
    parameters: LookupParameters
  },
  {
    name: `%lookupgt`,
    returnType: `int`,
    parameters: LookupParameters
  },
  {
    name: `%lookupge`,
    returnType: `int`,
    parameters: LookupParameters
  },
  {
    name: `%max`,
    returnType: `any`,
    parameters: [
      {name: `variable`, type: [`any`], continuous: true}
    ]
  },
  {
    name: `%maxarr`,
    returnType: `any`,
    parameters: [
      {name: `array`, type: [`any`], isArray: true, base: true},
      {name: `startIndex`, type: [`number`], optional: true},
      {name: `length`, type: [`number`], optional: true}
    ]
  },
  {
    name: `%min`,
    returnType: `any`,
    parameters: [
      {name: `variable`, type: [`any`], continuous: true}
    ]
  },
  {
    name: `%minarr`,
    returnType: `any`,
    parameters: [
      {name: `array`, type: [`any`], isArray: true, base: true},
      {name: `startIndex`, type: [`number`], optional: true},
      {name: `length`, type: [`number`], optional: true}
    ]
  },
  {
    name: `%minutes`,
    returnType: `datetime`,
    parameters: [
      {name: `duration`, type: [`number`], base: true}
    ]
  },
  {
    name: `%months`,
    returnType: `datetime`,
    parameters: [
      {name: `duration`, type: [`number`], base: true}
    ]
  },
  {
    name: `%mseconds`,
    returnType: `datetime`,
    parameters: [
      {name: `duration`, type: [`number`], base: true}
    ]
  },
  {
    name: `%msg`,
    parameters: [
      {name: `messageId`, type: [`string`]},
      {name: `messageFile`, type: [`string`]},
      {name: `replacementText`, type: [`string`], optional: true}
    ]
  },
  {
    name: `%nullind`,
    returnType: `ind`,
    parameters: [
      {name: `field`, type: [`any`]},
    ]
  },
  {
    name: `%occur`,
    returnType: `int`,
    parameters: [
      {name: `struct`, type: [`any`], isArray: true},
    ]
  },
  {
    name: `%open`,
    returnType: `int`,
    parameters: [
      {name: `fileName`, type: [`file`], base: true},
    ]
  },
  {
    name: `%seconds`,
    returnType: `datetime`,
    parameters: [
      {name: `duration`, type: [`number`], base: true}
    ]
  },
  {
    name: `%paddr`,
    returnType: `pointer`,
    parameters: [
      {name: `procedure`, type: [`any`]}
    ]
  },
  {
    name: `%parms`,
    returnType: `number`,
    parameters: [] 
  },
  {
    name: `%parmnum`,
    returnType: `number`,
    parameters: [
      {name: `parameterName`, type: [`any`]}
    ]
  },
  {
    name: `%proc`,
    returnType: `string`,
    parameters: []
  },
  {
    name: `%range`,
    parameters: [
      {name: `lowerLimit`, type: [`any`]},
      {name: `upperLimit`, type: [`any`]}
    ]
  },
  {
    name: `%realloc`,
    returnType: `pointer`,
    parameters: [
      {name: `base`, type: [`pointer`], base: true},
      {name: `bytes`, type: [`number`]}
    ]
  },
  {
    name: `%rem`,
    returnType: `number`,
    parameters: [
      {name: `n`, type: [`number`]},
      {name: `m`, type: [`number`]}
    ]
  },
  {
    name: `%replace`,
    returnType: `string`,
    parameters: [
      {name: `replacement`, type: [`string`]},
      {name: `source`, type: [`string`]},
      {name: `startIndex`, type: [`number`], optional: true},
      StringOptions,
    ]
  },
  {
    name: `%right`,
    returnType: `string`,
    parameters: [
      {name: `value`, type: [`string`], base: true},
      {name: `length`, type: [`number`]},
      StringOptions
    ]
  },
  {
    name: `%shtdn`,
    returnType: `ind`,
    parameters: []
  },
  {
    name: `%size`,
    returnType: `number`,
    parameters: [
      {name: `value`, type: [`any`], base: true}
    ]
  },
  {
    name: `%sqrt`,
    returnType: `number`,
    parameters: [
      {name: `expression`, type: [`number`], base: true}
    ]
  },
  {
    name: `%status`,
    returnType: `string`,
    parameters: [
      {name: `fileName`, type: [`file`], base: true}
    ]
  },
  {
    name: `%str`,
    returnType: `string`,
    parameters: [
      {name: `base`, type: [`pointer`], base: true},
      {name: `byteLength`, type: [`number`], optional: true},
    ]
  },
  {
    name: `%subarr`,
    returnType: `any`,
    parameters: [
      {name: `array`, type: [`any`], isArray: true, base: true},
      {name: `startIndex`, type: [`number`]},
      {name: `length`, type: [`number`], optional: true}
    ]
  },
  {
    name: `%subdt`,
    returnType: `datetime`,
    parameters: [
      {name: `value`, type: [`datetime`], base: true},
      {name: `unit`, type: [`special`], detail: RpgleTimeUnits.join(`, `)},
      {name: `digits`, type: [`number`], optional: true},
      {name: `decimalPositions`, type: [`number`], optional: true}
    ]
  },
  {
    name: `%time`,
    returnType: `time`,
    parameters: [
      { name: `value`, type: [`any`], base: true },
      { name: `format`, type: [`special`] }
    ]
  },
  {
    name: `%timestamp`,
    returnType: `timestamp`,
    parameters: [
      { name: `value`, type: [`any`] },
    ]
  },
  {
    name: `%ucs2`,
    returnType: `ucs2`,
    parameters: [
      { name: `expression`, type: [`any`] },
      { name: `ccsid`, type: [`number`], optional: true }
    ]
  },
  {
    name: `%uns`,
    returnType: `uns`,
    parameters: [
      { name: `value`, type: [`number`, `string`], base: true },
    ]
  },
  {
    name: `%xfoot`,
    returnType: `number`,
    parameters: [
      { name: `array`, type: [`any`], isArray: true, base: true, detail: `Numeric array` }
    ]
  },
  {
    name: `%xlate`,
    returnType: `string`,
    parameters: [
      {name: `from`, type: [`string`]},
      {name: `to`, type: [`string`]},
      {name: `baseValue`, type: [`string`]},
      StringOptions
    ]
  },
  {
    name: `%years`,
    returnType: `datetime`,
    parameters: [
      {name: `duration`, type: [`number`], base: true}
    ]
  },
]