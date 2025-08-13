import { RpglePrimitiveType, RpgleType, RpgleVariableType, typeToPrimitive } from "../../../../../language/models/cache";

const RpgleDateFormats = [`*YMD`,`*DMY`,`*JUL`,`*JOBRUN`,`*CYMD`,`*CDMY`,`*CMDY`,`*ISO`,`*USA`,`*EUR`,`*JIS`,`*LONGJUL`,`*LONGJOBRUN`];
const RpgleTimeUnits = [`*MSECONDS`, `*SECONDS`, `*MINUTES`, `*HOURS`, `*DAYS`, `*MONTHS`, `*YEARS`]; // TODO: add shorthard

export interface BuiltInFunctionParameter {
  name: string;
  type: RpgleType[];
  optional?: boolean;
  base?: boolean;
  detail?: string;
}

export interface BuiltInFunction {
  name: string;
  parameters: BuiltInFunctionParameter[]
  returnType?: RpgleType;
}

export function getBuiltInsForType(type: RpgleType): BuiltInFunction[] {
  const primitiveType = typeToPrimitive(type);
  return BuiltInFunctions.filter(func => func.parameters.some(param => 
    param.base && 
    (param.type.includes(`any`) || param.type.includes(type) || (primitiveType && param.type.includes(primitiveType)))));
}

export function getBuiltIn(name: string): BuiltInFunction | undefined {
  return BuiltInFunctions.find(func => func.name.toLowerCase() === name.toLowerCase());
}

const BuiltInFunctions: BuiltInFunction[] = [
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
    name: `%elem`,
    returnType: `int`,
    parameters: [
      { name: `array`, type: [`any`], base: true },
    ]
  },
  {
    name: `%float`,
    returnType: `float`,
    parameters: [
      { name: `value`, type: [`number`, `string`], base: true },
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
    name: `%seconds`,
    returnType: `datetime`,
    parameters: [
      {name: `duration`, type: [`number`], base: true}
    ]
  },
  {
    name: `%size`,
    returnType: `number`,
    parameters: [
      {name: `value`, type: [`any`], base: true}
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
    name: `%uns`,
    returnType: `uns`,
    parameters: [
      { name: `value`, type: [`number`, `string`], base: true },
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