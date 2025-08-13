import { RpgleVariableType } from "../../../../../language/models/cache";

export interface BuiltInFunctionParameter {
  name: string;
  type: RpgleVariableType[];
  optional?: boolean;
  base?: boolean;
}

export interface BuiltInFunction {
  name: string;
  parameters: BuiltInFunctionParameter[]
  returnType?: RpgleVariableType;
}

export function getBuiltInsForType(type: RpgleVariableType): BuiltInFunction[] {
  return BuiltInFunctions.filter(func => func.parameters.some(param => param.type.includes(type)));
}

export function getBuiltIn(name: string): BuiltInFunction | undefined {
  return BuiltInFunctions.find(func => func.name.toLowerCase() === name.toLowerCase());
}

const BuiltInFunctions: BuiltInFunction[] = [
  {name: `%subst`, returnType: `char`, parameters: [
    { name: `string`, type: [`char`, `varchar`], base: true },
    { name: `start`, type: [`int`, `uns`] },
    { name: `length`, type: [`int`, `uns`], optional: true }
  ]},
  {name: `%trim`, returnType: `char`, parameters: [
    { name: `string`, type: [`char`, `varchar`], base: true },
  ]},
  {name: `%trimr`, returnType: `char`, parameters: [
    { name: `string`, type: [`char`, `varchar`], base: true },
  ]},
  {name: `%triml`, returnType: `char`, parameters: [
    { name: `string`, type: [`char`, `varchar`], base: true },
  ]},
  {
    name: `%len`,
    returnType: `int`,
    parameters: [
      { name: `string`, type: [`char`, `varchar`], base: true }
    ]
  },
  {
    name: `%upper`,
    returnType: `char`,
    parameters: [
      { name: `string`, type: [`char`, `varchar`], base: true },
    ]
  },
  {
    name: `%lower`,
    returnType: `char`,
    parameters: [
      { name: `string`, type: [`char`, `varchar`], base: true },
    ]
  },
  {
    name: `%split`,
    returnType: `char`,
    parameters: [
      { name: `string`, type: [`char`, `varchar`], base: true },
      { name: `delimiter`, type: [`char`, `varchar`] }
    ]
  },
  {
    name: `%scanrpl`,
    returnType: `char`,
    parameters: [
      { name: `search`, type: [`char`, `varchar`] },
      { name: `replace`, type: [`char`, `varchar`] },
      { name: `string`, type: [`char`, `varchar`], base: true },
    ]
  },
  {
    name: `%scan`,
    returnType: `int`,
    parameters: [
      { name: `search`, type: [`char`, `varchar`] },
      { name: `string`, type: [`char`, `varchar`], base: true },
      { name: `startPosition`, type: [`int`, `uns`], optional: true },
      { name: `length`, type: [`int`, `uns`], optional: true }
    ]
  },
  {
    name: `%scanr`,
    returnType: `int`,
    parameters: [
      { name: `search`, type: [`char`, `varchar`] },
      { name: `string`, type: [`char`, `varchar`], base: true },
      { name: `startPosition`, type: [`int`, `uns`], optional: true },
      { name: `length`, type: [`int`, `uns`], optional: true }
    ]
  }
]