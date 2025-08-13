import { RpglePrimitiveType, RpgleVariableType, typeToPrimitive } from "../../../../../language/models/cache";

export interface BuiltInFunctionParameter {
  name: string;
  type: (RpgleVariableType|RpglePrimitiveType)[];
  optional?: boolean;
  base?: boolean;
}

export interface BuiltInFunction {
  name: string;
  parameters: BuiltInFunctionParameter[]
  returnType?: RpgleVariableType;
}

export function getBuiltInsForType(type: RpgleVariableType): BuiltInFunction[] {
  const primitiveType = typeToPrimitive(type);
  return BuiltInFunctions.filter(func => func.parameters.some(param => 
    param.base && 
    (param.type.includes(type) || (primitiveType && param.type.includes(primitiveType)))));
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
  }
]