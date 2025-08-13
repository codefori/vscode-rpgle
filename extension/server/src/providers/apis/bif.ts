import { RpgleVariableType } from "../../../../../language/models/cache";

export interface BuiltInFunctionParameter {
  name: string;
  type: RpgleVariableType[];
  optional?: boolean
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
    { name: `string`, type: [`char`, `varchar`] },
    { name: `start`, type: [`int`, `uns`] },
    { name: `length`, type: [`int`, `uns`], optional: true }
  ]},
  {name: `%trim`, returnType: `char`, parameters: [
    { name: `string`, type: [`char`, `varchar`] },
  ]},
  {
    name: `%length`,
    returnType: `int`,
    parameters: [
      { name: `string`, type: [`char`, `varchar`] }
    ]
  }
]