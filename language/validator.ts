import { IssueRange } from "./parserTypes";
import { Token } from "./types";

interface GenericTokenFormat {
  not?: boolean;
  type: string[];
  value?: string;
};

interface GenericWhenRule {
  when: GenericTokenFormat;
  start?: boolean;
  then?: GenericTokenFormat[];
  end?: boolean;
}

const Operators = [`plus`, `minus`, `divide`, `asterisk`, `comma`, `dot`];
const ValueTypes = [`string`, `number`, `special`, `hex`, `builtin`];
const ExprParts = [...Operators, ...ValueTypes];

const GenericRules: GenericWhenRule[] = [
  {
    when: {
      type: [`format`]
    },
    start: true,
    then: [],
    end: true,
  },
  {
    when: {
      type: [`directive`, `declare`],
    },
    start: true
  },
  {
    when: {
      type: [`seperator`],
    },
    then: [{not: true, type: Operators}]
  },
  {
    when: {
      type: [`openbracket`],
    },
    then: [{not: true, type: Operators}]
  },
  {
    when: {
      type: [`closebracket`],
    },
    then: [{not: true, type: ValueTypes}]
  },
  {
    when: {
      type: Operators
    },
    then: [{not: true, type: Operators}]
  },
  {
    when: {
      type: ValueTypes
    },
    then: [{not: true, type: ValueTypes}]
  },
  {
    when: {
      type: Operators
    },
    then: [{type: ValueTypes}]
  },
];
Object.freeze(GenericRules);

export function validateTokens(tokens: Token[]): IssueRange|undefined {
  for (let i = 0; i < tokens.length; i++) {
    let token = tokens[i];
    let cToken = tokens[i];

    const currentRule = GenericRules.find(rule => rule.when.type.includes(cToken.type) && rule.when.value === undefined || rule.when.value === cToken.value);

    if (currentRule) {
      let cI = i;
      if (cI !== 0 && currentRule.start) {
        // Throw error. This can only be at the start
        return {
          offset: {position: cToken.range.start, end: cToken.range.end},
          type: `Validator`,
          message: `Token is expected at the start of statements`
        }
      }

      if (currentRule.then && currentRule.then.length > 0) {
        for (const thenItem of currentRule.then) {
          cI++;

          cToken = tokens[cI];

          if (cToken) {
            const typeMatch = thenItem.type.includes(cToken.type) && (thenItem.value ? thenItem.value === cToken.value : true);
            const isError = (thenItem.not ? typeMatch : !typeMatch);

            if (isError) {
              // Token unexpected
              return {
                offset: {position: cToken.range.start, end: cToken.range.end},
                type: `Validator`,
                message: `Token not expected`
              }
            }
          } else if (thenItem.not !== true) {
            return {
              offset: {position: token.range.start, end: token.range.end},
              type: `Validator`,
              message: `'${thenItem.type.join()}' is ${thenItem.not ? `not` : ``} expected.`
            }
          }
        }

        cI++;
      }

      if (cI !== (tokens.length-1) && currentRule.end) {
        cToken = tokens[cI];

        if (cToken) {
        // Throw error. This can only be at the end
          return {
            offset: {position: cToken.range.start, end: cToken.range.end},
            type: `Validator`,
            message: `Token should be at the end of the statement only.`
          }
        }
      }
    }
  }

  return;
}