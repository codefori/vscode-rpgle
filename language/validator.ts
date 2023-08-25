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

const Seperators = [`plus`, `minus`, `divide`, `asterisk`, `seperator`, `comma`];
const ValueTypes = [`string`, `number`, `special`, `hex`, `builtin`];
const ExprParts = [...Seperators, ...ValueTypes];

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
      type: ExprParts
    },
    then: [{not: true, type: ExprParts}]
  },
  {
    when: {
      type: Seperators
    },
    then: [{type: ValueTypes}]
  },
];
Object.freeze(GenericRules);

export function validateTokens(tokens: Token[]): IssueRange|undefined {
  for (let i = 0; i < tokens.length; i++) {
    let token = tokens[i];

    const currentRule = GenericRules.find(rule => rule.when.type.includes(token.type) && rule.when.value === undefined || rule.when.value === token.value);

    if (currentRule) {
      let cToken: Token;
      let cI = i;
      if (cI !== 0 && currentRule.start) {
        // Throw error. This can only be at the start
        return {
          offset: {position: cToken.range.start, end: cToken.range.end},
          type: `InvalidToken`
        }
      }

      if (currentRule.then && currentRule.then.length > 0) {
        for (const thenItem of currentRule.then) {
          cI++;

          cToken = tokens[cI];

          const typeMatch = thenItem.type.includes(cToken.type) && (thenItem.value ? thenItem.value === cToken.value : true);
          const isError = (thenItem.not ? typeMatch : !typeMatch);

          if (isError) {
            // Token unexpected
            return {
              offset: {position: cToken.range.start, end: cToken.range.end},
              type: `InvalidToken`
            }
          }
        }

        cI++;
      }

      if (cI !== (tokens.length-1) && currentRule.end) {
        cToken = tokens[cI];

        // Throw error. This can only be at the end
        return {
          offset: {position: cToken.range.start, end: cToken.range.end},
          type: `InvalidToken`
        }
      }
    }
  }

  return;
}