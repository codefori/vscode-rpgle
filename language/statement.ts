import { createBlocks } from "./tokens";
import { IRange, IRangeWithLine, Token } from "./types";

export default class Statement {
	private beginBlock = false;

  constructor(public tokens: Token[], public range: IRangeWithLine, public indent: number = 0) {}

	getTokenByOffset(offset: number) {
		const blockSearch = (tokens: Token[]): Token|undefined => {
			const token = tokens.find(token => offset >= token.range.start && offset <= token.range.end);
			
			if (token?.type === `block` && token.block) {
				return blockSearch(token.block);
			}

			return token;
		}

		return blockSearch(this.tokens);
	}

  asBlocks() {
    return createBlocks(this.tokens);
  }

  static getParameters(tokens: Token[]) {
    let parameters: Token[] = [];
    let newBlock: Token[] = [];

    for (let i = 0; i < tokens.length; i++) {
      if (tokens[i].type === `seperator`) {
        parameters.push({
          type: `block`,
          block: newBlock,
          range: {
            line: newBlock[0].range.line,
            start: newBlock[0].range.start,
            end: newBlock[newBlock.length-1].range.end,
          }
        });

        newBlock = [tokens[i]];
      } else {
        newBlock.push(tokens[i]);
      }
    }

    if (newBlock.length > 0) {
      parameters.push({
        type: `block`,
        block: newBlock,
        range: {
          line: newBlock[0].range.line,
          start: newBlock[0].range.start,
          end: newBlock[newBlock.length-1].range.end,
        }
      });
    }

    return parameters;
  }

	static trimTokens(tokens: Token[]) {
    if (tokens.length > 0) {
      let realFirstToken = tokens.findIndex(t => t.type !== `newline`);
      if (realFirstToken < 0) realFirstToken = 0;

      let realLastToken = 0;

      for (let i = tokens.length - 1; i >= 0; i--) {
        if (tokens[i].type !== `newline`) {
          realLastToken = i + 1;
          break;
        }
      }

      tokens = tokens.slice(realFirstToken, realLastToken);
    }

    return tokens;
  }
}