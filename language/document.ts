// import Statement from "./statement";
import Statement from "./statement";
import { tokenise } from "./tokens";
import { IRange, Token } from "./types";

export enum StatementType {
	Normal = "Normal",
	Directive = "Directive",
}

export default class Document {
  statements: Statement[];
  constructor(content: string) {
    this.statements = [];

    this.parseStatements(tokenise(content));
  }

  private addStatement(indent: number, tokens: Token[]) {
    if (tokens.length > 0) {
      this.statements.push(new Statement(
        tokens,
        {
          line: tokens[0].range.line,
          start: tokens[0].range.start,
          end: tokens[tokens.length - 1].range.end,
        },
        indent
      ));
    }
  }

  private parseStatements(tokens: Token[]) {
    let currentStatementType: StatementType = StatementType.Normal;
    let newLineToken: Token;

    let lastLine = {
      need: true,
      index: -1
    };

    let statementStart = {
      index: 0,
    };

    for (let i = 0; i < tokens.length; i++) {
      switch (tokens[i].type) {
        case `semicolon`:
          const statementTokens = Statement.trimTokens(tokens.slice(statementStart.index, i));
          newLineToken = tokens[lastLine.index];
          const indent = statementTokens[0] && newLineToken ? (statementTokens[0].range.start - newLineToken.range.end) : 0;
          lastLine.need = true;

          this.addStatement(indent, statementTokens);

          statementStart = {
            index: i+1
          };
          break;

        case `format`:
          const formatTokens = Statement.trimTokens(tokens.slice(statementStart.index, i+1));
          if (formatTokens.length === 1) {
            newLineToken = tokens[lastLine.index]; 
            const indent = formatTokens[0] && newLineToken ? (formatTokens[0].range.start - newLineToken.range.end) : 0;
            lastLine.need = true;

            this.addStatement(indent, formatTokens);

            statementStart = {
              index: i+1
            };
          }
          break;


        case `comment`:
          const commentToken = Statement.trimTokens(tokens.slice(statementStart.index, i+1));
          if (commentToken.length === 1) {

            // Don't add the comment as a new statement if it proceeds another non-comment token
            if (tokens[i-1] && tokens[i-1].range.line !== commentToken[0].range.line) {
              newLineToken = tokens[lastLine.index];
              const indent = commentToken[0] && newLineToken ? (commentToken[0].range.start - newLineToken.range.end) : 0;
              lastLine.need = true;

              this.addStatement(indent, commentToken);
            }

            statementStart = {
              index: i+1
            };
          }
          break;

        case `directive`:
          const directiveTokens = Statement.trimTokens(tokens.slice(statementStart.index, i+1));
          if (directiveTokens[0].type === `directive`) {
            currentStatementType = StatementType.Directive;
          }
          break;

        case `newline`:
          if (currentStatementType === StatementType.Directive) {
            const statementTokens = Statement.trimTokens(tokens.slice(statementStart.index, i));
            newLineToken = tokens[lastLine.index];
            const indent = statementTokens[0] && newLineToken ? (statementTokens[0].range.start - newLineToken.range.end) : 0;
            this.addStatement(indent, statementTokens);

            statementStart = {
              index: i+1
            };

            currentStatementType = StatementType.Normal;
          }

          if (lastLine.need || (tokens[i-1] && tokens[i-1].type === `newline`)) { 
            lastLine.index = i;
            lastLine.need = false;
          }
          break;
      }
    }

    const lastStatementTokens = Statement.trimTokens(tokens.slice(statementStart.index, tokens.length));

    if (lastStatementTokens.length > 0) { 
      newLineToken = tokens[lastLine.index];
      const indent = lastStatementTokens[0] ? (lastStatementTokens[0].range.start - newLineToken.range.end) : 0;
      this.addStatement(indent, lastStatementTokens);
    }
  }

  getStatementByLine(line: number) {
    return this.statements.find(stmt => stmt.range.line === line);
  }

  getStatementByOffset(offset: number) {
    return this.statements.find((statement, i) => {
      const end = (this.statements[i + 1] ? this.statements[i + 1].range.start : statement.range.end);
      return (offset >= statement.range.start && offset < end) || (i === (this.statements.length - 1) && offset >= end);
    })
  }

  getTokenByOffset(offset: number): Token | undefined {
    const statement = this.getStatementByOffset(offset);

    if (statement) {
      return statement.getTokenByOffset(offset);
    }
  }
}