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
    let tokenBeforeStmt: Token;

    let statementStart = {
      index: 0,
    };

    for (let i = 0; i < tokens.length; i++) {
      switch (tokens[i].type) {
        case `semicolon`:
          const statementTokens = Statement.trimTokens(tokens.slice(statementStart.index, i));
          tokenBeforeStmt = tokens[i-statementTokens.length-1];
          const indent = statementTokens[0] && tokenBeforeStmt ? (statementTokens[0].range.start - tokenBeforeStmt.range.end) : 0;
          this.addStatement(indent, statementTokens);

          statementStart = {
            index: i+1
          };
          break;

        case `format`:
          const formatTokens = Statement.trimTokens(tokens.slice(statementStart.index, i+1));
          if (formatTokens.length === 1) {
            tokenBeforeStmt = tokens[i-formatTokens.length-1];
            const indent = formatTokens[0] && tokenBeforeStmt ? (formatTokens[0].range.start - tokenBeforeStmt.range.end) : 0;
            this.addStatement(indent, formatTokens);

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
            tokenBeforeStmt = tokens[i-statementTokens.length-1];
            const indent = statementTokens[0] && tokenBeforeStmt ? (statementTokens[0].range.start - tokenBeforeStmt.range.end) : 0;
            this.addStatement(indent, statementTokens);

            statementStart = {
              index: i+1
            };

            currentStatementType = StatementType.Normal;
          }
          break;
      }
    }

    const lastStatementTokens = Statement.trimTokens(tokens.slice(statementStart.index, tokens.length));
    tokenBeforeStmt = tokens[tokens.length-1-lastStatementTokens.length];
    const indent = lastStatementTokens[0] ? (lastStatementTokens[0].range.start - tokenBeforeStmt.range.end) : 0;
    this.addStatement(indent, lastStatementTokens);
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