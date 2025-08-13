import { Range, SignatureHelp, SignatureHelpParams } from "vscode-languageserver";
import { documents, getWordRangeAtPosition, parser, prettyKeywords } from '.';
import Parser from "../../../../language/parser";
import { getBuiltIn } from "./apis/bif";
import Statement from "../../../../language/statement";

export async function signatureHelpProvider(handler: SignatureHelpParams): Promise<SignatureHelp|undefined> {
  const currentPath = handler.textDocument.uri;
  const document = documents.get(currentPath);

  if (document) {
    const doc = await parser.getDocs(currentPath, document.getText());
    if (doc) {
      const isFree = (document.getText(Range.create(0, 0, 0, 6)).toUpperCase() === `**FREE`);
      const currentLine = document.getText(Range.create(
				handler.position.line,
				0,
				handler.position.line,
				200
			));

      let tokens = Parser.lineTokens(isFree ? currentLine : currentLine.length >= 7 ? ``.padEnd(7) + currentLine.substring(7) : ``, 0, 0, true);

      if (tokens.length > 0) {
				const cursorIndex = handler.position.character;

        // We need to find the innermost block we are part of
        const currentBlock = Parser.fromBlocksGetTokens(tokens, cursorIndex);
        const referenceToken = currentBlock.preToken;

        tokens = currentBlock.block;

        // Remove any tokens after the cursor
        tokens = tokens.filter(token => token.range.end <= cursorIndex);

        // Get the possible variable we're referring to

        // TODO: eventually support signatures from procedures

        if (referenceToken && referenceToken.type === `builtin` && referenceToken.value) {
          const builtIn = getBuiltIn(referenceToken.value);
          
          if (builtIn) {
            const baseSignature = builtIn.parameters.filter(p => !p.optional);

            const parameterBlock = Statement.getParameters(tokens);
            let currentParameter = parameterBlock.findIndex(p => p.range.start <= cursorIndex && p.range.end >= cursorIndex);

            if (currentParameter === -1) {
              currentParameter = parameterBlock.length > 0 ? parameterBlock.length-1 : 0;
            }

            return {
              activeSignature: 0,
              activeParameter: currentParameter,
              signatures: [{
                label: `${builtIn.name}(${baseSignature.map(p => p.name + `: ${p.type.join(`|`)}`).join(", ")})`,
                activeParameter: currentParameter,
                parameters: baseSignature.map(p => ({
                  label: `${p.name}: ${p.type.join(`|`)}`,
                  documentation: p.type.join(`, `)
                }))
              }]
            }
          }
        }
      }
    }
  }
  return;
}