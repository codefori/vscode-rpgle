import { Range, SignatureHelp, SignatureHelpParams, SignatureInformation } from "vscode-languageserver";
import { documents, getReturnValue, getWordRangeAtPosition, parser, prettyKeywords } from '.';
import Parser from "../../../../language/parser";
import { IleFunction, IleFunctionParameter, getBuiltIn } from "./apis/bif";
import Statement from "../../../../language/statement";
import Cache, { RpgleType } from "../../../../language/models/cache";

export async function signatureHelpProvider(handler: SignatureHelpParams): Promise<SignatureHelp | undefined> {
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

        // TODO: eventually support signatures from procedures

        let functionReference: IleFunction | undefined;
        if (referenceToken && referenceToken.value) {
          switch (referenceToken.type) {
            case `builtin`:
              functionReference = getBuiltIn(referenceToken.value);
              break;
            case `word`:
              functionReference = getFunctionInfo(doc, referenceToken.value);
              break;
          }
        }

        if (functionReference) {
          const parameterBlock = Statement.getParameters(tokens);
          let currentParameter = parameterBlock.findIndex(p => p.range.start <= cursorIndex && p.range.end >= cursorIndex);

          if (currentParameter === -1) {
            currentParameter = parameterBlock.length > 0 ? parameterBlock.length - 1 : 0;
          }

          // Support for continuous parameters
          if (currentParameter >= functionReference.parameters.length) {
            currentParameter = functionReference.parameters.length - 1;
          }

          let signatures: SignatureInformation[] = []

          const createTypeString = (parm: IleFunctionParameter): string => {
            let value = ``

            if (parm.continuous) {
              value += `...`
            }

            if (parm.isArray && parm.type.length > 1) {
              value += `(`
            }
            value += parm.type.join(`|`)
            if (parm.isArray && parm.type.length > 1) {
              value += `)`
            }

            if (parm.isArray) {
              value += `[]`
            }

            return value;
          }

          const createSignature = (parms: IleFunctionParameter[]): SignatureInformation => {
            return {
              label: `${functionReference.name}(${parms.map(p => p.name + `: ${createTypeString(p)}`).join(", ")}): ${functionReference.returnType}`,
              activeParameter: currentParameter,
              parameters: parms.map(p => ({
                label: `${p.name}: ${createTypeString(p)}`,
                documentation: createTypeString(p) + (p.detail ? ` - ` + p.detail : ``)
              }))
            };
          }

          let nextOptional = functionReference.parameters.findIndex(p => p.optional);
          signatures.push(createSignature(nextOptional === -1 ? functionReference.parameters : functionReference.parameters.slice(0, nextOptional)));

          if (nextOptional >= 0) {
            // Optional parameter become additional signatures.
            for (let i = nextOptional; i < functionReference.parameters.length; i++) {
              signatures.push(createSignature(functionReference.parameters.slice(0, i + 1)));
            }
          }

          const activeSignature = signatures.findIndex(sig => sig.parameters && sig.parameters.length > currentParameter);

          return {
            activeSignature: activeSignature > -1 ? activeSignature : 0,
            activeParameter: currentParameter,
            signatures
          }
        }
      }
    }
  }
  return;
}

function getFunctionInfo(doc: Cache, name: string): IleFunction | undefined {
  name = name.toLowerCase();
  const userFunc = doc.procedures.find(f => f.name.toLowerCase() === name);
  if (userFunc) {
    return {
      name: userFunc.name,
      returnType: getReturnValue(userFunc) as RpgleType,
      parameters: userFunc.subItems.map(p => ({
        name: p.name,
        type: [doc.resolveType(p).type?.name || `any`],
        isArray: p.keyword[`DIM`] !== undefined,
        optional: typeof p.keyword[`OPTIONS`] === `string` ? p.keyword[`OPTIONS`].toLowerCase().includes(`nopass`) : false,
        detail: p.description
      } satisfies IleFunctionParameter)),
    };
  }
}