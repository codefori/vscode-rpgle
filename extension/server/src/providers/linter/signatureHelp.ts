import { MarkedString, MarkupContent, MarkupKind, ParameterInformation, Range, SignatureHelp, SignatureHelpParams, SignatureHelpTriggerKind, SignatureInformation } from "vscode-languageserver";
import { documents, parser } from "..";
import { documentParseCache } from ".";
import Statement from "../../../../../language/statement";
import Declaration from "../../../../../language/models/declaration";

export async function signatureHelpProvider(params: SignatureHelpParams): Promise<SignatureHelp | undefined> {
  const uri = params.textDocument.uri;
  const document = documents.get(uri);

  if (document) {
    const isFree = (document.getText(Range.create(0, 0, 0, 6)).toUpperCase() === `**FREE`);

    if (isFree) {
      const offset = document?.offsetAt(params.position);
      const parsedDocument = documentParseCache[uri];

      if (parsedDocument) {
        const statement = parsedDocument.getStatementByOffset(offset);
        if (statement) {
          const withBlocks = statement.asBlocks();
          const currentTokenIx = withBlocks.findIndex(token => offset >= token.range.start && offset <= token.range.end);

          // Check we're in a block type
          if (currentTokenIx >= 0 && withBlocks[currentTokenIx].type === `block`) {
            // Check the token before is a valid word
            if (withBlocks[currentTokenIx - 1] && withBlocks[currentTokenIx - 1].type === `word`) {
              const nameToken = withBlocks[currentTokenIx - 1];
              const parmBlock = withBlocks[currentTokenIx];

              const nameValue = nameToken.value!.toUpperCase();
              const lineNumber = nameToken.range.line;

              if (document) {
                const doc = await parser.getDocs(uri);
                if (doc) {

                  // If they're typing inside of a procedure, let's get the stuff from there too
                  const currentProcedure = doc.procedures.find((proc, index) =>
                    lineNumber >= proc.range.start &&
                    (lineNumber <= proc.range.end + 1 || index === doc.procedures.length - 1)
                  );

                  const possibleFunction: Declaration | undefined = [
                    currentProcedure && currentProcedure.scope ? currentProcedure.scope.procedures.find(proc => proc.name.toUpperCase() === nameValue) : undefined,
                    doc.procedures.find(proc => proc.name.toUpperCase() === nameValue)
                  ].find(x => x); // find the first non-undefined item

                  if (possibleFunction) {
                    const parms = Statement.getParameters(parmBlock.block!);
                    const currentParm = parms.findIndex(token => offset >= token.range.start && offset <= token.range.end);
                    const activeParameter = currentParm >= 0 ? currentParm : 0;

                    let retrunValue = possibleFunction.keywords.filter(keyword => !keyword.startsWith(`EXTPROC`));
                    if (retrunValue.length === 0) retrunValue = [`void`];

                    const parmsString = possibleFunction.subItems.map(x => x.name).join(` : `);

                    let currentSig: SignatureInformation = {
                      label: `${possibleFunction.name}(${parmsString}): ${retrunValue.join(` `)}`,
                      activeParameter,
                      documentation: possibleFunction.description.trim().length > 0 ?
                        {
                          kind: MarkupKind.Markdown,
                          value: `---\n\n${possibleFunction.description}`
                        } : undefined,
                        
                      parameters: possibleFunction.subItems.map((parm, i): ParameterInformation => {
                        const docLines = [
                          `\`${parm.name}: ${parm.keywords.join(` `)}\``,
                          parm.description.trim().length ? parm.description : undefined
                        ].filter(x => x).join(`\n\n`);

                        return {
                          label: parm.name, 
                          documentation: {
                            kind: MarkupKind.Markdown,
                            value: docLines
                          }
                        }
                      })
                    };

                    return {signatures: [currentSig], activeSignature: 0, activeParameter};
                  }
                }
              }
            }
          }

          return { signatures: [] };
        }
      }
    }
  }

  return;
}