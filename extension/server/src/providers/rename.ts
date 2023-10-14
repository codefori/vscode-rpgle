
import { documents, getRangeAtPosition, getWordRangeAtPosition, parser } from '.';
import { PrepareRenameParams, Range, RenameParams, WorkspaceEdit } from "vscode-languageserver";
import Linter from '../../../../language/linter';

export async function renamePrepareProvider(params: PrepareRenameParams): Promise<Range | undefined> {
  const uri = params.textDocument.uri;
  const currentPos = params.position;
  const document = documents.get(uri);

  if (document) {
    const isFree = (document.getText(Range.create(0, 0, 0, 6)).toUpperCase() === `**FREE`);

    let selected = getRangeAtPosition(document, currentPos);
    if (selected) {
      let word = selected.value.trim();

      if (word) {
        if (word.endsWith(`;`)) {
          const pieces = word.split(`;`);
          word = pieces[0];
        }
      }

      const doc = await parser.getDocs(uri, document.getText());

      if (doc) {
        if (isFree) {
          Linter.getErrors(
            {
              uri,
              content: document.getText()
            },
            {
              CollectReferences: true
            },
            doc
          );
        }

        const def = doc.findDefinition(currentPos.line, word);

        if (def) {
          const currentSelectedRef = def?.references.find(r => document.positionAt(r.offset.position).line === currentPos.line);

          if (currentSelectedRef) {
            return Range.create(
              document.positionAt(currentSelectedRef.offset.position),
              document.positionAt(currentSelectedRef.offset.end)
            )
          } else {
            return selected.range;
          }
        }
      }
    }
  }
  return;
}

export async function renameRequestProvider(params: RenameParams): Promise<WorkspaceEdit | undefined> {
  return;
}