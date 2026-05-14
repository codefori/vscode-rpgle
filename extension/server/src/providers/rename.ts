
import { documents, getWordRangeAtPosition, parser } from '.';
import { PrepareRenameParams, Range, RenameParams, TextEdit, WorkspaceEdit } from "vscode-languageserver";
import Linter from '../../../../language/linter';
import Cache from '../../../../language/models/cache';
import Declaration from '../../../../language/models/declaration';

export async function renamePrepareProvider(params: PrepareRenameParams): Promise<Range | undefined> {
  const uri = params.textDocument.uri;
  const currentPos = params.position;
  const document = documents.get(uri);

  if (document) {
    const doc = await parser.getDocs(uri, document.getText());

    if (doc) {
      const def = Cache.referenceByOffset(uri, doc, document.offsetAt(currentPos));

      if (def) {
        const uniqueUris = def.references.map(ref => ref.uri).filter((value, index, self) => self.indexOf(value) === index);

        if (uniqueUris.length > 1) {
          return;
        }

        const currentSelectedRef = def?.references.find(r => document.positionAt(r.offset.start).line === currentPos.line);

        if (currentSelectedRef) {
          return Range.create(
            document.positionAt(currentSelectedRef.offset.start),
            document.positionAt(currentSelectedRef.offset.end)
          )
        }
      }
    }
  }

  return;
}

export async function renameRequestProvider(params: RenameParams): Promise<WorkspaceEdit | undefined> {
  const uri = params.textDocument.uri;
  const currentPos = params.position;
  const document = documents.get(uri);

  if (document) {
    const isFree = (document.getText(Range.create(0, 0, 0, 6)).toUpperCase() === `**FREE`);

    const doc = await parser.getDocs(uri, document.getText());

    if (doc) {
      const def = Cache.referenceByOffset(uri, doc, document.offsetAt(currentPos));

      if (def) {
        let edits: {[uri: string]: TextEdit[]} = {};

        const uniqueUris = def.references.map(ref => ref.uri).filter((value, index, self) => self.indexOf(value) === index);

        for (const uri of uniqueUris) {
          edits[uri] = def.references.filter(ref => ref.uri === uri).map(ref => ({
            newText: params.newName,
            range: Range.create(
              document.positionAt(ref.offset.start),
              document.positionAt(ref.offset.end)
            )
          }));
        }
    
        const workspaceEdit: WorkspaceEdit = {
          changes: edits
        }
    
        return workspaceEdit;
      }
    }
  }
  
  return;
}