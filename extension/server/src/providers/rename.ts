
import { documents, getRangeAtPosition, getWordRangeAtPosition, parser } from '.';
import { PrepareRenameParams, Range, RenameParams, TextEdit, WorkspaceEdit } from "vscode-languageserver";
import Linter from '../../../../language/linter';
import Cache from '../../../../language/models/cache';
import Declaration from '../../../../language/models/declaration';

export async function renamePrepareProvider(params: PrepareRenameParams): Promise<Range | undefined> {
  const uri = params.textDocument.uri;
  const currentPos = params.position;
  const document = documents.get(uri);

  if (document) {
    const isFree = (document.getText(Range.create(0, 0, 0, 6)).toUpperCase() === `**FREE`);

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

      const def = Cache.refereneceByOffset(doc, document.offsetAt(currentPos));

      if (def) {
        const currentSelectedRef = def?.references.find(r => document.positionAt(r.offset.position).line === currentPos.line);

        if (currentSelectedRef) {
          return Range.create(
            document.positionAt(currentSelectedRef.offset.position),
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

      const def = Cache.refereneceByOffset(doc, document.offsetAt(currentPos));

      if (def) {
        const edits: TextEdit[] = def.references.map(ref => ({
          newText: params.newName,
          range: Range.create(
            document.positionAt(ref.offset.position),
            document.positionAt(ref.offset.end)
          )
        }));
    
        const workspaceEdit: WorkspaceEdit = {
          changes: {
            [document.uri]: edits
          }
        }
    
        return workspaceEdit;
      }
    }
  }
  
  return;
}