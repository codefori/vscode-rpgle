
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

      const def = refereneceByOffset(doc, document.offsetAt(currentPos));

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

      const def = refereneceByOffset(doc, document.offsetAt(currentPos));

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

function refereneceByOffset(scope: Cache, offset: number): Declaration|undefined {
  const props: (keyof Cache)[] = [`parameters`, `subroutines`, `procedures`, `files`, `variables`, `structs`, `constants`, `indicators`];

  for (const prop of props) {
    const list = scope[prop] as unknown as Declaration[];
    for (const def of list) {
      let possibleRef: boolean;

      // Search top level
      possibleRef = def.references.some(r => offset >= r.offset.position && offset <= r.offset.end);
      if (possibleRef) return def;

      // Search any subitems
      if (def.subItems.length > 0) {
        for (const subItem of def.subItems) {
          possibleRef = subItem.references.some(r => offset >= r.offset.position && offset <= r.offset.end);
          if (possibleRef) return subItem;
        }
      }

      // Search scope if any
      if (def.scope) {
        const inScope = refereneceByOffset(def.scope, offset);
        if (possibleRef) return inScope;
      }
    }
  }
}