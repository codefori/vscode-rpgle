import { FoldingRange, FoldingRangeParams } from "vscode-languageserver";
import { documents } from ".";
import { getBlockRanges } from "../../../../language/blocks";

export async function foldingRangeProvider(handler: FoldingRangeParams): Promise<FoldingRange[]> {
	const currentPath = handler.textDocument.uri;
  const document = documents.get(currentPath);
  
  if (document) {
    const ranges = getBlockRanges(document.getText());
    return ranges.map(range => FoldingRange.create(range.start, range.end!));
  }

  return [];
}