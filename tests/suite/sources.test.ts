import setupParser, { getSourcesContent, getSourcesList } from "../parserSetup";
import { test, expect, describe } from "vitest";
import path from "path";

const parser = setupParser();

// The purpose of this file is to test the parser against all the sources in the sources directory to ensure it doesn't crash.

test("Source Directory Tests", async () => {
  const list = await getSourcesList();

  for (const source of list) {
    const basename = path.basename(source);
    const baseContent = await getSourcesContent(source);

    // These are typing tests. Can the parser accept half documents without crashing?

    let content = ``;

    let baseContentSplitUpIntoPieces = [];
    const pieceSize = Math.ceil(baseContent.length / 20);
    for (let i = 0; i < baseContent.length; i += pieceSize) {
      baseContentSplitUpIntoPieces.push(baseContent.slice(i, i + pieceSize));
    }

    for (let i = 0; i < baseContentSplitUpIntoPieces.length; i++) {
      content += baseContentSplitUpIntoPieces[i];

      await parser.getDocs(basename, content, {collectReferences: true, ignoreCache: true, withIncludes: true});
    }
  }
});
