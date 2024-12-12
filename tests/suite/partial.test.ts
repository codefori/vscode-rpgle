import setupParser, { getFileContent, getSourcesList, getTestProjectsDir } from "../parserSetup";
import { test, expect, describe } from "vitest";
import path from "path";

const timeout = 1000 * 60 * 20; // 20 minutes
const parser = setupParser();

// The purpose of this file is to test the parser against all the sources in the sources directory to ensure it doesn't crash.

test("Parser partial tests", { timeout }, async () => {
  const projects = getTestProjectsDir();

  const SPLIT_SIZE = 10;
  let totalFiles = 0;

  for (const projectPath of projects) {
    const parser = setupParser(projectPath);
    const list = await getSourcesList(projectPath);

    totalFiles += list.length;

    for (let i = 0; i < list.length; i++) {
      const relativePath = list[i];
      const basename = path.basename(relativePath);

      const rs = performance.now();
      const baseContent = await getFileContent(relativePath);
      const re = performance.now();

      // These are typing tests. Can the parser accept half documents without crashing?

      let content = ``;

      let baseContentSplitUpIntoPieces = [];

      const pieceSize = Math.ceil(baseContent.length / SPLIT_SIZE);
      for (let i = 0; i < baseContent.length; i += pieceSize) {
        baseContentSplitUpIntoPieces.push(baseContent.substring(i, i + pieceSize));
      }

      // console.log(`Testing ${basename} (${i}/${list.length})...`);

      let lengths: number[] = [];
      for (let i = 0; i < baseContentSplitUpIntoPieces.length; i++) {
        content += baseContentSplitUpIntoPieces[i];

        const ps = performance.now();
        const doc = await parser.getDocs(basename, content, { collectReferences: true, ignoreCache: true, withIncludes: false });
        const pe = performance.now();

        // console.log(`\tParsed ${i+1}/${baseContentSplitUpIntoPieces.length} (${content.length}) in ${pe - ps}ms. Got ${doc.getNames().length} names.`);

        lengths.push(pe - ps);
      }

      // const lengthsAverage = lengths.reduce((a, b) => a + b, 0) / lengths.length;
      // const total = lengths.reduce((a, b) => a + b, 0);
      // const last = lengths[lengths.length - 1];
      // console.log(`\tAverage: ${lengthsAverage}ms, Full: ${last}ms, Total: ${total}`);
      // console.log(``);
    }
  }

  console.log(`Parsed ${totalFiles} files, ${SPLIT_SIZE} times each.`);
});