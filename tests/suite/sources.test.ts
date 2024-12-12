import setupParser, { getFileContent, getSourcesList, getTestProjectsDir } from "../parserSetup";
import { test } from "vitest";
import path from "path";
import { fail } from "assert";
import Declaration from "../../language/models/declaration";
import Cache from "../../language/models/cache";
import { Reference } from "../../language/parserTypes";

const timeout = 1000 * 60 * 10; // 10 minutes

// The purpose of this file is to test the parser against all the sources in the sources directory to ensure it doesn't crash.

test("Generic reference tests", { timeout }, async () => {
  const projects = getTestProjectsDir();

  let totalFiles = 0;

  for (const projectPath of projects) {
    const parser = setupParser(projectPath);
    const list = await getSourcesList(projectPath);

    totalFiles += list.length;

    for (let i = 0; i < list.length; i++) {
      const relativePath = list[i];
      const basename = path.basename(relativePath);

      const baseContent = await getFileContent(relativePath);

      const ps = performance.now();
      const doc = await parser.getDocs(basename, baseContent, { collectReferences: true, ignoreCache: true, withIncludes: true });
      const pe = performance.now();

      let cachedFiles: {[uri: string]: string} = {};
      let referencesCollected = 0;
      let errorCount = 0;

      const printReference = (def: Declaration, content: string, ref: Reference) => {
        console.log({
          def: def.name,
          uri: ref.uri,
          offset: ref.offset,
          content: content.substring(ref.offset.start, ref.offset.end),
          about: content.substring(ref.offset.start - 10, ref.offset.end + 10)
        })
      }

      const checkReferences = async (def: Declaration) => {
        const refs = def.references;
        const uniqueUris = refs.map(r => r.uri).filter((value, index, self) => self.indexOf(value) === index);

        for (const refUri of uniqueUris) {
          if (refUri === basename) {
            cachedFiles[refUri] = baseContent;
          }
          
          if (!cachedFiles[refUri]) {
            cachedFiles[refUri] = await getFileContent(refUri);
          }
        }

        for (const ref of refs) {
          const offsetContent = cachedFiles[ref.uri].substring(ref.offset.start, ref.offset.end);

          if (offsetContent.toUpperCase() === def.name.toUpperCase()) {
            referencesCollected++;
          } else {
            errorCount++;
            printReference(def, cachedFiles[ref.uri], ref);
          }
        }
      }

      const checkScope = async (scope: Cache) => {
        for (const def of [...scope.variables, ...scope.subroutines, ...scope.procedures, ...scope.constants, ...scope.structs, ...scope.files, ...scope.tags, ...scope.sqlReferences]) {
          await checkReferences(def);

          if (def.subItems && def.subItems.length > 0) {
            for (const sub of def.subItems) {
              await checkReferences(sub);
            }
          }

          if (def.scope) {
            await checkScope(def.scope);
          }
        }
      }

      const ss = performance.now();
      await checkScope(doc);
      const se = performance.now();

      if (errorCount > 0) {
        fail(`Found ${errorCount} errors in ${basename}`);
      }

      // console.log(`Parsed ${basename} in ${pe - ps}ms. Validated in ${se-ss} (${i+1}/${list.length}). Found ${referencesCollected} references.`);
    }
  }

  console.log(`Parsed ${totalFiles} files.`);
});