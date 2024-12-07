import setupParser, { getSourcesContent, getSourcesList } from "../parserSetup";
import { test } from "vitest";
import path from "path";
import { fail } from "assert";
import Declaration from "../../language/models/declaration";
import Cache from "../../language/models/cache";

const timeout = 1000 * 60 * 10; // 10 minutes
const parser = setupParser();

// The purpose of this file is to test the parser against all the sources in the sources directory to ensure it doesn't crash.

test("Generic reference tests", {timeout}, async () => {
  const list = await getSourcesList();

  for (let i = 0; i < list.length; i++) {
    const relativePath = list[i];
    const basename = path.basename(relativePath);

    const baseContent = await getSourcesContent(relativePath);

    const ps = performance.now();
    const doc = await parser.getDocs(basename, baseContent, {collectReferences: true, ignoreCache: true, withIncludes: true});
    const pe = performance.now();

    let errorCount = 0;

    for (const def of doc.variables) {
      for (const ref of def.references) {
        const offsetContent = baseContent.substring(ref.offset.position, ref.offset.end);

        if (offsetContent.toUpperCase() !== def.name.toUpperCase()) {
          errorCount++;
        }
      }
    }

    const checkSubItems = (def: Declaration) => {
      if (def.subItems && def.subItems.length > 0) {
        for (const sub of def.subItems) {
          for (const ref of sub.references) {
            const offsetContent = baseContent.substring(ref.offset.position, ref.offset.end);

            if (offsetContent.toUpperCase() !== sub.name.toUpperCase()) {
              console.log({
                name: sub.name,
                offset: ref.offset,
                offsetContent,
                about: baseContent.substring(ref.offset.position - 10, ref.offset.end + 10)
              })
              errorCount++;
            }
          }
        }
      }
    }

    const checkScope = (scope: Cache) =>{
      for (const def of [...scope.variables, ...scope.subroutines, ...scope.procedures, ...scope.constants, ...scope.structs, ...scope.files]) {
        for (const ref of def.references) {
          const offsetContent = baseContent.substring(ref.offset.position, ref.offset.end);

          if (offsetContent.toUpperCase() !== def.name.toUpperCase()) {
            console.log({
              name: def.name,
              offset: ref.offset,
              offsetContent,
              about: baseContent.substring(ref.offset.position - 30, ref.offset.end + 30)
            })
            errorCount++;
          }
        }

        checkSubItems(def);
        if (def.scope) {
          checkScope(def.scope);
        }
      }
    }

    checkScope(doc);

    if (errorCount > 0) {
      fail(`Found ${errorCount} errors in ${basename}`);
    }

    console.log(`Parsed ${basename} in ${pe - ps}ms (${i+1}/${list.length})`);
  }
});