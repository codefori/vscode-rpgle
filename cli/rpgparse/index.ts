import glob from "glob";
import Parser from "../../language/parser";
import { readFileSync, writeFileSync, existsSync, mkdirSync } from "fs";
import path from "path";

main();

function printHelp() {
  console.log(`
Usage: "" [options]

Options:
  -f, --files <glob>       Specify the files to scan (required)
  -c, --cwd <directory>    Specify the current working directory
  -d, --outdir <directory> Specify the output directory
  -o, --output <file>      Specify the output file
  -h, --help               Display this help message
  `);
}

async function main() {
  const parms = process.argv.slice(2);

  let cwd = process.cwd();
  let scanGlob = `**/*.{SQLRPGLE,sqlrpgle,RPGLE,rpgle}`;
  let filesProvided: boolean = false;
  let output: string = undefined;
  let outDir: string = undefined;

  for (let i = 0; i < parms.length; i++) {
    switch (parms[i]) {
      case `-f`:
      case `--files`:
        scanGlob = parms[i + 1];
        filesProvided = true;
        i++;
        break;
      case `-c`:
      case `--cwd`:
        cwd = parms[i + 1];
        i++;
        break;

      case `-d`:
      case `--outdir`:
        outDir = parms[i + 1];
        i++;
        break;

      case `-o`:
      case `--output`:
        output = parms[i + 1];
        i++;
        break;

      case `-h`:
      case `--help`:
        printHelp();
        process.exit(0);
    }
  }

  if (!filesProvided) {
    console.log(`files not provided`);
    process.exit(1);
  }

  let parser: Parser = new Parser();
  let files: string[];

  try {
    files = getFiles(cwd, scanGlob);
  } catch (e) {
    error(e.message || e);
    process.exit(1);
  }

  for (const file of files) {
    try {
      const content = readFileSync(file, { encoding: `utf-8` });

      if (
        content.length > 6 &&
        content.substring(0, 6).toLowerCase() === `**free`
      ) {
        const docs = await parser.getDocs(file, content, {
          withIncludes: true,
          collectReferences: true,
        });

        const filterdDocs = JSON.stringify(docs.filterCache(), null, 2);

        if (output) {
          const outputPath = path.join(outDir ?? cwd, output);
          try {
            if (outDir) {
              if (!existsSync(outDir)) {
                mkdirSync(outDir, { recursive: true });
              }
            }
            writeFileSync(outputPath, filterdDocs, {
              encoding: `utf-8`,
              flag: `w`,
            });
          } catch (err) {
            console.log(err);
          }
        } else {
          console.log(filterdDocs);
        }
      }
    } catch (e) {
      error(`failed to parse ${file}: ${e.message || e}`);
      process.exit(1);
    }
  }
} // end of main

function getFiles(cwd: string, globPath: string): string[] {
  return glob.sync(globPath, {
    cwd,
    absolute: true,
    nocase: true,
  });
}

function error(line: string) {
  process.stdout.write(line + `\n`);
}
