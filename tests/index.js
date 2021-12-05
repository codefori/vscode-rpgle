
const fs = require(`fs/promises`);
const glob = require(`glob`);

const Uri = require(`vscode-uri`).URI;

// Force a polyfill for the test

let Module = require(`module`);
let originalRequire = Module.prototype.require;

/** @ts-ignore */
Module.prototype.require = function(){
  // We have to re-implement some VS Code APIs
  // due to the fact we rely on the fact that
  // we have to support 3 file systems (file, member, streamfile)
  switch (arguments[0]) {
  case `vscode`:
    return {
      Uri,
      Position: require(`./models/Position`),
      Range: require(`./models/Range`),
      workspace: {
        workspaceFolders: [
          {
            uri: Uri.parse(process.cwd()),
            name: `workspace`,
          },
        ],
        fs: {
          readFile: (uri) => {
            return fs.readFile(uri.fsPath);
          }
        },
        findFiles: async (path) => {
          // Sync is fine here since it's just in test
          const files = glob.sync(path, {
            cwd: process.cwd(),
            absolute: true,
            nocase: true,
          });

          return files.map((file) => Uri.file(file));
        }
      }
    };
  default:
    return originalRequire.apply(this, arguments);
  }
};

async function run() {
  console.log(`Running tests...`);

  const suite = require(`./suite`);
  const testNames = Object.keys(suite);

  console.log(`Running ${testNames.length} tests: ${testNames.join(`, `)}`);

  for (const testName of testNames) {
    const test = suite[testName];
    console.log(`Running ${testName}`);
    await test();
  }
};

run();