
const fs = require(`fs/promises`);
const glob = require(`glob`);

const Uri = require(`vscode-uri`).URI;

/**
 * This is a barebones vscode models implementation.
 * It is used so we can test the parser and linter,
 * which used some classes from vscode.
 */
module.exports = {
  Uri,
  Position: require(`./Position`),
  Range: require(`./Range`),
  EndOfLine: {
    LF: 1,
    CRLF: 2,
  },
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
    openTextDocument: async (uri) => {
      const content = (await module.exports.workspace.fs.readFile(uri)).toString();

      return {
        uri,
        eol: content.indexOf(`\r\n`) >= 0 ? module.exports.EndOfLine.CRLF : module.exports.EndOfLine.LF,
        getText: () => {return content}
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