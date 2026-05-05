import path from "path";
import { test, expect } from "vitest";
import { readFile } from "fs/promises";
import Parser from "../../language/parser";
import { URI } from "vscode-uri";
import { resolveWorkspaceIncludePath } from "../../extension/server/src/includeResolver";

const TESTS_DIR = path.join(__dirname, "..");

test("resolves absolute path by joining workspace fsPath and relative include", () => {
  const workspaceUri = URI.file("/home/user/project").toString();
  const { absolutePath } = resolveWorkspaceIncludePath(workspaceUri, "includes/mylib.rpgleinc");
  expect(absolutePath).toBe("/home/user/project/includes/mylib.rpgleinc");
});

test("resolves leading ./ in include path", () => {
  // path.join normalises './' just like path.posix.join would.
  const workspaceUri = URI.file("/home/user/project").toString();
  const { absolutePath } = resolveWorkspaceIncludePath(workspaceUri, "./includes/mylib.rpgleinc");
  expect(absolutePath).toBe("/home/user/project/includes/mylib.rpgleinc");
});

test("fileUri is a valid file:// URI", () => {
  const workspaceUri = URI.file("/home/user/project").toString();
  const { fileUri } = resolveWorkspaceIncludePath(workspaceUri, "includes/mylib.rpgleinc");
  expect(fileUri).toMatch(/^file:\/\/\//);
});

test("fileUri fsPath roundtrips to absolutePath", () => {
  // Critical invariant: URI.file(absolutePath).toString() must produce a URI
  // whose .fsPath equals absolutePath. This ensures position.path (set during
  // parsing) can be compared directly to textDocument.uri in LSP providers.
  const workspaceUri = URI.file("/home/user/project").toString();
  const { absolutePath, fileUri } = resolveWorkspaceIncludePath(workspaceUri, "includes/mylib.rpgleinc");
  expect(URI.parse(fileUri).fsPath).toBe(absolutePath);
});

test("fileUri equals URI.file(absolutePath) — not URI.from with path", () => {
  // Regression guard: the old code used URI.from({scheme, path: absolutePath}).
  // On Windows that percent-encodes ':' and '\' in the path component, producing
  // a URI that never matches textDocument.uri. URI.file() handles native paths
  // correctly on all platforms.
  const workspaceUri = URI.file("/home/user/project").toString();
  const { absolutePath, fileUri } = resolveWorkspaceIncludePath(workspaceUri, "includes/mylib.rpgleinc");
  expect(fileUri).toBe(URI.file(absolutePath).toString());
});

test("URI.from with path percent-encodes Windows separators — documents the original bug", () => {
  // This shows WHY the fix was needed: URI.from({path}) does not normalise
  // Windows-style paths, so backslashes and colons get percent-encoded.
  const winPath = "C:\\project\\src\\file.rpgle";
  const buggyUri = URI.from({ scheme: "file", path: winPath }).toString();
  expect(buggyUri).toContain("%3A"); // ':' encoded
  expect(buggyUri).toContain("%5C"); // '\' encoded
  // A provider comparing this to textDocument.uri ("file:///c:/project/...") would
  // never find a match, silently breaking completion/symbols/hover.
});

/**
 * Parser that uses resolveWorkspaceIncludePath (the actual server helper) to
 * build include URIs — the same way server.ts does after the fix.
 */
function setupParserWithProductionFetch(workspaceRoot: string): Parser {
  const workspaceFolderUri = URI.file(workspaceRoot).toString();
  const parser = new Parser();

  parser.setIncludeFileFetch(async (_baseFile: string, includeFile: string) => {
    if (
      (includeFile.startsWith(`'`) && includeFile.endsWith(`'`)) ||
      (includeFile.startsWith(`"`) && includeFile.endsWith(`"`))
    ) {
      includeFile = includeFile.substring(1, includeFile.length - 1);
    }

    // Use the production helper — same code path as server.ts
    const resolved = resolveWorkspaceIncludePath(workspaceFolderUri, includeFile);

    try {
      const content = await readFile(resolved.absolutePath, { encoding: "utf-8" });
      return { found: true, uri: resolved.fileUri, content };
    } catch {
      return { found: false };
    }
  });

  return parser;
}

test("include position.path is a valid file:// URI", async () => {
  const parser = setupParserWithProductionFetch(TESTS_DIR);
  const baseUri = URI.file(path.join(TESTS_DIR, "source.rpgle")).toString();

  const lines = [`**FREE`, `/copy './rpgle/copy1.rpgle'`].join(`\n`);
  const cache = await parser.getDocs(baseUri, lines, { withIncludes: true, ignoreCache: true });
  expect(cache).toBeDefined();
  if (!cache) throw new Error("Expected parser cache to be defined");

  expect(cache.includes.length).toBe(1);
  expect(cache.procedures.length).toBe(1);
  expect(cache.procedures[0].position.path).toMatch(/^file:\/\/\//);
});

test("include position.path basename matches the included file", async () => {
  const parser = setupParserWithProductionFetch(TESTS_DIR);
  const baseUri = URI.file(path.join(TESTS_DIR, "source.rpgle")).toString();

  const lines = [`**FREE`, `/copy './rpgle/copy1.rpgle'`].join(`\n`);
  const cache = await parser.getDocs(baseUri, lines, { withIncludes: true, ignoreCache: true });
  expect(cache).toBeDefined();
  if (!cache) throw new Error("Expected parser cache to be defined");

  expect(cache.procedures.length).toBe(1);
  expect(path.basename(URI.parse(cache.procedures[0].position.path).fsPath)).toBe("copy1.rpgle");
});

test("include position.path matches textDocument.uri — the provider invariant", async () => {
  // Autocompletion, documentSymbols and other providers filter declarations by
  //   decl.position.path === handler.textDocument.uri
  // For this to work, the URI stored during parsing must be identical to the
  // file:// URI VSCode hands to the provider when the include file is open.
  const parser = setupParserWithProductionFetch(TESTS_DIR);
  const includeAbsPath = path.join(TESTS_DIR, "rpgle", "copy1.rpgle");
  const expectedTextDocumentUri = URI.file(includeAbsPath).toString();
  const baseUri = URI.file(path.join(TESTS_DIR, "source.rpgle")).toString();

  const lines = [`**FREE`, `/copy './rpgle/copy1.rpgle'`].join(`\n`);
  const cache = await parser.getDocs(baseUri, lines, { withIncludes: true, ignoreCache: true });
  expect(cache).toBeDefined();
  if (!cache) throw new Error("Expected parser cache to be defined");

  expect(cache.procedures.length).toBe(1);
  expect(cache.procedures[0].position.path).toBe(expectedTextDocumentUri);
});

test("include position.path fsPath roundtrips to the original absolute path", async () => {
  const parser = setupParserWithProductionFetch(TESTS_DIR);
  const includeAbsPath = path.join(TESTS_DIR, "rpgle", "copy1.rpgle");
  const baseUri = URI.file(path.join(TESTS_DIR, "source.rpgle")).toString();

  const lines = [`**FREE`, `/copy './rpgle/copy1.rpgle'`].join(`\n`);
  const cache = await parser.getDocs(baseUri, lines, { withIncludes: true, ignoreCache: true });
  expect(cache).toBeDefined();
  if (!cache) throw new Error("Expected parser cache to be defined");

  expect(cache.procedures.length).toBe(1);
  expect(URI.parse(cache.procedures[0].position.path).fsPath).toBe(includeAbsPath);
});
