import path from 'path';
import { URI } from 'vscode-uri';

/**
 * Resolves a relative include path against a workspace folder, returning the
 * absolute native file-system path and a well-formed file:// URI.
 *
 * Two intentional choices here (both fixing Windows compatibility):
 *
 *   1. path.join  — OS-aware; preserves Windows drive letters such as
 *      "C:\project" that path.posix.join would mangle.
 *
 *   2. URI.file() — constructs a file:// URI from a native path correctly on
 *      both platforms. URI.from({ scheme, path }) does NOT normalise Windows
 *      backslashes and produces percent-encoded URIs (e.g. "C%3A%5Cproject")
 *      that never match the clean file:// URIs VSCode uses as textDocument.uri,
 *      which broke features like autocompletion and document symbols.
 *
 * @param workspaceFolderUri  A workspace folder URI string (e.g. WorkspaceFolder.uri)
 * @param includeRelativePath The relative include path, already stripped of quotes
 */
export function resolveWorkspaceIncludePath(
	workspaceFolderUri: string,
	includeRelativePath: string
): { absolutePath: string; fileUri: string } {
	const workspaceFsPath = URI.parse(workspaceFolderUri).fsPath;
	const absolutePath = path.join(workspaceFsPath, includeRelativePath);
	return {
		absolutePath,
		fileUri: URI.file(absolutePath).toString(),
	};
}

