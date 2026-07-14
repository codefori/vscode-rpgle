import { URI } from 'vscode-uri';
import * as path from 'path';

export const ILE_EXTENSIONS: readonly string[] = ['.rpgle', '.sqlrpgle'];
export const OPM_EXTENSIONS: readonly string[] = ['.rpg', '.sqlrpg'];
export const DEPRECATED_OPM_EXTENSIONS: readonly string[] = ['.rpg36', '.rpg38', '.sqlrpg38'];

export function getExtensionFromUri(uri: string): string {
  const parsed = URI.parse(uri);
  return path.extname(parsed.path).toLowerCase();
}

export function isOpmFileByUri(uri: string): boolean {
  const extension = getExtensionFromUri(uri);
  return [...OPM_EXTENSIONS, ...DEPRECATED_OPM_EXTENSIONS].includes(extension);
}

export function isIleFileByUri(uri: string): boolean {
  const extension = getExtensionFromUri(uri);
  return ILE_EXTENSIONS.includes(extension);
}