import { URI } from 'vscode-uri';
import * as path from 'path';
import { OpmParser } from './opm/parser';
import Parser from './ile/parser';
import Cache from './models/cache';
import Declaration from './models/declaration';

export type tablePromise = (name: string, aliases?: boolean) => Promise<Declaration[]>;
export type includeFilePromise = (baseFile: string, includeString: string) => Promise<{found: boolean, uri?: string, content?: string}>;

/**
 * Common interface for both parsers
 */
export interface IParser {
  getDocs(uri: string, content: string, options?: any): Promise<Cache | undefined>;
  setTableFetch(promise: tablePromise): void;
  setIncludeFileFetch(promise: includeFilePromise): void;
  clearParsedCache?(path: string): void;
  clearTableCache?(): void;
}

export const ILE_EXTENSIONS: readonly string[] = ['.rpgle', '.sqlrpgle'];
export const OPM_EXTENSIONS: readonly string[] = ['.rpg', '.sqlrpg'];
export const DEPRECATED_OPM_EXTENSIONS: readonly string[] = ['.rpg36', '.rpg38', '.sqlrpg38'];

/**
 * Factory to get appropriate parser based on file extension
 */
export class ParserFactory {
  static ILE_EXTENSIONS = ILE_EXTENSIONS;
  static OPM_EXTENSIONS = OPM_EXTENSIONS;
  static DEPRECATED_OPM_EXTENSIONS = DEPRECATED_OPM_EXTENSIONS;

  /**
   * Get parser for file based on extension
   * .rpg → OPM Parser
   * .rpgle, .sqlrpgle → ILE Parser
   */
  /**
   * Extract the file extension from a URI using vscode-uri to correctly
   * handle query strings, fragments, and encoding.
   * e.g. "file.rpgle?readonly%3Dfalse" → ".rpgle"
   */
  private static getExtension(uri: string): string {
    const parsed = URI.parse(uri);
    return path.extname(parsed.path).toLowerCase();
  }

  static getParser(uri: string): IParser {
    if (ParserFactory.isOpmFile(uri)) {
      return new OpmParser();
    }

    // Default to ILE parser for .rpgle, .sqlrpgle, etc.
    return new Parser();
  }

  static isOpmFile(uri: string): boolean {
    const extension = ParserFactory.getExtension(uri);
    return [...ParserFactory.OPM_EXTENSIONS, ...ParserFactory.DEPRECATED_OPM_EXTENSIONS].includes(extension);
  }

  static isIleFile(uri: string): boolean {
    const extension = ParserFactory.getExtension(uri);
    return ParserFactory.ILE_EXTENSIONS.includes(extension);
  }
}
