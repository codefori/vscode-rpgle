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

/**
 * Factory to get appropriate parser based on file extension
 */
export class ParserFactory {
  /**
   * Get parser for file based on extension
   * .rpg → OPM Parser
   * .rpgle, .sqlrpgle → ILE Parser
   */
  /**
   * Extract the file extension from a URI, stripping any query string or fragment first.
   * e.g. "file.rpgle?readonly%3Dfalse" → "rpgle"
   */
  private static getExtension(uri: string): string {
    const path = uri.split('?')[0];
    const dotIndex = path.lastIndexOf('.');
    return dotIndex !== -1 ? path.substring(dotIndex + 1).toLowerCase() : '';
  }

  static getParser(uri: string): IParser {
    const extension = ParserFactory.getExtension(uri);

    if (extension === 'rpg' || extension === 'sqlrpg') {
      return new OpmParser();
    }

    // Deprecated source types
    if (extension === 'rpg36' || extension === 'rpg38' || extension === 'sqlrpg38') {
      return new OpmParser();
    }

    // Default to ILE parser for .rpgle, .sqlrpgle, etc.
    return new Parser();
  }

  static isOpmFile(uri: string): boolean {
    const extension = ParserFactory.getExtension(uri);

    if (extension === 'rpg' || extension === 'sqlrpg') {
      return true;
    }

    // Deprecated source types
    if (extension === 'rpg36' || extension === 'rpg38' || extension === 'sqlrpg38') {
      return true;
    }

    return false;
  }

  static isIleFile(uri: string): boolean {
    const extension = ParserFactory.getExtension(uri);
    return extension === 'rpgle' || extension === 'sqlrpgle';
  }
}
