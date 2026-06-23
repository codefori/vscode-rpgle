import { OpmParser } from './opm/parser';
import Parser from './ile/parser';
import Cache from './models/cache';
import Declaration from './models/declaration';
import {
  ILE_EXTENSIONS,
  OPM_EXTENSIONS,
  DEPRECATED_OPM_EXTENSIONS,
  isIleFileByUri,
  isOpmFileByUri,
} from './utils/fileRouting';

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
  static ILE_EXTENSIONS = ILE_EXTENSIONS;
  static OPM_EXTENSIONS = OPM_EXTENSIONS;
  static DEPRECATED_OPM_EXTENSIONS = DEPRECATED_OPM_EXTENSIONS;

  static getParser(uri: string): IParser {
    if (ParserFactory.isOpmFile(uri)) {
      return new OpmParser();
    }

    // Default to ILE parser for .rpgle, .sqlrpgle, etc.
    return new Parser();
  }

  static isOpmFile(uri: string): boolean {
    return isOpmFileByUri(uri);
  }

  static isIleFile(uri: string): boolean {
    return isIleFileByUri(uri);
  }
}
