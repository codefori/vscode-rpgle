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
  static getParser(uri: string): IParser {
    const extension = uri.toLowerCase().split('.').pop();

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
    const lower = uri.toLowerCase();

    if (lower.endsWith('.rpg') || lower.endsWith('.sqlrpg')) {
      return true;
    }

    // Deprecated source types
    if (lower.endsWith('.rpg36') || lower.endsWith('.rpg38') || lower.endsWith('.sqlrpg38')) {
      return true;
    }

    return false;
  }

  static isIleFile(uri: string): boolean {
    const lower = uri.toLowerCase();
    return lower.endsWith('.rpgle') || lower.endsWith('.sqlrpgle');
  }
}
