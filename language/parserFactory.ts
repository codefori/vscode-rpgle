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
  getDocs(uri: string, content: string, options?: any): Promise<Cache>;
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
    
    // Default to ILE parser for .rpgle, .sqlrpgle, etc.
    return new Parser();
  }
  
  static isOpmFile(uri: string): boolean {
    return uri.toLowerCase().endsWith('.rpg') || uri.toLowerCase().endsWith('.sqlrpg');
  }
  
  static isIleFile(uri: string): boolean {
    const lower = uri.toLowerCase();
    return lower.endsWith('.rpgle') || lower.endsWith('.sqlrpgle');
  }
}
