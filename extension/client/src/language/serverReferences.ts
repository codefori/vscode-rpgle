import { commands, Definition, DocumentSymbol, languages, Location, ProgressLocation, Range, SymbolInformation, SymbolKind, TextDocument, Uri, window, workspace } from "vscode";
import { getInstance } from "../base";
import IBMi from "@halcyontech/vscode-ibmi-types/api/IBMi";
import { ConnectionConfiguration } from "@halcyontech/vscode-ibmi-types/api/Configuration";
import { IBMiMember } from "@halcyontech/vscode-ibmi-types";

export function getServerSymbolProvider() {
  let latestFetch: ExportInfo[]|undefined;

  return languages.registerWorkspaceSymbolProvider({
    provideWorkspaceSymbols: async (query, token): Promise<SymbolInformation[]> => {
      const instance = getInstance();
      const editor = window.activeTextEditor;

      // Since this is a members only resolve.
      const documentIsValid = (doc: TextDocument) => {
        return doc.uri.scheme === `member` && doc.languageId === `rpgle`;
      }

      if (instance && instance.getConnection()) {
        const connection = instance.getConnection();
        const config = connection.config! //TODO in vscode-ibmi 3.0.0 - change to getConfig()

        let member: IBMiMember|undefined;

        if (editor && documentIsValid(editor.document)) {
          const uriPath = editor.document.uri.path;
          member = connection.parserMemberPath(uriPath);
        }

        const libraryList = getLibraryList(config, member);

        if (query.length === 0 || !latestFetch) {
          latestFetch = await binderLookup(connection, libraryList, {generic: query});
        }

        return latestFetch.map(e => {
          return new SymbolInformation(
            e.symbolName,
            SymbolKind.Function,
            e.moduleName,
            new Location(e.assumedUri, new Range(0, 0, 0, 0))
          )
        })
      }

      return [];
    },

    resolveWorkspaceSymbol: async (symbol, token): Promise<SymbolInformation | undefined> => {
      const matchingSymbol = await window.withProgress({location: ProgressLocation.Window, title: `Fetching symbol ${symbol.name}`}, () => getSymbolFromDocument(symbol.location.uri, symbol.name));

      if (matchingSymbol) {
        return new SymbolInformation(
          matchingSymbol.name,
          matchingSymbol.kind,
          symbol.containerName,
          new Location(symbol.location.uri, matchingSymbol.selectionRange)
        )
      }

      return symbol;
    }
  })
}

export function getServerImplementationProvider() {
  return languages.registerImplementationProvider({language: `rpgle`, scheme: `member`}, {
    async provideImplementation(document, position, token): Promise<Definition|undefined> {
      const instance = getInstance();

      if (instance && instance.getConnection()) {
        const word = document.getText(document.getWordRangeAtPosition(position));
        const connection = instance.getConnection();
        const config = connection.config! //TODO in vscode-ibmi 3.0.0 - change to getConfig()

        const uriPath = document.uri.path;
        const member = connection.parserMemberPath(uriPath);

        const libraryList = getLibraryList(config, member);

        const exports = await binderLookup(connection, libraryList, {specific: word});

        if (exports.length) {
          const exportsInLibraryListOrder = libraryList.map(lib => exports.find(e => e.sourceLibrary === lib)).filter(e => e) as ExportInfo[];

          const resultingLocation = await window.withProgress({location: ProgressLocation.Window, title: `Resolving ${word}`}, async (progress) => {
            for (const exportInfo of exportsInLibraryListOrder) {
              progress.report({message: `checking ${exportInfo.moduleLibrary}/${exportInfo.moduleName}`});
              const uri = exportInfo.assumedUri;

              const possibleSymbol = await getSymbolFromDocument(uri, word);
              if (possibleSymbol) {
                return new Location(uri, possibleSymbol.selectionRange);
              } 
            }
          });

          return resultingLocation;
        }

        return;
      }
    },
  });
}

async function getSymbolFromDocument(docUri: Uri, name: string): Promise<DocumentSymbol | undefined> {
  try {
    const openedDocument = await workspace.openTextDocument(docUri);
    const symbols = await getDocumentSymbols(openedDocument.uri);
    return symbols.find(s => s.name.toUpperCase() === name.toUpperCase());
  } catch (e) {
    console.log(e);
  }

  return;
}

function getLibraryList(config: ConnectionConfiguration.Parameters, member?: IBMiMember): string[] {
  let libraryList = [config.currentLibrary, ...config.libraryList];

  if (member) {
    const editorLibrary = member.library;
    if (editorLibrary) {
      if (!libraryList.includes(editorLibrary)) {
        libraryList.unshift(editorLibrary);
      }
    }
  }

  return libraryList;
}

function getDocumentSymbols(uri: Uri) {
  return commands.executeCommand<DocumentSymbol[]>(`vscode.executeDocumentSymbolProvider`, uri) || [];
}

interface ExportInfo {
  symbolName: string;
  programLibrary: string;
  programName: string;
  moduleLibrary: string;
  moduleName: string;
  sourceLibrary: string;
  sourceFile: string;
  sourceMember: string;
  attribute: string;
  assumedUri: Uri;
}

async function binderLookup(connection: IBMi, libraryList: string[], filter: {specific?: string, generic?: string} = {}) {
  let symbolClause = ``;

  if (filter.generic) {
    symbolClause = filter.generic ? `UPPER(b.SYMBOL_NAME) like '%${filter.generic.toUpperCase()}%' and` : ``;
  } else if (filter.specific) {
    symbolClause = filter.specific ? `UPPER(b.SYMBOL_NAME) = '${filter.specific.toUpperCase()}' and` : ``;
  }

  const libraryInList = libraryList.map(lib => `'${lib.toUpperCase()}'`).join(`, `);

  const statement = [
    `select`,
    `	b.SYMBOL_NAME,`,
    `	b.PROGRAM_LIBRARY as PGM_LIB,`,
    `	c.ENTRY as PGM_NAME,`,
    `	a.BOUND_MODULE_LIBRARY as MOD_LIB, `,
    `	a.BOUND_MODULE as MOD_NAME, `,
    // ...(streamFileSupported ? [`a.SOURCE_STREAM_FILE_PATH as PATH,`] : []),
    `	a.SOURCE_FILE_LIBRARY as LIB, `,
    `	a.SOURCE_FILE as SPF, `,
    `	a.SOURCE_FILE_MEMBER as MBR,`,
    ` a.MODULE_ATTRIBUTE as ATTR`,
    `from QSYS2.BOUND_MODULE_INFO as a`,
    `right join QSYS2.PROGRAM_EXPORT_IMPORT_INFO as b`,
    `	on a.PROGRAM_LIBRARY = b.PROGRAM_LIBRARY and a.PROGRAM_NAME = b.PROGRAM_NAME`,
    `right join qsys2.BINDING_DIRECTORY_INFO as c`,
    `	on c.ENTRY = b.PROGRAM_NAME`,
    `where ${symbolClause}`,
    `  (c.BINDING_DIRECTORY_LIBRARY in (${libraryInList})) and`,
    `  ((c.ENTRY_LIBRARY = b.PROGRAM_LIBRARY) or (c.ENTRY_LIBRARY = '*LIBL' and b.PROGRAM_LIBRARY in (${libraryInList}))) and`,
    `  (a.SOURCE_FILE_MEMBER is not null)`,
    // `  (${streamFileSupported ? `a.SOURCE_STREAM_FILE_PATH is not null or` : ``} a.SOURCE_FILE_MEMBER is not null)`
  ].join(` `);

  let exports: ExportInfo[] = [];

  try {
    const results = await connection.runSQL(statement);

    exports = results.map((r): ExportInfo => {
      return {
        symbolName: r.SYMBOL_NAME as string,
        programLibrary: r.PGM_LIB as string,
        programName: r.PGM_NAME as string,
        moduleLibrary: r.MOD_LIB as string,
        moduleName: r.MOD_NAME as string,
        sourceLibrary: r.LIB as string,
        sourceFile: r.SPF as string,
        sourceMember: r.MBR as string,
        attribute: r.ATTR as string,
        assumedUri: Uri.from({
          scheme: `member`,
          path: [``, r.LIB, r.SPF, `${r.MBR}.${r.ATTR}`].join(`/`)
        })
      }
    })

  } catch (e) {
    console.log(e);
  }

  return exports;
}