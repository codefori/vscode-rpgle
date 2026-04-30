import { commands, ExtensionContext, Uri, window } from "vscode";
import { clearAllCache, clearTableCache, getCache } from "./requests";
import { LanguageClient } from "vscode-languageclient/node";

export function registerCommands(context: ExtensionContext, client: LanguageClient) {
  context.subscriptions.push(
    commands.registerCommand(`vscode-rpgle.server.reloadCache`, () => {
      clearTableCache(client);
    }),

    commands.registerCommand(`vscode-rpgle.server.clearAllCache`, () => {
      clearAllCache(client);
      window.showInformationMessage(`RPGLE caches cleared.`);
    }),

    commands.registerCommand(`vscode-rpgle.server.getCache`, (uri: Uri) => {
      return getCache(client, uri);
    })
  )
}