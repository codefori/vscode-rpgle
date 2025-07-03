import { commands, ExtensionContext, Uri, window } from "vscode";
import { clearTableCache, getCache } from "./requests";
import { LanguageClient } from "vscode-languageclient/node";

export function registerCommands(context: ExtensionContext, client: LanguageClient) {
  context.subscriptions.push(
    commands.registerCommand(`vscode-rpgle.server.reloadCache`, () => {
      clearTableCache(client);
    }),

    commands.registerCommand(`vscode-rpgle.server.getCache`, (uri: Uri) => {
      return getCache(client, uri);
    })
  )
}