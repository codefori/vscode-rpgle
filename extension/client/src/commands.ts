import { commands, ExtensionContext, window } from "vscode";
import { clearTableCache } from "./requests";
import { LanguageClient } from "vscode-languageclient/node";

export function registerCommands(context: ExtensionContext, client: LanguageClient) {
  context.subscriptions.push(
    commands.registerCommand(`vscode-rpgle.server.reloadCache`, () => {
      clearTableCache(client);
    })
  )
}