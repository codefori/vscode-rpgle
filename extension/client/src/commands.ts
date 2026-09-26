import { commands, ExtensionContext, Uri } from "vscode";
import { clearTableCache, getCache } from "./requests";
import { LanguageClient } from "vscode-languageclient/node";

export function registerCommands(context: ExtensionContext, client: LanguageClient) {
  const refreshFileFieldDefinitionsCache = async () => {
    await client.sendRequest(`refreshTableCache`, true);
    await clearTableCache(client, 'manual', true);
  };

  context.subscriptions.push(
    commands.registerCommand(`vscode-rpgle.refreshFileFieldDefinitionsCache`, refreshFileFieldDefinitionsCache),

    commands.registerCommand(`vscode-rpgle.server.reloadCache`, refreshFileFieldDefinitionsCache),

    commands.registerCommand(`vscode-rpgle.server.getCache`, (uri: Uri) => {
      return getCache(client, uri);
    })
  )
}