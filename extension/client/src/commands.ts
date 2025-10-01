import { commands, ExtensionContext, Uri, window, workspace } from "vscode";
import { clearTableCache, getCache } from "./requests";
import { LanguageClient } from "vscode-languageclient/node";

export function registerCommands(context: ExtensionContext, client: LanguageClient) {
  context.subscriptions.push(
    commands.registerCommand(`vscode-rpgle.generateBinderSource`, async (content: string) => {
      const document = await workspace.openTextDocument({ language: `bnd`, content: content });
      await window.showTextDocument(document);
    }),

    commands.registerCommand(`vscode-rpgle.server.reloadCache`, () => {
      clearTableCache(client);
    }),

    commands.registerCommand(`vscode-rpgle.server.getCache`, (uri: Uri) => {
      return getCache(client, uri);
    })
  )
}