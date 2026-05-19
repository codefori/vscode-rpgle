import { commands, ExtensionContext, Uri, window } from "vscode";
import { clearAllCache, clearTableCache, getCache, getCacheMetrics } from "./requests";
import { LanguageClient } from "vscode-languageclient/node";

function formatStats(label: string, hits: number, misses: number): string {
  const total = hits + misses;
  const hitRate = total > 0 ? `${Math.round((hits / total) * 100)}%` : `n/a`;
  return `${label}: ${hits} hits, ${misses} misses (hit rate ${hitRate})`;
}

export function registerCommands(context: ExtensionContext, client: LanguageClient) {
  context.subscriptions.push(

    commands.registerCommand(`vscode-rpgle.server.clearCache`, () => {
      clearAllCache(client);
      window.showInformationMessage(`RPGLE caches cleared.`);
    }),

    commands.registerCommand(`vscode-rpgle.server.viewCacheStats`, async () => {
      const stats = await getCacheMetrics(client);
      const message = [
        formatStats(`Parsed`, stats.parsed.hits, stats.parsed.misses),
        formatStats(`Table`, stats.table.hits, stats.table.misses),
        formatStats(`Include`, stats.include.hits, stats.include.misses),
      ].join(` | `);

      window.showInformationMessage(`RPGLE cache stats - ${message}`);
    }),

    commands.registerCommand(`vscode-rpgle.server.getCache`, (uri: Uri) => {
      return getCache(client, uri);
    })
  )
}