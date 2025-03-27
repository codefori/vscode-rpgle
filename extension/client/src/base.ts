import { CodeForIBMi } from "@halcyontech/vscode-ibmi-types";
import Instance from "@halcyontech/vscode-ibmi-types/Instance";
import { ConfigurationChangeEvent, Extension, extensions, workspace } from "vscode";

let baseExtension: Extension<CodeForIBMi>|undefined;

export async function checkAndWait() {
  baseExtension = extensions.getExtension(`halcyontechltd.code-for-ibmi`);

  if (baseExtension) {
    if (!baseExtension.isActive) {
      await baseExtension.activate();
    }
  }

  return getInstance();
}

/**
 * This should be used on your extension activation.
 */
export function loadBase(): CodeForIBMi|undefined {
  if (!baseExtension) {
    baseExtension = (extensions ? extensions.getExtension(`halcyontechltd.code-for-ibmi`) : undefined);
  }
  
  return (baseExtension && baseExtension.isActive && baseExtension.exports ? baseExtension.exports : undefined);
}

/**
 * Used when you want to fetch the extension 'instance' (the connection)
 */
export function getInstance(): Instance|undefined {
  const base = loadBase();
  return (base ? base.instance : undefined);
}

// Stolen directly from vscode-ibmi
export function onCodeForIBMiConfigurationChange<T>(props: string | string[], todo: (value: ConfigurationChangeEvent) => void) {
  const keys = (Array.isArray(props) ? props : Array.of(props)).map(key => `code-for-ibmi.${key}`);
  return workspace.onDidChangeConfiguration(async event => {
    if (keys.some(key => event.affectsConfiguration(key))) {
      todo(event);
    }
  })
}