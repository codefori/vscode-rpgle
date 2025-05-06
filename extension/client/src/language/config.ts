import { languages } from "vscode";

export function setLanguageSettings() {
  return languages.setLanguageConfiguration(`rpgle`, {
    wordPattern: /(-?\d*\.\d\w*)|([^\`\~\!\%\^\&\*\(\)\-\=\+\[\{\]\}\\\|\;\:\'\"\,\.\<\>\/\?\s]+)/g
  });
}