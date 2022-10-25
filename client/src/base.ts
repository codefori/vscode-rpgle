const vscode = require(`vscode`);

export default function getBase(): any|undefined {
  const baseExtension = (vscode.extensions ? vscode.extensions.getExtension(`halcyontechltd.code-for-ibmi`) : undefined);
  return (baseExtension && baseExtension.isActive && baseExtension.exports ? baseExtension.exports.instance : null);
}