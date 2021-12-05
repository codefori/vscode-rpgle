
const vscode = require(`vscode`);
const path = require(`path`);

const getInstance = require(`./base`);

module.exports = class {

  /**
   * @param {vscode.Uri} workingUri Path being worked with
   * @param {string} getPath IFS or member path to fetch (in the format of an RPGLE copybook)
   */
  static getPathInfo(workingUri, getPath) {

    /** @type {string} */
    let finishedPath = undefined;

    /** @type {string[]} */
    let memberPath = undefined;

    /** @type {"streamfile"|"member"|"file"|undefined} */
    let type = undefined;

    switch (workingUri.scheme) {
    case `file`:
      // Local file
      type = `file`;
      if (getPath.startsWith(`'`)) getPath = getPath.substring(1);
      if (getPath.endsWith(`'`)) getPath = getPath.substring(0, getPath.length - 1);

      if (getPath.startsWith(`/`)) {
        //Get from root
        finishedPath = getPath;
      } 

      else {
        finishedPath = path.posix.join(vscode.workspace.workspaceFolders[0].uri.path, getPath);
      };
      break;

    case `streamfile`:
      type = `streamfile`;
      //Fetch IFS

      if (getPath.startsWith(`'`)) getPath = getPath.substring(1);
      if (getPath.endsWith(`'`)) getPath = getPath.substring(0, getPath.length - 1);

      if (getPath.startsWith(`/`)) {
        //Get from root
        finishedPath = getPath;
      } 

      else {
        const instance = getInstance();
        const config = instance.getConfig();
        finishedPath = path.posix.join(config.homeDirectory, getPath);
      };
      break

    case `member`:
      //Fetch member
      const getLib = getPath.split(`/`);
      const getMember = getLib[getLib.length-1].split(`,`);
      const workingPath = workingUri.path.split(`/`);
      memberPath = [undefined, undefined, `QRPGLEREF`, undefined];

      if (workingPath.length === 4) { //ASP not included
        memberPath[1] = workingPath[1];
        memberPath[2] = workingPath[2];
      } else {
        memberPath[0] = workingPath[1];
        memberPath[1] = workingPath[2];
        memberPath[2] = workingPath[3];
      }

      switch (getMember.length) {
      case 1:
        memberPath[3] = getMember[0];
        break;
      case 2:
        memberPath[2] = getMember[0];
        memberPath[3] = getMember[1];
      }

      if (getLib.length === 2) {
        memberPath[1] = getLib[0];
      }

      if (memberPath[3].includes(`.`)) {
        memberPath[3] = memberPath[3].substr(0, memberPath[3].lastIndexOf(`.`));
      }

      finishedPath = memberPath.join(`/`);

      if (workingPath.length === 5) {
        finishedPath = `/${finishedPath}`;
      }

      type = `member`;
      break;
    }

    if (finishedPath)
      finishedPath = finishedPath.toUpperCase();

    return {type, memberPath, finishedPath};
  }
}