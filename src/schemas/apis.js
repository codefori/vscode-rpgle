
/** @type {{[name: string]: APIInterface}} */
exports.api = {
  printf: {
    type: `function`,
    insertText: `printf(\${1:value})\$0`,
    description: `Print to standard out`,
    prototype: [
      `///`,
      `// printf`,
      `// Print to standard out`,
      `// @param String value pointer`,
      `///`
      `dcl-pr printf int(10) extproc;`,
      `  value pointer value options(*string);`,
      `end-pr;`
    ]
  }
};

exports.apis = Object.keys(exports.api);