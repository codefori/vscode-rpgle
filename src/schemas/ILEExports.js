
/** @type {{[name: string]: APIInterface}} */
exports.api = {
  printf: {
    type: `function`,
    insertText: `printf(\${1:value})\$0`,
    detail: `int(10)`, // usually what it returns
    description: `Print to standard out`,
    prototype: [
      `///`,
      `// printf`,
      `// Print to standard out`,
      `// @param String value pointer`,
      `///`,
      `dcl-pr printf int(10) extproc;`,
      `  value pointer value options(*string);`,
      `end-pr;`
    ]
  }
};

exports.apis = Object.keys(exports.api);