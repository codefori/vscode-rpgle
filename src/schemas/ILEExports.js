
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
      `  format pointer value options(*string);`,
      `end-pr;`
    ],
    example: [
      `**free`,
      `dcl-s outtext varchar(50);`,
      `printf(outtext + '\\n');`,
    ]
  },
  Qp0zLprintf: {
    type: `function`,
    insertText: `QzshSystem(\${1:value})\$0`,
    detail: `int(10)`,
    description: `Prints user data specified by format-string as an information message type to the job log.`,
    prototype: [
      `///`,
      `// Qp0zLprintf`,
      `// Prints user data specified by format-string as`,
      `// an information message type to the job log.`,
      `// To ensure messages are written to the job log, always`,
      `// use a new line (\\n) at the end of each format-string.`,
      `//`,
      `// @param format-string`,
      `///`,
      `dcl-pr Qp0zLprintf int(10) extproc('Qp0zLprintf');`,
      `  format pointer value options(*string);`,
      `end-pr;`,
    ]
  },
  system: {
    type: `function`,
    insertText: `system(\${1:value})\$0`,
    detail: `int(10)`,
    description: `Execute an ILE command`,
    prototype: [
      `///`,
      `// system`,
      `// Execute an ILE command`,
      `// @param The command as string pointer`,
      `///`,
      `dcl-pr system int(10) extproc('system');`,
      `  command pointer value options(*string);`,
      `end-pr;`,
    ]
  },
  QzshSystem: {
    type: `function`,
    insertText: `QzshSystem(\${1:value})\$0`,
    detail: `int(10)`,
    description: `Runs the specified shell command by spawning a child process and invoking qsh`,
    prototype: [
      `///`,
      `// QzshSystem`,
      `// Runs the specified shell command by`,
      `// spawning a child process and invoking qsh`,
      `//`,
      `// @param The command as string pointer`,
      `///`,
      `dcl-pr QzshSystem int(10) extproc('QzshSystem');`,
      `  command pointer value options(*string);`,
      `end-pr;`,
    ]
  },
  getenv: {
    type: `function`,
    insertText: `getenv(\${1:'ENVVAR'})\$0`,
    detail: `pointer`,
    description: `Searches the job-level environment list for a string`,    
    prototype: [
      `///`,
      `// getenv`,
      `// Searches the job-level environment list for a string`,
      `//`,
      `// @param Environment variable name`,
      `///`,
      `dcl-pr getenv pointer extproc('getenv');`,
      `  name pointer value options(*string:*trim);`,
      `end-pr;`,
    ]
  }
};

exports.apis = Object.keys(exports.api);