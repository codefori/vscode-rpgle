
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
      `outtext = 'Hello world'`,
      `printf(outtext + '\\n');`,
    ]
  },
  Qp0zLprintf: {
    type: `function`,
    insertText: `Qp0zLprintf(\${1:value})\$0`,
    detail: `int(10)`,
    description: `Prints user data specified by format-string as an information message type to the job log.`,
    prototype: [
      `///`,
      `// Qp0zLprintf`,
      `// Prints user data specified by format-string as`,
      `// an information message type to the job log.`,
      `// To ensure messages are written to the job log, always`,
      `// use a new line (\\n) at the end of each format-string.`,
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
      `// @param Environment variable name`,
      `///`,
      `dcl-pr getenv pointer extproc('getenv');`,
      `  name pointer value options(*string:*trim);`,
      `end-pr;`,
    ],
    example: [
      `**free`,
      `dcl-s EnvVarValue char(500);`,
      `EnvVarValue = %str(getenv('ENVVAR'));`
    ]
  },
  putenv: {
    type: `function`,
    insertText: `putenv(\${1:'ENVVAR=value'})\$0`,
    detail: `int(10)`,
    description: `Creates or changes an environment variable`,
    prototype: [
      `///`,
      `// putenv`,
      `// Creates or changes an environment variable`,
      `// @param Environment variable name and value`,
      `///`,
      `dcl-pr putenv pointer extproc('putenv');`,
      `  change pointer value options(*string:*trim);`,
      `end-pr;`,
    ],
    example: [
      `**free`,
      `dcl-s EnvVarValue char(500);`,
      `EnvVarValue = 'Some value to store';`,
      `putenv('ENVVAR=' + EnvVarValue);`
    ]
  },
  QUSCRTUS: {
    type: `function`,
    insertText: `QUSCRTUS(\n  \${1:'NAME      LIB'}:\n  \${2:'LOG'}:\n  \${3:size}:\n  \${4:'*ALL'}:\n  \${5:'New object'}\n);$0`,
    detail: `void`,
    description: `Create User Space`,
    prototype: [
      `///`,
      `// QUSCRTUS`,
      `// Create User Space`,
      `///`,
      `dcl-pr QUSCRTUS extpgm('QUSCRTUS');`,
      `  qualifiedName char(20) const;`,
      `  extendedAtribute char(10) const;`,
      `  initialSize int(10) const;`,
      `  initialValue char(1) const;`,
      `  publicAuthority char(10) const;`,
      `  description char(50) const;`,
      `  // optional parm group 1`,
      `  UsrSpcRepl char(10) const options(*nopass);`,
      `  UsrSpcErrC likeds(ApiErrC) options(*nopass: *varsize);`,
      `  // optional parm group 2`,
      `  UsrSpcDomn char(10) const options(*nopass);`,
      `  // optional parm group 3`,
      `  UsrSpcReqS int(10) const options(*nopass);`,
      `  UsrSpcOptA char(1) const options(*nopass);`,
      `end-pr;`,
    ]
  }
};

exports.apis = Object.keys(exports.api);