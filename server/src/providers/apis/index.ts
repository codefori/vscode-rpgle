

interface APIInterface {
  type: "function"|"struct";
  insertText: string;
  /** Usually indicates what it will return */
  detail: string;
  description: string; 
  prototype: string[];
  example?: string[];
}

export const bodies: {[name: string]: APIInterface} = {
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
      `dcl-pr printf int(10) extproc('printf');`,
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
  },
  QUSDLTUS: {
    type: `function`,
    insertText: `QUSDLTUS(\${1:'NAME      LIB'}:\${2:APIERRC});`,
    detail: `void`,
    description: `Delete User Space`,
    prototype: [
      `///`,
      `// QUSDLTUS`,
      `// Delete User Space`,
      `///`,
      `dcl-pr QUSDLTUS extpgm('QUSDLTUS');`,
      `  qualifiedName char(20) const;`,
      `  errorResponse likeds(APIERRC_T) options(*varsize);`,
      `end-pr;`
    ]
  },
  APIERRC_T: {
    type: `struct`,
    insertText: `APIERRC_T`,
    detail: `ERRC0100 format`,
    description: `Error data structure`,
    prototype: [
      `///`,
      `// APPERRC template`,
      `// Used for error capturing`,
      `///`,
      `dcl-ds APIERRC_T Qualified Template;`,
      `  bytesProvided Int(10:0); // Inz(%size(ApiErrC))`,
      `  bytesAvailable Int(10:0);`,
      `  exceptionID Char(7);`,
      `  reserved Char(1);`,
      `  exceptionData Char(3000);`,
      `end-ds;`,
    ],
    example: [
      `**free`,
      `dcl-ds apierrc likeds(APIERRC_T);`,
      `apierrc.bytesProvided = %size(apierrc);`
    ]
  },
  "QCMDEXC": {
    "type": `function`,
    "description": `Execute a system command`,
    "detail": `void`,
    "insertText": `QCMDEXC(\n  \${1:commandString}:\n  \${2:commandLength}\n)$0`,
    "prototype": [
      `///`,
      `// QCMDEXC`,
      `// Execute a system command`,
      `///`,
      `Dcl-Pr QCMDEXC EXTPGM('QCMDEXC');`,
      `  commandString CHAR(32702) CONST OPTIONS(*VARSIZE);`,
      `  commandLength PACKED(15:5) CONST;`,
      `End-Pr;`
    ]
  },
  "QCLRDTAQ": {
    "type": `function`,
    "description": `Clear data queue`,
    "detail": `void`,
    "insertText": `QCLRDTAQ(\n  \${1:dqName}:\n  \${2:dqLibrary}:\n  \${3:keyOrder}:\n  \${4:keyDataLength}:\n  \${5:keyData}:\n  \${6:prErrCode}\n)$0`,
    "prototype": [
      `///`,
      `// QCLRDTAQ`,
      `// Clear data queue`,
      `///`,
      `Dcl-Pr QCLRDTAQ EXTPGM('QCLRDTAQ');`,
      `  dqName CHAR(10) CONST;`,
      `  dqLibrary CHAR(10) CONST;`,
      `  keyOrder CHAR(2) CONST OPTIONS(*NOPASS);`,
      `  keyDataLength PACKED(3:0) CONST OPTIONS(*NOPASS);`,
      `  keyData CHAR(256) CONST OPTIONS(*NOPASS:*VARSIZE);`,
      `  prErrCode LIKEDS(APIERRC_T) OPTIONS(*NOPASS:*VARSIZE);`,
      `End-Pr;`
    ]
  },
  "QSNDDTAQ": {
    "type": `function`,
    "description": `Send to data queue`,
    "detail": `void`,
    "insertText": `QSNDDTAQ(\n  \${1:dqName}:\n  \${2:dqLibrary}:\n  \${3:dataInLen}:\n  \${4:dataIn}:\n  \${5:keyDataLength}:\n  \${6:keyData}:\n  \${7:asyncRequest}:\n  \${8:isJourneyEntry}\n)$0`,
    "prototype": [
      `///`,
      `// QSNDDTAQ`,
      `// Send to data queue`,
      `///`,
      `Dcl-Pr QSNDDTAQ EXTPGM('QSNDDTAQ');`,
      `  dqName CHAR(10) CONST;`,
      `  dqLibrary CHAR(10) CONST;`,
      `  dataInLen PACKED(5:0);`,
      `  dataIn CHAR(65535) OPTIONS(*VARSIZE);`,
      `  keyDataLength PACKED(3:0) CONST OPTIONS(*NOPASS);`,
      `  keyData CHAR(256) CONST OPTIONS(*NOPASS:*VARSIZE);`,
      `  asyncRequest CHAR(10) CONST OPTIONS(*NOPASS);`,
      `  isJourneyEntry CHAR(10) CONST OPTIONS(*NOPASS);`,
      `End-Pr;`
    ]
  },
  "memcpy": {
    "type": `function`,
    "description": ``,
    "detail": `void`,
    "insertText": `memcpy(\n  \${1:target}:\n  \${2:source}:\n  \${3:length}\n)$0`,
    "prototype": [
      `///`,
      `// memcpy`,
      `// API`,
      `///`,
      `dcl-pr memcpy EXTPROC('__MEMCPY');`,
      `  target POINTER VALUE;`,
      `  source POINTER VALUE;`,
      `  length UNS(10) VALUE;`,
      `end-pr;`
    ]
  },
  "QRCVDTAQ": {
    "type": `function`,
    "description": ``,
    "detail": `void`,
    "insertText": `QRCVDTAQ(\n  \${1:Object}:\n  \${2:Library}:\n  \${3:DataLen}:\n  \${4:Data}:\n  \${5:WaitTime}:\n  \${6:KeyOrder}:\n  \${7:KeyLen}:\n  \${8:Key}\n)$0`,
    "prototype": [
      `///`,
      `// QRCVDTAQ`,
      `// API`,
      `///`,
      `dcl-pr QRCVDTAQ EXTPGM('QRCVDTAQ');`,
      `  Object CHAR(10);`,
      `  Library CHAR(10);`,
      `  DataLen PACKED(5);`,
      `  Data CHAR(DQ_LEN);`,
      `  WaitTime PACKED(5);`,
      `  KeyOrder CHAR(2) OPTIONS(*NOPASS);`,
      `  KeyLen PACKED(3) OPTIONS(*NOPASS);`,
      `  Key POINTER OPTIONS(*NOPASS);`,
      `end-pr;`
    ]
  },
  "RSLVSP2": {
    "type": `function`,
    "description": `Get library pointer`,
    "detail": `void`,
    "insertText": `RSLVSP2(\n  \${1:objectResult}:\n  \${2:rslvsp_in}\n)$0`,
    "prototype": [
      `///`,
      `// RSLVSP2`,
      `// Get library pointer`,
      `// @param Resulting pointer by reference`,
      `// @param Struct rslvsp_t`,
      `///`,
      `dcl-pr RSLVSP2 EXTPROC('_RSLVSP2');`,
      `  objectResult POINTER;`,
      `  rslvsp_in LIKEDS(RSLVSP_T) QUALIFIED;`,
      `end-pr;`
    ]
  },
  "RSLVSP4": {
    "type": `function`,
    "description": `Get object pointer`,
    "detail": `void`,
    "insertText": `RSLVSP4(\n  \${1:objectResult}:\n  \${2:rslvsp_in}:\n  \${3:libraryPointer}\n)$0`,
    "prototype": [
      `///`,
      `// RSLVSP4`,
      `// Get object pointer`,
      `// @param Resulting pointer by reference`,
      `// @param Struct rslvsp_t`,
      `// @param Library pointer from RSLVSP2`,
      `///`,
      `dcl-pr RSLVSP4 EXTPROC('_RSLVSP4');`,
      `  objectResult POINTER;`,
      `  rslvsp_in CHAR(34);`,
      `  libraryPointer POINTER;`,
      `end-pr;`
    ]
  },
  "QleActBndPgmLong": {
    "type": `function`,
    "description": `Activate service program`,
    "detail": `INT(20)`,
    "insertText": `QleActBndPgmLong(\n  \${1:serviceProgram}\n)$0`,
    "prototype": [
      `///`,
      `// QleActBndPgmLong`,
      `// Activate service program`,
      `// @param Pointer to service program from RSLVSP4`,
      `///`,
      `dcl-pr QleActBndPgmLong INT(20) EXTPROC('QLEACTBNDPGMLONG');`,
      `  serviceProgram POINTER;`,
      `end-pr;`
    ]
  },
  "RetrieveFunctionPointer": {
    "type": `function`,
    "description": `Retrieve function pointer`,
    "detail": `POINTER`,
    "insertText": `RetrieveFunctionPointer(\n  \${1:Mark}:\n  \${2:ExportNum}:\n  \${3:ExportNameLen}:\n  \${4:ExportName}:\n  \${5:rFuncPointer}:\n  \${6:rFuncResult}\n)$0`,
    "prototype": [
      `///`,
      `// RetrieveFunctionPointer`,
      `// Retrieve function pointer`,
      `// @param Value from QleActBndPgmLong`,
      `// @param Export number.`,
      `// @param Export name length`,
      `// @param Export name`,
      `// @param Resulting function pointer`,
      `// @param Resulting status code`,
      `///`,
      `dcl-pr RetrieveFunctionPointer POINTER EXTPROC('QLEGETEXPLONG');`,
      `  Mark INT(20);`,
      `  ///`,
      `  // @value 0 (zero is valid)`,
      `  ///`,
      `  ExportNum INT(10) VALUE;`,
      `  ExportNameLen INT(10);`,
      `  ExportName POINTER VALUE OPTIONS(*STRING);`,
      `  rFuncPointer POINTER;`,
      `  rFuncResult INT(10);`,
      `end-pr;`
    ]
  },
  "callpgmv": {
    "type": `function`,
    "description": ``,
    "detail": `void`,
    "insertText": `callpgmv(\n  \${1:pgm_ptr}:\n  \${2:argv}:\n  \${3:argc}\n)$0`,
    "prototype": [
      `///`,
      `// callpgmv`,
      `// API`,
      `///`,
      `dcl-pr callpgmv EXTPROC('_CALLPGMV');`,
      `  pgm_ptr POINTER;`,
      `  argv POINTER DIM(256);`,
      `  argc UNS(10) VALUE;`,
      `end-pr;`
    ]
  },
  "rslvsp_t": {
    "type": `struct`,
    "description": `Resolve object pointer struct`,
    "detail": `QUALIFIED TEMPLATE`,
    "insertText": `rslvsp_t`,
    "prototype": [
      `///`,
      `// rslvsp_t`,
      `// Resolve object struct`,
      `///`,
      `dcl-ds rslvsp_t QUALIFIED TEMPLATE;`,
      `  ///`,
      `  // @value x'0401' Library`,
      `  // @value x'0201' Program`,
      `  // @value x'0203' Service Program`,
      `  ///`,
      `  Obj_Type CHAR(2);`,
      `  Obj_Name CHAR(30);`,
      `  Auth CHAR(2) INZ(X'0000');`,
      `end-ds;`
    ]
  }
};

export const names = Object.keys(bodies);