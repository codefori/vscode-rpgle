module.exports = {
  indent: 2,
  PrototypeCheck: true,
  NoOCCURS: true,
  NoSELECTAll: true,
  UppercaseConstants: true,
  IncorrectVariableCase: true,
  StringLiteralDupe: true,
  NoSQLJoins: true,
  PrettyComments: true,
  NoGlobalSubroutines: true,
  NoExternalTo: [
    `QCMD`,
    `QP2TERM`,
    `QSH`,
    `SYSTEM`,
    `QCMDEXC`,
  ]
}