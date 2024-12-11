      **********************************************************************
      * Read configuration values from PCONFIG into variables
      * Uses *IN81 for CHAIN Not Found
      * This CopyBook needs: to be used with CBKPCFGDCL
      *                      FPCONFIG   IF   E           K DISK
      **********************************************************************
     C     GetConfig     BEGSR
      * Get BBS Name
     C                   EVAL      wCfgKey = 'BBSNAM'
     C     wCfgKey       CHAIN     PCONFIG                            81
     C   81              GOTO      ENDOFSR
     C                   EVAL      wCfgBBSNAM = CNFVAL
      * Get BBS Location Country Code
     C                   EVAL      wCfgKey = 'LOCCRY'
     C     wCfgKey       CHAIN     PCONFIG                            81
     C   81              GOTO      ENDOFSR
     C                   EVAL      wCfgLOCCRY = CNFVAL
      * Get BBS Location City
     C                   EVAL      wCfgKey = 'LOCCTY'
     C     wCfgKey       CHAIN     PCONFIG                            81
     C   81              GOTO      ENDOFSR
     C                   EVAL      wCfgLOCCTY = CNFVAL
      * Get BBS Time Zone
     C                   EVAL      wCfgKey = 'TIMZON'
     C     wCfgKey       CHAIN     PCONFIG                            81
     C   81              GOTO      ENDOFSR
     C                   EVAL      wCfgTIMZON = CNFVAL
      * Get BBS closed to new users?
     C                   EVAL      wCfgKey = 'CLOSED'
     C     wCfgKey       CHAIN     PCONFIG                            81
     C   81              GOTO      ENDOFSR
     C                   EVAL      wCfgCLOSED = CNFVAL
      * Get BBS New User Level
     C                   EVAL      wCfgKey = 'NUSLVL'
     C     wCfgKey       CHAIN     PCONFIG                            81
     C   81              GOTO      ENDOFSR
     C                   MOVEL     CNFVAL        wCfgNUSLVL
      * Get BBS New User enabled survey questions
     C                   EVAL      wCfgKey = 'NUSSVY'
     C     wCfgKey       CHAIN     PCONFIG                            81
     C   81              GOTO      ENDOFSR
     C                   EVAL      wCfgNUSSVY = CNFVAL
      * Get Show Access Level Description?
     C                   EVAL      wCfgKey = 'SHWALD'
     C     wCfgKey       CHAIN     PCONFIG                            81
     C   81              GOTO      ENDOFSR
     C                   EVAL      wCfgSHWALD = CNFVAL
     C                   OUT       wDTAARA
     C                   UNLOCK    wDTAARA
      * Get Show Welcome screen
     C                   EVAL      wCfgKey = 'SHWWEL'
     C     wCfgKey       CHAIN     PCONFIG                            81
     C   81              GOTO      ENDOFSR
     C                   EVAL      wCfgSHWWEL = CNFVAL
      * Get New User default Survey questions
     C                   EVAL      wCfgKey = 'NUSSVY'
     C     wCfgKey       CHAIN     PCONFIG                            81
     C   81              GOTO      ENDOFSR
     C                   EVAL      wCfgSurvey = CNFVAL
      * Get New User Registration notify
     C                   EVAL      wCfgKey = 'NUSRNF'
     C     wCfgKey       CHAIN     PCONFIG                            81
     C   81              GOTO      ENDOFSR
     C                   EVAL      wCfgNUSRNF = CNFVAL
      * Get Hide SysOp from Users List
     C                   EVAL      wCfgKey = 'HIDESO'
     C     wCfgKey       CHAIN     PCONFIG                            81
     C   81              GOTO      ENDOFSR
     C                   EVAL      wCfgHIDESO = CNFVAL
      * Get Highlight SysOp's messages
     C                   EVAL      wCfgKey = 'HLSOMS'
     C     wCfgKey       CHAIN     PCONFIG                            81
     C   81              GOTO      ENDOFSR
     C                   EVAL      wCfgHLSOMS = CNFVAL
     C     ENDOFSR       TAG
     C                   ENDSR
