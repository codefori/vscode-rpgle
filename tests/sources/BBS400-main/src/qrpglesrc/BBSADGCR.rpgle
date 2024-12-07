     H/TITLE Administration - General Configuration
     H COPYRIGHT('(C) 2020 David Asta under MIT License')
      * SYSTEM      : V4R5
      * PROGRAMMER  : David Asta
      * DATE-WRITTEN: 12/NOV/2020
      *
      * This program allows an Administrator user to display/change the
      *   general Configuration values of the BBS stored in PCONFIG
      **********************************************************************
     H/COPY DVBBS400/CURRENTSRC,CBKOPTIMIZ
      **********************************************************************
      * INDICATORS USED:
      * 80 - *ON turns DSPATR(PR), which protects fields from being changed
      * 81 - CBKPCFGREA CHAIN Not Found
      **********************************************************************
     FBBSADGCD  CF   E             WORKSTN
     FPCONFIG   UF   E           K DISK
      **********************************************************************
      * Data structures
     D/COPY DVBBS400/CURRENTSRC,CBKDTAARA
      * Constants
     D cKeysDft        C                   CONST('F10=Edit   F12=Go back')
     D cKeysEdit       C                   CONST('F10=Confirm Changes   F12=Can-
     D                                     cel')
     D cSavedOK        C                   CONST('Configuration was changed suc-
     D                                     cessfully.')
     D cSavedKO        C                   CONST('There was an error while writ-
     D                                     ting to PCONFIG.')
      * Variables
     D/COPY DVBBS400/CURRENTSRC,CBKPCFGDCL
     D wCfgKey         S              6A
     D wShowWelcome    S              1A
     ***********************************************************************
     C                   WRITE     HEADER
     C   80              EVAL      KEYSLS = cKeysDft
     C  N80              EVAL      KEYSLS = cKeysEdit
     C                   WRITE     FOOTER
     C                   EXFMT     BODY
     C                   CLEAR                   MSGLIN
     C                   EXSR      CheckFkeys
      **********************************************************************
      * Subroutine called automatically at startup
      **********************************************************************
     C     *INZSR        BEGSR
     C                   EVAL      SCRSCR = 'BBSADGC'
      * Protect fields from being modified
     C                   EVAL      *IN80 = *ON
      * Get values from PCONFIG and show them on screen
     C                   EXSR      GetConfig
     C                   EVAL      SCRNAM = wCfgBBSNAM
     C                   EVAL      SCRLCR = WCfgLOCCRY
     C                   EVAL      SCRLCT = wCfgLOCCTY
     C                   EVAL      SCRTZC = wCfgTIMZON
     C                   EVAL      SCRCLO = wCfgCLOSED
     C                   EVAL      SCRSAL = wCfgSHWALD
     C                   EVAL      SCRSWE = wCfgSHWWEL
     C                   EVAL      SCRHID = wCfgHIDESO
     C                   EVAL      SCRHLS = wCfgHLSOMS
     C                   EVAL      SCRNFY = wCfgNUSRNF
      * Get values from DATAARA and show them on screen
     C/COPY DVBBS400/CURRENTSRC,CBKHEADER
     C                   ENDSR
      **********************************************************************
      * Check Function keys pressed by the user
      **********************************************************************
     C     CheckFkeys    BEGSR
      * F10=Edit
     C                   IF        *IN10 = *ON
     C* N80              EXSR      SavePCONFIG
     C*  80              EVAL      *IN80 = *OFF
     C                   IF        *IN80 = *ON
     C                   EVAL      *IN80 = *OFF
     C                   ELSE
     C                   EXSR      SavePCONFIG
     C                   ENDIF
     C                   ENDIF
      * F12=Go back or F12=Cancel
     C                   IF        *IN12 = *ON
     C   80              MOVE      *ON           *INLR
     C   80              RETURN
     C  N80              EVAL      *IN80 = *ON
     C                   ENDIF
     C                   ENDSR
      **********************************************************************
      * Save changed values to PCONFIG
      **********************************************************************
     C     SavePCONFIG   BEGSR
      * BBS Name
     C     SCRNAM        IFNE      wCfgBBSNAM
     C                   EVAL      wCfgKey = 'BBSNAM'
     C     wCfgKey       CHAIN     PCONFIG                            81
     C  N81              EVAL      CNFVAL = SCRNAM
     C  N81              UPDATE    CONFIG                               81
     C   81              GOTO      UPDKO
     C                   ENDIF
      * BBS Location Country
     C     SCRLCR        IFNE      WCfgLOCCRY
     C                   EVAL      wCfgKey = 'LOCCRY'
     C     wCfgKey       CHAIN     PCONFIG                            81
     C  N81              EVAL      CNFVAL = SCRLCR
     C  N81              UPDATE    CONFIG                               81
     C   81              GOTO      UPDKO
     C                   ENDIF
      * BBS Location City
     C     SCRLCT        IFNE      wCfgLOCCTY
     C                   EVAL      wCfgKey = 'LOCCTY'
     C     wCfgKey       CHAIN     PCONFIG                            81
     C  N81              EVAL      CNFVAL = SCRLCT
     C  N81              UPDATE    CONFIG                               81
     C   81              GOTO      UPDKO
     C                   ENDIF
      * BBS Time Zone
     C     SCRTZC        IFNE      wCfgTIMZON
     C                   EVAL      wCfgKey = 'TIMZON'
     C     wCfgKey       CHAIN     PCONFIG                            81
     C  N81              EVAL      CNFVAL = SCRTZC
     C  N81              UPDATE    CONFIG                               81
     C   81              GOTO      UPDKO
     C                   ENDIF
      * Closed to New Users?
     C     SCRCLO        IFNE      wCfgCLOSED
     C                   EVAL      wCfgKey = 'CLOSED'
     C     wCfgKey       CHAIN     PCONFIG                            81
     C  N81              EVAL      CNFVAL = SCRCLO
     C  N81              UPDATE    CONFIG                               81
     C   81              GOTO      UPDKO
     C                   ENDIF
      * Show Access Level Description?
     C     SCRSAL        IFNE      wCfgSHWALD
     C                   EVAL      wCfgKey = 'SHWALD'
     C     wCfgKey       CHAIN     PCONFIG                            81
     C  N81              EVAL      CNFVAL = SCRSAL
     C  N81              UPDATE    CONFIG                               81
     C   81              GOTO      UPDKO
     C                   ENDIF
      * Show Welcome screen?
     C     SCRSWE        IFNE      wCfgSHWWEL
     C                   EVAL      wCfgKey = 'SHWWEL'
     C     wCfgKey       CHAIN     PCONFIG                            81
     C  N81              EVAL      CNFVAL = SCRSWE
     C  N81              UPDATE    CONFIG                               81
     C   81              GOTO      UPDKO
     C                   ENDIF
      * Hide SysOp from User Lists?
     C     SCRHID        IFNE      wCfgHIDESO
     C                   EVAL      wCfgKey = 'HIDESO'
     C     wCfgKey       CHAIN     PCONFIG                            81
     C  N81              EVAL      CNFVAL = SCRHID
     C  N81              UPDATE    CONFIG                               81
     C   81              GOTO      UPDKO
     C                   ENDIF
      * Hide SysOp from User Lists?
     C     SCRHLS        IFNE      wCfgHLSOMS
     C                   EVAL      wCfgKey = 'HLSOMS'
     C     wCfgKey       CHAIN     PCONFIG                            81
     C  N81              EVAL      CNFVAL = SCRHLS
     C  N81              UPDATE    CONFIG                               81
     C   81              GOTO      UPDKO
     C                   ENDIF
      * Notify New User Registration
     C     SCRNFY        IFNE      wCfgNUSRNF
     C                   EVAL      wCfgKey = 'NUSRNF'
     C     wCfgKey       CHAIN     PCONFIG                            81
     C  N81              EVAL      CNFVAL = SCRNFY
     C  N81              UPDATE    CONFIG                               81
     C   81              GOTO      UPDKO
     C                   ENDIF
     C     UPDOK         TAG
     C                   EVAL      MSGLIN = cSavedOK
     C                   GOTO      UPDEND
     C     UPDKO         TAG
     C                   EVAL      MSGLIN = cSavedKO
     C     UPDEND        TAG
     C                   EVAL      *IN80 = *ON
     C                   ENDSR
      **********************************************************************
     D/COPY DVBBS400/CURRENTSRC,CBKPCFGREA
