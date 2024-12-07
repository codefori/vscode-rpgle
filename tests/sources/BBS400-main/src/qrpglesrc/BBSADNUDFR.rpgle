     H/TITLE Administration - New User default values
     H COPYRIGHT('(C) 2020 David Asta under MIT License')
      * SYSTEM      : V4R5
      * PROGRAMMER  : David Asta
      * DATE-WRITTEN: 25/NOV/2020
      *
      * This program allows an Administrator user to display/change the
      *   New User default values
      **********************************************************************
     H/COPY DVBBS400/CURRENTSRC,CBKOPTIMIZ
      **********************************************************************
      * INDICATORS USED:
      * 80 - *ON turns DSPATR(PR), which protects fields from being changed
      * 81 - CBKPCFGREA CHAIN Not Found
      **********************************************************************
     FBBSADNUDFDCF   E             WORKSTN
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
     C                   EVAL      SCRSCR = 'BBSADNUDF'
      * Protect fields from being modified
     C                   EVAL      *IN80 = *ON
      * Get values from PCONFIG and show them on screen
     C                   EXSR      GetConfig
     C                   MOVEL     wCfgNUSLVL    SCRNUL
     C                   EVAL      SCRRNA = wCfgSvyRName
     C                   EVAL      SCRGND = wCfgSvyGendr
     C                   EVAL      SCRLOC = wCfgSvyLocat
     C                   EVAL      SCREML = wCfgSvyEmail
     C                   EVAL      SCRSVY = wCfgSurvey
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
      * Default Level
     C     SCRNUL        IFNE      wCfgNUSLVL
     C                   EVAL      wCfgKey = 'NUSLVL'
     C     wCfgKey       CHAIN     PCONFIG                            81
     C  N81              MOVEL     SCRNUL        CNFVAL
     C  N81              UPDATE    CONFIG                               81
     C   81              GOTO      UPDKO
     C                   ENDIF
      * Survey questions
     C                   EVAL      wCfgSvyRName = SCRRNA
     C                   EVAL      wCfgSvyGendr = SCRGND
     C                   EVAL      wCfgSvyLocat = SCRLOC
     C                   EVAL      wCfgSvyEmail = SCREML
     C     SCRSVY        IFNE      wCfgSurvey
     C                   EVAL      wCfgKey = 'NUSSVY'
     C     wCfgKey       CHAIN     PCONFIG                            81
     C  N81              EVAL      CNFVAL = wCfgSurvey
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
