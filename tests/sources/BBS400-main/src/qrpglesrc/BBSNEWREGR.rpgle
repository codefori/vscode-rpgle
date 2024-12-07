     H/TITLE New user Registration
     H COPYRIGHT('(C) 2020 David Asta under MIT License')
      * SYSTEM      : V4R5
      * PROGRAMMER  : David Asta
      * DATE-WRITTEN: 19/NOV/2020
      *
      * This program allows an new user to register in the BBS
      **********************************************************************
      * INDICATORS USED:
      * 30 - *ON User was created on PUSERS
      * 70 - *ON to enable changing Nickname
      **********************************************************************
      * Compiler flags
     H ALWNULL(*USRCTL)
     H/COPY DVBBS400/CURRENTSRC,CBKOPTIMIZ
      **********************************************************************
     FBBSNEWREGDCF   E             WORKSTN
     F                                     INFDS(dsINFDS)
     FPUSERS    UF A E           K DISK
     FPINVALNICKIF   E           K DISK
      **********************************************************************
      * Data structures
     D/COPY DVBBS400/CURRENTSRC,CBKDTAARA
     D/COPY DVBBS400/CURRENTSRC,CBKUSRHIDS
     D dsTodayNowC     DS
     D  wTodNowTime                   6A
     D  wTodNowDate                   6A
     D dsINFDS         DS
     D wLinPos               370    371
     D                 DS
     D wLin                    1      2B 0
     D wL                      2      2
     D                 DS
     D wPos                    1      2B 0
     D wP                      2      2
      * Constants
     D cErrValidKO     C                   CONST('Something is not right. Pleas-
     D                                     e check data on screen.')
     D cErrValidOK     C                   CONST('User created. Go back (F12) a-
     D                                     nd log in.')
     D cErrExists      C                   CONST('This Nickname is already take-
     D                                     n. Please enter a different one.')
     D cPassNoMatch    C                   CONST('Passwords do not match. Pleas-
     D                                     enter again.')
     D cInvalidNick    C                   CONST('This Nickname is not allowed -
     D                                     in this BBS.')
      * Variables
     D wUserLvlD       S              2P 0
     D wMsgUID20       S             20A
     D wMsgUIDhash     S             32A
     D wHashMode       S              1A   INZ('P')
     D wHlpMode        S              1A   INZ('R')
     D wHlpFiller1     S             10A   INZ(*BLANKS)
     D wHlpFiller2     S             10A   INZ(*BLANKS)
     D wHlpUser        S             10A
     D wTodNowDateD    S              6P 0
     D wTodayNowD      S             12P 0
     D wTodayNowDate   S               D
     C/COPY DVBBS400/CURRENTSRC,CBKPCFGDCL
     ***********************************************************************
     C                   WRITE     HEADER
     C                   WRITE     FOOTER
     C                   EXFMT     BODY
     C                   CLEAR                   MSGLIN
     C  N30              EXSR      ValidateData
     C                   EXSR      ChkFkeys
      **********************************************************************
      * Subroutine called automatically at startup
      **********************************************************************
     C     *INZSR        BEGSR
     C                   EVAL      SCRSCR = 'BBSNEWREG'
     C                   EVAL      *IN30 = *OFF
      * Get values from DATAARA and show them on screen
     C*COPY DVBBS400/CURRENTSRC,CBKHEADER
      * Get New User default values from DTAAARA
     C                   IN        wDTAARA
     C                   UNLOCK    wDTAARA
      * Enable only the questions configured as enabled by the SysOp
     C                   EXSR      EnableSurvey
     C                   ENDSR
      **********************************************************************
      * Check Function keys pressed by the user
      **********************************************************************
     C     ChkFkeys      BEGSR
      * F12=Cancel
     C                   IF        *IN12 = *ON
     C                   MOVE      *ON           *INLR
     C                   RETURN
     C                   ENDIF
     C                   ENDSR
      **********************************************************************
      * Validate data entered by the user
      **********************************************************************
     C     ValidateData  BEGSR
      * Check that Nickname is not an invalid one
     C     SCRNCK        CHAIN     PINVALNICK                         90
     C  N90              EVAL      MSGLIN = cInvalidNick
     C  N90              GOTO      ENDVALIDATE
      * Check that Nickname doesn't already exists
     C     SCRNCK        CHAIN     PUSERS                             90
     C  N90              EVAL      MSGLIN = cErrExists
     C  N90              GOTO      ENDVALIDATE
      * Check Passwords
     C                   IF        SCRPW1 = *BLANKS
     C                   EVAL      MSGLIN = cErrValidKO
     C                   GOTO      ENDVALIDATE
     C                   ENDIF
     C                   IF        SCRPW2 = SCRPW1
      * All looks good. Add record to PUSERS
     C                   EXSR      AddNewUser
     C                   EVAL      MSGLIN = cErrValidOK
     C                   EVAL      *IN30 = *ON
      * Send Notification Message?
     C     wNewUsrNtfy   IFNE      *BLANKS
     C                   EVAL      wHlpUser = wNewUsrNtfy
     C                   CALL      'BBSHELPERC'
     C                   PARM                    wHlpMode
     C                   PARM                    wHlpFiller1
     C                   PARM                    wHlpFiller2
     C                   PARM                    wHlpUser
     C                   ENDIF
     C                   ELSE
      * Password do not match
     C                   EVAL      MSGLIN = cPassNoMatch
     C                   ENDIF
     C     ENDVALIDATE   TAG
     C                   ENDSR
      **********************************************************************
      * Add new user to PUSERS
      **********************************************************************
     C     AddNewUser    BEGSR
     C                   EVAL      USRNCK = SCRNCK
     C                   EVAL      USRSTA = 'E'
     C                   MOVEL     wNUSLVL       wUserLvlD
     C                   EVAL      USRLVL = wUserLvlD
     C                   EVAL      USRRNA = SCRRNA
     C                   EVAL      USRGND = SCRGND
     C                   EVAL      USRCTR = SCRLCC
     C                   EVAL      USRCTY = SCRLCT
     C                   EVAL      USREML = SCREML
     C                   TIME                    wTodayNowD
     C                   MOVEL     wTodayNowD    dsTodayNowC
     C                   MOVEL     wTodNowDate   wTodNowDateD
     C     *DMY          MOVEL     wTodNowDateD  wTodayNowDate
     C                   EVAL      USRREG = wTodayNowDate
     C                   EVAL      USRLLD = wTodayNowDate
     C                   EVAL      USRNLG = 0
     C                   EVAL      USRNFY = 'N'
      * User's Personal Information Hides
     C                   EVAL      wHideRealName = SCRRNAH
     C                   EVAL      wHideLocation = SCRLOCH
     C                   EVAL      wHideGender = SCRGDRH
     C                   EVAL      wHideEmail = SCREMLH
     C                   EVAL      USRHID = wUserOptHides
      * Hash (MD5) the password
     C     SCRNCK        CAT       SCRPW1:0      wMsgUID20
     C                   CALL      'GETMD5HASH'
     C                   PARM                    wHashMode
     C                   PARM                    wMsgUID20
     C                   PARM                    wMsgUIDhash
     C                   EVAL      USRPAS = wMsgUIDhash
     C                   WRITE     RUSER                                91
     C   91'ERROR 91'    DSPLY
     C                   ENDSR
      **********************************************************************
      * Enable only the questions configured as enabled by the SysOp
      **********************************************************************
     C     EnableSurvey  BEGSR
     C                   EVAL      wCfgSurvey = wNUSSVY
     C                   SETOFF                                       606162
     C                   SETOFF                                       6364
     C                   IF        wCfgSvyRName = 'Y'
     C                   EVAL      *IN60 = *ON
     C                   ENDIF
     C                   IF        wCfgSvyLocat = 'Y'
     C                   EVAL      *IN61 = *ON
     C                   ENDIF
     C                   IF        wCfgSvyGendr = 'Y'
     C                   EVAL      *IN62 = *ON
     C                   ENDIF
     C                   IF        wCfgSvyEmail = 'Y'
     C                   EVAL      *IN64 = *ON
     C                   ENDIF
     C                   ENDSR
