     H/TITLE Change User Options
     H COPYRIGHT('(C) 2020 David Asta under MIT License')
      * SYSTEM      : V4R5
      * PROGRAMMER  : David Asta
      * DATE-WRITTEN: 22/NOV/2020
      *
      * This program allows an user to change the User's data
      **********************************************************************
      * Compiler flags
     H ALWNULL(*USRCTL)
     H/COPY DVBBS400/CURRENTSRC,CBKOPTIMIZ
      **********************************************************************
     FBBSUSROPTDCF   E             WORKSTN
     FPUSERS    UF A E           K DISK
      **********************************************************************
      * Data structures
     D/COPY DVBBS400/CURRENTSRC,CBKDTAARA
     D/COPY DVBBS400/CURRENTSRC,CBKUSRHIDS
      * Constants
     D cPassNoMatch    C                   CONST('Passwords do not match. Pleas-
     D                                     e enter again.')
     D cUpdateOK       C                   CONST('Data successfully updated.')
      * Variables
     D wMsgUID20       S             20A
     D wMsgUIDhash     S             32A
     D wHashMode       S              1A   INZ('P')
     ***********************************************************************
     C                   WRITE     HEADER
     C                   WRITE     FOOTER
     C                   EXFMT     BODY
     C                   CLEAR                   MSGLIN
     C                   EXSR      ChkFkeys
      **********************************************************************
      * Subroutine called automatically at startup
      **********************************************************************
     C     *INZSR        BEGSR
     C                   EVAL      SCRSCR = 'BBSUSROPT'
      * Get values from DATAARA and show them on screen
     C*COPY DVBBS400/CURRENTSRC,CBKHEADER
      * Load User data and show it on the screen
     C                   EXSR      GetUserData
     C                   ENDSR
      **********************************************************************
      * Check Function keys pressed by the user
      **********************************************************************
     C     ChkFkeys      BEGSR
      * F10=Confirm Changes
     C                   IF        *IN10 = *ON
     C                   EXSR      ValidateData
     C                   ENDIF
      * F12=Cancel
     C                   IF        *IN12 = *ON
     C                   MOVE      *ON           *INLR
     C                   RETURN
     C                   ENDIF
     C                   ENDSR
      **********************************************************************
      * Load User data and show it on the screen
      **********************************************************************
     C     GetUserData   BEGSR
     C     wUser         CHAIN     PUSERS
     C                   IF        NOT %FOUND
     C     'ERROR 33'    DSPLY
     C                   ELSE
     C                   EVAL      SCRRNA = USRRNA
     C                   EVAL      SCRLCC = USRCTR
     C                   EVAL      SCRLCT = USRCTY
     C                   EVAL      SCRGND = USRGND
     C                   EVAL      SCREML = USREML
     C                   EVAL      SCRNFY = USRNFY
     C                   EVAL      wUserOptHides = USRHID
     C                   EVAL      SCRRNAH = wHideRealName
     C                   EVAL      SCRLOCH = wHideLocation
     C                   EVAL      SCRGDRH = wHideGender
     C                   EVAL      SCREMLH = wHideEmail
      * Blank out the password. We will then be able to check if userd wants
      * to change password if SCRPW1 and SCRPW2 are not blanks and equal
     C                   EVAL      SCRPW1 = *BLANKS
     C                   EVAL      SCRPW2 = *BLANKS
     C                   ENDIF
     C                   ENDSR
      **********************************************************************
      * Validate data entered by the user.
      * If correct, update PUSERS
      * else, show error message
      **********************************************************************
     C     ValidateData  BEGSR
      * Check Passwords if not *BLANKS (i.e. user modified the password)
     C     SCRPW1        IFNE      *BLANKS
     C     SCRPW1        ANDNE     SCRPW2
      * Password do not match
     C                   EVAL      MSGLIN = cPassNoMatch
     C                   ELSE
      * All looks good. Add record to PUSERS
     C                   EXSR      UpdateUser
     C                   ENDIF
     C     NOUPDATE      TAG
     C                   ENDSR
      **********************************************************************
      * Update user in PUSERS
      **********************************************************************
     C     UpdateUser    BEGSR
     C     wUser         CHAIN     PUSERS
     C                   IF        NOT %FOUND
     C     'ERROR 33'    DSPLY
     C                   ELSE
     C                   EVAL      USRRNA = SCRRNA
     C                   EVAL      USRGND = SCRGND
     C                   EVAL      USRCTR = SCRLCC
     C                   EVAL      USRCTY = SCRLCT
     C                   EVAL      USREML = SCREML
     C                   EVAL      USRNFY = SCRNFY
     C                   EVAL      wHideRealName = SCRRNAH
     C                   EVAL      wHideLocation = SCRLOCH
     C                   EVAL      wHideGender = SCRGDRH
     C                   EVAL      wHideEmail = SCREMLH
     C                   EVAL      USRHID = wUserOptHides
      * Hash (MD5) the password (if it was modified)
     C     SCRPW1        IFNE      *BLANKS
     C     wUser         CAT       SCRPW1:0      wMsgUID20
     C                   CALL      'GETMD5HASH'
     C                   PARM                    wHashMode
     C                   PARM                    wMsgUID20
     C                   PARM                    wMsgUIDhash
     C                   EVAL      USRPAS = wMsgUIDhash
     C                   ENDIF
     C                   UPDATE    RUSER                                91
     C  N91              EVAL      MSGLIN = cUpdateOK
     C   91'ERROR 91'    DSPLY
     C                   ENDIF
     C                   ENDSR
