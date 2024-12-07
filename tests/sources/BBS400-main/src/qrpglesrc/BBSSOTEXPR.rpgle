     H/TITLE BBS SysOp Tools - Perform Users' expiration
     H COPYRIGHT('(C) 2020 David Asta under MIT License')
      * SYSTEM      : V4R5
      * PROGRAMMER  : David Asta
      * DATE-WRITTEN: 06/DEC/2020
      *
      * This program is part of the SysOp Tools.
      * It checks if a user has expired (based on its Access Level's
      *  expiration days, and if so changes the Access Level to the
      *  expiring level configured for the current Access Level
      **********************************************************************
     H/COPY DVBBS400/CURRENTSRC,CBKOPTIMIZ
      **********************************************************************
     FPUSERS    UF   E           K DISK
     FLSESSIONS IF   E           K DISK
     FPACCLVS   IF   E           K DISK
      **********************************************************************
      * Data structures
     D dsLastLDate     DS
     D  wLastLDateY                   4A
     D  wLastLDateM                   2A
     D  wLastLDateD                   2A
     D dsTodayNow      DS
     D  wTodNowTime                   6A
     D  wTodNowDate                   6A
      * Constants
     D cErrNFSess      C                   CONST('Not found in PSESSIONS.')
     D cErrNFAccL      C                   CONST('Not found in PACCLVS.')
     D cExpired        C                   CONST('Expired.')
      * Variables
     D wStatEnabled    S              1A   INZ('I')
     D wDisplayMsg     S             50A
     D wTodayNowD      S             12P 0
     D wLastLDateN     S              8P 0
     D wTodNowDateN    S              6P 0
     D wTodNowD        S               D
     D wLastLD         S               D
     D wNumDays        S              5P 0
      **********************************************************************
      * Declare composite keys
     C     KSESSIONS     KLIST
     C                   KFLD                    USRNCK
     C                   KFLD                    wStatEnabled
      * Get Today (now) date and time
     C                   TIME                    wTodayNowD
     C                   MOVEL     wTodayNowD    dsTodayNow
     C                   MOVEL     wTodNowDate   wTodNowDateN
     C                   TIME                    wTodNowD
      * For each user (in PUSERS), check if last log in (in LSESSIONS) is
      *  older than the number of days until today configured for the
      *  User's Access Level (in PACCLVS)
      * If user has never logged in (USRNLG = 0) then compare with
      *  Registration Date (USRREG)
     C     *START        SETLL     PUSERS
     C                   READ      PUSERS
     C                   DOU       %EOF
      *    Only for users with status Enabled
     C                   IF        USRSTA = 'E'
      *    Get last login date
     C                   EXSR      GetLastLDate
     C                   ENDIF
     C                   READ      PUSERS
     C                   ENDDO
      * End of program
     C                   MOVE      *ON           *INLR
     C                   RETURN
      **********************************************************************
      * Get last login date
      **********************************************************************
     C     GetLastLDate  BEGSR
     C                   IF        USRNLG = 0
      * User has never logged in. Extract Registration Date
     C                   EXTRCT    USRREG:*Y     wLastLDateY
     C                   EXTRCT    USRREG:*M     wLastLDateM
     C                   EXTRCT    USRREG:*D     wLastLDateD
     C                   ELSE
      * User has logged in
     C     KSESSIONS     SETLL     LSESSIONS
     C     KSESSIONS     READE     LSESSIONS
     C                   IF        NOT %FOUND
      * User did log in, but somehow the Session record is not there
     C                   GOTO      ENDGETDATE
     C                   ENDIF
      * Extract Last Login Date
     C                   EXTRCT    SESTMS:*Y     wLastLDateY
     C                   EXTRCT    SESTMS:*M     wLastLDateM
     C                   EXTRCT    SESTMS:*D     wLastLDateD
     C                   ENDIF
      * Calculate the difference of days between obtained date and today
     C                   MOVEL     dsLastLDate   wLastLDateN
     C                   MOVEL     wLastLDateN   wLastLD
     C     wTodNowD      SUBDUR    wLastLD       wNumDays:*D
      * Get number of days for Access Level Expiration
     C     USRLVL        CHAIN     PACCLVS
     C                   IF        %FOUND
      * Did expire?
     C     wNumDays      IFGT      LVLEXP
      * If LVLEXP = 0, it never expires
     C     LVLEXP        ANDGT     0
      *      Yes, then change Access Level to Expired Access Level
     C                   EVAL      USRLVL = LVLEXL
     C                   UPDATE    RUSER
     C                   ENDIF
     C                   ENDIF
     C     ENDGETDATE    TAG
     C                   ENDSR
