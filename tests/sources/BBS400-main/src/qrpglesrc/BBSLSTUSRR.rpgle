     H/TITLE List of Users online (SFL)
     H COPYRIGHT('(C) 2020 David Asta under MIT License')
      * SYSTEM      : V4R5
      * PROGRAMMER  : David Asta
      * DATE-WRITTEN: 18/NOV/2020
      *
      * This program shows all Users that are current logged in (online),
      *   and allows to send a User-to-User message to selected User(s)
      **********************************************************************
      * Compiler flags
     H ALWNULL(*USRCTL)
     H/COPY DVBBS400/CURRENTSRC,CBKOPTIMIZ
      **********************************************************************
      * INDICATORS USED:
      * 25 - Roll key
      * 30 - Users Online Now
      * 31 - Users Online Today
      * 32 - Users List (All)
      * 33 - Users Management (for Administrators)
      * 40 - SFLDSP
      * 41 - SFLCLR
      * 42 - SFLEND(*MORE)
      * 45 - Highlight in WHT if is same as current user
      * 91 - NOT FOUND for CHAIN
      **********************************************************************
     FBBSLSTUSRDCF   E             WORKSTN
     F                                     SFILE(SF:wRRN)
     FLLOGGED   IF   E           K DISK
     FLUSERS    IF   E           K DISK
     FPUSERS    UF   E           K DISK    RENAME(RUSER:PUSER)
      **********************************************************************
      * Data structures
     D/COPY DVBBS400/CURRENTSRC,CBKDTAARA
     D/COPY DVBBS400/CURRENTSRC,CBKUSRHIDS
     D dsTodayNowC     DS
     D  wTodNowTime                   6A
     D  wTodNowDate                   6A
      * Constants
     D cErrOptNoAdmin  C                   CONST('Value entered for field is no-
     D                                     t valid. Valid values listed in mess-
     D                                     age help.')
      * Variables
     D/COPY DVBBS400/CURRENTSRC,CBKUSEWINS
     D pMode           S              1A
     D wTodayNowD      S             12P 0
     D wTodNowTimeD    S              6P 0
     D wTodNowDateD    S              6P 0
     D wTodayDate      S               D
     D wRRN            S              4P 0
     D wCfgKey         S              6A
     D wMode           S              1A
     D wSubject        S             45A
     ***********************************************************************
     C                   WRITE     FOOTER
     C                   EXFMT     SFLCTL
     C                   CLEAR                   MSGLIN
     C                   EXSR      ChkFkeys
     C   40              EXSR      ChkOptions
      **********************************************************************
      * Subroutine called automatically at startup
      **********************************************************************
     C     *INZSR        BEGSR
     C                   EVAL      SCRSCR = 'BBSLSTUSR'
      * Receive parameters
     C     *ENTRY        PLIST
     C                   PARM                    pMode
      * Get values from DATAARA and show them on screen
     C/COPY DVBBS400/CURRENTSRC,CBKHEADER
      * Get Today's date/time
     C                   TIME                    wTodayNowD
     C                   MOVEL     wTodayNowD    dsTodayNowC
     C                   MOVEL     wTodNowTime   wTodNowTimeD
     C                   MOVEL     wTodNowDate   wTodNowDateD
      * Initialise variables and indicators, and load subfile
     C                   SETOFF                                       303132
     C                   SETOFF                                       33
     C                   SELECT
     C                   WHEN      pMode = 'N'
      *           List users Online Now
     C                   EVAL      *IN30 = *ON
     C                   WHEN      pMode = 'T'
      *           List users Online Today
     C                   EVAL      *IN31 = *ON
     C                   WHEN      pMode = 'A'
      *           List all users
     C                   EVAL      *IN32 = *ON
     C                   WHEN      pMode = 'M'
      *           List all users (for Administrator)
     C                   EVAL      *IN33 = *ON
     C                   OTHER
     C                   EVAL      *INLR = *ON
     C                   RETURN
     C                   ENDSL
     C                   SETOFF                                       434445
     C                   Z-ADD     0             wRRN
     C                   EXSR      LoadSFL
     C                   ENDSR
      **********************************************************************
      * Check Function keys pressed by the user
      **********************************************************************
     C     ChkFkeys      BEGSR
      * F5=Refresh
     C                   IF        *IN05 = *ON
     C                   EXSR      ReLoadSFL
     C                   ENDIF
      * F12=Go back
     C                   IF        *IN12 = *ON
     C                   EVAL      *INLR = *ON
     C                   RETURN
     C                   ENDIF
     C                   ENDSR
      **********************************************************************
      * Check Options entered by the user
      **********************************************************************
     C     ChkOptions    BEGSR
     C                   READC     SF                                     91
     C                   DOW       *IN91 = *OFF
     C                   SELECT
     C                   WHEN      SCROPT = '2'
      * Change Access Level (Only from Administration)
     C                   IF        *IN33 = *ON
     C                   EVAL      wMode = 'N'
     C                   EVAL      wWinNumber = SCRLVL
     C                   EVAL      wWinF3 = *OFF
     C                   CALL      'BBSWINASKR'
     C                   PARM                    wMode
     C                   PARM                    wWinText
     C                   PARM                    wWinNumber
     C                   PARM                    wWinF3
     C                   IF        wWinF3 = *OFF
     C                   EVAL      SCRLVL = wWinNumber
     C                   EXSR      UpdateUser
     C                   ENDIF
     C                   ELSE
     C                   EVAL      MSGLIN = cErrOptNoAdmin
     C                   ENDIF
     C                   WHEN      SCROPT = '4'
      * Delete User (Only from Administration)
     C                   IF        *IN33 = *ON
      *       Ask for confirmation before deleting
     C     'Delete'      CAT       SCRNIC:1      wConfirmQ
     C                   EVAL      wConfirmA = 'N'
     C                   EVAL      wConfirmF3 = *OFF
     C                   CALL      'BBSWINYNR'
     C                   PARM                    wConfirmQ
     C                   PARM                    wConfirmA
     C                   PARM                    wConfirmF3
     C                   IF        wConfirmF3 = *OFF AND wConfirmA = 'Y'
     C     SCRNIC        CHAIN     PUSERS
     C                   IF        %FOUND
     C                   DELETE    PUSER
     C                   ENDIF
     C                   ENDIF
     C                   ELSE
     C                   EVAL      MSGLIN = cErrOptNoAdmin
     C                   ENDIF
     C                   WHEN      SCROPT = '6'
      * Send a Message to User
     C                   EVAL      wMode = 'U'
     C                   CALL      'BBSNEWMSGR'
     C                   PARM                    wMode
     C                   PARM                    wSubject
     C                   PARM                    SCRNIC
     C                   ENDSL
     C                   EVAL      SCROPT = *BLANKS
     C                   READC     SF                                     91
     C                   ENDDO
     C                   EXSR      ReLoadSFL
     C                   ENDSR
      **********************************************************************
      * If we want to reload the subfile, we need to clear it up first
      **********************************************************************
     C     ReLoadSFL     BEGSR
     C                   EVAL      *IN41 = *ON
     C                   WRITE     SFLCTL
     C                   EVAL      *IN41 = *OFF
     C                   Z-ADD     0             wRRN
     C                   EXSR      LoadSFL
     C                   ENDSR
      **********************************************************************
      * Load all records from PLOGGED
      * All records are loaded (load-all SFL) until the end of the file or
      *  until SFLSIZ is reached
      **********************************************************************
     C     LoadSFL       BEGSR
     C   30*START        SETLL     LLOGGED
     C   31*START        SETLL     LUSERS
     C   32*START        SETLL     PUSERS
     C   33*START        SETLL     PUSERS
     C                   DOU       %EOF
     C   30              READ      LLOGGED
     C   30LGDNCK        CHAIN     PUSERS                             91
     C   31              READ      LUSERS
     C   32              READ      PUSERS
     C   33              READ      PUSERS
     C                   IF        %EOF
     C                   LEAVE
     C                   ENDIF
      * Get User's data hides
     C                   EVAL      wUserOptHides = USRHID
      * Put data on screen
     C                   EXSR      LoadWhichUsrs
      * If we have loaded 9999 records, we cannot add more. Stop loop
     C                   IF        wRRN = 9999
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
      * If we loaded at least 1 record, enable SFL
     C                   IF        wRRN > 0
     C                   EVAL      *IN40  = *ON
     C                   ENDIF
     C                   EVAL      *IN42  = *ON
     C                   ENDSR
      **********************************************************************
      * Depending on pMode, we show different users (online, all, etc.)
      **********************************************************************
     C     LoadWhichUsrs BEGSR
      * If is SysOp and Hide SysOp = Y and is not from Administration,
      *  then do not show it
     C     USRNCK        IFEQ      'SYSOP'
     C     wHIDESO       ANDEQ     'Y'
     C     *IN33         ANDEQ     *OFF
     C                   GOTO      SKIPIT
     C                   ENDIF
     C                   IF        *IN30 = *ON
      *       Only users Online Now
     C                   EXSR      Logged2SFL
     C                   ENDIF
      *
     C                   IF        *IN31 = *ON
      *       Only users Online Today
     C                   IF        %NULLIND(USRLLD)
     C                   GOTO      SKIPIT
     C                   ELSE
     C     *DMY          MOVEL     wTodNowDateD  wTodayDate
     C     USRLLD        IFNE      wTodayDate
     C                   GOTO      SKIPIT
     C                   ELSE
     C                   EXSR      User2SFL
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
      *
     C                   IF        *IN32 = *ON OR *IN33 = *ON
      *       Users List (All or Administration)
     C                   EXSR      User2SFL
     C                   ENDIF
      * Add record to SFL and increment RRN
     C                   ADD       1             wRRN
     C                   WRITE     SF
     C     SKIPIT        TAG
     C                   ENDSR
      **********************************************************************
      * Put data of Logged now users into the screen
      **********************************************************************
     C     Logged2SFL    BEGSR
     C                   EVAL      SCRNIC = LGDNCK
     C                   EVAL      SCRLVL = LGDLVL
     C                   EXSR      GetOrHide
     C     *DMY          MOVEL     LGDDAT        SCRDAT
     C     *HMS          MOVEL     LGDTIM        SCRTIM
     C                   EVAL      SCRNLG = LGDNLG
      * Highlight Sender's Nickname?
     C                   IF        LGDNCK = wUser
     C                   EVAL      *IN45 = *ON
     C                   ELSE
     C                   EVAL      *IN45 = *OFF
     C                   ENDIF
     C                   ENDSR
      **********************************************************************
      * Put data of All or (Online Today) users into the screen
      **********************************************************************
     C     User2SFL      BEGSR
     C                   EVAL      SCRNIC = USRNCK
     C                   EVAL      SCRLVL = USRLVL
     C                   IF        %NULLIND(USRLLD)
     C                   EVAL      SCRDAT = *BLANKS
     C                   EVAL      SCRTIM = *BLANKS
     C                   ELSE
     C     *DMY          MOVEL     USRLLD        SCRDAT
     C     *HMS          MOVEL     USRLLT        SCRTIM
     C                   ENDIF
     C                   EVAL      SCRNLG = USRNLG
     C                   EXSR      GetOrHide
     C     *DMY          MOVEL     USRREG        SCRREG
      * Highlight Sender's Nickname?
     C                   IF        USRNCK = wUser
     C                   EVAL      *IN45 = *ON
     C                   ELSE
     C                   EVAL      *IN45 = *OFF
     C                   ENDIF
     C                   ENDSR
      **********************************************************************
      * After SysOp has changed the User's Access Level, update PUSERS
      **********************************************************************
     C     UpdateUser    BEGSR
     C     SCRNIC        CHAIN     PUSERS
     C                   IF        %FOUND
     C                   EVAL      USRLVL = SCRLVL
     C                   UPDATE    PUSER
     C                   ENDIF
     C                   ENDSR
      **********************************************************************
      * Based on PUSERS' USRHID, show or not User's personal information
      **********************************************************************
     C     GetOrHide     BEGSR
      * Blanks fields first
     C                   EVAL      SCRLCC = *BLANKS
     C                   EVAL      SCRLCT = *BLANKS
     C                   EVAL      SCRGND = *BLANKS
     C                   EVAL      SCREML = *BLANKS
     C     *DMY          MOVEL     USRREG        SCRREG
      * Now check if is to be shown or not
     C                   IF        wHideLocation = 'N'
     C                   EVAL      SCRLCC = USRCTR
     C                   EVAL      SCRLCT = USRCTY
     C                   ENDIF
     C                   IF        wHideGender = 'N'
     C                   EVAL      SCRGND = USRGND
     C                   ENDIF
     C                   IF        wHideEmail = 'N'
     C                   EVAL      SCREML = USREML
     C                   ENDIF
     C                   ENDSR
