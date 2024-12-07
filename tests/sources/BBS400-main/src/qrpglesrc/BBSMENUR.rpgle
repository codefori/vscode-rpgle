     H/TITLE Main Menu
     H COPYRIGHT('(C) 2020 David Asta under MIT License')
      * SYSTEM      : V4R5
      * PROGRAMMER  : David Asta
      * DATE-WRITTEN: 26/NOV/2020
      *
      * This program shows the Main Menu of the BBS to a logged in User
      **********************************************************************
     H/COPY DVBBS400/CURRENTSRC,CBKOPTIMIZ
      **********************************************************************
     FBBSMENUD  CF   E             WORKSTN
     FPSESSIONS UF A E             DISK
      **********************************************************************
      * Data structures
     D/COPY DVBBS400/CURRENTSRC,CBKDTAARA
      * Variables
     D wBlanks         S             45A   INZ(*BLANKS)
     D wMode           S              1A
      **********************************************************************
     C                   WRITE     HEADER
     C                   WRITE     FOOTER
     C   25              WRITE     MNUSOP
     C                   EXFMT     MNUUSR
     C                   EXSR      CheckFkeys
      **********************************************************************
      * Subroutine called automatically at startup
      **********************************************************************
     C     *INZSR        BEGSR
     C                   EVAL      SCRSCR = 'BBSMENU'
      * Get values from DTAARA and show them on the screen
     D/COPY DVBBS400/CURRENTSRC,CBKHEADER
      * Get Last Login Date from DTAARA too
     C                   IN        wDTAARA
     C                   EVAL      SCRLLG = wUserLstLogin
     C                   UNLOCK    wDTAARA
      * Enable Menu options depending on User Authorisations
     C                   EVAL      wUserLvlAuths = wLvlAuths
     C                   SETOFF                                       252627
     C                   SETOFF                                       282930
     C                   IF        wAuthListUser = 'Y'
     C                   EVAL      *IN26  = *ON
     C                   ENDIF
     C                   IF        wAuthSysInfor = 'Y'
     C                   EVAL      *IN27  = *ON
     C                   ENDIF
     C                   IF        wAuthPostMsgs = 'Y'
     C                   EVAL      *IN28  = *ON
     C                   ENDIF
     C                   IF        wAuthMsgUsers = 'Y'
     C                   EVAL      *IN29  = *ON
     C                   ENDIF
     C                   IF        wAuthWhosOnli = 'Y'
     C                   EVAL      *IN30  = *ON
     C                   ENDIF
      * Enable Administration Menu option, if level is 99 (SysOp)
     C                   IF        wUserLvl = '99'
     C                   EVAL      *IN25  = *ON
     C                   ENDIF
     C                   ENDSR
      **********************************************************************
      * Check Functions keys pressed by user
      **********************************************************************
     C     CheckFkeys    BEGSR
      * Normal User (non-SysOp) keys
     C                   SELECT
     C                   WHEN      *IN03 = *ON
      * F3=Exit (Log off)
      *       Record entry in PSESSIONS and exit
     C                   TIME                    SESTMS
     C                   EVAL      SESNCK = wUser
     C                   EVAL      SESTYP = 'O'
     C                   WRITE     RSESSION
     C                   EVAL      *INLR = *ON
     C                   RETURN
     C                   WHEN      *IN04 = *ON
      * F4= List Boards & Sub-Boards
     C                   CALL      'BBSBRDLR'
     C                   PARM                    wBlanks
     C                   WHEN      *IN05 = *ON
      * F5=Read new posts since last login
     C                   CALL      'BBSMSGSLLR'
     C                   WHEN      *IN06 = *ON
      * F6=Post message
     C                   EVAL      wMode = 'P'
     C                   CALL      'BBSNEWMSGR'
     C                   PARM                    wMode
     C                   PARM                    wBlanks
     C                   PARM                    wBlanks
     C                   PARM                    wBlanks
     C                   WHEN      *IN07 = *ON
      * F7=Read messages sent to you
     C                   CALL      'BBSRDU2UMR'
     C                   WHEN      *IN08 = *ON
      * F8=Send message to another user
     C                   EVAL      wMode = 'U'
     C                   CALL      'BBSNEWMSGR'
     C                   PARM                    wMode
     C                   PARM                    wBlanks
     C                   PARM                    wBlanks
     C                   PARM                    wBlanks
     C                   WHEN      *IN09 = *ON
      * F9=Send message to the SysOp
     C                   EVAL      wMode = 'S'
     C                   CALL      'BBSNEWMSGR'
     C                   PARM                    wMode
     C                   PARM                    wBlanks
     C                   PARM                    wBlanks
     C                   PARM                    wBlanks
     C                   WHEN      *IN10 = *ON
      * F10=System Information
     C                   CALL      'BBSSYSINFC'
     C                   WHEN      *IN11 = *ON
      * F11=BBS Information
     C                   CALL      'BBSINFC'
     C                   WHEN      *IN13 = *ON
      * F13=Users Online today
     C                   EVAL      wMode = 'T'
     C                   CALL      'BBSLSTUSRR'
     C                   PARM                    wMode
     C                   WHEN      *IN14 = *ON
      * F14=List All users
     C                   EVAL      wMode = 'A'
     C                   CALL      'BBSLSTUSRR'
     C                   PARM                    wMode
     C                   WHEN      *IN15 = *ON
      * F15=Who is online now
     C                   EVAL      wMode = 'N'
     C                   CALL      'BBSLSTUSRR'
     C                   PARM                    wMode
     C                   WHEN      *IN16 = *ON
      * F16=Change User Options
     C                   CALL      'BBSUSROPTR'
     C                   WHEN      *IN17 = *ON
      * F17=View Text Files (IFS)
     C                   CALL      'BBSIFSFILR'
     C                   PARM                    wMode
     C                   WHEN      *IN18 = *ON
      * F18=External Programs
     C                   EVAL      wMode = 'U'
     C                   CALL      'BBSETPGMSR'
     C                   PARM                    wMode
     C                   WHEN      *IN19 = *ON
      * F19=View/Answer Polls
     C                   EVAL      wMode = 'U'
     C                   CALL      'BBSPOLLSLR'
     C                   PARM                    wMode
      * SysOp keys
     C                   WHEN      *IN02 = *ON AND *IN25 = *ON
     C                   CALL      'BBSADMMNUR'
     C                   ENDSL
     C                   ENDSR
