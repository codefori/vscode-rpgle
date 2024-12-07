     H/TITLE Sub-Board's Messages List (SFL)
     H COPYRIGHT('(C) 2020 David Asta under MIT License')
      * SYSTEM      : V4R5
      * PROGRAMMER  : David Asta
      * DATE-WRITTEN: 15/NOV/2020
      *
      * This program shows all messages from a specific Sub-Board, ordered
      *   descent by posted date/time
      **********************************************************************
     H/COPY DVBBS400/CURRENTSRC,CBKOPTIMIZ
      **********************************************************************
      * INDICATORS USED:
      * 25 - Roll key
      * 40 - SFLDSP
      * 41 - SFLCLR
      * 42 - SFLEND(*MORE)
      * 43 - Highlight whole record in RED if is a SysOp (Acc. Lvl 99)
      * 44 - Highlight Sender in WHT if is same as current user
      * 92 - NOT FOUND for CHAIN
      **********************************************************************
     FBBSMSGSD  CF   E             WORKSTN
     F                                     SFILE(SF:wRRN)
     FLMESSGS   IF   E           K DISK
     FPBOARDS   IF   E           K DISK
     FPSBORDS   IF   E           K DISK
      **********************************************************************
      * Data structures
     D/COPY DVBBS400/CURRENTSRC,CBKDTAARA
      * Constants
     D cErrLvlLowP     C                   CONST('Your Access Level is too low -
     D                                     for posting messages in this Sub-Boa-
     D                                     rd.')
      * Variables
     D pBoardUID       S              8A
     D pSBoardUID      S              8A
     D wRRN            S              4P 0
     D wCfgKey         S              6A
     D wMode           S              1A   INZ('B')
     D wRcpnt          S             10A
     D wBlanks         S             45A   INZ(*BLANKS)
     D wSubject        S             45A
     D wUserLvlD       S              2P 0
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
      * Receive parameters
     C     *ENTRY        PLIST
     C                   PARM                    pBoardUID
     C                   PARM                    pSBoardUID
      * Get Board and Sub-Board short and long descriptions
     C     pBoardUID     CHAIN     PBOARDS                            92
     C   92              EVAL      SCRBRS = '*ERROR*'
     C   92              EVAL      SCRBRL = '*ERROR*'
     C  N92              EVAL      SCRBRS = BRDSHT
     C  N92              EVAL      SCRBRL = BRDLNG
     C     KSBORDS       KLIST
     C                   KFLD                    pBoardUID
     C                   KFLD                    pSBoardUID
     C     KSBORDS       CHAIN     PSBORDS                            92
     C   92              EVAL      SCRSBS = '*ERROR*'
     C   92              EVAL      SCRSBL = '*ERROR*'
     C  N92              EVAL      SCRSBS = SBRSHT
     C  N92              EVAL      SCRSBL = SBRLNG
      * Define composite key to access PMESSGS
     C     KMSGS         KLIST
     C                   KFLD                    pBoardUID
     C                   KFLD                    pSBoardUID
     C                   EVAL      SCRSCR = 'BBSMSGS'
      * Get values from DATAARA and show them on screen
     C/COPY DVBBS400/CURRENTSRC,CBKHEADER
      * Initialise variables and indicators, and load subfile
     C                   MOVEL     wUserLvl      wUserLvlD
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
      * F6=Post message
     C                   IF        *IN06 = *ON
      * Check Access Level to Post
     C                   IF        wUserLvlD >= SBRPLV
     C                   EVAL      wRcpnt = pBoardUID
     C                   CALL      'BBSNEWMSGR'
     C                   PARM                    wMode
     C                   PARM                    wBlanks
     C                   PARM                    wRcpnt
     C                   PARM                    pSBoardUID
     C                   ELSE
     C                   EVAL      MSGLIN = cErrLvlLowP
     C                   ENDIF
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
     C                   WHEN      SCROPT = '5'
      * Display Message
     C                   CALL      'BBSRDMSGR'
     C                   PARM                    wMode
     C                   PARM                    SCRMUI
     C                   PARM                    SCRBRS
     C                   PARM                    SCRSBS
     C                   WHEN      SCROPT = '7'
      * Reply to Message
     C                   EVAL      wRcpnt = pBoardUID
     C     'Re:'         CAT       SCRSBJ:1      wSubject
     C                   CALL      'BBSNEWMSGR'
     C                   PARM                    wMode
     C                   PARM                    wSubject
     C                   PARM                    wRcpnt
     C                   PARM                    pSBoardUID
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
      * Load all messages from PMESSGS
      * All records are loaded (load-all SFL) until the end of the file or
      *  until SFLSIZ is reached
      **********************************************************************
     C     LoadSFL       BEGSR
     C     KMSGS         SETLL     LMESSGS
     C                   DOU       %EOF
     C     KMSGS         READE     LMESSGS
     C                   IF        %EOF
     C                   LEAVE
     C                   ENDIF
      * Put data on screen
     C                   EXSR      Data2SFL
     C                   ADD       1             wRRN
     C                   WRITE     SF
      * If we have loaded 9999 records, we cannot add more. Stop loop
     C                   IF        wRRN = 9999
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
      * If we loaded at least 1 record, enable SFL
     C                   IF        wRRN > 0
     C                   EVAL      *IN40  = *ON
     C                   ENDIF
      * As we loaded the entire file, we can set on SFLEND
     C                   EVAL      *IN42  = *ON
     C                   ENDSR
      **********************************************************************
      * Put data from files to screen
      * Also gets the corresponding nickname of the user that sent the
      *  message, from PUSERS
      **********************************************************************
     C     Data2SFL      BEGSR
     C                   EVAL      SCRSBJ = MSGSBJ
     C     *DMY          MOVEL     MSGDAT        SCRDAT
     C     *HMS          MOVEL     MSGTIM        SCRTIM
     C                   EVAL      SCRSND = MSGSND
     C                   EVAL      SCRMUI = MSGUID
      * Highlight Sender's Nickname?
     C                   IF        MSGSND = wUser
     C                   EVAL      *IN44 = *ON
     C                   ELSE
     C                   EVAL      *IN44 = *OFF
     C                   ENDIF
     C                   IF        MSGSND = 'SYSOP' AND wHLSOMS = 'Y'
     C                   EVAL      *IN43 = *ON
     C                   ELSE
     C                   EVAL      *IN43 = *OFF
     C                   ENDIF
     C                   ENDSR
