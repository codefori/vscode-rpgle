     H/TITLE List all Sub-Boards available (Access Lvl) for the user
     H COPYRIGHT('(C) 2020 David Asta under MIT License')
      * SYSTEM      : V4R5
      * PROGRAMMER  : David Asta
      * DATE-WRITTEN: 19/NOV/2020
      *
      * This program shows the list of Sub-Boards that the user can access,
      *   depending on the user's Access Level
      **********************************************************************
     H/COPY DVBBS400/CURRENTSRC,CBKOPTIMIZ
      **********************************************************************
      * INDICATORS USED:
      * 25 - Roll key
      * 40 - SFLDSP
      * 41 - SFLCLR
      * 42 - SFLEND(*MORE)
      * 60 - F3 pressed on a window
      **********************************************************************
     FBBSSBRLD  CF   E             WORKSTN
     F                                     SFILE(SF:wRRN)
     FPSBORDS   UF A E           K DISK
     FPMESSGS   UF   E           K DISK
      **********************************************************************
      * Data structures
     D/COPY DVBBS400/CURRENTSRC,CBKDTAARA
      * Constants
     D cUp             C                   CONST('ABCDEFGHIJKLMNOPQRSTUVWXYZ')
     D cLo             C                   CONST('abcdefghijklmnopqrstuvwxyz')
     D cNoSBoards      C                   CONST('No Sub-Boards are accessible -
     D                                     for your Access Level.')
     D cKeysAdmin      C                   CONST('F5=Refresh   F6=Create   F10=-
     D                                     Additional Information   F12=Go back-
     D                                     ')
     D cKeysUser       C                   CONST('F5=Refresh   F10=Additional I-
     D                                     nformation   F12=Go back')
     D cErrOptNoAdmin  C                   CONST('Value entered for field is no-
     D                                     t valid. Valid values listed in mess-
     D                                     age help.')
     D cErrMaxLvl99    C                   CONST('Maximum Level can only be 99.-
     D                                     ')
     D cErrLvlLowR     C                   CONST('Your Access Level is too low -
     D                                     for reading messages in this Sub-Boa-
     D                                     rd.')
     D cErrLvlLowP     C                   CONST('Your Access Level is too low -
     D                                     for posting messages in this Sub-Boa-
     D                                     rd.')
      * Variables
     D/COPY DVBBS400/CURRENTSRC,CBKUSEWINS
     D pMode           S              1A
     D pBoardID        S              8A
     D wRRN            S              4P 0
     D wUserLvlD       S              2P 0
     D wBlanks         S             45A   INZ(*BLANKS)
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
     C                   PARM                    pBoardID
     C                   PARM                    pMode
     C                   IF        pMode = 'A'
     C                   EVAL      *IN50 = *ON
     C                   EVAL      KEYLST = cKeysAdmin
     C                   ELSE
     C                   EVAL      *IN50 = *OFF
     C                   EVAL      KEYLST = cKeysUser
     C                   ENDIF
      * Declare Composite Keys
     C     KSBORDS       KLIST
     C                   KFLD                    SCRBRD
     C                   KFLD                    SCRSBS
     C     KMESSGS       KLIST
     C                   KFLD                    SCRBRD
     C                   KFLD                    SCRSBS
      * Get values from DATAARA and show them on screen
     C/COPY DVBBS400/CURRENTSRC,CBKHEADER
      * Initialise variables and load subfile
     C                   EVAL      SCRSCR = 'BBSSBRL'
     C                   MOVEL     wUserLvl      wUserLvlD
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
      * F6=Create (only from Administration Menu)
     C                   IF        *IN06 = *ON AND *IN50 = *ON
     C                   EXSR      CreateNewSBord
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
     C                   WHEN      SCROPT = '1'
      * Change Access Level (only from Administration Menu)
     C                   IF        *IN50 = *ON
     C                   EXSR      ChgAccessLvl
      * Post a message (only from outside Administration Menu)
     C                   ELSE
     C                   IF        wUserLvlD >= SCRPLV
     C                   EVAL      pMode = 'B'
     C                   CALL      'BBSNEWMSGR'
     C                   PARM                    pMode
     C                   PARM                    wBlanks
     C                   PARM                    SCRBRD
     C                   PARM                    SCRSBS
     C                   ELSE
     C                   EVAL      MSGLIN = cErrLvlLowP
     C                   ENDIF
     C                   ENDIF
     C                   WHEN      SCROPT = '2'
      * Change Read Level (only from Administration Menu)
     C   50              EXSR      ChgReadLvl
     C  N50              EVAL      MSGLIN = cErrOptNoAdmin
     C                   WHEN      SCROPT = '3'
      * Change Post Level (only from Administration Menu)
     C   50              EXSR      ChgPostLvl
     C  N50              EVAL      MSGLIN = cErrOptNoAdmin
     C                   WHEN      SCROPT = '4'
      * Delete Sub-Board (only from Administration Menu)
     C   50              EXSR      DeleteSBoard
     C  N50              EVAL      MSGLIN = cErrOptNoAdmin
     C                   WHEN      SCROPT = '5'
      * Display Messages on the Sub-Board
     C                   IF        *IN50 = *OFF
      * Only available outside Administration Menu
     C                   IF        wUserLvlD >= SCRRLV
     C                   CALL      'BBSMSGSR'
     C                   PARM                    SCRBRD
     C                   PARM                    SCRSBS
     C                   ELSE
     C                   EVAL      MSGLIN = cErrLvlLowR
     C                   ENDIF
     C                   ELSE
     C                   EVAL      MSGLIN = cErrOptNoAdmin
     C                   ENDIF
     C                   WHEN      SCROPT = '7'
      * Rename Sub-Board (only from Administration Menu)
     C   50              EXSR      RenameSBoard
     C  N50              EVAL      MSGLIN = cErrOptNoAdmin
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
      * Load all records from PSBORDS
      * All records are loaded (load-all SFL) until the end of the file or
      *  until SFLSIZ is reached
      **********************************************************************
     C     LoadSFL       BEGSR
     C     pBoardID      SETLL     PSBORDS
     C                   DOU       %EOF
     C     pBoardID      READE     PSBORDS
     C                   IF        %EOF
     C                   LEAVE
     C                   ENDIF
      * Put data on screen (if User has enough Access Level)
     C                   IF        wUserLvlD >= SBRALV
     C                   EXSR      Data2SFL
     C                   ADD       1             wRRN
     C                   WRITE     SF
      * If we have loaded 9999 records, we cannot add more. Stop loop
     C                   IF        wRRN = 9999
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
     C                   ENDDO
      * If we loaded at least 1 record, enable SFL
     C                   IF        wRRN > 0
     C                   EVAL      *IN40  = *ON
     C                   ENDIF
     C                   EVAL      *IN42  = *ON
     C                   ENDSR
      **********************************************************************
      * Put data into a SFL record
      **********************************************************************
     C     Data2SFL      BEGSR
     C                   EVAL      SCRBRD = pBoardID
     C                   EVAL      SCRLNG = SBRLNG
     C                   EVAL      SCRRLV = SBRRLV
     C                   EVAL      SCRPLV = SBRPLV
     C                   EVAL      SCRSBS = SBRSHT
      * Only show Access Level on Administration
     C   50              EVAL      SCRALV = SBRALV
      * Get number of Posts
     C                   EVAL      SCRNMS = 0
     C     KSBORDS       SETLL     PMESSGS                              98
     C                   DOW       *IN98 = *OFF
     C     KSBORDS       READE     PMESSGS                                98
     C   98              LEAVE
     C                   ADD       1             SCRNMS
     C                   ENDDO
     C                   ENDSR
      **********************************************************************
      * Ask the Admin user for a Short Name,
      * and create a record with default values
      **********************************************************************
     C     CreateNewSBordBEGSR
     C                   EVAL      wWinMode = 'T'
     C                   EVAL      wWinText = *BLANKS
     C                   EVAL      wWinNumber = 0
     C                   EXSR      AskNewInfo
     C                   IF        wWinF3 = *OFF
     C                   EVAL      SBRBRD = pBoardID
     C                   EVAL      SBRSHT = wWinText
      * Convert it to Uppercase
     C     cLo:cUp       XLATE     SBRSHT        SBRSHT
     C                   EVAL      SBRLNG = SBRSHT
     C                   EVAL      SBRALV = 99
     C                   EVAL      SBRRLV = 99
     C                   EVAL      SBRPLV = 99
     C                   WRITE     RSBORD
     C                   EXSR      ReLoadSFL
     C                   ENDIF
     C                   ENDSR
      **********************************************************************
      * Change Access Level (only from Administration Menu)
      **********************************************************************
     C     ChgAccessLvl  BEGSR
     C                   EVAL      wWinMode = 'N'
     C                   EVAL      wWinText = *BLANKS
     C                   EVAL      wWinNumber = SCRALV
     C                   EXSR      AskNewInfo
     C                   IF        wWinF3 = *OFF
      *       Check the value is less than 100
     C                   IF        wWinNumber > 99
     C                   EVAL      MSGLIN = cErrMaxLvl99
     C                   ELSE
     C     KSBORDS       CHAIN     PSBORDS                            60
     C  N60              EVAL      SBRALV = wWinNumber
     C  N60              UPDATE    RSBORD
     C                   ENDIF
     C                   ENDIF
     C                   ENDSR
      **********************************************************************
      * Change Read Level (only from Administration Menu)
      **********************************************************************
     C     ChgReadLvl    BEGSR
     C                   EVAL      wWinMode = 'N'
     C                   EVAL      wWinText = *BLANKS
     C                   EVAL      wWinNumber = SCRRLV
     C                   EXSR      AskNewInfo
     C                   IF        wWinF3 = *OFF
      *       Check the value is less than 100
     C                   IF        wWinNumber > 99
     C                   EVAL      MSGLIN = cErrMaxLvl99
     C                   ELSE
     C     KSBORDS       CHAIN     PSBORDS                            60
     C  N60              EVAL      SBRRLV = wWinNumber
     C  N60              UPDATE    RSBORD
     C                   ENDIF
     C                   ENDIF
     C                   ENDSR
      **********************************************************************
      * Change Post Level (only from Administration Menu)
      **********************************************************************
     C     ChgPostLvl    BEGSR
     C                   EVAL      wWinMode = 'N'
     C                   EVAL      wWinText = *BLANKS
     C                   EVAL      wWinNumber = SCRPLV
     C                   EXSR      AskNewInfo
     C                   IF        wWinF3 = *OFF
      *       Check the value is less than 100
     C                   IF        wWinNumber > 99
     C                   EVAL      MSGLIN = cErrMaxLvl99
     C                   ELSE
     C     KSBORDS       CHAIN     PSBORDS                            60
     C  N60              EVAL      SBRPLV = wWinNumber
     C  N60              UPDATE    RSBORD
     C                   ENDIF
     C                   ENDIF
     C                   ENDSR
      **********************************************************************
      * Delete Sub-Board (only from Administration Menu)
      **********************************************************************
     C     DeleteSBoard  BEGSR
      * Ask for confirmation before deleting
     C     'Delete'      CAT       SCRSBS:1      wConfirmQ
     C                   EVAL      wConfirmA = 'N'
     C                   EVAL      wConfirmF3 = *OFF
     C                   CALL      'BBSWINYNR'
     C                   PARM                    wConfirmQ
     C                   PARM                    wConfirmA
     C                   PARM                    wConfirmF3
     C                   IF        wConfirmF3 = *OFF AND wConfirmA = 'Y'
     C     KSBORDS       CHAIN     PSBORDS
     C                   IF        %FOUND
     C                   DELETE    RSBORD
      * Delete Messages too
     C     KMESSGS       SETLL     PMESSGS
     C     KMESSGS       READE     PMESSGS
     C                   DOU       %EOF
     C                   DELETE    RMESSG
     C     KMESSGS       READE     PMESSGS
     C                   ENDDO
     C                   ENDIF
     C                   ENDIF
     C                   ENDSR
      **********************************************************************
      * Rename Sub-Board (only from Administration Menu)
      **********************************************************************
     C     RenameSBoard  BEGSR
     C                   EVAL      wWinMode = 'T'
     C                   EVAL      wWinText = SCRLNG
     C                   EVAL      wWinNumber = 0
     C                   EXSR      AskNewInfo
     C  N60KSBORDS       CHAIN     PSBORDS                            60
     C  N60              EVAL      SBRLNG = wWinText
     C  N60              UPDATE    RSBORD
     C                   ENDSR
      **********************************************************************
      * Show a window asking for a Text or a number
      * and if F3 was not pressed, update PSBORDS
      **********************************************************************
     C     AskNewInfo    BEGSR
     C                   EVAL      wWinF3 = *OFF
     C                   CALL      'BBSWINASKR'
     C                   PARM                    wWinMode
     C                   PARM                    wWinText
     C                   PARM                    wWinNumber
     C                   PARM                    wWinF3
     C                   EVAL      *IN60 = wWinF3
     C                   ENDSR
