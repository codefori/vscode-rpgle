     H/TITLE List all Boards available (Access Lvl) for the user
     H COPYRIGHT('(C) 2020 David Asta under MIT License')
      * SYSTEM      : V4R5
      * PROGRAMMER  : David Asta
      * DATE-WRITTEN: 19/NOV/2020
      *
      * This program shows the list of Boards that the user can access,
      *   depending on the user's Access Level
      **********************************************************************
     H/COPY DVBBS400/CURRENTSRC,CBKOPTIMIZ
      **********************************************************************
      * INDICATORS USED:
      * 25 - Roll key
      * 40 - SFLDSP
      * 41 - SFLCLR
      * 42 - SFLEND(*MORE)
      * 98 - EOF
      * 99 - EOF
      **********************************************************************
     FBBSBRDLD  CF   E             WORKSTN
     F                                     SFILE(SF:wRRN)
     FPBOARDS   UF A E           K DISK
     FPSBORDS   IF   E           K DISK
     FPMESSGS   IF   E           K DISK
      **********************************************************************
      * Data structures
     D/COPY DVBBS400/CURRENTSRC,CBKDTAARA
      * Constants
     D cUp             C                   CONST('ABCDEFGHIJKLMNOPQRSTUVWXYZ')
     D cLo             C                   CONST('abcdefghijklmnopqrstuvwxyz')
     D cKeysAdmin      C                   CONST('F5=Refresh   F6=Create   F12=-
     D                                     Go back')
     D cKeysUser       C                   CONST('F5=Refresh   F12=Go back')
     D cNoBoards       C                   CONST('No boards are accessible for -
     D                                     your Access Level.')
     D cErrOptNoAdmin  C                   CONST('Value entered for field is no-
     D                                     t valid. Valid values listed in mess-
     D                                     age help.')
     D cErrMaxLvl99    C                   CONST('Maximum Level can only be 99.-
     D                                     ')
      * Variables
     D/COPY DVBBS400/CURRENTSRC,CBKUSEWINS
     D pMode           S              1A
     D wRRN            S              4P 0
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
     C                   PARM                    pMode
     C                   IF        pMode = 'A'
     C                   EVAL      *IN50 = *ON
     C                   EVAL      KEYLST = cKeysAdmin
     C                   ELSE
     C                   EVAL      *IN50 = *OFF
     C                   EVAL      KEYLST = cKeysUser
     C                   ENDIF
      * Get values from DATAARA and show them on screen
     C/COPY DVBBS400/CURRENTSRC,CBKHEADER
      * Declare Composite Keys
     C     KMESSGS       KLIST
     C                   KFLD                    SBRBRD
     C                   KFLD                    SBRSHT
      * Initialise variables and load subfile
     C                   EVAL      SCRSCR = 'BBSBRDL'
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
     C                   EXSR      CreateNewBoard
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
     C                   WHEN      SCROPT = '4'
      * Delete (only from Administration Menu)
     C                   IF        *IN50 = *ON
      *         Ask for confirmation before deleting
     C     'Delete'      CAT       SCRSHT:1      wConfirmQ
     C                   EVAL      wConfirmA = 'N'
     C                   EVAL      wConfirmF3 = *OFF
     C                   CALL      'BBSWINYNR'
     C                   PARM                    wConfirmQ
     C                   PARM                    wConfirmA
     C                   PARM                    wConfirmF3
     C                   IF        wConfirmF3 = *OFF AND wConfirmA = 'Y'
     C     SCRSHT        CHAIN     PBOARDS
     C                   IF        %FOUND
     C                   DELETE    RBOARD
     C                   ENDIF
     C                   ENDIF
     C                   ELSE
     C                   EVAL      MSGLIN = cErrOptNoAdmin
     C                   ENDIF
     C                   WHEN      SCROPT = '5'
      * List Sub-Boards
     C                   CALL      'BBSSBRLR'
     C                   PARM                    SCRSHT
     C                   PARM                    pMode
     C                   WHEN      SCROPT = '7'
      * Rename Short (only from Administration Menu)
     C                   EVAL      wWinMode = 'T'
     C                   EVAL      wWinText = SCRSHT
     C                   EVAL      wWinNumber = 0
     C                   EVAL      wWinF3 = *OFF
     C                   CALL      'BBSWINASKR'
     C                   PARM                    wWinMode
     C                   PARM                    wWinText
     C                   PARM                    wWinNumber
     C                   PARM                    wWinF3
     C                   IF        wWinF3 = *OFF
     C     SCRSHT        CHAIN     PBOARDS
     C                   EVAL      BRDSHT = wWinText
      * Convert it to Uppercase
     C     cLo:cUp       XLATE     BRDSHT        BRDSHT
     C                   IF        %FOUND
     C                   UPDATE    RBOARD
     C                   ENDIF
     C                   ENDIF
     C                   WHEN      SCROPT = '8'
      * Rename Long (only from Administration Menu)
     C                   EVAL      wWinMode = 'T'
     C                   EVAL      wWinText = SCRLNG
     C                   EVAL      wWinNumber = 0
     C                   EVAL      wWinF3 = *OFF
     C                   CALL      'BBSWINASKR'
     C                   PARM                    wWinMode
     C                   PARM                    wWinText
     C                   PARM                    wWinNumber
     C                   PARM                    wWinF3
     C                   IF        wWinF3 = *OFF
     C     SCRSHT        CHAIN     PBOARDS
     C                   EVAL      BRDLNG = wWinText
     C                   IF        %FOUND
     C                   UPDATE    RBOARD
     C                   ENDIF
     C                   ENDIF
     C                   WHEN      SCROPT = '9'
      * Change Access Level (only from Administration Menu)
     C                   IF        *IN50 = *ON
     C                   EVAL      wWinMode = 'N'
     C                   EVAL      wWinText = *BLANKS
     C                   EVAL      wWinNumber = SCRALV
     C                   EVAL      wWinF3 = *OFF
     C                   CALL      'BBSWINASKR'
     C                   PARM                    wWinMode
     C                   PARM                    wWinText
     C                   PARM                    wWinNumber
     C                   PARM                    wWinF3
     C                   IF        wWinF3 = *OFF
      *       Check the value is less than 100
     C                   IF        wWinNumber > 99
     C                   EVAL      MSGLIN = cErrMaxLvl99
     C                   ELSE
     C     SCRSHT        CHAIN     PBOARDS
     C                   EVAL      BRDALV = wWinNumber
     C                   IF        %FOUND
     C                   UPDATE    RBOARD
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
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
      * Load all records from PBOARDS
      * All records are loaded (load-all SFL) until the end of the file or
      *  until SFLSIZ is reached
      **********************************************************************
     C     LoadSFL       BEGSR
     C     *START        SETLL     PBOARDS
     C                   DOU       %EOF
     C                   READ      PBOARDS
     C                   IF        %EOF
     C                   LEAVE
     C                   ENDIF
      * Put data on screen (if User has enough Access Level)
     C                   IF        wUserLvlD >= BRDALV
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
      * As we loaded the entire file, we can set on SFLEND
     C                   EVAL      *IN42  = *ON
     C                   ENDSR
      **********************************************************************
      * Put data into a SFL record
      **********************************************************************
     C     Data2SFL      BEGSR
     C                   EVAL      SCRSHT = BRDSHT
     C                   EVAL      SCRLNG = BRDLNG
     C                   EVAL      SCRLNG = BRDLNG
     C   50              EVAL      SCRALV = BRDALV
      * Get number of Sub-Boards
     C                   EVAL      SCRNSB = 0
     C                   EVAL      SCRNMS = 0
     C     BRDSHT        SETLL     PSBORDS                              99
     C                   DOW       *IN99 = *OFF
     C     BRDSHT        READE     PSBORDS                                99
     C   99              LEAVE
     C                   ADD       1             SCRNSB
      * Get number of Posts on read Sub-Board
     C     KMESSGS       SETLL     PMESSGS                              98
     C                   DOW       *IN98 = *OFF
     C     KMESSGS       READE     PMESSGS                                98
     C   98              LEAVE
     C                   ADD       1             SCRNMS
     C                   ENDDO
     C                   ENDDO
     C                   ENDSR
      **********************************************************************
      * Ask the Admin user for a Short Name,
      * and create a record with default values
      **********************************************************************
     C     CreateNewBoardBEGSR
     C                   EVAL      wWinMode = 'T'
     C                   EVAL      wWinText = *BLANKS
     C                   EVAL      wWinNumber = 0
     C                   EVAL      wWinF3 = *OFF
     C                   CALL      'BBSWINASKR'
     C                   PARM                    wWinMode
     C                   PARM                    wWinText
     C                   PARM                    wWinNumber
     C                   PARM                    wWinF3
     C                   IF        wWinF3 = *OFF
     C                   EVAL      BRDSHT = wWinText
      * Convert it to Uppercase
     C     cLo:cUp       XLATE     BRDSHT        BRDSHT
     C                   EVAL      BRDLNG = BRDSHT
     C                   EVAL      BRDALV = 99
     C                   WRITE     RBOARD
     C                   EXSR      ReLoadSFL
     C                   ENDIF
     C                   ENDSR
