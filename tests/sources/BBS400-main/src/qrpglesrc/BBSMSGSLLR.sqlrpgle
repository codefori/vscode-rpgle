     H/TITLE List all Messages posted since last User's login
     H COPYRIGHT('(C) 2020 David Asta under MIT License')
      * SYSTEM      : V4R5
      * PROGRAMMER  : David Asta
      * DATE-WRITTEN: 29/NOV/2020
      *
      * This program retrieves the list of all messages posted since last
      *   time the user logged in.
      * It's an SQLRPGLE, so performance isn't that great. As per my own
      * tests on my Model 150. It's 2.5 to 3.5 slower than RPGLE.
      **********************************************************************
      * INDICATORS USED:
      * 25 - Roll key
      * 40 - SFLDSP
      * 41 - SFLCLR
      * 42 - SFLEND(*MORE)
      **********************************************************************
      * Compiler flags
     H OPTIMIZE(*FULL)
      **********************************************************************
     FBBSMSGSLLDCF   E             WORKSTN
     F                                     SFILE(SF:wRRN)
      **********************************************************************
      * Data structures
     D/COPY DVBBS400/CURRENTSRC,CBKDTAARA
     D dsMessages      DS
     D   wMSGBRD                      8A
     D   wMSGSBD                      8A
     D   wMSGUID                     32A
     D   wMSGSBJ                     45A
     D   wMSGDAT                       D
     D   wMSGTIM                       T
     D   wMSGSND                     10A
      * Constants
     D cMsgNoRecords   C                   CONST('There no new messages.')
      * Variables
     D wRRN            S              4P 0
     D wMode           S              1A   INZ('B')
     D wRcpnt          S             10A
     D wSubject        S             45A
      **********************************************************************
     C                   WRITE     FOOTER
     C                   EXFMT     SFLCTL
     C                   CLEAR                   MSGLIN
     C                   EXSR      ChkFkeys
     C   40              EXSR      ChkOptions
      **********************************************************************
      * Subroutine called automatically at startup
      **********************************************************************
     C     *INZSR        BEGSR
     C                   EVAL      SCRSCR = 'BBSMSGSLL'
      * Get values from DTAARA and show them on the screen
     C/COPY DVBBS400/CURRENTSRC,CBKHEADER
      * Declare SQL cursor
     C/EXEC SQL
     C+  DECLARE C1 CURSOR FOR
     C+    SELECT MSGBRD, MSGSBD, MSGUID, MSGSBJ, MSGDAT, MSGTIM, MSGSND
     C+    FROM PSBORDS AS S JOIN PMESSGS AS M
     C+      ON S.SBRBRD = M.MSGBRD
     C+       AND S.SBRSHT = M.MSGSBD
     C+    WHERE MSGDAT = :wUserLstLogin
     C+    ORDER BY MSGDAT DESC, MSGTIM DESC
     C/END-EXEC
      * Initialise Relative Record Number variable and load subfile
     C                   Z-ADD     0             wRRN
     C                   EXSR      LoadSFL
     C                   ENDSR
      **********************************************************************
      * Check Functions keys pressed by the user
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
     C     SCROPT        IFNE      *BLANKS
     C                   SELECT
     C                   WHEN      SCROPT = '5'
      * Display Message
     C                   CALL      'BBSRDMSGR'
     C                   PARM                    wMode
     C                   PARM                    SCRMUI
     C                   PARM                    SCRBRD
     C                   PARM                    SCRSBD
     C                   WHEN      SCROPT = '7'
      * Reply to Message
     C                   EVAL      wRcpnt = SCRBRD
     C     'Re:'         CAT       SCRSBJ:1      wSubject
     C                   CALL      'BBSNEWMSGR'
     C                   PARM                    wMode
     C                   PARM                    wSubject
     C                   PARM                    wRcpnt
     C                   PARM                    SCRSBD
     C                   ENDSL
     C                   EVAL      SCROPT = *BLANKS
     C                   ENDIF
     C                   READC     SF                                     91
     C                   EXSR      ReLoadSFL
     C                   ENDDO
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
      * All records are loaded (load-all SFL) until the end of the file
      *  or SFLSIZ is reached
      **********************************************************************
     C     LoadSFL       BEGSR
      * Open SQL cursor
     C/EXEC SQL
     C+  OPEN C1
     C/END-EXEC
      * Loop until SQL returns no more records
     C                   DOU       SQLCOD = 100
     C/EXEC SQL
     C+  FETCH C1 INTO :dsMessages
     C/END-EXEC
     C                   IF        SQLCOD = 100
     C                   LEAVE
     C                   ENDIF
      * Add data to the SFL and write it on the screen
     C                   EXSR      Data2SFL
     C                   ADD       1             wRRN
     C                   WRITE     SF
      * If we've loaded 9999 records, we cannot add more. Stop loop
     C                   IF        wRRN = 9999
     C                   GOTO      CLOSESQL
     C                   ENDIF
     C                   ENDDO
      * If we loaded at least 1 record, enable SFL
     C                   IF        wRRN > 0
     C                   EVAL      *IN40 = *ON
     C                   ELSE
     C                   EVAL      MSGLIN = cMsgNoRecords
     C                   ENDIF
      * Turn on SFLEND
     C                   EVAL      *IN42 = *ON
     C     CLOSESQL      TAG
      * Close SQL cursor
     C/EXEC SQL
     C+  CLOSE C1
     C/END-EXEC
     C                   ENDSR
      **********************************************************************
      * Put data into a SFL record
      **********************************************************************
     C     Data2SFL      BEGSR
     C                   EVAL      SCRBRD = wMSGBRD
     C                   EVAL      SCRSBD = wMSGSBD
     C                   EVAL      SCRSBJ = wMSGSBJ
     C     *DMY          MOVEL     wMSGDAT       SCRDAT
     C     *HMS          MOVEL     wMSGTIM       SCRTIM
     C                   EVAL      SCRSND = wMSGSND
     C                   EVAL      SCRMUI = wMSGUID
     C                   ENDSR
