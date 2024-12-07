     H/TITLE Polls List (SFL)
     H COPYRIGHT('(C) 2020 David Asta under MIT License')
      * SYSTEM      : V4R5
      * PROGRAMMER  : David Asta
      * DATE-WRITTEN: 07/DEC/2020
      *
      * This program shows all polls available.
      **********************************************************************
     H/COPY DVBBS400/CURRENTSRC,CBKOPTIMIZ
      **********************************************************************
      * INDICATORS USED:
      * 25 - Roll key
      * 32 - Polls List (All)
      * 33 - Polls List (for Administrators)
      * 40 - SFLDSP
      * 41 - SFLCLR
      * 42 - SFLEND(*MORE)
      * 50 - Include/Exclude Deleted
      * 92 - NOT FOUND for CHAIN
      **********************************************************************
     FBBSPOLLSLDCF   E             WORKSTN
     F                                     SFILE(SF:wRRN)
     FPPOLLSQ   UF   E           K DISK
     FPPOLLSA   IF   E           K DISK
      **********************************************************************
      * Data structures
     D/COPY DVBBS400/CURRENTSRC,CBKDTAARA
      * Constants
     D cKeysAdmin1     C                   CONST('F5=Refresh   F6=Create   F12=-
     D                                     Go back   F17=Include Deleted/Closed-
     D                                     ')
     D cKeysAdmin2     C                   CONST('F5=Refresh   F6=Create   F12=-
     D                                     Go back   F17=Exclude Deleted/Closed-
     D                                     ')
     D cKeysUser1      C                   CONST('F5=Refresh   F12=Go back   F1-
     D                                     7=Include Deleted/Closed')
     D cKeysUser2      C                   CONST('F5=Refresh   F12=Go back   F1-
     D                                     7=Exclude Deleted/Closed')
     D cErrOptNoAdmin  C                   CONST('Value entered for field is no-
     D                                     t valid. Valid values listed in mess-
     D                                     age help.')
     D cErrNotOpen     C                   CONST('The selected poll is not open-
     D                                     .')
      * Variables
     D/COPY DVBBS400/CURRENTSRC,CBKUSEWINS
     D pMode           S              1A
     D wMode           S              1A
     D wRRN            S              4P 0
     D wBlanks         S              8A   INZ(*BLANKS)
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
     C                   EVAL      SCRSCR = 'BBSPOLLSL'
     C                   SETOFF                                       3350
      * Receive parameters
     C     *ENTRY        PLIST
     C                   PARM                    pMode
     C                   IF        pMode = 'A'
     C                   EVAL      KEYLST = cKeysAdmin1
     C                   EVAL      *IN33 = *ON
     C                   ELSE
     C                   EVAL      KEYLST = cKeysUser1
     C                   ENDIF
      * Declare composite keys
     C     KPOLLSA       KLIST
     C                   KFLD                    POLUID
     C                   KFLD                    wUser
      * Get values from DATAARA and show them on screen
     C/COPY DVBBS400/CURRENTSRC,CBKHEADER
      * Initialise variables and indicators, and load subfile
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
      * F6=Create
     C                   IF        *IN06 = *ON
     C                   EVAL      wMode = 'C'
     C                   CALL      'BBSPOLLR'
     C                   PARM                    wMode
     C                   PARM                    wBlanks
     C                   ENDIF
      * F12=Go back
     C                   IF        *IN12 = *ON
     C                   EVAL      *INLR = *ON
     C                   RETURN
     C                   ENDIF
      * F17=Include/Exclude Closed/Deleted Polls
     C                   IF        *IN17 = *ON
     C                   IF        *IN50 = *ON
     C                   EVAL      *IN50 = *OFF
     C                   IF        pMode = 'A'
     C                   EVAL      KEYLST = cKeysAdmin1
     C                   ELSE
     C                   EVAL      KEYLST = cKeysUser1
     C                   ENDIF
     C                   ELSE
     C                   EVAL      *IN50 = *ON
     C                   IF        pMode = 'A'
     C                   EVAL      KEYLST = cKeysAdmin2
     C                   ELSE
     C                   EVAL      KEYLST = cKeysUser2
     C                   ENDIF
     C                   ENDIF
     C                   EXSR      ReLoadSFL
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
      * Edit (only from Administration
     C                   IF        pMode = 'A'
     C                   EVAL      wMode = 'E'
     C                   CALL      'BBSPOLLR'
     C                   PARM                    wMode
     C                   PARM                    SCRPUI
     C                   ELSE
     C                   EVAL      MSGLIN = cErrOptNoAdmin
     C                   ENDIF
     C                   WHEN      SCROPT = '3'
      * Close (only from Administration
     C                   IF        pMode = 'A'
      *         Ask for confirmation before closing it
     C                   EVAL      wConfirmQ = 'Close poll?'
     C                   EVAL      wConfirmA = 'N'
     C                   EVAL      wConfirmF3 = *OFF
     C                   CALL      'BBSWINYNR'
     C                   PARM                    wConfirmQ
     C                   PARM                    wConfirmA
     C                   PARM                    wConfirmF3
     C                   IF        wConfirmF3 = *OFF AND wConfirmA = 'Y'
     C     SCRPUI        CHAIN     PPOLLSQ                            92
     C  N92              EVAL      POLSTA = 'C'
     C  N92              UPDATE    RPOLL
     C                   ENDIF
     C                   ELSE
     C                   EVAL      MSGLIN = cErrOptNoAdmin
     C                   ENDIF
     C                   WHEN      SCROPT = '4'
      * Delete (only from Administration
     C                   IF        pMode = 'A'
      *         Ask for confirmation before deleting it
     C                   EVAL      wConfirmQ = 'Delete poll?'
     C                   EVAL      wConfirmA = 'N'
     C                   EVAL      wConfirmF3 = *OFF
     C                   CALL      'BBSWINYNR'
     C                   PARM                    wConfirmQ
     C                   PARM                    wConfirmA
     C                   PARM                    wConfirmF3
     C                   IF        wConfirmF3 = *OFF AND wConfirmA = 'Y'
     C     SCRPUI        CHAIN     PPOLLSQ                            92
     C  N92              EVAL      POLSTA = 'D'
     C  N92              UPDATE    RPOLL
     C                   ENDIF
     C                   ELSE
     C                   EVAL      MSGLIN = cErrOptNoAdmin
     C                   ENDIF
     C                   WHEN      SCROPT = '5'
      * Display
     C                   EVAL      wMode = 'W'
     C                   CALL      'BBSPOLLR'
     C                   PARM                    wMode
     C                   PARM                    SCRPUI
     C                   WHEN      SCROPT = '6'
      * Vote
     C                   IF        SCRSTA = 'O'
     C                   EVAL      wMode = 'V'
     C                   CALL      'BBSPOLLR'
     C                   PARM                    wMode
     C                   PARM                    SCRPUI
     C                   ELSE
     C                   EVAL      MSGLIN = cErrNotOPen
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
      * Load all polls from PPOLLSQ
      * All records are loaded (load-all SFL) until the end of the file or
      *  until SFLSIZ is reached
      **********************************************************************
     C     LoadSFL       BEGSR
     C     *START        SETLL     PPOLLSQ
     C                   DOU       %EOF
     C                   READ      PPOLLSQ
     C                   IF        %EOF
     C                   LEAVE
     C                   ENDIF
      * Put data on screen
     C                   IF        POLSTA = 'O'
     C                   EXSR      Data2SFL
     C                   ADD       1             wRRN
     C                   WRITE     SF
     C                   ELSE
     C                   IF        *IN50 = *ON
     C                   EXSR      Data2SFL
     C                   ADD       1             wRRN
     C                   WRITE     SF
     C                   ENDIF
     C                   ENDIF
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
      * Also gets number of votes and the curent user's vote (if any)
      **********************************************************************
     C     Data2SFL      BEGSR
     C                   EVAL      SCRPUI = POLUID
     C                   EVAL      SCRQUE = POLQUE
     C     *DMY          MOVEL     POLEND        SCREND
      * Status
     C                   EVAL      SCRSTA = POLSTA
     C                   SELECT
     C                   WHEN      POLSTA = 'O'
     C                   EVAL      SCRSTS = 'Open  '
     C                   WHEN      POLSTA = 'C'
     C                   EVAL      SCRSTS = 'Closed'
     C                   WHEN      POLSTA = 'D'
     C                   EVAL      SCRSTS = 'Deletd'
     C                   ENDSL
      * Get Number of votes and current user's vote
     C                   EXSR      GetVotes
     C                   ENDSR
      **********************************************************************
      * Get Number of votes
      **********************************************************************
     C     GetVotes      BEGSR
     C                   EVAL      SCRVOT = 0
     C                   EVAL      SCRMYV = *BLANKS
     C     POLUID        SETLL     PPOLLSA
     C     POLUID        READE     PPOLLSA
     C                   DOW       NOT %EOF
     C                   IF        ANSUSR = wUser
     C                   EVAL      SCRMYV = 'Yes'
     C                   ENDIF
     C                   ADD       1             SCRVOT
     C     POLUID        READE     PPOLLSA
     C                   ENDDO
     C                   ENDSR
