     H/TITLE List User-to-User Messages (SFL)
     H COPYRIGHT('(C) 2020 David Asta under MIT License')
      * SYSTEM      : V4R5
      * PROGRAMMER  : David Asta
      * DATE-WRITTEN: 17/NOV/2020
      *
      * This program shows all messages sent to current user, from other
      *   users (i.e. User-to-User messages)
      **********************************************************************
     H/COPY DVBBS400/CURRENTSRC,CBKOPTIMIZ
      **********************************************************************
      * INDICATORS USED:
      * 25 - Roll key
      * 40 - SFLDSP
      * 41 - SFLCLR
      * 42 - SFLEND(*MORE)
      * 50 - Include/Exclude Deleted
      * 91 - SFL EOF
      * 92 - CHAIN Not Found
      **********************************************************************
     FBBSRDU2UMDCF   E             WORKSTN
     F                                     SFILE(SF:wRRN)
     FLUUMSGSRCPIF   E           K DISK    RENAME(RUUMSG:LUUMSG)
     FPUUMSGS   UF   E           K DISK
      **********************************************************************
      * Data structures
     D/COPY DVBBS400/CURRENTSRC,CBKDTAARA
      * Constants
     D wKeyInclDltd    C                   CONST('F5=Refresh   F12=Go back   F1-
     D                                     7=Include Deleted')
     D wKeyExclDltd    C                   CONST('F5=Refresh   F12=Go back   F1-
     D                                     7=Exclude Deleted')
      * Variables
     D/COPY DVBBS400/CURRENTSRC,CBKUSEWINS
     D wRRN            S              4P 0
     D wMode           S              1A
     D wSubject        S             45A
     D wBlanks         S              8A   INZ(*BLANKS)
     ***********************************************************************
     C   50              EVAL      KEYLST = wKeyExclDltd
     C  N50              EVAL      KEYLST = wKeyInclDltd
     C                   WRITE     FOOTER
     C                   EXFMT     SFLCTL
     C                   CLEAR                   MSGLIN
     C                   EXSR      ChkFkeys
     C   40              EXSR      ChkOptions
      **********************************************************************
      * Subroutine called automatically at startup
      **********************************************************************
     C     *INZSR        BEGSR
     C                   EVAL      *IN50 = *OFF
     C                   EVAL      SCRSCR = 'BBSRDU2UM'
      * Get values from DATAARA and show them on screen
     C/COPY DVBBS400/CURRENTSRC,CBKHEADER
      * Initialise variables and load subfile
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
      * F17=Include/Exclude Deleted Messages
     C                   IF        *IN17 = *ON
     C                   IF        *IN50 = *ON
     C                   EVAL      *IN50 = *OFF
     C                   ELSE
     C                   EVAL      *IN50 = *ON
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
     C     SCROPT        IFNE      *BLANKS
     C                   SELECT
     C                   WHEN      SCROPT = '3'
      * Reply to the Message
     C                   EVAL      wMode = 'R'
     C                   EVAL      wSubject = SCRSBJ
     C                   CALL      'BBSNEWMSGR'
     C                   PARM                    wMode
     C                   PARM                    wSubject
     C                   PARM                    SCRSND
     C                   PARM                    wBlanks
     C                   WHEN      SCROPT = '4'
      * Delete Message
      *         Ask for confirmation before deleting
     C                   EVAL      wConfirmQ = 'Delete Message?'
     C                   EVAL      wConfirmA = 'N'
     C                   EVAL      wConfirmF3 = *OFF
     C                   CALL      'BBSWINYNR'
     C                   PARM                    wConfirmQ
     C                   PARM                    wConfirmA
     C                   PARM                    wConfirmF3
     C                   IF        wConfirmF3 = *OFF AND wConfirmA = 'Y'
     C     SCRMUI        CHAIN     PUUMSGS                            92
     C  N92              EVAL      UUMSTA = 'D'
     C  N92              UPDATE    RUUMSG
     C   92'ERROR 92'    DSPLY
     C                   ENDIF
     C                   WHEN      SCROPT = '5'
      * Display Message
     C                   EVAL      wMode = 'U'
     C                   CALL      'BBSRDMSGR'
     C                   PARM                    wMode
     C                   PARM                    SCRMUI
     C                   PARM                    wBlanks
     C                   PARM                    wBlanks
     C                   WHEN      SCROPT = '7'
      * Mark Read/Unread
     C                   EXSR      MarkReUnre
     C                   ENDSL
     C                   ENDIF
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
      * Load all messages from PUUMSGS for current User as Recipient
      * All records are loaded (load-all SFL) until the end of the file or
      *  until SFLSIZ is reached
      **********************************************************************
     C     LoadSFL       BEGSR
     C     wUser         SETLL     LUUMSGSRCP
     C                   DOU       %EOF
     C     wUser         READE     LUUMSGSRCP
     C                   IF        %EOF
     C                   LEAVE
     C                   ENDIF
      * Put data on screen
      *   Check if user wants to Include or Exclude deleted messages
     C                   IF        UUMSTA = 'D'
     C                   IF        *IN50 = *ON
     C                   EXSR      Data2SFL
     C                   ADD       1             wRRN
     C                   WRITE     SF
     C                   ENDIF
     C                   ELSE
     C                   EXSR      Data2SFL
     C                   ADD       1             wRRN
     C                   WRITE     SF
     C                   ENDIF
     C*                  ADD       1             wRRN
     C*                  WRITE     SF
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
      * Put data from files to screen
      **********************************************************************
     C     Data2SFL      BEGSR
     C                   EVAL      SCRSBJ = UUMSBJ
     C                   EVAL      SCRSND = UUMSND
     C     *DMY          MOVEL     UUMDAT        SCRDAT
     C                   EVAL      SCRTIM = UUMTIM
     C                   EVAL      SCRSTA = UUMSTA
     C                   EVAL      SCRMUI = UUMUID
      * Set colours depending of Read/Unread/Delete
     C                   SETOFF                                       4546
     C                   IF        UUMSTA = 'U'
     C                   EVAL      *IN45 = *ON
     C                   ENDIF
     C                   IF        UUMSTA = 'D'
     C                   EVAL      *IN46 = *ON
     C                   ENDIF
     C                   ENDSR
      **********************************************************************
      * If a message is Read, mark it as Unread, and vice versa
      **********************************************************************
     C     MarkReUnre    BEGSR
     C     SCRMUI        CHAIN     PUUMSGS                            92
     C   92              GOTO      ERROR92
     C                   SELECT
     C                   WHEN      SCRSTA = 'R'
     C                   EVAL      UUMSTA = 'U'
     C                   WHEN      SCRSTA = 'U'
     C                   EVAL      UUMSTA = 'R'
     C                   ENDSL
     C                   UPDATE    RUUMSG
     C     ERROR92       TAG
     C   92'ERROR 92'    dsply
     C                   ENDSR
