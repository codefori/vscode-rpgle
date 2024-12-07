     H/TITLE External Programs List (SFL)
     H COPYRIGHT('(C) 2020 David Asta under MIT License')
      * SYSTEM      : V4R5
      * PROGRAMMER  : David Asta
      * DATE-WRITTEN: 21/NOV/2020
      *
      * This program shows the list of the configured External Programs,
      * Allows a user to run any of the programs
      * Allows the SysOp to add/edit/delete
      **********************************************************************
     H/COPY DVBBS400/CURRENTSRC,CBKOPTIMIZ
      **********************************************************************
      * INDICATORS USED:
      * 25 - Roll key
      * 40 - SFLDSP
      * 41 - SFLCLR
      * 42 - SFLEND(*MORE)
      * 80 - *ON = Administration mode
      **********************************************************************
     FBBSETPGMSDCF   E             WORKSTN
     F                                     SFILE(SF:wRRN)
     FPEXTPGMS  UF A E           K DISK
      **********************************************************************
      * Data structures
     D/COPY DVBBS400/CURRENTSRC,CBKDTAARA
      * Constants
     D cNone           C                   CONST('There are no External Program-
     D                                     s defined yet.')
     D cErrDupli       C                   CONST('That Menu Option have been ad-
     D                                     ded already.')
     D cErrOptNoAdmin  C                   CONST('Value entered for field is no-
     D                                     t valid. Valid values listed in mess-
     D                                     age help.')
      * Variables
     D/COPY DVBBS400/CURRENTSRC,CBKUSEWINS
     D pMode           S              1A
     D wRRN            S              4P 0
     D wMenuOptD       S              2P 0
     D wPrevMenuOpt    S              2P 0
     D wMode           S              1A
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
     C                   EVAL      *IN80 = *ON
     C                   ELSE
     C                   EVAL      *IN80 = *OFF
     C                   ENDIF
      * Get values from DATAARA and show them on screen
     C/COPY DVBBS400/CURRENTSRC,CBKHEADER
      * Initialise variables and load subfile
     C                   MOVEL     wUserLvl      wUserLvlD
     C                   EVAL      SCRSCR = 'BBSETPGMSR'
     C                   Z-ADD     0             wRRN
     C                   EXSR      LoadSFL
     C                   ENDSR
      **********************************************************************
      * Check Function keys pressed by the user
      **********************************************************************
     C     ChkFkeys      BEGSR
      * F6=Add
     C                   IF        *IN06 = *ON
     C                   EVAL      wMode = 'A'
     C                   CALL      'BBSAEPGMMR'
     C                   PARM                    wMode
     C                   PARM                    SCRORD
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
      * Edit External Program (only from Administration)
     C                   IF        pMode = 'A'
     C                   EVAL      wMode = 'E'
     C                   CALL      'BBSAEPGMMR'
     C                   PARM                    wMode
     C                   PARM                    SCRORD
     C                   EXSR      ReLoadSFL
     C                   ELSE
     C                   EVAL      MSGLIN = cErrOptNoAdmin
     C                   ENDIF
     C                   WHEN      SCROPT = '4'
      * Delete External Program (only from Administration)
     C                   IF        pMode = 'A'
      *         Ask for confirmation before deleting
     C     'Delete'      CAT       SCRNAM:1      wConfirmQ
     C                   EVAL      wConfirmA = 'N'
     C                   EVAL      wConfirmF3 = *OFF
     C                   CALL      'BBSWINYNR'
     C                   PARM                    wConfirmQ
     C                   PARM                    wConfirmA
     C                   PARM                    wConfirmF3
     C                   IF        wConfirmF3 = *OFF AND wConfirmA = 'Y'
     C     SCRORD        CHAIN     PEXTPGMS
     C                   IF        %FOUND
     C                   DELETE    RETPGM
     C                   ENDIF
     C                   ENDIF
     C                   EXSR      ReLoadSFL
     C                   ELSE
     C                   EVAL      MSGLIN = cErrOptNoAdmin
     C                   ENDIF
     C                   WHEN      SCROPT = '5'
      * Run External Program
     C                   EVAL      wMode = 'C'
     C                   CALL      'BBSHELPERC'
     C                   PARM                    wMode
     C                   PARM                    SCROBJ
     C                   PARM                    SCRLIB
     C                   PARM                    wUser
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
      * Load all records from PEXTPGMS
      * All records are loaded (load-all SFL) until the end of the file or
      *  until SFLSIZ is reached
      **********************************************************************
     C     LoadSFL       BEGSR
     C     *START        SETLL     PEXTPGMS
     C                   DOU       %EOF
     C                   READ      PEXTPGMS
     C                   IF        %EOF
     C                   LEAVE
     C                   ENDIF
      * Add to SFL only if User's Access Level is high enough
     C                   IF        wUserLvlD >= EXTALV
     C                   EXSR      Data2SFL
     C                   ADD       1             wRRN
     C                   WRITE     SF
     C                   ENDIF
      * If we have loaded 9999 records, we cannot add more. Stop loop
     C                   IF        wRRN = 9999
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
      * If we loaded at least 1 record, enable SFL
     C                   IF        wRRN > 0
     C                   EVAL      *IN40  = *ON
     C                   ELSE
     C                   EVAL      MSGLIN = cNone
     C                   ENDIF
     C                   EVAL      *IN42  = *ON
     C                   ENDSR
      **********************************************************************
      * Put data into a SFL record
      **********************************************************************
     C     Data2SFL      BEGSR
     C                   EVAL      SCRNAM = EXTPGM
     C                   EVAL      SCROBJ = EXTOBJ
     C                   EVAL      SCRLIB = EXTLIB
     C                   IF        pMode = 'A'
     C                   EVAL      SCRALV = EXTALV
     C                   EVAL      SCRORD = EXTORD
     C                   EVAL      SCRPGO = EXTOBJ
     C                   EVAL      SCRPGL = EXTLIB
     C                   ELSE
     C                   EVAL      SCRALV = 0
     C                   EVAL      SCRORD = 0
     C                   EVAL      SCRPGO = *BLANKS
     C                   EVAL      SCRPGL = *BLANKS
     C                   ENDIF
     C                   ENDSR
