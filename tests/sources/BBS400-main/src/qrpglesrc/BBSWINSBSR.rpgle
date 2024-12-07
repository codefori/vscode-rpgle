     H/TITLE Window - Select Board/Sub-Board
     H COPYRIGHT('(C) 2020 David Asta under MIT License')
      * SYSTEM      : V4R5
      * PROGRAMMER  : David Asta
      * DATE-WRITTEN: 16/NOV/2020
      *
      * This program shows a window in which a user can select first a
      *   Board and then a Sub-Board
      **********************************************************************
      * INDICATORS USED:
      * 30 - SFLDSP
      * 32 - SFLEND(*MORE)
      * 40 - *ON if pMode = 'B', *OFF if pMode = 'S'
      **********************************************************************
     H/COPY DVBBS400/CURRENTSRC,CBKOPTIMIZ
      **********************************************************************
     FBBSWINSBSWCF   E             WORKSTN
     F                                     SFILE(SF:wRRN)
     FPBOARDS   IF   E           K DISK
     FPSBORDS   IF   E           K DISK
      **********************************************************************
      * Data structures
     D/COPY DVBBS400/CURRENTSRC,CBKDTAARA
      * Variables
     D pMode           S              1A
     D pBoardUID       S              8A
     D pSBoardUID      S              8A
     D wRRN            S              4P 0
      **********************************************************************
     C                   WRITE     WINDOW
     C                   EXFMT     SFCTL
      * If F12 was pressed, exit this program
     C                   IF        *IN12 = *ON
     C                   EVAL      *INLR = *ON
     C                   RETURN
     C                   ENDIF
      * If REC is greater than 0, it means user has selected a record.
      * CHAIN to the subfile to retrieve the Board's UID, and then return
      *  back to the calling program.
     C                   IF        REC > 0
     C     REC           CHAIN     SF
     C                   EVAL      pSBoardUID = SCRSHT
     C                   EVAL      *INLR = *ON
     C                   RETURN
     C                   ENDIF
      **********************************************************************
      * Subroutine called automatically at startup
      **********************************************************************
     C     *INZSR        BEGSR
      * Receive parameters
     C     *ENTRY        PLIST
     C                   PARM                    pMode
     C                   PARM                    pBoardUID
     C                   PARM                    SL
     C                   PARM                    SP
     C                   PARM                    pSBoardUID
      * Do we display Boards or Sub-Boards selection?
     C                   IF        pMode = 'B'
     C                   EVAL      *IN40 = *ON
     C                   ELSE
     C                   EVAL      *IN40 = *OFF
     C                   ENDIF
      * Initialise RRN
     C                   Z-ADD     0             wRRN
      * Load the subfile
     C   40              EXSR      LoadSFLBRD
     C  N40              EXSR      LoadSFLSBRD
      * If we have loaded at least 1 record into the SFL, turn on SFLDSP
     C                   IF        wRRN > 0
     C                   EVAL      *IN30 = *ON
     C                   ENDIF
      * Allow SFLEND(*MORE) to be displayed
     C                   EVAL      *IN32 = *ON
     C                   ENDSR
      **********************************************************************
      * Load data from PBOARDS into the subfile
      **********************************************************************
     C     LoadSFLBRD    BEGSR
     C     *START        SETLL     PBOARDS
     C                   DOU       %EOF
     C                   READ      PBOARDS
      * If EOF or we already loaded 9999 records, exit this loop
     C                   IF        %EOF OR wRRN = 9999
     C                   LEAVE
     C                   ENDIF
     C                   EVAL      SCRSHT = BRDSHT
     C                   EVAL      SCRLNG = BRDLNG
     C                   ADD       1             wRRN
     C                   WRITE     SF
     C                   ENDDO
     C                   ENDSR
      **********************************************************************
      * Load data from PSBORDS into the subfile
      **********************************************************************
     C     LoadSFLSBRD   BEGSR
     C     pBoardUID     SETLL     PSBORDS
     C                   DOU       %EOF
     C     pBoardUID     READE     PSBORDS
      * If EOF or we already loaded 9999 records, exit this loop
     C                   IF        %EOF OR wRRN = 9999
     C                   LEAVE
     C                   ENDIF
     C                   EVAL      SCRSHT = SBRSHT
     C                   EVAL      SCRLNG = SBRLNG
     C                   ADD       1             wRRN
     C                   WRITE     SF
     C                   ENDDO
     C                   ENDSR
