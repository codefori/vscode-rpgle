     H/TITLE Add/View External Program
     H COPYRIGHT('(C) 2020 David Asta under MIT License')
      * SYSTEM      : V4R5
      * PROGRAMMER  : David Asta
      * DATE-WRITTEN: 10/DEC/2020
      *
      * This program allows the SysOp to Add a new External Program or Edit
      *   an existing one
      **********************************************************************
     H/COPY DVBBS400/CURRENTSRC,CBKOPTIMIZ
      **********************************************************************
      * INDICATORS USED:
      * 89 - *OFF = Check of newly entered data was OK
      * 90 - CHAIN not found
      * 91 - UPDATE/WRITE error
      **********************************************************************
     FBBSAEPGMMDCF   E             WORKSTN
     FPEXTPGMS  UF A E           K DISK
      **********************************************************************
      * Data structures
     D/COPY DVBBS400/CURRENTSRC,CBKDTAARA
      * Constants
     D cTitleAdd       C                   CONST('BBS400 - Add a new External P-
     D                                     rogram')
     D cTitleEdit      C                   CONST('BBS400 - Edit an External Pro-
     D                                     gram')
     D cAddOK          C                   CONST('New External Program successf-
     D                                     ully added.')
     D cModifOK        C                   CONST('External Program successfully-
     D                                      modified.')
     D cSaveKO         C                   CONST('There was an error when tryin-
     D                                     g to save the data.')
     D cOrdExists      C                   CONST('The entered Menu Order is alr-
     D                                     eady assigned to another External Pr-
     D                                     ogram.')
     D cOptExists      C                   CONST('The entered Menu Option is al-
     D                                     ready assigned to another External P-
     D                                     Program.')
      * Variables
     D pMode           S              1A
     D pOrder          S              2P 0
     D wPrevOrd        S              2P 0
     ***********************************************************************
     C                   WRITE     HEADER
     C                   WRITE     FOOTER
     C                   EXFMT     BODY
     C                   CLEAR                   MSGLIN
     C                   EXSR      CheckFkeys
      **********************************************************************
      * Subroutine called automatically at startup
      **********************************************************************
     C     *INZSR        BEGSR
     C                   EVAL      SCRSCR = 'BBSAEPGMM'
      * Receive parameters
     C     *ENTRY        PLIST
     C                   PARM                    pMode
     C                   PARM                    pOrder
     C                   SELECT
     C                   WHEN      pMode = 'A'
     C                   EVAL      SCRNAM = cTitleAdd
     C                   WHEN      pMode = 'E'
     C                   EVAL      SCRNAM = cTitleEdit
     C                   EXSR      LoadData
     C                   ENDSL
      * Get values from DATAARA and show them on screen
     C/COPY DVBBS400/CURRENTSRC,CBKHEADER
      * Check User Level, to prevent that this program is called directly
      *   by somebody else
     C*    wUserLvl      IFNE      '99'
     C*                  EVAL      *INLR = *ON
     C*                  RETURN
     C*                  ENDIF
     C                   ENDSR
      **********************************************************************
      * Check Function keys pressed by the user
      **********************************************************************
     C     CheckFkeys    BEGSR
      * F10=Save changes
     C                   IF        *IN10 = *ON
     C                   SELECT
     C                   WHEN      pMode = 'A'
      * Add a new External Program
     C                   EXSR      AddEPGM
     C                   WHEN      pMode = 'E'
      * Edit an existing External Program
     C                   EXSR      ModifyEPGM
     C                   ENDSL
     C                   ENDIF
      * F12=Go back
     C                   IF        *IN12 = *ON
     C                   EVAL      *INLR = *ON
     C                   RETURN
     C                   ENDIF
     C                   ENDSR
      **********************************************************************
      * Load data and show it on screen
      **********************************************************************
     C     LoadData      BEGSR
     C     pOrder        CHAIN     PEXTPGMS
     C                   IF        %FOUND
     C                   EVAL      SCRPNM = EXTPGM
     C                   EVAL      SCROBJ = EXTOBJ
     C                   EVAL      SCRLIB = EXTLIB
     C                   EVAL      SCRORD = EXTORD
     C                   EVAL      SCRALV = EXTALV
     C                   IF        pMode  = 'E'
     C                   EVAL      wPrevOrd = SCRORD
     C                   ENDIF
     C                   ELSE
     C     'LOAD ERROR'  DSPLY
     C                   ENDIF
     C                   ENDSR
      **********************************************************************
      * Add a new External Program
      **********************************************************************
     C     AddEPGM       BEGSR
     C     SCRORD        CHAIN     PEXTPGMS                           81
     C  N81              EVAL      MSGLIN = cOrdExists
     C  N81              GOTO      ENDADD
     C                   EVAL      EXTORD = SCRORD
     C                   EVAL      EXTPGM = SCRPNM
     C                   EVAL      EXTOBJ = SCROBJ
     C                   EVAL      EXTLIB = SCRLIB
     C                   EVAL      EXTALV = SCRALV
     C                   WRITE     RETPGM                               91
     C   91              EVAL      MSGLIN = cSaveKO
     C  N91              EVAL      MSGLIN = cAddOK
     C     ENDADD        TAG
     C                   ENDSR
      **********************************************************************
      * Edit an existing External Program
      **********************************************************************
     C     ModifyEPGM    BEGSR
     C     SCRORD        CHAIN     PEXTPGMS                           81
     C                   IF        SCRORD = wPrevOrd
     C                   EVAL      *IN81 = *ON
     C                   ENDIF
     C  N81              EVAL      MSGLIN = cOrdExists
     C  N81              GOTO      ENDEDIT
     C                   EVAL      EXTORD = SCRORD
     C                   EVAL      EXTPGM = SCRPNM
     C                   EVAL      EXTOBJ = SCROBJ
     C                   EVAL      EXTLIB = SCRLIB
     C                   EVAL      EXTALV = SCRALV
     C                   UPDATE    RETPGM                               91
     C   91              EVAL      MSGLIN = cSaveKO
     C  N91              EVAL      MSGLIN = cModifOK
     C     ENDEDIT       TAG
     C                   ENDSR
