     H/TITLE Administration Menu
     H COPYRIGHT('(C) 2020 David Asta under MIT License')
      * SYSTEM      : V4R5
      * PROGRAMMER  : David Asta
      * DATE-WRITTEN: 26/NOV/2020
      *
      * This program shows the Administration Menu to a logged in SysOp
      **********************************************************************
      * INDICATORS USED:
      * 33 - *ON = BBS is in Maintenance Mode
      **********************************************************************
     H/COPY DVBBS400/CURRENTSRC,CBKOPTIMIZ
      **********************************************************************
     FBBSADMMNUDCF   E             WORKSTN
     FPCONFIG   UF   E           K DISK
      **********************************************************************
      * Data structures
     D/COPY DVBBS400/CURRENTSRC,CBKDTAARA
      * Variables
     D wCfgKey         S              6A
     D wMode           S              1A
      **********************************************************************
     C                   WRITE     HEADER
     C                   WRITE     FOOTER
     C                   EXFMT     MNUADM
     C                   EXSR      CheckFkeys
      **********************************************************************
      * Subroutine called automatically at startup
      **********************************************************************
     C     *INZSR        BEGSR
     C                   EVAL      SCRSCR = 'BBSADMMNU'
      * Get values from DTAARA and show them on the screen
     D/COPY DVBBS400/CURRENTSRC,CBKHEADER
     C                   EXSR      UpdMaintOnScr
     C                   ENDSR
      **********************************************************************
      * Check Functions keys pressed by user
      **********************************************************************
     C     CheckFkeys    BEGSR
     C                   SELECT
     C                   WHEN      *IN12 = *ON
      * F12=Go back
     C                   EVAL      *INLR = *ON
     C                   RETURN
     C                   WHEN      *IN13 = *ON
      * F13=General configuration
     C                   CALL      'BBSADGCR'
     C                   WHEN      *IN14 = *ON
      * F14=Boards & Sub-Boards
     C                   EVAL      wMode = 'A'
     C                   CALL      'BBSBRDLR'
     C                   PARM                    wMode
     C                   WHEN      *IN15 = *ON
      * F15=Users Management
     C                   EVAL      wMode = 'M'
     C                   CALL      'BBSLSTUSRR'
     C                   PARM                    wMode
     C                   WHEN      *IN16 = *ON
      * F16=Access Levels
     C                   CALL      'BBSAACCLVR'
     C                   WHEN      *IN17 = *ON
      * F17=Polls
     C                   EVAL      wMode = 'A'
     C                   CALL      'BBSPOLLSLR'
     C                   PARM                    wMode
     C                   WHEN      *IN18 = *ON
      * F18=New User default values
     C                   CALL      'BBSADNUDFR'
     C                   WHEN      *IN19 = *ON
      * F19=Text Files (IFS)
     C                   EVAL      wMode = 'A'
     C                   CALL      'BBSIFSFILR'
     C                   PARM                    wMode
     C                   WHEN      *IN20 = *ON
      * F20=External Programs
     C                   EVAL      wMode = 'A'
     C                   CALL      'BBSETPGMSR'
     C                   PARM                    wMode
     C                   WHEN      *IN24 = *ON
      * F24=Set Maintenance Mode ON/OFF
     C                   EVAL      wCfgKey = 'MAINTM'
     C     wCfgKey       CHAIN     PCONFIG                            41
     C   41'ERROR 41'    dsply
     C   41              GOTO      ENDOFSR
     C   33              EVAL      CNFVAL = 'N'
     C   33              EVAL      wMAINTM = 'N'
     C  N33              EVAL      CNFVAL = 'Y'
     C  N33              EVAL      wMAINTM = 'Y'
     C                   UPDATE    CONFIG                               41
     C  N41              EXSR      UpdMaintOnScr
     C                   ENDSL
     C     ENDOFSR       TAG
     C                   ENDSR
      **********************************************************************
      * Update Maintenance Mode on Screen
      **********************************************************************
     C     UpdMaintOnScr BEGSR
     C                   IF        wMAINTM = 'Y'
     C                   EVAL      *IN33 = *ON
     C                   ELSE
     C                   EVAL      *IN33 = *OFF
     C                   ENDIF
     C                   ENDSR
