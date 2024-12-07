     H/TITLE Read Message
     H COPYRIGHT('(C) 2020 David Asta under MIT License')
      * SYSTEM      : V4R5
      * PROGRAMMER  : David Asta
      * DATE-WRITTEN: 17/NOV/2020
      *
      * This program allows an user to read a message
      * If the message is a User-to-User, by opening the message it's marked
      *   as read
      **********************************************************************
      * INDICATORS USED:
      * 30 - *ON when it's a Post. *OFF when it's a User Message
      **********************************************************************
      * Compiler flags
     H ALWNULL(*USRCTL)
     H/COPY DVBBS400/CURRENTSRC,CBKOPTIMIZ
      **********************************************************************
     FBBSRDMSGD CF   E             WORKSTN
     FPUUMSGS   UF   E           K DISK
     FLMESSGSUIDIF   E           K DISK
      **********************************************************************
      * Data structures
     D/COPY DVBBS400/CURRENTSRC,CBKDTAARA
     D dsMsgText       DS
     D  SCRM01
     D  SCRM02
     D  SCRM03
     D  SCRM04
     D  SCRM05
     D  SCRM06
     D  SCRM07
     D  SCRM08
     D  SCRM09
     D  SCRM10
     D  SCRM11
     D  SCRM12
     D  SCRM13
     D  SCRM14
     D  SCRM15
      * Variables
     D pMode           S              1A
     D pMsgUID         S             32A
     D pBoardUID       S              8A
     D pSBoardUID      S              8A
     D wUsrUID         S             10A
     D wRcpnt          S             10A
     D wSubject        S             45A
      **********************************************************************
     C                   WRITE     HEADER
     C                   WRITE     FOOTER
     C                   EXFMT     BODY
     C                   EXSR      ChkFkeys
      **********************************************************************
      * Subroutine called automatically at startup
      **********************************************************************
     C     *INZSR        BEGSR
     C                   EVAL      SCRSCR = 'BBSRDMSG'
      * Get Mode by parameter
      *  pMode = 'B' -> Read message from Sub-Board
      *  pMode = 'U' -> Read message from User-to-User
     C     *ENTRY        PLIST
     C                   PARM                    pMode
     C                   PARM                    pMsgUID
     C                   PARM                    pBoardUID
     C                   PARM                    pSBoardUID
      * and call corresponding subroutine
     C                   SELECT
     C                   WHEN      pMode = 'B'
     C                   EVAL      SCRBRD = pBoardUID
     C                   EVAL      SCRSBD = pSBoardUID
     C                   EVAL      *IN30  = *ON
     C                   EXSR      ReadSBrMsg
     C                   WHEN      pMode = 'U'
     C                   EVAL      *IN30  = *OFF
     C                   EXSR      ReadU2UMsg
     C                   ENDSL
      * Get values from DTAARA and show them on the screen
     C/COPY DVBBS400/CURRENTSRC,CBKHEADER
     C                   ENDSR
      **********************************************************************
      * Check Functions keys pressed by user
      **********************************************************************
     C     ChkFkeys      BEGSR
      * F6=Reply to Post
     C                   IF        *IN06 = *ON
     C                   EVAL      pMode = 'B'
     C     'Re:'         CAT       SCRSBJ:1      wSubject
     C                   EVAL      wRcpnt = SCRBRD
     C                   CALL      'BBSNEWMSGR'
     C                   PARM                    pMode
     C                   PARM                    wSubject
     C                   PARM                    wRcpnt
     C                   PARM                    SCRSBD
     C                   ENDIF
      * F7=Reply to User
     C                   IF        *IN07 = *ON
     C                   EVAL      pMode = 'R'
     C                   CALL      'BBSNEWMSGR'
     C                   PARM                    pMode
     C                   PARM                    SCRSBJ
     C                   PARM                    SCRSND
     C                   ENDIF
      * F12=Go back
     C                   IF        *IN12 = *ON
     C                   EVAL      *INLR = *ON
     C                   RETURN
     C                   ENDIF
     C                   ENDSR
      **********************************************************************
      * Read a message from PUUMSGS
      **********************************************************************
     C     ReadU2UMsg    BEGSR
     C     pMsgUID       CHAIN     PUUMSGS
     C                   IF        %FOUND
      * Put data on screen
     C     *DMY          MOVEL     UUMDAT        SCRDAT
     C                   EVAL      SCRTIM = UUMTIM
     C                   EVAL      SCRSND = UUMSND
     C                   EVAL      SCRSBJ = UUMSBJ
     C                   EVAL      dsMsgText = UUMTXT
     C                   EVAL      SCRMUI = UUMUID
      * Mark it as Read
     C                   EVAL      UUMSTA = 'R'
     C                   UPDATE    RUUMSG
     C                   ELSE
     C     'ERROR 40'    dsply
     C                   ENDIF
     C                   ENDSR
      **********************************************************************
      * Read a message from PMESSGS
      **********************************************************************
     C     ReadSBrMsg    BEGSR
     C     pMsgUID       CHAIN     LMESSGSUID
     C                   IF        %FOUND
      * Put data on screen
     C     *DMY          MOVEL     MSGDAT        SCRDAT
     C                   EVAL      SCRTIM = MSGTIM
     C                   EVAL      SCRSND = MSGSND
     C                   EVAL      SCRSBJ = MSGSBJ
     C                   EVAL      dsMsgText = MSGTXT
     C                   EVAL      SCRMUI = MSGUID
     C                   ELSE
     C     'ERROR 42'    dsply
     C                   ENDIF
     C                   ENDSR
