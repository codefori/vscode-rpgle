     H/TITLE Post a message to a Sub-Board or send a message to a User/SysOp
     H COPYRIGHT('(C) 2020 David Asta under MIT License')
      * SYSTEM      : V4R5
      * PROGRAMMER  : David Asta
      * DATE-WRITTEN: 13/NOV/2020
      *
      * This program allows an user to post a message to a specific
      *   Sub-Board or to send a message to another user or to the SysOp
      **********************************************************************
      * INDICATORS USED:
      * 30 - Post to Sub-Board
      * 31 - Message to User
      * 32 - Message to SysOp
      * 33 - Enable F4 and F6. Disable it once message is sent
      * 34 - *ON if all parts of message where validated OK
      * 35 - *ON if recipient User not found in DB
      * 51 - WRITE error
      **********************************************************************
      * Compiler flags
     H ALWNULL(*USRCTL)
     H/COPY DVBBS400/CURRENTSRC,CBKOPTIMIZ
      **********************************************************************
     FBBSNEWMSGDCF   E             WORKSTN
     F                                     INFDS(dsINFDS)
     FPBOARDS   IF   E           K DISK
     FPSBORDS   IF   E           K DISK
     FPUSERS    IF   E           K DISK
     FPUUMSGS   UF A E             DISK
     FPMESSGS   UF A E             DISK
      **********************************************************************
      * Constants
     D cErrLvlLowP     C                   CONST('Your Access Level is too low -
     D                                     for posting messages in this Sub-Boa-
     D                                     rd.')
     D cErrNoBrdSel    C                   CONST('Please select a Board first')
     D cErrValidKO     C                   CONST('Something is not right. Pleas-
     D                                     e check data on screen.')
     D cErrSndMsg      C                   CONST('ERROR: Message could not be s-
     D                                     ent')
     D cErrRcpNotFnd   C                   CONST('The user recipient of the mes-
     D                                     sage could not be found as a registe-
     D                                     red user.')
     D cMsgSent        C                   CONST('Message successfully sent.')
     D cKeysBeforeBrd  C                   CONST('F4=Prompt   F6=Send   F12=Can-
     D                                     cel')
     D cKeysBeforeU2U  C                   CONST('F6=Send   F12=Cancel')
     D cKeysAfter      C                   CONST('F12=Go back')
      * Data structures
     D/COPY DVBBS400/CURRENTSRC,CBKDTAARA
     D dsTodayNowC     DS
     D  wTodNowTime                   6A
     D  wTodNowDate                   6A
     D dsINFDS         DS
     D  wLinPos              370    371
     D                 DS
     D  wLin                   1      2B 0
     D  wL                     2      2
     D                 DS
     D  wPos                   1      2B 0
     D  wP                     2      2
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
     D/COPY DVBBS400/CURRENTSRC,CBKUSEWINS
     D pMode           S              1A
     D pSubject        S             45A
     D pRcpnt          S             10A
     D pSBoard         S              8A
     D wGetUIDfor      S              1A
     D wBoardUID       S              8A   INZ(*BLANKS)
     D wSBoardUID      S              8A   INZ(*BLANKS)
     D wSL             S              3P 0
     D wSP             S              3P 0
     D wMsgUID         S             16A
     D wTodayNowD      S             12P 0
     D wTodNowTimeD    S              6P 0
     D wTodNowDateD    S              6P 0
     D wMsgUID17       S             17A
     D wMsgUIDhash     S             32A
     D wHashMode       S              1A   INZ('M')
     D wHlpMode        S              1A   INZ('N')
     D wHlpFiller1     S             10A   INZ(*BLANKS)
     D wHlpFiller2     S             10A   INZ(*BLANKS)
     D wHlpUser        S             10A
     D wUserLvlD       S              2P 0
      **********************************************************************
     C                   WRITE     HEADER
     C                   WRITE     FOOTER
     C                   EXFMT     BODY
     C                   EXSR      CheckFkeys
      **********************************************************************
      * Subroutine called automatically at startup
      **********************************************************************
     C     *INZSR        BEGSR
     C                   EVAL      SCRSCR = 'BBSNEWMSG'
     C                   MOVEL     wUserLvl      wUserLvlD
      * Initialise indicators for pMode
     C                   SETOFF                                       293031
     C                   SETOFF                                       323551
     C                   SETON                                        33
      * Declare Composite Keys
     C     KSBORDS       KLIST
     C                   KFLD                    wBoardUID
     C                   KFLD                    wSBoardUID
      * Get parameters
     C     *ENTRY        PLIST
     C                   PARM                    pMode
     C                   PARM                    pSubject
     C                   PARM                    pRcpnt
     C                   PARM                    pSBoard
      * and set corresponding indicators
     C                   SELECT
     C                   WHEN      pMode = 'P'
      *  pMode = 'B' -> Post message to a Sub-Board (to be selected)
     C                   EVAL      wBoardUID = pRcpnt
     C                   EVAL      wSBoardUID  = pSBoard
     C                   EVAL      SCRPOB = pRcpnt
     C                   EVAL      SCRSBJ = pSubject
     C     pSBoard       IFNE      *BLANKS
     C     KSBORDS       CHAIN     PSBORDS                            50
     C   50              EVAL      SCRPOS = '*ERROR*'
     C  N50              EVAL      SCRPOS = SBRLNG
     C  N50              EVAL      SCRSBS = SBRSHT
     C                   ENDIF
     C                   EVAL      *IN30 = *ON
     C                   EVAL      FKEYSL = cKeysBeforeBrd
     C                   WHEN      pMode = 'B'
      *  pMode = 'B' -> Post message to pre-selected Sub-Board
     C                   EVAL      wBoardUID = pRcpnt
     C                   EVAL      wSBoardUID  = pSBoard
     C                   EVAL      SCRPOB = pRcpnt
     C                   EVAL      SCRSBJ = pSubject
     C     pSBoard       IFNE      *BLANKS
     C     KSBORDS       CHAIN     PSBORDS                            50
     C   50              EVAL      SCRPOS = '*ERROR*'
     C  N50              EVAL      SCRPOS = SBRLNG
     C  N50              EVAL      SCRSBS = SBRSHT
     C                   ENDIF
     C                   EVAL      *IN30 = *ON
     C                   EVAL      *IN29 = *ON
     C                   EVAL      FKEYSL = cKeysBeforeBrd
     C                   WHEN      pMode = 'U'
      *  pMode = 'U' -> User-to-User message
     C                   IF        pRcpnt = wUser
      *    Ask for confirmation if recipient is the same as current user
     C                   EXSR      AskMsgToSelf
     C                   ENDIF
     C                   EVAL      *IN31 = *ON
     C                   EVAL      FKEYSL = cKeysBeforeU2U
     C                   EVAL      SCRUSR = pRcpnt
     C                   WHEN      pMode = 'R'
      *  pMode = 'R' -> Reply to a User-to-User message
      *  pSubject and pRcpnt are used for 'reply to a message'
     C                   IF        pRcpnt = wUser
      *    Ask for confirmation if recipient is the same as current user
     C                   EXSR      AskMsgToSelf
     C                   ENDIF
     C                   EVAL      *IN31 = *ON
     C                   EVAL      FKEYSL = cKeysBeforeU2U
     C     'Re:'         CAT       pSubject:1    SCRSBJ
     C                   EVAL      SCRUSR = pRcpnt
     C                   WHEN      pMode = 'S'
      *  pMode = 'S' -> User-to-SysOp message
     C                   EVAL      *IN32 = *ON
     C                   EVAL      FKEYSL = cKeysBeforeU2U
     C                   ENDSL
      * Get values from DTAARA and show them on the screen
     C/COPY DVBBS400/CURRENTSRC,CBKHEADER
     C                   ENDSR
      **********************************************************************
      * Check Functions keys pressed by user
      **********************************************************************
     C     CheckFkeys    BEGSR
      * F4=Prompt
     C                   IF        *IN04 = *ON
     C                   EVAL      wLin = 0
     C                   EVAL      wPos = 0
     C                   MOVEL     wLinPos       wL
     C                   MOVE      wLinPos       wP
     C                   EVAL      wSL = wLin
     C                   EVAL      wSP = wPos
     C                   SELECT
     C                   WHEN      wPos = 15
      *    Prompt for Boards
     C                   EVAL      wGetUIDfor = 'B'
     C                   CALL      'BBSWINSBSR'
     C                   PARM                    wGetUIDfor
     C                   PARM                    wBoardUID
     C                   PARM                    wSL
     C                   PARM                    wSP
     C                   PARM                    wBoardUID
      *    Get Board Short Name from received wBoardUID
     C     wBoardUID     CHAIN     PBOARDS                            50
     C   50              EVAL      SCRPOB = '*ERROR*'
     C  N50              EVAL      SCRPOB = BRDSHT
     C                   WHEN      wPos = 26
      *    Prompt for Sub-Boards
     C                   IF        wBoardUID = *BLANKS
     C                   EVAL      MSGLIN = cErrNoBrdSel
     C                   ELSE
     C                   EVAL      wGetUIDfor = 'S'
     C                   CALL      'BBSWINSBSR'
     C                   PARM                    wGetUIDfor
     C                   PARM                    wBoardUID
     C                   PARM                    wSL
     C                   PARM                    wSP
     C                   PARM                    wSBoardUID
      *    Get Sub-Board Short Name from received wSoardUID
     C     KSBORDS       CHAIN     PSBORDS                            50
     C   50              EVAL      SCRPOS = '*ERROR*'
     C  N50              EVAL      SCRPOS = SBRLNG
     C  N50              EVAL      SCRSBS = SBRSHT
     C                   ENDIF
     C                   ENDSL
     C                   ENDIF
      * F6=Send
     C                   IF        *IN06 = *ON
     C                   EXSR      ValiAndSend
     C                   ENDIF
      * F12=Go back
     C                   IF        *IN12 = *ON
     C                   EVAL      *INLR = *ON
     C                   RETURN
     C                   ENDIF
     C                   ENDSR
      **********************************************************************
      * Validate entered data on screen and 'send' (store) message
      **********************************************************************
     C     ValiAndSend   BEGSR
     C                   EVAL      *IN34 = *OFF
      * Is user posting or messaging?
     C                   SELECT
     C                   WHEN      pMode = 'P' OR pMode = 'B'
      * Posting to a Sub-Board: Board, Sub-Board, Subject and Text not empty
     C     SCRPOB        IFNE      *BLANKS
     C     SCRPOS        ANDNE     *BLANKS
     C     SCRSBJ        ANDNE     *BLANKS
     C     SCRM01        ANDNE     *BLANKS
      *   Check that User has enough Posting Access Level
     C                   EVAL      wBoardUID = SCRPOB
     C                   EVAL      wSBoardUID = SCRSBS
     C     KSBORDS       CHAIN     PSBORDS
     C                   IF        %FOUND AND wUserLvlD >= SBRPLV
     C                   EVAL      *IN34 = *ON
     C                   EXSR      PostToSBoard
     C                   ELSE
     C                   EVAL      MSGLIN = cErrLvlLowP
     C                   EVAL      *IN35 = *ON
     C                   ENDIF
     C                   ENDIF
     C                   WHEN      pMode = 'U' OR pMode = 'R'
      * Message to another User: User, Subject and Text not empty
     C     SCRUSR        IFNE      *BLANKS
     C     SCRSBJ        ANDNE     *BLANKS
     C     SCRM01        ANDNE     *BLANKS
      *       Check if recipient exists
     C     SCRUSR        CHAIN     PUSERS                             35
     C   35              EVAL      MSGLIN = cErrRcpNotFnd
     C  N35              EVAL      *IN34 = *ON
     C  N35              EVAL      UUMRCP = SCRUSR
     C  N35              EXSR      PutU2UtoDB
      *       Send notification of User wants to
     C                   IF        USRNFY = 'Y'
     C                   EVAL      wHlpUser = SCRUSR
     C                   CALL      'BBSHELPERC'
     C                   PARM                    wHlpMode
     C                   PARM                    wHlpFiller1
     C                   PARM                    wHlpFiller2
     C                   PARM                    wHlpUser
     C                   ENDIF
     C                   ENDIF
     C                   WHEN      pMode = 'S'
      * Message to SysOp: Subject and Text not empty
     C     SCRSBJ        IFNE      *BLANKS
     C     SCRM01        ANDNE     *BLANKS
     C                   EVAL      *IN34 = *ON
     C                   EVAL      UUMRCP = 'SYSOP'
     C                   EXSR      PutU2UtoDB
     C                   ENDIF
     C                   ENDSL
      * Depending on WRITE error or success, display message
      *   and disable F keys, so cannot be sent twice
     C                   IF        *IN34 = *ON
     C   51              EVAL      MSGLIN = cErrSndMsg
     C  N51              EVAL      MSGLIN = cMsgSent
     C  N51              EVAL      *IN33 = *OFF
     C  N51              EVAL      FKEYSL = cKeysAfter
     C                   ELSE
     C  N35              EVAL      MSGLIN = cErrValidKO
     C                   ENDIF
     C                   ENDSR
      **********************************************************************
      * Post a Message to a Sub-Board
      **********************************************************************
     C     PostToSBoard  BEGSR
      * Board UID
     C                   EVAL      MSGBRD = wBoardUID
      * Sub-Board UID
     C                   EVAL      MSGSBD = wSBoardUID
      * Message UID
     C                   EXSR      GetMsgUID
     C                   EVAL      MSGUID = wMsgUIDhash
      * Subject
     C                   EVAL      MSGSBJ = SCRSBJ
      * Date and Time posted
     C                   MOVEL     wTodNowTime   wTodNowTimeD
     C                   MOVEL     wTodNowDate   wTodNowDateD
     C     *DMY          MOVEL     wTodNowDateD  MSGDAT
     C     *HMS          MOVEL     wTodNowTimeD  MSGTIM
      * Sender is current User
     C                   MOVEL     wUser         MSGSND
      * Message text
     C                   EVAL      MSGTXT = dsMsgText
      * Write message to DB
     C                   WRITE     RMESSG                               51
     C                   ENDSR
      **********************************************************************
      * Put User-to-User data into DB
      **********************************************************************
     C     PutU2UtoDB    BEGSR
      * Message UID
     C                   EXSR      GetMsgUID
     C                   EVAL      UUMUID = wMsgUIDhash
      * Status is Unread for new messages
     C                   EVAL      UUMSTA = 'U'
      * Sender is current User
     C                   MOVEL     wUser         UUMSND
      * Subject
     C                   EVAL      UUMSBJ = SCRSBJ
      * Date and Time sent
     C                   MOVEL     wTodNowTime   wTodNowTimeD
     C                   MOVEL     wTodNowDate   wTodNowDateD
     C     *DMY          MOVEL     wTodNowDateD  UUMDAT
     C     *HMS          MOVEL     wTodNowTimeD  UUMTIM
      * Message Text
     C                   EVAL      UUMTXT = dsMsgText
      * Write message to DB
     C                   WRITE     RUUMSG                               51
     C                   ENDSR
      **********************************************************************
      * Get UID for new message
      **********************************************************************
     C     GetMsgUID     BEGSR
     C                   TIME                    wTodayNowD
     C                   MOVEL     wTodayNowD    dsTodayNowC
     C     wUser         CAT       dsTodayNowC:0 wMsgUID17
     C                   CALL      'GETMD5HASH'
     C                   PARM                    wHashMode
     C                   PARM                    wMsgUID17
     C                   PARM                    wMsgUIDhash
     C                   ENDSR
      **********************************************************************
      * Ask for confirmation if recipient is the same as current user
      **********************************************************************
     C     AskMsgToSelf  BEGSR
     C                   EVAL      wConfirmQ = 'Message to you?'
     C                   EVAL      wConfirmA = 'N'
     C                   EVAL      wConfirmF3 = *OFF
     C                   CALL      'BBSWINYNR'
     C                   PARM                    wConfirmQ
     C                   PARM                    wConfirmA
     C                   PARM                    wConfirmF3
     C                   IF        wConfirmF3 = *OFF AND wConfirmA = 'N'
     C                   EVAL      *INLR = *ON
     C                   RETURN
     C                   ENDIF
     C                   ENDSR
