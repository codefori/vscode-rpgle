     H/TITLE Login Screen
     H COPYRIGHT('(C) 2020 David Asta under MIT License')
      * SYSTEM      : V4R5
      * PROGRAMMER  : David Asta
      * DATE-WRITTEN: 11/NOV/2020
      *
      * This program shows the Login Screen with Sign On (Username/Password)
      *   and checks that the entered username and password are valid.
      *   If user entered 'New' as User and left Password empty, we trigger
      *   the creation of a New User (if system is open to new users).
      * This program also loads the QTEMP/DTAARA with data used by other
      *   programs that will be called within the BBS
      **********************************************************************
      * Compiler flags
     H ALWNULL(*USRCTL)
     H/COPY DVBBS400/CURRENTSRC,CBKOPTIMIZ
      **********************************************************************
      * INDICATORS USED:
      * 30 - *ON = Valid Username/Password
      * 31 - *OFF = Already logged in
      * 32 - *ON = BBS is CLOSED to new users
      * 33 - *ON = BBS is in Maintenance Mode
      **********************************************************************
     FBBSLGIND  CF   E             WORKSTN
     FPUSERS    UF   E           K DISK
     FPCONFIG   IF   E           K DISK
     FPACCLVS   IF   E           K DISK
     FPLOGGED   UF A E           K DISK
     FPSESSIONS UF A E             DISK
      **********************************************************************
      * Data structures
     D/COPY DVBBS400/CURRENTSRC,CBKDTAARA
     D dsTodayNowC     DS
     D  wTodNowTime                   6A
     D  wTodNowDate                   6A
      * Constants
     D cUserPasErr     C                   CONST('Invalid Username/Password com-
     D                                     bination.')
     D cAlreadyOn      C                   CONST('This user is already logged i-
     D                                     n in another session.')
     D cIsClosed       C                   CONST('This BBS is currently not acc-
     D                                     epting new users.')
     D cIsMaintenance  C                   CONST('This BBS is currently in Main-
     D                                     tenance Mode.')
      * Variables
     D wCfgKey         S              6A
     D wUserLvlD       S              2P 0
     D wShowWelcome    S              1A
     D wTodayNowD      S             12P 0
     D wTodNowTimeD    S              6P 0
     D wTodNowDateD    S              6P 0
     D wMsgUID20       S             20A
     D wMsgUIDhash     S             32A
     D wHashMode       S              1A   INZ('P')
     ***********************************************************************
     C                   WRITE     HEADER
     C                   WRITE     FOOTER
     C                   EXFMT     BODY
      * If F3 was pressed by the user, exit the application.
     C                   IF        *IN03 = *ON
     C                   MOVE      *ON           *INLR
     C                   RETURN
     C                   ENDIF
      * Otherwise...
      * If User = NEW, trigger New User Registration
     C     SCRUSR        IFEQ      'NEW'
     C   32              EVAL      MSGLIN = cIsClosed
     C   32              GOTO      ENDLOGIC
     C   33              EVAL      MSGLIN = cIsMaintenance
     C   33              GOTO      ENDLOGIC
     C                   CALL      'BBSNEWREGR'
     C                   ELSE
      * Check that user and password are not empty
     C     SCRUSR        IFNE      *BLANKS
     C     SCRPAS        ANDNE     *BLANKS
      * Check that the user is not already logged in in another session
     C     SCRUSR        CHAIN(N)  PLOGGED                            31
      *        it was already logged in
     C  N31              EVAL      MSGLIN = cAlreadyOn
      *        it was not logged in. Check Password
     C   31              EXSR      ChkUserPass
      *       user/password correct
      * Check Access Level is not zero
     C                   IF        USRLVL = 0
     C                   EVAL      MSGLIN = cUserPasErr
     C                   GOTO      ENDLOGIC
     C                   ENDIF
     C   30              EXSR      ProceedLogin
     C                   ELSE
     C                   EVAL      MSGLIN = cUserPasErr
     C                   ENDIF
     C     ENDLOGIC      TAG
     C                   ENDIF
      **********************************************************************
      * Subroutine called automatically at startup
      **********************************************************************
     C     *INZSR        BEGSR
     C                   EVAL      *IN30 = *OFF
      * Get configuration and put it in the DTAARA
     C                   EXSR      GetConfig
      *       Show Welcome screen?
     C                   IF        wShowWelcome = 'Y'
     C                   CALL      'BBSWELCC'
     C                   ENDIF
      * Get Today's date/time
     C                   TIME                    wTodayNowD
     C                   MOVEL     wTodayNowD    dsTodayNowC
     C                   MOVEL     wTodNowTime   wTodNowTimeD
     C                   MOVEL     wTodNowDate   wTodNowDateD
      * Show Title on screen
     C                   IN        wDTAARA
     C                   UNLOCK    wDTAARA
     C     'BBS400'      CAT       wSoftVer:1    SCRVER
     C                   EVAL      SCRNAM = wBBSNAM
      * If BBS is CLOSED to new users, activate info message
     C                   IF        wCLOSED = 'Y'
     C                   EVAL      *IN32 = *ON
     C                   ENDIF
      * If BBS is in Maintenance Mode, activate info message
     C                   IF        wMAINTM = 'Y'
     C                   EVAL      *IN33 = *ON
     C                   ENDIF
     C                   ENDSR
      **********************************************************************
      * Read configuration values from PCONFIG into DTAARA
      **********************************************************************
     C     GetConfig     BEGSR
     C     *LOCK         IN        wDTAARA
      * Get BBS Name
     C                   EVAL      wCfgKey = 'BBSNAM'
     C     wCfgKey       CHAIN     PCONFIG                            41
     C   41              GOTO      ENDOFSR
     C                   EVAL      wBBSNAM = CNFVAL
      * Get BBS Location Country Code
     C                   EVAL      wCfgKey = 'LOCCRY'
     C     wCfgKey       CHAIN     PCONFIG                            41
     C   41              GOTO      ENDOFSR
     C                   EVAL      wLOCCRY = CNFVAL
      * Get BBS Location City
     C                   EVAL      wCfgKey = 'LOCCTY'
     C     wCfgKey       CHAIN     PCONFIG                            41
     C   41              GOTO      ENDOFSR
     C                   EVAL      wLOCCTY = CNFVAL
      * Get BBS Time Zone
     C                   EVAL      wCfgKey = 'TIMZON'
     C     wCfgKey       CHAIN     PCONFIG                            41
     C   41              GOTO      ENDOFSR
     C                   EVAL      wTIMZON = CNFVAL
      * Get BBS closed to new users?
     C                   EVAL      wCfgKey = 'CLOSED'
     C     wCfgKey       CHAIN     PCONFIG                            41
     C   41              GOTO      ENDOFSR
     C                   EVAL      wCLOSED = CNFVAL
      * Get BBS New User Level
     C                   EVAL      wCfgKey = 'NUSLVL'
     C     wCfgKey       CHAIN     PCONFIG                            41
     C   41              GOTO      ENDOFSR
     C                   EVAL      wNUSLVL = CNFVAL
      * Get BBS New User enabled survey questions
     C                   EVAL      wCfgKey = 'NUSSVY'
     C     wCfgKey       CHAIN     PCONFIG                            41
     C   41              GOTO      ENDOFSR
     C                   EVAL      wNUSSVY = CNFVAL
      * Get Show Access Level Description?
     C                   EVAL      wCfgKey = 'SHWALD'
     C     wCfgKey       CHAIN     PCONFIG                            41
     C   41              GOTO      ENDOFSR
     C                   EVAL      wSHWALD = CNFVAL
      * Get Show Welcome screen
     C                   EVAL      wCfgKey = 'SHWWEL'
     C     wCfgKey       CHAIN     PCONFIG                            41
     C   41              GOTO      ENDOFSR
     C                   EVAL      wShowWelcome = CNFVAL
      * Get Notify to, when New User Registration
     C                   EVAL      wCfgKey = 'NUSRNF'
     C     wCfgKey       CHAIN     PCONFIG                            41
     C   41              GOTO      ENDOFSR
     C                   EVAL      wNewUsrNtfy = CNFVAL
      * Get Maintenance Mode
     C                   EVAL      wCfgKey = 'MAINTM'
     C     wCfgKey       CHAIN     PCONFIG                            41
     C   41              GOTO      ENDOFSR
     C                   EVAL      wMAINTM = CNFVAL
      * Get Hide SysOp from Users Lists
     C                   EVAL      wCfgKey = 'HIDESO'
     C     wCfgKey       CHAIN     PCONFIG                            41
     C   41              GOTO      ENDOFSR
     C                   EVAL      wHIDESO = CNFVAL
      * Highlight SysOp's messages
     C                   EVAL      wCfgKey = 'HLSOMS'
     C     wCfgKey       CHAIN     PCONFIG                            41
     C   41              GOTO      ENDOFSR
     C                   EVAL      wHLSOMS = CNFVAL
     C     ENDOFSR       TAG
     C                   OUT       wDTAARA
     C                   UNLOCK    wDTAARA
     C                   ENDSR
      **********************************************************************
      * Update DTAARA with data from the just now authenticated user
      **********************************************************************
     C     UpdateDTAARA  BEGSR
     C     *LOCK         IN        wDTAARA
      * Get User data
     C                   EVAL      wUser = SCRUSR
     C                   MOVEL     USRLVL        wUserLvl
     C     *DMY          MOVEL     USRLLD        wUserLstLogin
      * Get User's Level data
     C                   MOVEL     wUserLvl      wUserLvlD
     C     wUserLvlD     CHAIN     PACCLVS
     C                   IF        %FOUND
     C                   IF        wSHWALD = 'Y'
      * Show Access Level Description
     C                   EVAL      wLvlDescr = LVLDSC
     C                   ENDIF
      * Get User's Level Authorisations
     C                   EVAL      wAuthListUser = LVLALU
     C                   EVAL      wAuthSysInfor = LVLASI
     C                   EVAL      wAuthPostMsgs = LVLAPM
     C                   EVAL      wAuthMsgUsers = LVLAMU
     C                   EVAL      wAuthWhosOnli = LVLAWO
     C                   EVAL      wLvlAuths = wUserLvlAuths
     C                   ELSE
      * Data couldn't be read. Show error and Exit application
     C     'ERROR LGN01' DSPLY
     C                   EVAL      *INLR = *ON
     C                   RETURN
     C                   ENDIF
     C                   OUT       wDTAARA
     C                   UNLOCK    wDTAARA
     C                   ENDSR
      **********************************************************************
      * Check that Username and Password match with what is stored in the DB
      **********************************************************************
     C     ChkUserPass   BEGSR
      * if BBS is in Maintenance Mode, only SysOp can log in
     C     *IN33         IFEQ      *ON
     C     SCRUSR        ANDNE     'SYSOP'
     C                   EVAL      MSGLIN = cIsMaintenance
     C                   ELSE
      * Check Password for Username
     C     SCRUSR        CHAIN(N)  PUSERS
     C                   IF        %FOUND
     C* Check if hashed password is matching with stored in DB hash
     C     SCRUSR        CAT       SCRPAS:0      wMsgUID20
     C                   CALL      'GETMD5HASH'
     C                   PARM                    wHashMode
     C                   PARM                    wMsgUID20
     C                   PARM                    wMsgUIDhash
     C                   IF        wMsgUIDhash = USRPAS
      * Username/Password correct
     C                   MOVE      *ON           *IN30
     C                   ELSE
      * Username/Password combination not found
     C                   EVAL      MSGLIN = cUserPasErr
     C                   ENDIF
     C                   ELSE
      * Username not found
     C                   EVAL      MSGLIN = cUserPasErr
     C                   ENDIF
     C                   ENDIF
     C                   ENDSR
      **********************************************************************
      * User and Password correct. Proceed with the Log in
      **********************************************************************
     C     ProceedLogin  BEGSR
     C     SCRUSR        CHAIN     PUSERS
      * Update Last Sign On date and number of logons on PUSERS
     C     *DMY          MOVEL     wTodNowDateD  USRLLD
     C     *HMS          MOVEL     wTodNowTimeD  USRLLT
     C                   ADD       1             USRNLG
     C                   UPDATE    RUSER
      * Update DTAARA
     C                   EXSR      UpdateDTAARA
      * Record entry in PLOGGED
     C                   EVAL      LGDNCK = USRNCK
     C                   EVAL      LGDLVL = USRLVL
     C                   EVAL      LGDLCC = USRCTR
     C                   EVAL      LGDLCT = USRCTY
     C                   EVAL      LGDNLG = USRNLG
     C*                  TIME                    wTodayNowD
     C*                  MOVEL     wTodayNowD    dsTodayNowC
     C*                  MOVEL     wTodNowTime   wTodNowTimeD
     C*                  MOVEL     wTodNowDate   wTodNowDateD
     C     *DMY          MOVEL     wTodNowDateD  LGDDAT
     C     *HMS          MOVEL     wTodNowTimeD  LGDTIM
     C                   WRITE     RLOGGD
      * Record entry in PSESSIONS
     C                   TIME                    SESTMS
     C                   EVAL      SESNCK = wUser
     C                   EVAL      SESTYP = 'I'
     C                   WRITE     RSESSION
      *       Show Main Menu
     C                   CALL      'BBSMENUR'
      * When user exits the Main Menu, we exit the application completely
      *    remove record from PLOGGED
     C     wUser         CHAIN     PLOGGED
     C                   DELETE    RLOGGD
      *    and exit the application completely
     C                   MOVE      *ON           *INLR
     C                   RETURN
     C                   ENDSR
