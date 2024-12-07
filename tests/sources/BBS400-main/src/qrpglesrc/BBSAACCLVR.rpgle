     H/TITLE List all Access Levels and allow to add/edit/delete
     H COPYRIGHT('(C) 2020 David Asta under MIT License')
      * SYSTEM      : V4R5
      * PROGRAMMER  : David Asta
      * DATE-WRITTEN: 20/NOV/2020
      *
      * This program shows the list of Access Levels, and allows to edit or
      *   delete them, as well as add new ones
      **********************************************************************
     H/COPY DVBBS400/CURRENTSRC,CBKOPTIMIZ
      **********************************************************************
      * INDICATORS USED:
      * 25 - Roll key
      * 40 - SFLDSP
      * 41 - SFLCLR
      * 42 - SFLEND(*MORE)
      **********************************************************************
     FBBSAACCLVDCF   E             WORKSTN
     F                                     SFILE(SF:wRRN)
     FPACCLVS   UF A E           K DISK
      **********************************************************************
      * Data structures
     D/COPY DVBBS400/CURRENTSRC,CBKDTAARA
      * Constants
     D wErrDuplAccLvl  C                   CONST('Access Level already exists')
     D wOptsLin1       C                   CONST(' 4=Delete   7=Rename    8=Cha-
     D                                     nge Expire days     9=Change Expire -
     D                                     to Lvl')
     D wOptsLin2       C                   CONST('10=Switch List Users   11=Swi-
     D                                     tch System Info.   12=Switch Post...-
     D                                     ')
     D wOptsLin3       C                   CONST('13=Switch Message Users      -
     D                                             14=Switch See Who''s Online.-
     D                                     ..')
      * Variables
     D wRRN            S              4P 0
     D wOptions        S               N   INZ(*ON)
     D wPtr            S               *
     D wYesNo          S              1A   BASED(wPtr)
     D wWinMode        S              1A
     D wWinText        S             30A
     D wWinNumber      S              3P 0
     D wWinF3          S               N
     D wAccLvlD        S              2P 0
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
      * Get values from DATAARA and show them on screen
     C/COPY DVBBS400/CURRENTSRC,CBKHEADER
      * Initialise variables and load subfile
     C                   EVAL      SCRSCR = 'BBSAACCLV'
     C                   EVAL      OPTSL1 = wOptsLin1
     C                   EVAL      OPTSL2 = wOptsLin2
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
     C                   EXSR      AddNewAccLvl
     C                   ENDIF
      * F12=Go back
     C                   IF        *IN12 = *ON
     C                   EVAL      *INLR = *ON
     C                   RETURN
     C                   ENDIF
      * F23=More options
     C                   IF        *IN23 = *ON
     C                   IF        wOptions = *ON
     C                   EVAL      wOptions = *OFF
     C                   EVAL      OPTSL1 = wOptsLin3
     C                   EVAL      OPTSL2 = *BLANKS
     C                   ELSE
     C                   EVAL      wOptions = *ON
     C                   EVAL      OPTSL1 = wOptsLin1
     C                   EVAL      OPTSL2 = wOptsLin2
     C                   ENDIF
     C                   ENDIF
     C                   ENDSR
      **********************************************************************
      * Check Options entered by the user
      **********************************************************************
     C     ChkOptions    BEGSR
     C                   READC     SF                                     91
     C                   DOW       *IN91 = *OFF
     C                   SELECT
     C                   WHEN      SCROPT = '4'
      * Delete an Access Level
     C     SCRLVL        CHAIN     PACCLVS
     C                   IF        %FOUND
     C                   DELETE    RACCLV
     C                   ENDIF
     C                   WHEN      SCROPT = '7'
      * Rename
     C                   EVAL      wWinMode = 'T'
     C                   EVAL      wWinText = SCRLVD
     C                   EVAL      wWinNumber = 0
     C                   EVAL      wWinF3 = *OFF
     C                   CALL      'BBSWINASKR'
     C                   PARM                    wWinMode
     C                   PARM                    wWinText
     C                   PARM                    wWinNumber
     C                   PARM                    wWinF3
     C                   IF        wWinF3 = *OFF
     C                   EVAL      SCRLVD = wWinText
     C                   EXSR      UpdateDB
     C                   ENDIF
     C                   WHEN      SCROPT = '8'
      * Change Expire days
     C                   EVAL      wWinMode = 'N'
     C                   EVAL      wWinText = *BLANKS
     C                   EVAL      wWinNumber = SCREXP
     C                   EVAL      wWinF3 = *OFF
     C                   CALL      'BBSWINASKR'
     C                   PARM                    wWinMode
     C                   PARM                    wWinText
     C                   PARM                    wWinNumber
     C                   PARM                    wWinF3
     C                   IF        wWinF3 = *OFF
     C                   EVAL      SCREXP = wWinNumber
     C                   EXSR      UpdateDB
     C                   ENDIF
     C                   WHEN      SCROPT = '9'
      * Change Expire to Lvl
     C                   EVAL      wWinMode = 'N'
     C                   EVAL      wWinText = *BLANKS
     C                   EVAL      wWinNumber = SCREXL
     C                   EVAL      wWinF3 = *OFF
     C                   CALL      'BBSWINASKR'
     C                   PARM                    wWinMode
     C                   PARM                    wWinText
     C                   PARM                    wWinNumber
     C                   PARM                    wWinF3
     C                   IF        wWinF3 = *OFF
     C                   EVAL      SCREXL = wWinNumber
     C                   EXSR      UpdateDB
     C                   ENDIF
     C                   WHEN      SCROPT = '10'
      * Switch List Users
     C                   EVAL      wPtr = %ADDR(SCRALU)
     C                   EXSR      SwitchYesNo
     C                   EXSR      UpdateDB
     C                   WHEN      SCROPT = '11'
      * Switch System Info.
     C                   EVAL      wPtr = %ADDR(SCRASI)
     C                   EXSR      SwitchYesNo
     C                   EXSR      UpdateDB
     C                   WHEN      SCROPT = '12'
      * Switch Post
     C                   EVAL      wPtr = %ADDR(SCRAPM)
     C                   EXSR      SwitchYesNo
     C                   EXSR      UpdateDB
     C                   WHEN      SCROPT = '13'
      * Switch Message Users
     C                   EVAL      wPtr = %ADDR(SCRAMU)
     C                   EXSR      SwitchYesNo
     C                   EXSR      UpdateDB
     C                   WHEN      SCROPT = '14'
      * Switch See Who's Online
     C                   EVAL      wPtr = %ADDR(SCRAWO)
     C                   EXSR      SwitchYesNo
     C                   EXSR      UpdateDB
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
      * Load all records from PACCLVS
      * All records are loaded (load-all SFL) until the end of the file or
      *  until SFLSIZ is reached
      **********************************************************************
     C     LoadSFL       BEGSR
     C     *START        SETLL     PACCLVS
     C                   DOU       %EOF
     C                   READ      PACCLVS
     C                   IF        %EOF
     C                   LEAVE
     C                   ENDIF
     C                   EXSR      Data2SFL
     C                   ADD       1             wRRN
     C                   WRITE     SF
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
      * Put data into a SFL record
      **********************************************************************
     C     Data2SFL      BEGSR
     C                   EVAL      SCRLVL = LVLLVL
     C                   EVAL      SCRLVD = LVLDSC
     C                   EVAL      SCREXP = LVLEXP
     C                   EVAL      SCREXL = LVLEXL
     C                   EVAL      SCRALU = LVLALU
     C                   EVAL      SCRASI = LVLASI
     C                   EVAL      SCRAPM = LVLAPM
     C                   EVAL      SCRAMU = LVLAMU
     C                   EVAL      SCRAWO = LVLAWO
     C                   ENDSR
      **********************************************************************
      * Switches the value pointed by the wYesNo pointer
      *  from Y to N and viceversa
      **********************************************************************
     C     SwitchYesNo   BEGSR
     C                   IF        wYesNo = 'Y'
     C                   EVAL      wYesNo = 'N'
     C                   ELSE
     C                   EVAL      wYesNo = 'Y'
     C                   ENDIF
     C                   ENDSR
      **********************************************************************
      * Updates an Access Level in PACCLVS with values changed by the user
      **********************************************************************
     C     UpdateDB      BEGSR
     C     SCRLVL        CHAIN     PACCLVS
     C                   IF        %FOUND
     C                   EVAL      LVLDSC = SCRLVD
     C                   EVAL      LVLEXP = SCREXP
     C                   EVAL      LVLEXL = SCREXL
     C                   EVAL      LVLALU = SCRALU
     C                   EVAL      LVLASI = SCRASI
     C                   EVAL      LVLAPM = SCRAPM
     C                   EVAL      LVLAMU = SCRAMU
     C                   EVAL      LVLAWO = SCRAWO
     C                   UPDATE    RACCLV
     C                   ENDIF
     C                   ENDSR
      **********************************************************************
      * Add a new Access Level tp PACCLVS
      *  The user will be ask to enter the Access Level number,
      *  and the rest of fields will be default
      **********************************************************************
     C     AddNewAccLvl  BEGSR
     C                   EVAL      wWinMode = 'N'
     C                   EVAL      wWinText = *BLANKS
     C                   EVAL      wWinNumber = 0
     C                   EVAL      wWinF3 = *OFF
     C                   CALL      'BBSWINASKR'
     C                   PARM                    wWinMode
     C                   PARM                    wWinText
     C                   PARM                    wWinNumber
     C                   PARM                    wWinF3
     C                   IF        wWinF3 = *OFF
      * Check that the Access Lvl entered by the user doesn't already exist
     C                   EVAL      wAccLvlD = wWinNumber
     C     wAccLvlD      CHAIN     PACCLVS
     C                   IF        %FOUND
     C                   EVAL      MSGLIN = wErrDuplAccLvl
     C                   ELSE
      * Add to DB
     C                   EVAL      LVLLVL = wWinNumber
     C                   EVAL      LVLDSC = 'NEW ACCESS LEVL'
     C                   EVAL      LVLEXP = 0
     C                   EVAL      LVLEXL = 0
     C                   EVAL      LVLALU = 'N'
     C                   EVAL      LVLASI = 'N'
     C                   EVAL      LVLAPM = 'N'
     C                   EVAL      LVLAMU = 'N'
     C                   EVAL      LVLAWO = 'N'
     C                   WRITE     RACCLV
     C                   ENDIF
     C                   ENDIF
     C                   ENDSR
