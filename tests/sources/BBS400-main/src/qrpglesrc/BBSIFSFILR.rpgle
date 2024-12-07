     H/TITLE List all IFS files available (Access Lvl) for the user
     H COPYRIGHT('(C) 2020 David Asta under MIT License')
      * SYSTEM      : V4R5
      * PROGRAMMER  : David Asta
      * DATE-WRITTEN: 28/NOV/2020
      *
      * This program shows the list of IFS files that the user can access,
      *   depending on the user's Access Level
      **********************************************************************
     H/COPY DVBBS400/CURRENTSRC,CBKOPTIMIZ
      **********************************************************************
      * INDICATORS USED:
      * 25 - Roll key
      * 40 - SFLDSP
      * 41 - SFLCLR
      * 42 - SFLEND(*MORE)
      * 98 - EOF
      * 99 - EOF
      **********************************************************************
     FBBSIFSFILDCF   E             WORKSTN
     F                                     SFILE(SF:wRRN)
     FPIFSFILES UF A E           K DISK
      **********************************************************************
      * Data structures
     D/COPY DVBBS400/CURRENTSRC,CBKDTAARA
      * Constants
     D cErrOptNoAdmin  C                   CONST('Value entered for field is no-
     D                                     t valid. Valid values listed in mess-
     D                                     age help.')
     D cKeysAdmin      C                   CONST('F5=Refresh   F6=Add   F12=Go -
     D                                     back')
     D cKeysUser       C                   CONST('F5=Refresh   F12=Go back')
     D cNoFiles        C                   CONST('No files are accessible for y-
     D                                     our Access Level.')
     D cErrMaxLvl99    C                   CONST('Maximum Level can only be 99.-
     D                                     ')
      * Variables
     D/COPY DVBBS400/CURRENTSRC,CBKUSEWINS
     D pMode           S              1A
     D wRRN            S              4P 0
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
     C                   EVAL      *IN50 = *ON
     C                   EVAL      KEYLST = cKeysAdmin
     C                   ELSE
     C                   EVAL      *IN50 = *OFF
     C                   EVAL      KEYLST = cKeysUser
     C                   ENDIF
      * Get values from DATAARA and show them on screen
     C/COPY DVBBS400/CURRENTSRC,CBKHEADER
      * Initialise variables and load subfile
     C                   EVAL      SCRSCR = 'BBSIFSFIL'
     C                   MOVEL     wUserLvl      wUserLvlD
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
      * F6=Add (only from Administration Menu)
     C                   IF        *IN06 = *ON AND *IN50 = *ON
     C                   EXSR      AddNewFile
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
      * Change Access Level (only from Administration Menu)
     C                   IF        *IN50 = *ON
     C                   EVAL      wWinMode = 'N'
     C                   EVAL      wWinText = *BLANKS
     C                   EVAL      wWinNumber = SCRALV
     C                   EVAL      wWinF3 = *OFF
     C                   CALL      'BBSWINASKR'
     C                   PARM                    wWinMode
     C                   PARM                    wWinText
     C                   PARM                    wWinNumber
     C                   PARM                    wWinF3
     C                   IF        wWinF3 = *OFF
     C     SCRNAM        CHAIN     PIFSFILES
     C                   IF        %FOUND
     C                   EVAL      IFSALV = wWinNumber
     C                   UPDATE    RIFSFILE
     C                   ENDIF
     C                   ENDIF
     C                   ELSE
     C                   EVAL      MSGLIN = cErrOptNoAdmin
     C                   ENDIF
     C                   WHEN      SCROPT = '3'
      * Change Path (only from Administration Menu)
     C                   IF        *IN50 = *ON
     C                   EVAL      wWinMode = 'T'
     C                   EVAL      wWinText = SCRPTH
     C                   EVAL      wWinNumber = 0
     C                   EVAL      wWinF3 = *OFF
     C                   CALL      'BBSWINASKR'
     C                   PARM                    wWinMode
     C                   PARM                    wWinText
     C                   PARM                    wWinNumber
     C                   PARM                    wWinF3
     C                   IF        wWinF3 = *OFF
     C     SCRNAM        CHAIN     PIFSFILES
     C                   IF        %FOUND
     C                   EVAL      IFSPTH = wWinText
     C                   UPDATE    RIFSFILE
     C                   ENDIF
     C                   ENDIF
     C                   ELSE
     C                   EVAL      MSGLIN = cErrOptNoAdmin
     C                   ENDIF
     C                   WHEN      SCROPT = '4'
      * Delete (only from Administration Menu)
     C                   IF        *IN50 = *ON
      *         Ask for confirmation before deleting
     C     'Delete'      CAT       SCRNAM:1      wConfirmQ
     C                   EVAL      wConfirmA = 'N'
     C                   EVAL      wConfirmF3 = *OFF
     C                   CALL      'BBSWINYNR'
     C                   PARM                    wConfirmQ
     C                   PARM                    wConfirmA
     C                   PARM                    wConfirmF3
     C                   IF        wConfirmF3 = *OFF AND wConfirmA = 'Y'
     C     SCRNAM        CHAIN     PIFSFILES
     C                   IF        %FOUND
     C                   DELETE    RIFSFILE
     C                   ENDIF
     C                   ENDIF
     C                   ELSE
     C                   EVAL      MSGLIN = cErrOptNoAdmin
     C                   ENDIF
     C                   WHEN      SCROPT = '5'
      * Display IFS File
     C                   CALL      'BBSDSPIFSC'
     C                   PARM                    SCRPTH
     C                   PARM                    SCRNAM
     C                   WHEN      SCROPT = '7'
      * Rename Filename (only from Administration Menu)
     C                   IF        *IN50 = *ON
     C                   EVAL      wWinMode = 'T'
     C                   EVAL      wWinText = SCRNAM
     C                   EVAL      wWinNumber = 0
     C                   EVAL      wWinF3 = *OFF
     C                   CALL      'BBSWINASKR'
     C                   PARM                    wWinMode
     C                   PARM                    wWinText
     C                   PARM                    wWinNumber
     C                   PARM                    wWinF3
     C                   IF        wWinF3 = *OFF
     C     SCRNAM        CHAIN     PIFSFILES
     C                   IF        %FOUND
     C                   EVAL      IFSFNM = wWinText
     C                   UPDATE    RIFSFILE
     C                   ENDIF
     C                   ENDIF
     C                   ELSE
     C                   EVAL      MSGLIN = cErrOptNoAdmin
     C                   ENDIF
     C                   WHEN      SCROPT = '8'
      * Rename Description (only from Administration Menu)
     C                   IF        *IN50 = *ON
     C                   EVAL      wWinMode = 'T'
     C                   EVAL      wWinText = SCRDSC
     C                   EVAL      wWinNumber = 0
     C                   EVAL      wWinF3 = *OFF
     C                   CALL      'BBSWINASKR'
     C                   PARM                    wWinMode
     C                   PARM                    wWinText
     C                   PARM                    wWinNumber
     C                   PARM                    wWinF3
     C                   IF        wWinF3 = *OFF
     C     SCRNAM        CHAIN     PIFSFILES
     C                   IF        %FOUND
     C                   EVAL      IFSDSC = wWinText
     C                   UPDATE    RIFSFILE
     C                   ENDIF
     C                   ENDIF
     C                   ELSE
     C                   EVAL      MSGLIN = cErrOptNoAdmin
     C                   ENDIF
     C                   WHEN      SCROPT = '9'
      * Change Category (only from Administration Menu)
     C                   IF        *IN50 = *ON
     C                   EVAL      wWinMode = 'T'
     C                   EVAL      wWinText = SCRCAT
     C                   EVAL      wWinNumber = 0
     C                   EVAL      wWinF3 = *OFF
     C                   CALL      'BBSWINASKR'
     C                   PARM                    wWinMode
     C                   PARM                    wWinText
     C                   PARM                    wWinNumber
     C                   PARM                    wWinF3
     C                   IF        wWinF3 = *OFF
     C     SCRNAM        CHAIN     PIFSFILES
     C                   IF        %FOUND
     C                   EVAL      IFSCAT = wWinText
     C                   UPDATE    RIFSFILE
     C                   ENDIF
     C                   ENDIF
     C                   ELSE
     C                   EVAL      MSGLIN = cErrOptNoAdmin
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
      * Load all records from PIFSFILES
      * All records are loaded (load-all SFL) until the end of the file or
      *  until SFLSIZ is reached
      **********************************************************************
     C     LoadSFL       BEGSR
     C     *START        SETLL     PIFSFILES
     C                   DOU       %EOF
     C                   READ      PIFSFILES
     C                   IF        %EOF
     C                   LEAVE
     C                   ENDIF
      * Put data on screen (if User has enough Access Level)
     C                   IF        wUserLvlD >= IFSALV
     C                   EXSR      Data2SFL
     C                   ADD       1             wRRN
     C                   WRITE     SF
      * If we have loaded 9999 records, we cannot add more. Stop loop
     C                   IF        wRRN = 9999
     C                   LEAVE
     C                   ENDIF
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
      * Put data into a SFL record
      **********************************************************************
     C     Data2SFL      BEGSR
     C                   EVAL      SCRNAM = IFSFNM
     C                   EVAL      SCRDSC = IFSDSC
     C                   EVAL      SCRCAT = IFSCAT
     C   50              EVAL      SCRALV = IFSALV
     C                   EVAL      SCRPTH = IFSPTH
     C                   ENDSR
      **********************************************************************
      * Ask the Admin user for a Filename,
      * and create a record with default values
      **********************************************************************
     C     AddNewFile    BEGSR
     C                   EVAL      wWinMode = 'T'
     C                   EVAL      wWinText = *BLANKS
     C                   EVAL      wWinNumber = 0
     C                   EVAL      wWinF3 = *OFF
     C                   CALL      'BBSWINASKR'
     C                   PARM                    wWinMode
     C                   PARM                    wWinText
     C                   PARM                    wWinNumber
     C                   PARM                    wWinF3
     C                   IF        wWinF3 = *OFF
     C                   EVAL      IFSFNM = wWinText
     C                   EVAL      IFSPTH = 'Path ?'
     C                   EVAL      IFSDSC = *BLANKS
     C                   EVAL      IFSCAT = *BLANKS
     C                   EVAL      IFSALV = 99
     C                   WRITE     RIFSFILE
     C                   EXSR      ReLoadSFL
     C                   ENDIF
     C                   ENDSR
