     H/TITLE Poll (View/Vote/Create/Edit)
     H COPYRIGHT('(C) 2020 David Asta under MIT License')
      * SYSTEM      : V4R5
      * PROGRAMMER  : David Asta
      * DATE-WRITTEN: 07/DEC/2020
      *
      * This program allows to View and Vote a Poll, for normal User
      *   and also to Create and Edit, for Administrator
      **********************************************************************
     H/COPY DVBBS400/CURRENTSRC,CBKOPTIMIZ
      **********************************************************************
      * INDICATORS USED:
      * 80 - *ON turns DSPATR(PR), which protects fields from being changed
      * 81 - *ON turns DSPATR(PR), which protects voting fields
      * 82 - *OFF shows Internal name field, for when Admin creates a poll
      * 90 - CHAIN not found
      * 91 - UPDATE/WRITE error
      **********************************************************************
     FBBSPOLLD  CF   E             WORKSTN
     FPPOLLSQ   UF A E           K DISK
     FPPOLLSA   UF A E           K DISK
      **********************************************************************
      * Data structures
     D/COPY DVBBS400/CURRENTSRC,CBKDTAARA
      * Constants
     D cViewing        C                   CONST('F5=Refresh   F10=Change Vote -
     D                                       F12=Go back')
     D cVoting         C                   CONST('F5=Refresh   F10=Confirm Vote-
     D                                        F12=Go back')
     D cEditing        C                   CONST('F5=Refresh   F10=Confirm Chan-
     D                                     ges   F12=Go back')
     D cMsgVoteChgnd   C                   CONST('Vote successfully changed.')
     D cMsgPollChgnd   C                   CONST('Poll successfully changed.')
     D cMsgPollCrea    C                   CONST('Poll successfully created.')
     D cErrNotOpen     C                   CONST('The selected poll is not open-
     D                                     .')
      * Variables
     D pMode           S              1A
     D pPollUID        S              8A
     D wCfgKey         S              6A
     D wNumVotes       S              6P 0
     D wCurrentVote    S              1P 0
     D wNewVote        S              1P 0
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
     C                   EVAL      SCRSCR = 'BBSPOLL'
      * Receive parameters
      * pMode = C - Create, E - Edit, W - Display, V - Vote
     C     *ENTRY        PLIST
     C                   PARM                    pMode
     C                   PARM                    pPollUID
      * Protect fields from being modified
     C                   SETON                                        808182
      * Unprotect fields depending on pMode
     C                   SELECT
     C                   WHEN      pMode = 'C'
     C                   EVAL      *IN80 = *OFF
     C                   EVAL      *IN82 = *OFF
     c                   EVAL      KEYLST = cEditing
     C                   WHEN      pMode = 'E'
     C                   EVAL      *IN80 = *OFF
     c                   EVAL      KEYLST = cEditing
     C                   WHEN      pMode = 'V'
     C                   EVAL      *IN81 = *OFF
     C                   EVAL      KEYLST = cVoting
     C                   WHEN      pMode = 'W'
     C                   EVAL      KEYLST = cViewing
     C                   ENDSL
      * Get values from DATAARA and show them on screen
     C/COPY DVBBS400/CURRENTSRC,CBKHEADER
      * Declare composite keys
     C     KPOLLSA       KLIST
     C                   KFLD                    POLUID
     C                   KFLD                    wUser
      * Get data from the poll and show it on screen
     C                   EXSR      LoadData
     C                   ENDSR
      **********************************************************************
      * Check Function keys pressed by the user
      **********************************************************************
     C     CheckFkeys    BEGSR
      * F5=Refresh
     C                   IF        *IN05 = *ON
     C                   EXSR      LoadData
     C                   ENDIF
      * F10=Change Vote/Confirm changes
     C                   IF        *IN10 = *ON
     C                   SELECT
     C                   WHEN      pMode = 'C'
     C                   EVAL      *IN80 = *ON
     C                   EXSR      CreatePoll
     C                   EVAL      *IN82 = *ON
     C                   WHEN      pMode = 'E'
     C                   EVAL      *IN80 = *ON
     C                   EXSR      UpdatePoll
     C                   WHEN      pMode = 'W'
     C                   IF        POLSTA = 'O'
     C                   EVAL      *IN81 = *OFF
     C                   EVAL      pMode = 'V'
     C                   EVAL      KEYLST = cVoting
     C                   ELSE
     C                   EVAL      MSGLIN = cErrNotOpen
     C                   ENDIF
     C                   WHEN      pMode = 'V'
     C                   IF        POLSTA = 'O'
     C                   EVAL      *IN81 = *ON
     C                   EXSR      ChangeVote
     C                   EXSR      LoadData
     C                   ELSE
     C                   EVAL      MSGLIN = cErrNotOpen
     C                   ENDIF
     C                   EVAL      pMode = 'W'
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
     C                   EVAL      wNumVotes = 0
     C                   EVAL      SCRNV1 = 0
     C                   EVAL      SCRNV2 = 0
     C                   EVAL      SCRNV3 = 0
     C                   EVAL      SCRNV4 = 0
     C                   EVAL      SCRNV5 = 0
     C                   EVAL      SCRPC1 = 0
     C                   EVAL      SCRPC2 = 0
     C                   EVAL      SCRPC3 = 0
     C                   EVAL      SCRPC4 = 0
     C                   EVAL      SCRPC5 = 0
     C     pPollUID      CHAIN     PPOLLSQ                            90
     C   90              GOTO      ENDLOAD
     C                   EVAL      SCRQUE = POLQUE
     C     *DMY          MOVEL     POLEND        SCREND
     C                   EVAL      SCRAN1 = POLAN1
     C                   EVAL      SCRAN2 = POLAN2
     C                   EVAL      SCRAN3 = POLAN3
     C                   EVAL      SCRAN4 = POLAN4
     C                   EVAL      SCRAN5 = POLAN5
      * Get number of votes for each answer
      *  and get current user's vote
     C     pPollUID      SETLL     PPOLLSA
     C     pPollUID      READE     PPOLLSA
     C                   DOW       NOT %EOF
     C                   SELECT
     C                   WHEN      ANSANS = 1
     C                   ADD       1             SCRNV1
     C                   IF        ANSUSR = wUser
     C                   EVAL      SCRA1V = 'X'
     C                   EVAL      wCurrentVote = ANSANS
     C                   ENDIF
     C                   WHEN      ANSANS = 2
     C                   ADD       1             SCRNV2
     C                   IF        ANSUSR = wUser
     C                   EVAL      SCRA2V = 'X'
     C                   EVAL      wCurrentVote = ANSANS
     C                   ENDIF
     C                   WHEN      ANSANS = 3
     C                   ADD       1             SCRNV3
     C                   IF        ANSUSR = wUser
     C                   EVAL      SCRA3V = 'X'
     C                   EVAL      wCurrentVote = ANSANS
     C                   ENDIF
     C                   WHEN      ANSANS = 4
     C                   ADD       1             SCRNV4
     C                   IF        ANSUSR = wUser
     C                   EVAL      SCRA4V = 'X'
     C                   EVAL      wCurrentVote = ANSANS
     C                   ENDIF
     C                   WHEN      ANSANS = 5
     C                   ADD       1             SCRNV5
     C                   IF        ANSUSR = wUser
     C                   EVAL      SCRA5V = 'X'
     C                   EVAL      wCurrentVote = ANSANS
     C                   ENDIF
     C                   ENDSL
     C     pPollUID      READE     PPOLLSA
     C                   ENDDO
      * Calculate percentages of each answer
     C                   ADD       SCRNV1        wNumVotes
     C                   ADD       SCRNV2        wNumVotes
     C                   ADD       SCRNV3        wNumVotes
     C                   ADD       SCRNV4        wNumVotes
     C                   ADD       SCRNV5        wNumVotes
     C                   IF        wNumVotes > 0
     C                   EVAL      SCRPC1 = (SCRNV1 / wNumVotes) * 100
     C                   EVAL      SCRPC2 = (SCRNV2 / wNumVotes) * 100
     C                   EVAL      SCRPC3 = (SCRNV3 / wNumVotes) * 100
     C                   EVAL      SCRPC4 = (SCRNV4 / wNumVotes) * 100
     C                   EVAL      SCRPC5 = (SCRNV5 / wNumVotes) * 100
     C                   ENDIF
     C     ENDLOAD       TAG
     C                   ENDSR
      **********************************************************************
      * Change vote
      * If the user has entered multiple X, we only take the first found
      **********************************************************************
     C     ChangeVote    BEGSR
     C                   IF        SCRA1V = 'X'
     C                   EVAL      wNewVote  = 1
     C                   GOTO      ENDCHGVOTE
     C                   ENDIF
     C                   IF        SCRA2V = 'X'
     C                   EVAL      wNewVote  = 2
     C                   GOTO      ENDCHGVOTE
     C                   ENDIF
     C                   IF        SCRA3V = 'X'
     C                   EVAL      wNewVote  = 3
     C                   GOTO      ENDCHGVOTE
     C                   ENDIF
     C                   IF        SCRA4V = 'X'
     C                   EVAL      wNewVote  = 4
     C                   GOTO      ENDCHGVOTE
     C                   ENDIF
     C                   IF        SCRA5V = 'X'
     C                   EVAL      wNewVote  = 5
     C                   GOTO      ENDCHGVOTE
     C                   ENDIF
     C     ENDCHGVOTE    TAG
     C     wNewVote      IFNE      wCurrentVote
     C     KPOLLSA       CHAIN     PPOLLSA
     C                   IF        %FOUND
      * Changing a vote on this poll
     C                   EVAL      ANSANS = wNewVote
     C                   UPDATE    RANSWR
     C                   EVAL      MSGLIN  = cMsgVoteChgnd
     C                   ELSE
      * First time voting on this poll
     C                   EVAL      ANSPUI = pPollUID
     C                   EVAL      ANSUSR = wUser
     C                   EVAL      ANSANS = wNewVote
     C                   WRITE     RANSWR                               91
     C  N91              EVAL      MSGLIN = cMsgVoteChgnd
     C                   ENDIF
     C                   ENDIF
     C                   ENDSR
      **********************************************************************
      * Update Poll
      **********************************************************************
     C     UpdatePoll    BEGSR
     C                   EVAL      POLQUE = SCRQUE
     C                   EVAL      POLAN1 = SCRAN1
     C                   EVAL      POLAN2 = SCRAN2
     C                   EVAL      POLAN3 = SCRAN3
     C                   EVAL      POLAN4 = SCRAN4
     C                   EVAL      POLAN5 = SCRAN5
     C     *DMY          MOVEL     SCREND        POLEND
     C                   UPDATE    RPOLL                                91
     C  N91              EVAL      MSGLIN = cMsgPollChgnd
     C                   ENDSR
      **********************************************************************
      * Create Poll
      **********************************************************************
     C     CreatePoll    BEGSR
     C                   EVAL      POLUID = SCRUID
     C                   EVAL      POLSTA = 'O'
     C                   EVAL      POLQUE = SCRQUE
     C                   EVAL      POLAN1 = SCRAN1
     C                   EVAL      POLAN2 = SCRAN2
     C                   EVAL      POLAN3 = SCRAN3
     C                   EVAL      POLAN4 = SCRAN4
     C                   EVAL      POLAN5 = SCRAN5
     C     *DMY          MOVEL     SCREND        POLEND
     C                   WRITE     RPOLL                                91
     C  N91              EVAL      MSGLIN = cMsgPollCrea
     C                   ENDSR
