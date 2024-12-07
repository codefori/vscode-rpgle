     H/TITLE Window - Ask user to enter Y for Yes or N for No
     H COPYRIGHT('(C) 2020 David Asta under MIT License')
      * SYSTEM      : V4R5
      * PROGRAMMER  : David Asta
      * DATE-WRITTEN: 20/SEP/2020
      *
      * This program shows a window that allows the user to enter a single
      *   character that represent Yes or No to a specific question.
      **********************************************************************
     H/COPY DVBBS400/CURRENTSRC,CBKOPTIMIZ
      **********************************************************************
     FBBSWINYNW CF   E             WORKSTN
      **********************************************************************
      * Parameters
     D pQuestion       S             15A
     D pAnswer         S              1A
     D pF3             S               N
      **********************************************************************
      * Receive Parameters
     C     *ENTRY        PLIST
     C                   PARM                    pQuestion
     C                   PARM                    pAnswer
     C                   PARM                    pF3
      * Put the received parameter value on the screen
     C                   EVAL      QUESTION = pQuestion
      * Show Window and wait for user interaction
     C                   EXFMT     WINASK
      * If F3 was not pressed, update the parameter's value
     C                   IF        *IN03 = *OFF
     C                   EVAL      pAnswer = YESNO
     C                   ENDIF
      * Exit program
     C                   EVAL      pF3 = *IN03
     C                   EVAL      *INLR = *ON
