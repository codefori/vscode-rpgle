     H/TITLE Window - Ask user to enter a new value (Text or Number)
      * SYSTEM      : V4R5
      * PROGRAMMER  : David Asta
      * DATE-WRITTEN: 21/NOV/2020
      *
      * This program shows a window that allows the user to enter a new
      *   value for a Text or a Number.
      **********************************************************************
     H/COPY DVBBS400/CURRENTSRC,CBKOPTIMIZ
      **********************************************************************
     FBBSWINASKWCF   E             WORKSTN
      **********************************************************************
      * Parameters
     D pMode           S              1A
     D pText           S             30A
     D pNumber         S              3P 0
     D pF3             S               N
      **********************************************************************
      * Receive Parameters
     C     *ENTRY        PLIST
     C                   PARM                    pMode
     C                   PARM                    pText
     C                   PARM                    pNumber
     C                   PARM                    pF3
      **********************************************************************
     C                   SELECT
     C                   WHEN      pMode = 'T'
     C                   EVAL      *IN50 = *ON
     C                   EXSR      AskText
     C                   WHEN      pMode = 'N'
     C                   EVAL      *IN50 = *OFF
     C                   EXSR      AskNumber
     C                   ENDSL
      * Exit this program
     C                   EVAL      pF3 = *IN03
     C                   EVAL      *INLR = *ON
     C                   RETURN
      **********************************************************************
      * Ask for a Text value
      **********************************************************************
     C     AskText       BEGSR
     C                   EVAL      NEWTXT = pText
     C                   EXFMT     WINGETVAL
      * If F3 was not pressed, update the parameter's value
     C                   IF        *IN03 = *OFF
     C                   EVAL      pText = NEWTXT
     C                   ENDIF
     C                   ENDSR
      **********************************************************************
      * Ask for a Number value
      **********************************************************************
     C     AskNumber     BEGSR
     C                   EVAL      NEWNUM = pNumber
     C                   EXFMT     WINGETVAL
      * If F3 was not pressed, update the parameter's value
     C                   IF        *IN03 = *OFF
     C                   EVAL      pNumber = NEWNUM
     C                   ENDIF
     C                   ENDSR
