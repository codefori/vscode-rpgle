      * To use this Copybook:
      *   Declare:   D wMsgSta 1A
      *   Declare:   D wMsgStaTxt 10A
     C     GetMsgStatTxt BEGSR
     C                   SELECT
     C                   WHEN      wMsgSta = 'D'
     C                   EVAL      wMsgStaTxt = 'Deleted'
     C                   WHEN      wMsgSta = 'R'
     C                   EVAL      wMsgStaTxt = 'Read'
     C                   WHEN      wMsgSta = 'U'
     C                   EVAL      wMsgStaTxt = 'Unread'
     C                   ENDSL
     C                   ENDSR
