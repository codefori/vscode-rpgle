      * Get values from DTAARA and put them on screen
     C                   IN        wDTAARA
      *       Name of this BBS
     C                   EVAL      SCRBBS = wBBSNAM
      *       Nickname of logged in user
     C                   EVAL      SCRNCK = wUser
      *       Access Level of logged in user
     C                   MOVEL     wUserLvl      SCRLVL
      *       Description of Access Level
     C                   IF        wSHWALD = 'Y'
     C                   EVAL      SCRLVD = wLvlDescr
     C                   ELSE
     C                   EVAL      SCRLVD = *BLANK
     C                   ENDIF
     C                   UNLOCK    wDTAARA
