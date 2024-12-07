     H/TITLE Generate the MD5 Hash of a string received by parameter
     H COPYRIGHT('(C) 2020 David Asta under MIT License')
      * SYSTEM      : V4R5
      * PROGRAMMER  : David Asta
      * DATE-WRITTEN: 13/NOV/2020
      *
      * This program generates an MD5 Hash of a string received by parameter
      * It's a slightly modified version of the one posted at
      *   https://archive.midrange.com/midrange-l/200006/msg01334.html
      *   by Bruce (bvinning) in 27th June 2000
      **********************************************************************
      * Compiler flags
     H/COPY DVBBS400/CURRENTSRC,CBKOPTIMIZ
     H DFTACTGRP(*NO) ACTGRP('QILE') BNDDIR('QC2LE')
      **********************************************************************
     D Cipher          PR                  extproc('_CIPHER')
     D                                 *   value
     D                                 *   value
     D                                 *   value
     D Convert         PR                  extproc('_XLATEB')
     D                                 *   value
     D                                 *   value
     D                               10U 0 value
     D cvthc           PR                  extproc('cvthc')
     D                                1
     D                                1
     D                               10I 0 value
     D Controls        DS
     D Function                       5I 0 inz(5)
     D HashAlg                        1    inz(x'00')
     D Sequence                       1    inz(x'00')
     D DataLngth                     10I 0 inz(15)
     D Unused                         8    inz(*LOVAL)
     D HaschCtxPt                      *   inz(%addr(HashWorkArea))
     D HashWorkArea    S             96    inz(*LOVAL)
     D Msg             S             50
     D ReceiverHex     S             16
     D ReceiverPtr     S               *   inz(%addr(ReceiverHex))
     D ReceiverChr     S             32
     D SourcePtr       S               *   inz(%addr(Msg))
     D StartMap        S            256
     D To819           S            256
     D CCSID1          S             10I 0 inz(37)
     D ST1             S             10I 0 inz(0)
     D L1              S             10I 0 inz(%size(StartMap))
     D CCSID2          S             10I 0 inz(819)
     D ST2             S             10I 0 inz(0)
     D GCCASN          S             10I 0 inz(0)
     D L2              S             10I 0 inz(%size(To819))
     D L3              S             10I 0
     D L4              S             10I 0
     D FB              S             12
     D                 DS
     D x                              5I 0
     D LowX                    2      2
     D pMode           S              1
     D pString         S             50
     D wStr4MSGUID     S             17
     D wStr4Passwd     S             20
      **********************************************************************
     C     *ENTRY        PLIST
     C                   PARM                    pMode
     C                   PARM                    pString
     C                   PARM                    ReceiverChr
     C                   SELECT
     C                   WHEN      pMode = 'M'
      * Mode M - We want to generate a Message UID
     C                   EVAL      wStr4MSGUID = pString
     C                   EVAL      Msg = wStr4MSGUID
     C                   WHEN      pMode = 'P'
      * Mode P - We want to hash a Password
     C                   EVAL      wStr4Passwd = pString
     C                   EVAL      Msg = wStr4Passwd
     C                   ENDSL
      * Get all single byte EBCDIC hex values
     C     0             do        255           x
     C                   eval      %subst(StartMap:x+1:1) = LowX
     C                   enddo
      * Get conversion table for 819 from 37
     C                   call      'QTQCVRT'
     C                   parm                    CCSID1
     C                   parm                    ST1
     C                   parm                    StartMap
     C                   parm                    L1
     C                   parm                    CCSID2
     C                   parm                    ST2
     C                   parm                    GCCASN
     C                   parm                    L2
     C                   parm                    To819
     C                   parm                    L3
     C                   parm                    L4
     C                   parm                    FB
      * Set message text
     C                   eval      DataLngth = %len(%trimr(Msg))
      * Now change Msg to 819 from 37 using MI
     C                   callp     Convert(%addr(Msg)
     C                                     :%addr(To819)
     C                                     :%size(Msg))
      * Get MD5 for Msg
     C                   callp     Cipher(%addr(ReceiverPtr)
     C                                    :%addr(Controls)
     C                                    :%addr(SourcePtr))
      * Convert nibbles to characters
     C                   callp     cvthc(ReceiverChr
     C                                   :ReceiverHex
     C                                   :%size(ReceiverChr))
      * Display the "proof"
     C*    ReceiverChr   dsply
     C                   eval      *INLR = *ON
     C                   return
