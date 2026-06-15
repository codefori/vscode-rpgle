      /DEFINE SYSEXITKEY
     H/include COZTOOLSRC/qcpysrc,cozHeader

     FASKDATE   CF   E             WORKSTN INFDS(WSDS) INDDS(indds)

     D PSDS           SDS                  Qualified
     D  pgmName          *PROC
     D  pgmLib                       10A   overlay(PSDS:81 )

     D WSDS            DS                  Qualified
     D  fKey                          1A   Overlay(wsds:369)

     D indds           DS                  Qualified
     D  mmyy                          1N   Overlay(indds:32)
     D  range                         1N   Overlay(indds:33)
     D  twoDates                      1N   Overlay(indds:42)
     D  yyyy                          1N   Overlay(indds:44)

     D rtnCLVar_T      DS                  Qualified
     D  attr                          1A
     D  type                          3U 0 overlay(attr)
     D  decpos                        3U 0
     D  length                        3U 0
     D  dec                           3U 0 Overlay(decPos)
     D  len                           3U 0 Overlay(Length)
     D  value                       256A

     D yearMonth       DS                  Inz
     D  date1YYYY                     4S 0
     D  date1YY                       2S 0 Overlay(date1YYYY:3)
     D  date2YYYY                     4S 0
     D  date2YY                       2S 0 Overlay(date2YYYY:3)

     D ad              DS                  Qualified Inz
     D  isoDate                       8S 0
     D  numDate                       8S 0
     D  charDate                      8A   Overlay(isoDate:1)
     D  realDate1                      D   datFmt(*ISO)
     D  realDate2                      D   datFmt(*ISO)
     D   isoYYYY                      4S 0 Overlay(isoDate:1)
     D   isoMM                        2S 0 Overlay(isoDate:5)
     D   isoDD                        2S 0 Overlay(isoDate:7)
     D   isoDay                       2S 0 Overlay(isoDate:7)
     D   isoYY                        2S 0 Overlay(isoDate:3)
     D   isoChar                      8A   Overlay(isoDate:1)
     D  realDate                       D   DatFmt(*ISO)
     D   realYYYY                     4S 0 Overlay(realDate:1)
     D   realYY                       2S 0 Overlay(realDate:3)
     D   realMM                       2S 0 Overlay(realDate:6)
     D   realDD                       2S 0 Overlay(realDate:9)
     D   realDay                      2S 0 Overlay(realDate:9)
     D   day                          2S 0 Overlay(realDate:9)
     D   realChar                    10A   Overlay(realDate:1)
     D  realChar1                    10A   Overlay(realDate1:1)
     D  realChar2                    10A   Overlay(realDate2:1)
     D  dateFmt                      10A
     D  rtnFmt1                      10A
     D  rtnFmt2                      10A


     D dates_t         DS                  Qualified Inz
     D  count                         5I 0
     D  date                          7A
     D  title                        35A   Varying


     D checkDate       PR            10I 0
     D   inDate                      10A   Const
     D   inFmt                       10A   Const

     D cvtToRtnDate    PR            10A   Varying
     D  inValue                      10A   Const
     D  inFmt                        10A   Const
     D  toFmt                        10A   Const

      /include COZTOOLSRC/qcpysrc,msg
      /include COZTOOLSRC/qcpysrc,scanrpl
      /include COZTOOLSRC/qcpysrc,apiProtos
      /include COZTOOLSRC/qcpysrc,job

      /include COZTOOLSRC/qcpysrc,convert
      /include COZTOOLSRC/qcpysrc,dates
      /include COZTOOLSRC/qcpysrc,fKeys

     D entryPList      PR                  extpgm('ASKDATE')
     D  dateFmt                      10A   Const
     D  dateSep                       1A   Const
     D  date1                              Const LikeDS(dates_T)
     D  date2                              Const LikeDS(dates_T)
     D  titles                             Const LikeDS(titles_T)
         // Returned date (output)
     D rtnDate                             LikeDS(rtnCLVar_T)
     D                                     OPTIONS(*NOPASS:*OMIT)
         // Date format of returned date (input)
     D rtnFmt                        10A   Varying  Const
     D                                     OPTIONS(*NOPASS:*OMIT)
         // Date to return (input)
     D rtnDate2                            LikeDS(rtnCLVar_T)
     D                                     OPTIONS(*NOPASS:*OMIT)
         // Date format of returned date (input)
     D rtnFmt2                       10A   Varying Const
     D                                     OPTIONS(*NOPASS:*OMIT)
     D  rtnMM                         2P 0 OPTIONS(*NOPASS:*OMIT)
     D  rtnYY                         4P 0 OPTIONS(*NOPASS:*OMIT)
     D  rtnMM2                        2P 0 OPTIONS(*NOPASS:*OMIT)
     D  rtnYY2                        4P 0 OPTIONS(*NOPASS:*OMIT)
         // Date to return (input)
     D  rtnFKey                       1A   OPTIONS(*NOPASS:*OMIT)

     D entryPList      PI
     D  dateFmt                      10A   Const
     D  dateSep                       1A   Const
     D  date1                              Const LikeDS(dates_T)
     D  date2                              Const LikeDS(dates_T)
     D  titles                             Const LikeDS(titles_T)
         // Returned date (output)
     D rtnDate                             LikeDS(rtnCLVar_T)
     D                                     OPTIONS(*NOPASS:*OMIT)
         // Date format of returned date (input)
     D rtnFmt1                       10A   Varying  Const
     D                                     OPTIONS(*NOPASS:*OMIT)
         // Date to return (input)
     D rtnDate2                            LikeDS(rtnCLVar_T)
     D                                     OPTIONS(*NOPASS:*OMIT)
         // Date format of returned date (input)
     D rtnFmt2                       10A   Varying Const
     D                                     OPTIONS(*NOPASS:*OMIT)
     D  rtnMM                         2P 0 OPTIONS(*NOPASS:*OMIT)
     D  rtnYY                         4P 0 OPTIONS(*NOPASS:*OMIT)
     D  rtnMM2                        2P 0 OPTIONS(*NOPASS:*OMIT)
     D  rtnYY2                        4P 0 OPTIONS(*NOPASS:*OMIT)
         // fKey pressed to return from AskDate
     D  rtnFKey                       1A   OPTIONS(*NOPASS:*OMIT)

     D addDots         PR            35A
     D  inValue                      35A   Const Varying


     D date_T          DS                  Qualified Inz
     D  date                           D   DATFMT(*ISO)
     D  MM                            2S 0 Overlay(date:6)
     D  DD                            2S 0 Overlay(date:9)
     D  YY                            2S 0 Overlay(date:3)
     D  YYYY                          4S 0 Overlay(date:1)
     D theDate         DS                  LikeDS(date_T) Inz(*LIKEDS)
     D myDate          S               D   Inz(D'0001-01-01') DATFMT(*ISO)
     D bOneDate        S              1N   Inz(*OFF)
     D bPeriod         S              1N   Inz(*OFF)

     D titles_T        DS                  Qualified Inz
     D  count                         5I 0
     D  title                        35A   Varying
     D  subtitle1                    32A   Varying
     D  subtitle2                    32A   Varying
     D  promptText                   60A   Varying

     D Sep             S              1A
     D fmt             S             10A
     D jobDateFmt      S              5A
     D rtnValue        S             10A
     D center          S             35A   Varying
     D len             S             10I 0
     C                   MOVE      *ON           *INLR
      /free
           bOneDate = (%subst(date2.date:2) = '999999');
           bPeriod = (ad.dateFmt = 'MMYYYY') or (ad.dateFmt = 'YYYYMM') or
                     (ad.dateFmt = 'MMYY')   or (ad.dateFmt = 'YYMM');

           indds.yyyy = (ad.dateFmt = 'MMYYYY') or (ad.dateFmt = 'YYYYMM');
           indds.twoDates = NOT bOneDate;

           if (dateSep = '0');
             evalR Sep = %trimR(ad.rtnFmt1);
             if (%check(':;,./\-=+&0':Sep) = 1);
               ad.rtnFmt1 = %trimR(ad.rtnFmt1) + '0';
             endif;
           endif;

           if (dateSep = '0');
              evalR Sep = %trimR(ad.rtnFmt2);
              if (%check(':;,./\-=+&0':Sep) = 1);
                ad.rtnFmt2 = %trimR(ad.rtnFmt2) + '0';
              ENDIF;
           ENDIF;

           if (titles.title = '');
              if (date2.date = '0000000'); // *DFT?
                 userTitle = getMsgText('COZ7601' :
                                %trimR(psds.pgmLib) + '/COZTOOLS');
                 promptext = getMsgText('COZ7603' :
                                %trimR(psds.pgmLib) + '/COZTOOLS');
              else;
                 userTitle = getMsgText('COZ7602' :
                                %trimR(psds.pgmLib) + '/COZTOOLS');
                 promptext = getMsgText('COZ7605' :
                                %trimR(psds.pgmLib) + '/COZTOOLS');
              endif;
           else;
              userTitle = titles.title;
           endif;

           if (titles.subtitle1 = '');   // *DFT?
              subTitle = '';
           else;
              subTitle = titles.subtitle1;
           endif;

           if (titles.subtitle2 = '');   // *DFT?
              subTitle2 = '';
           else;
              subTitle2 = titles.subtitle2;
           endif;
           if (titles.promptText <> '');   // *DFT?
              prompText = titles.promptText;
           endif;

           len = %checkR(' ' : userTitle);
           if (len < %size(userTitle) and len > 1);
              len = %size(userTitle) - len;
              %len(center) = %div(len:2);
              userTitle = center + userTitle;
           endif;
           len = %checkR(' ' : subTitle);
           if (len < %size(subTitle) and len > 1);
              len = %size(subTitle) - len;
              %len(center) = %div(len:2);
              subTitle = center + subTitle;
           endif;
           len = %checkR(' ' : subTitle2);
           if (len < %size(subTitle2) and len > 1);
              len = %size(subTitle2) - len;
              %len(center) = %div(len:2);
              subTitle2 = center + subTitle2;
           endif;

               // ONE DATE PROMPT?
           if (bOneDate);
              if (date1.title = '');
                date1Text = getMsgText('COZ7609' :
                                %trimR(psds.pgmLib) + '/COZTOOLS');
              else;
                date1Text = date1.title;
              endif;
           else;
              if (date1.title = '');
                date1Text = getMsgText('COZ7607' :
                                %trimR(psds.pgmLib) + '/COZTOOLS');
              else;
                date1Text = date1.title;
              endif;
              if (date2.title = '');
                date2Text = getMsgText('COZ7608' :
                                %trimR(psds.pgmLib) + '/COZTOOLS');
              else;
                date2Text = date2.title;
              endif;
           endif;
           date1Text = addDots( date1Text );
           date2Text = addDots( date2Text );

           if (%subst(date1.date:2) = '111111');      // *CURRENT/*JOB date
              ad.realDate = %date(*DATE);
           elseif (%subst(date1.date:2) = '222222');  // *SYSDATE date
              ad.realDate = %date();
           elseif (%subst(date1.date:2) = '333333');  // *BOM (begining of Month)
              ad.realDate = %date();
              ad.day = 1;
           elseif (%subst(date1.date:2) = '444444');  // *EOM (end of Month)
              ad.realDate = %date();
              ad.day = 1;
              ad.realDate = ad.realDate + %Months(1);
              ad.realDate = ad.realDate - %Days(1);
           elseif (%subst(date1.date:2) = '555555');  // *PRVEOM (previous end of Month)
              ad.realDate = %date();
              ad.day = 1;
              ad.realDate = ad.realDate - %days(1);
           elseif (%subst(date1.date:2) = '000000');
              clear ad.realDate;
           else;
              test(DE) *CYMD0 date1.date;
              if NOT %ERROR();
                ad.realDate = %date(date1.date:*CYMD0);
                date1MM = %subDT(ad.realDate:*MONTHS);
                date1YYYY = %subDt(ad.realDate:*YEARS);
              endif;
           endif;
           test(e) ad.realDate;
           if (%error() or ad.realDate = D'0001-01-01');
             clear ad.realDate1;
             clear date1Date;
             clear date1yyyy;
             clear date1yy;
             clear date1mm;
           else;
             ad.realDate1 = ad.realDate;
             ad.isoChar   = %char(ad.realDate:*ISO0);
             date1Date = cvtDateEx( ad.isoChar : '*ISO' : ad.datefmt);
             date1MM   = ad.isoMM;
             date1YYYY = ad.isoYYYY;
           endif;

           if (NOT bOneDate);
             if (%subst(date2.date:2) = '111111');      // *CURRENT/*JOB date
                ad.realDate = %date(*DATE);
             elseif (%subst(date2.date:2) = '222222');  // *SYSDATE date
                ad.realDate = %date();
             elseif (%subst(date2.date:2) = '333333');  // *BOM (begining of Month)
                ad.realDate = %date();
                ad.day = 1;
             elseif (%subst(date2.date:2) = '444444');  // *EOM (end of Month)
                ad.realDate = %date();
                ad.day = 1;
                ad.realDate = ad.realDate + %Months(1);
                ad.realDate = ad.realDate - %Days(1);
             elseif (%subst(date2.date:2) = '555555');  // *PRVEOM (previous end of Month)
                ad.realDate = %date();
                ad.day = 1;
                ad.realDate = ad.realDate - %days(1);
             elseif (%subst(date2.date:2) = '666666');  // *FROMDATE
                ad.realDate = ad.realDate1;
             elseif (%subst(date2.date:2) = '000000');
                clear ad.realDate;
             else;
                 test(DE) *CYMD0 date2.date;
                 if NOT %ERROR();
                   ad.realDate = %date(date2.date:*CYMD0);
                   date2MM = %subDT(ad.realDate:*MONTHS);
                   date2YYYY = %subDt(ad.realDate:*YEARS);
                 endif;
              endif;
              test(e) ad.realDate;
              if (%error() or ad.realDate = D'0001-01-01');
                clear ad.realDate2;
                clear date2Date;
                clear date2yyyy;
                clear date2yy;
                clear date2mm;
              else;
                ad.realDate2 = ad.realDate;
                ad.isoChar   = %char(ad.realDate:*ISO0);
                date2Date = cvtDateEx( ad.isoChar : '*ISO' : ad.datefmt);
                date2MM   = ad.isoMM;
                date2YYYY = ad.isoYYYY;
              endif;
           endif;

         dou ((wsds.fKey = coz.Enter or wsds.fKey = coz.F10) and errMsg='');
           if (bPeriod);
             if (%subst(ad.dateFmt:1:1) = 'Y');
               exfmt askYYYYMM;   // YY[YY] / MM
             else;
               exfmt askMMYYYY;   // MM / YY[YY]
             endif;
           else;
             exfmt askDate2;
           endif;

           if (wsds.fKey = coz.Enter);
             if (bPeriod);
               if (date1yyyy < 100);
                 date1yyyy += 2000;  // Default to 20xx when only YY is entered.
               endif;
               if (NOT bOneDate);
                 if (date2yyyy < 100);
                   date2yyyy += 2000;  // Default to 20xx when only YY is entered.
                 endif;
               endif;
             endif;
           endif;

           if (%Addr(rtnFKey) <> *NULL);
              rtnfKey = wsds.fKey;
           endif;
           if (wsds.fKey = coz.F3 or wsds.fKey = coz.F12);
              setExitKey( wsds.fkey );
              return;
           endif;

           if (wsds.fKey = F5);  // F5=Refresh
              reset date1Date;
              reset date2Date;

              reset date2MM;
              reset date2YY;
              reset date1MM;
              reset date1YY;
           endif;
           if (checkDate( date1Date : ad.dateFmt ) <> 0); // Date Error?
             errMsg = 'Invalid date 1';
             iter;
           endif;
           if ((NOT bOneDate) and checkDate( date2Date : ad.dateFmt) <> 0); // Date Error?
             errMsg = 'Invalid date 2';
             iter;
           endif;
           clear errMsg;
         enddo;

                // We store the date in the date1Date just in case
                // the caller requests that the MMYYYY date be
                // returned as a real date. We also default to
                // 1 for the day of the month, in this context.
           if (bPeriod);
             ad.isoyyyy = date1yyyy;
             ad.isomm   = date1mm;
             ad.isodd   = 1;
             ad.realDate1 = %date(ad.isoDate:*ISO);
             date1Date = cvtDateEx( ad.isoChar : '*ISO' : ad.dateFmt);
             if (NOT bOneDate);
                ad.isoyyyy = date2yyyy;
                ad.isomm   = date2mm;
                ad.isodd   = 1;
                ad.realDate2 = %date(ad.isoDate:*ISO);
                date2Date = cvtDateEx( ad.isoChar : '*ISO' : ad.dateFmt);
             endif;
           else;
             ad.isoChar = cvtDateEx( date1Date : ad.dateFmt : '*ISO0');
             ad.realDate1 = %date(ad.isoDate:*ISO);
             if (NOT bOneDate);
                ad.isoChar = cvtDateEx( date2Date : ad.dateFmt : '*ISO0');
                ad.realDate2 = %date(ad.isoDate:*ISO);
             endif;
           endif;

             // Store DATE1DATE into the base RTNDATE variable;
           if (date1date <> '' and %addr(rtnDate) <> *NULL);
             if (ad.rtnFmt1 <> '');
                fmt = ad.rtnFmt1;
             else;
                fmt = ad.dateFmt;
             endif;
             if (%addr(rtnDate) <> *NULL and rtnDate.attr = coz.MI_TYPE.T_Char);
                 %subst(rtnDate.value:1:rtnDate.len) =
                        cvtDateEx( ad.realChar1 : '*ISO' : ad.rtnFmt1);
             else;
               rtnValue = cvtDateEx(ad.realChar1:'*ISO': %TrimR(fmt:' 0')+'0');
                 // Convert the date to the CL variable RTNVAR format
                cvtToDec( %addr(rtnValue) : coz.MI_TYPE.T_Zoned :
                          %len(%trimR(rtnValue)) : 0 :
                          %addr(rtnDate.Value) : rtnDate.attr :
                                rtnDate.len    : rtnDate.decpos );
             endif;
           endif;

             // Store DATE2DATE into the base RTNDATE2 variable;
           if ((NOT bOneDate) and
               date2date <> '' and  %addr(rtnDate2) <> *NULL);
             if (%subst(date2Date:1:2) = '*F') or       // *Fromdate
                 (%subst(date2Date:1:1) = 'F')  or      // Fromdate
                 (%subst(date2Date:1:1) = 'S')  or      // Same
                 (%subst(date2Date:1:2) = '*S');        // *Same
                ad.realDate2 = ad.realDate1;
             elseif (%subst(date2Date:1:2) = '*C') or   // *Current
                    (%subst(date2Date:1:1) = 'C')  or   // Current
                    (%subst(date2Date:1:4) = '*TOD') or // *Today
                    (%subst(date2Date:1:3) = 'TOD');    // Today
                ad.realDate2 = %Date();
             elseif (%subst(date2Date:1:2) = '*Y') or   // *Yesterday
                    (%subst(date2Date:1:1) = 'Y');      // Yesterday
                ad.realDate2 = %Date() - %days(1);
             elseif (%subst(date2Date:1:4) = '*TOM') or // *Tomorrow
                    (%subst(date2Date:1:3) = 'TOM');    // Tomorrow
                ad.realDate2 = %Date() + %days(1);
             endif;

             if (ad.rtnFmt2 <> '' and %addr(rtnFmt2) <> *NULL);
               fmt = ad.rtnFmt2;
             else;
               fmt = date2Fmt;
             endif;

             if (rtnDate2.attr = coz.MI_TYPE.T_Char);
                 %subst(rtnDate2.value:1:rtnDate2.len) =
                        cvtDateEx( ad.realChar2 : '*ISO' : fmt);
             else;
               rtnValue = cvtDateEx(ad.realChar2:'*ISO':%TrimR(fmt)+'0');
                 // Convert the date to the CL variable RTNVAR format
                cvtToDec( %addr(rtnValue) : coz.MI_TYPE.T_Zoned :
                          %len(%trimR(rtnValue)) : 0 :
                          %addr(rtnDate2.Value) : rtnDate2.attr :
                                rtnDate2.len    : rtnDate2.decpos );
             endif;
           endif;


           ad.isoDate = %dec(ad.realDate1:*ISO);
           if (%addr(rtnMM) <> *NULL);
             if (bPeriod);
               rtnMM = date1MM;
             else;
               rtnMM = ad.isoMM;
             endif;
           endif;
           if (%addr(rtnYY) <> *NULL);
             if (%scan('YYYY':ad.datefmt) > 0);
               rtnYY = date1YYYY;
             elseif  (%scan('YY':ad.datefmt) > 0);
               rtnYY = date1YY;
             else;
               rtnYY = ad.isoYYYY;
             endif;
           endif;
           if (NOT bOneDate);
            ad.isoDate = %dec(ad.realDate2:*ISO);
            if (%addr(rtnMM2) <> *NULL);
              if (bPeriod);
                rtnMM2 = date2MM;
              else;
                rtnMM2 = ad.isoMM;
              endif;
            endif;
            if (%addr(rtnYY2) <> *NULL);
              if (%scan('YYYY':ad.datefmt) > 0);
                rtnYY2 = date2YYYY;
              elseif  (%scan('YY':ad.datefmt) > 0);
                rtnYY2 = date2YY;
              else;
                rtnYY2 = ad.isoYYYY;
              endif;
            endif;
           endif;


      /end-free
     CSR   *INZSR        BEGSR
      /free
           program = psds.pgmname;
           clear ad; // Clear any prior remnants.

           ad.dateFmt = %trim(dateFmt: '*/-.&;:, ');
           date1Fmt = ad.dateFmt;
           if (date1Fmt = 'JOB');
              getJobNLS( '*' : *OMIT : *OMIT : *OMIT : *OMIT : jobDateFmt );
              jobDateFmt=%subst(jobDateFmt:2:3);
              date1Fmt = ScanRpl('M' : 'MM' : jobDateFmt);
              date1Fmt = ScanRpl('D' : 'DD' : date1Fmt);
              date1Fmt = ScanRpl('Y' : 'YY' : date1Fmt);
           endif;
           if (date1Fmt = 'USA');
              date1Fmt = 'MMDDYYYY';
           elseif (date1Fmt = 'ISO');
              date1Fmt = 'YYYYMMDD';
           elseif (date1Fmt = 'JIS');
              date1Fmt = 'YYYYMMDD';
           elseif (date1Fmt = 'EUR');
              date1Fmt = 'DDMMYYYY';
           endif;

           date2Fmt = date1Fmt;

           if (date2.date = '0000000') or  // One Date only?
                  (date1fmt = 'MMYY' or date1Fmt = 'MMYYYY');
                 userTitle = getMsgText('COZ7601' :
                                %trimR(psds.pgmLib) + '/COZTOOLS');
                 promptext = getMsgText('COZ7603' :
                                %trimR(psds.pgmLib) + '/COZTOOLS');
              else;   // Two dates?
                 userTitle = getMsgText('COZ7602' :
                                %trimR(psds.pgmLib) + '/COZTOOLS');
                 promptext = getMsgText('COZ7605' :
                                %trimR(psds.pgmLib) + '/COZTOOLS');
           endif;

           if (date1fmt = 'MMYY' or date1Fmt = 'MMYYYY');
              test(DE) *CYMD0 date1.date;
              if NOT %ERROR();
                 myDate = %Date(date1.date:*CYMD0);
              elseif (date1.date = '0010000');  // *JOB/*CURRENT?
                 myDate = %Date();
              elseif (date1.date = '0020000');  // *EOM?
                 myDate = GetEndOfMonth();
              endif;
              if (myDate <> D'0001-01-01');
                 date1MM = %subdt(myDate:*MONTHS);
                 date1YYYY = %subdt(myDate:*YEARS);
              endif;
           endif;


            // *BLANKS   = 000000
            // *JOB      = 111111
            // *SYS      = 222222
            // *BOM      = 333333
            // *EOM      = 444444
            // *PREVEOM  = 555555
            // *NONE     = 999999

           if (%addr(date1) <> *NULL);
             if (%subst(date1.date:2) = '000000' or date1.date = '');  // *NONE or *BLANKS?
                 date1Date = '';
             elseif (%subst(date1.date:2) = '111111');  // *JOB/*CURRENT?
                     date1Date = %char(%Date(*DATE) : *JOBRUN);
             elseif (%subst(date1.date:2) = '222222');  // *System Date?
                     date1Date = %char(%Date() : *JOBRUN);
             elseif (%subst(date1.date:2) = '444444');  // *EOM?
                     date1Date = %char( GetEndOfMonth() : *JOBRUN);
             elseif (%subst(date1.date:2) = '333333');  // *BOM? (first of month)
                     date1Date = %char( %date() -
                                 %days(%subdt( %date() : *DAYS)-1) : *JOBRUN);
             elseif (%subst(date1.date:2) = '555555');  // *PREVEOM?
                     theDate.date = %date() - %MONTHS(1);
                     date1Date = %char( getEndOfMonth(theDate.date) : *JOBRUN);
             else;
                test(DE) *CYMD0 date1.date;
                if NOT %ERROR();
                   date1Date = %char(%Date(date1.date:*CYMD0):*JOBRUN);
                else;
                   date1Date = %char(%Date() : *JOBRUN);
                endif;
             endif;
           endif;

           if (%addr(date2) <> *NULL and
               %subst(date2.date:2) <> '999999' and
               %subst(date2.date:2) <> '');
             if (date2.date = '0000000' or %subst(date2.date:2) = '');  // *NONE or *BLANKS?
                     date2Date = '';
             elseif (%subst(date2.date:2) = '111111');  // *JOB/*CURRENT?
                     date2Date = %char(%Date() : *JOBRUN);
             elseif (%subst(date2.date:2) = '222222');  // *SYS Date
                     date2Date = %char(%Date() : *JOBRUN);
             elseif (%subst(date2.date:2) = '444444');  // *EOM?
                     date2Date = %char( GetEndOfMonth() : *JOBRUN);
             elseif (%subst(date2.date:2) = '333333');  // *BOM? (first of month)
                     date2Date = %char( %date() -
                                 %days(%subdt( %date() : *DAYS)-1) : *JOBRUN);
             elseif (%subst(date2.date:2) = '555555');  // *PREVEOM?
                     theDate.date = %date() - %MONTHS(1);
                     date2Date = %char( getEndOfMonth(theDate.date) : *JOBRUN);
             elseif (%subst(date2.date:2) = '666666');  // *FROMDATE?
                   date2Date = '*FROMDATE';
             else;
                test(DE) *CYMD0 date2.date;
                if NOT %ERROR();
                   date2Date = %char(%Date(date2.date:*CYMD0):*JOBRUN);
                else;
                   date2Date = %char(%Date() : *JOBRUN);
                endif;
             endif;
           endif;

           if (date1Date <> '' and date1Date <> *ALL'0' and
                 (dateFmt <> '*JOB' and dateFmt <> '*JOBRUN') );
              date1Date = cvtDateEx( date1Date  : '*JOB' : dateFmt);
           endif;
           if (date2Date <> '' and date2Date <> *ALL'0' and
               date2Date <> '*FROMDATE' and
                 (dateFmt <> '*JOB' and dateFmt <> '*JOBRUN') );
             date2Date = cvtDateEx( date2Date : '*JOB' : dateFmt);
           endif;
           if (%addr(rtnFmt1) <> *NULL and rtnFmt1 <> '');
             ad.rtnFmt1 = rtnFmt1;
           endif;
           if (%addr(rtnFmt2) <> *NULL and rtnFmt2 <> '');
             if (rtnFmt2 = '*FMT1' or rtnFmt2 = '*FMT');
               ad.rtnFmt2 = rtnFmt1;
             else;
             ad.rtnFmt2 = rtnFmt2;
           endif;
           endif;
      /end-free
     CSR   ENDINZSR      ENDSR

     P addDots         B
     D addDots         PI            35A
     D  inValue                      35A   Const Varying
     D dots            S             35A
      /free
           if (inValue <> '' and %len(inValue) > 0);
           evalR dots = *ALL' .';
           %subst(dots:1:%len(%TrimR(inValue))+1) = %trimR(inValue);
           if (%subst(dots:35:1) = ' ');
               dots = %trimR(dots) + ':';
           endif;
           return dots;
           endif;
           return invalue;
      /end-free
     P addDots         E


     P cvtToRtnDate    B
     D cvtToRtnDate    PI            10A   Varying
     D  inValue                      10A   Const
     D  inFmt                        10A   Const
     D  toFmt                        10A   Const

     D fromDate        S             10A
     D toDate          S             10A
     D dateFmt         S             10A   Inz('*ISO')
     D rtnValue        S             10A
      /free
           fromDate = scanRpl('/' : '' : inValue);
           fromDate = scanRpl(':' : '' : fromDate);
           fromDate = scanRpl('.' : '' : fromDate);
           fromDate = scanRpl('-' : '' : fromDate);
           if (toFmt <> '');
              dateFmt = toFmt;
           endif;
           monitor;
            toDate = cvtDateEx( inValue : inFmt : dateFmt );
            ad.isoChar = cvtDateEx( inValue : inFmt : '*ISO0' );
           on-error;
              return '*ERR';
           endmon;
           rtnValue = toDate;
           len = %Len(%TrimR(rtnValue));
           if (len = 3 or len = 5 or len = 7);
              rtnValue = '0' + rtnValue;
           endif;
           return %trimR(rtnValue);
      /end-free
     P cvtToRtnDate    E

     P checkDate       B
     D checkDate       PI            10I 0
     D   inDate                      10A   Const
     D   inFmt                       10A   Const
     D useFmt          S             10A
     D useDate         S             10A
     D isoDate         S             10A
     D ddPos           S             10I 0
      /free
           if (inFmt = 'MMYY' or inFmt = 'MMYYYY' or
               inFmt = 'YYMM' or inFmt = 'YYYYMM');  // Month & Year only?
             useFmt = ScanRpl('MM' : 'MMDD' : inFmt);
             ddPos = %Scan('DD' : useFmt);
             if (ddPos > 0);
               useDate = %Replace('01':inDate:ddPos:0); // Insert Day(01)
             endif;
           else;
             useDate = inDate;
             useFmt = inFmt;
           endif;
           monitor;
           isoDate = cvtDateEx(useDate : useFmt : '*ISO');
           on-error;
             return -1;
           endmon;
           test(DE) *ISO isoDate;
           if %error();
             return -1;
           endif;
           return 0;
      /end-free
     P checkDate       E