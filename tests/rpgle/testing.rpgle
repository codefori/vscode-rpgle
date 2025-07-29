      

      /copy qcpysrc,h_spec

      //*------------------------------------------------------------------------------------------*

       // Procedure prototypes for modul 'IQDBRTVFD'

      /copy qcpysrc,iqdbrtvfd
      /copy qcpysrc,rvfd0100
      /copy qcpysrc,qdbrtvfd
      /copy qcpysrc,qmhsndpm
      /copy qcpysrc,qmhrsnem
      /copy qcpysrc,types
      /copy qcpysrc,errc0100

     d main...
     d                 pr
     d                                     extproc('main')
     d  io_fldInf                 32767a         options(*varsize)              <-- Message informat
     d  i_lenFldInf                  10i 0 const                                --> Length of messag
     d  i_format                      8a   const                                --> Format name
     d  i_qFile                            const likeds(qObj_t)                 --> Qualified messag
     d  i_rcdFmt                     10a   const                                --> Return format co
     d  i_override                    1a   const                                --> Override proc.
     d  i_system                     10a   const                                --> System
     d  i_fmtType                    10a   const                                --> Format type
     d  io_errCode                32767a         options(*varsize)              <-> Error code

     d intToExtDataType...
     d                 pr             1a
     d                                     extproc('intToExtDataType')
     d  i_intDataType                 2a   const

     d intToExtUsage...
     d                 pr             1a
     d                                     extproc('intToExtUsage')
     d  i_intUsage                    1A   const

     d intToExtDatTimFmt...
     d                 pr            10a   varying
     d                                     extproc('intToExtDatTimFmt')
     d  i_intDataType                 2a   const
     d  i_intFmt                      1a   const

     d intToExtDatTimSep...
     d                 pr            10a   varying
     d                                     extproc('intToExtDatTimSep')
     d  i_intDataType                 2a   const
     d  i_intSep                      1a   const

     d isBinary...
     d                 pr              n
     d                                     extproc('isGraphic')
     d  i_intDataType                 2a   const

     d isDataLink...
     d                 pr              n
     d                                     extproc('isDataLink')
     d  i_intDataType                 2a   const

     d isGraphic...
     d                 pr              n
     d                                     extproc('isGraphic')
     d  i_intDataType                 2a   const

     d isNumeric...
     d                 pr              n
     d                                     extproc('isNumeric')
     d  i_intDataType                 2a   const

     d kill...
     d                 pr
     d                                     extproc('kill')
     d  i_msgText                   256a   const varying options(*varsize)

       // QDBRTVFD API data types
     d TYPE_BINARY...
     d                 c                   x'0000'
     d TYPE_FLOAT...
     d                 c                   x'0001'
     d TYPE_ZONED...
     d                 c                   x'0002'
     d TYPE_PACKED...
     d                 c                   x'0003'
     d TYPE_CHAR...
     d                 c                   x'0004'
     d TYPE_VAR_CHAR...
     d                 c                   x'8004'
     d TYPE_GRAPHIC...
     d                 c                   x'0005'
     d TYPE_VAR_GRAPHIC...
     d                 c                   x'8005'
     d TYPE_DBCS...
     d                 c                   x'0006'
     d TYPE_VAR_DBCS...
     d                 c                   x'8006'
     d TYPE_CLOB...
     d                 c                   x'4004'
     d TYPE_DATE...
     d                 c                   x'000B'
     d TYPE_TIME...
     d                 c                   x'000C'
     d TYPE_TIMESTAMP...
     d                 c                   x'000D'
     d TYPE_DECFLOAT...
     d                 c                   x'000F'
     d TYPE_DATALINK...
     d                 c                   x'8044'
     d TYPE_NULL...
     d                 c                   x'FFFF'

      //*==========================================================================================*
      //* Program entry point                                                                      *
      //*==========================================================================================*

     d IQDBRTVFD...
     d                 pi
     d  gio_fldInf                32767a         options(*varsize)              <-- Message informat
     d  gi_lenFldInf                 10i 0 const                                --> Length of messag
     d  gi_format                     8a   const                                --> Format name
     d  gi_qFile                           const likeds(qObj_t)                 --> Qualified messag
     d  gi_rcdFmt                    10a   const                                --> Return format co
     d  gio_errCode               32767a         options(*varsize)              <-> Error code
     d  gi_override                   1a   const options(*nopass)               --> Override proc.
     d  gi_system                    10a   const options(*nopass)               --> System
     d  gi_fmtType                   10a   const options(*nopass)               --> Format type

     d p_override      c                   7
     d p_system        c                   8
     d p_fmtType       c                   9

     d override        s                   like(gi_override)
     d system          s                   like(gi_system)
     d fmtType         s                   like(gi_fmtType)
      /free

       *inlr = *on;

       if (%parms() >= p_override and %addr(gi_override) <> *null);
          override = gi_override;
       else;
          override = IQDBRTVFD_OVERRIDE_NO;
       endif;

       if (%parms() >= p_system and %addr(gi_system) <> *null);
          system = gi_system;
       else;
          system = IQDBRTVFD_SYSTEM_LCL;
       endif;

       if (%parms() >= p_fmtType and %addr(gi_fmtType) <> *null);
          fmtType = gi_fmtType;
       else;
          fmtType = IQDBRTVFD_FMT_TYPE_EXT;
       endif;

       main(gio_fldInf: gi_lenFldInf: gi_format: gi_qFile
            : gi_rcdFmt: override: system: fmtType: gio_errCode);

      /end-free

      //*==========================================================================================*
      //* Main procedure                                                                           *
      //*==========================================================================================*
     p main...
     p                 b
     d                 pi
     d  io_fldInf                 32767a         options(*varsize)              <-- Message informat
     d  i_lenFldInf                  10i 0 const                                --> Length of messag
     d  i_format                      8a   const                                --> Format name
     d  i_qFile                            const likeds(qObj_t)                 --> Qualified messag
     d  i_rcdFmt                     10a   const                                --> Return format co
     d  i_override                    1a   const                                --> Override proc.
     d  i_system                     10a   const                                --> System
     d  i_fmtType                    10a   const                                --> Format type
     d  io_errCode                32767a         options(*varsize)              <-> Error code

     d header          ds                  likeds(iqdbrtvfd_header_t)
     d                                     based(pHeader)

     d fldInf          ds                  qualified based(pFldInf)
     d  bytRtn                       10i 0
     d  bytAvl                       10i 0

     d sizeFldInf      s             10i 0

     d qFileRet        ds                  likeds(qObj_t) inz

     d errCode         ds                  likeds(errCode_t)
     d                                     based(pErrCode)

     d offset          s             10i 0
     d numFields       s             10i 0
     d rvfd0100        ds                  likeds(rvfd0100_t)
     d                                     based(pRvfd0100)

       // Format Definition Header (Qdb_Qddfmt)
     d Qdb_Qddfmt      ds                  likeds(Qdb_Qddfmt_t)
     d                                     based(pQdb_Qddfmt)

       // Field Header (Qdb_Qddffld)
     d Qdb_Qddffld     ds                  likeds(Qdb_Qddffld_t)
     d                                     based(pQdb_Qddffld)

       // Field Text (Qdb_Qddfftxt)
     d Qdb_Qddfftxt    ds                  likeds(Qdb_Qddfftxt_t)
     d                                     based(pQdb_Qddfftxt)

     d isVarying       s               n
     d BLANK_TEXT      s                   like(Qdb_Qddfftxt.Qddfftxt)

     d rsnm0100        ds                  likeds(rsnm0100_t) inz
     d lclErrCode      ds                  likeds(errCode_t) inz
      /free

       if (i_lenFldInf < IQDBRTVFD_HEADER_MIN_SIZE);
          kill('Minimum length of field information is not valid: ' +
               %char(i_lenFldInf) +
               '. The minimum size is ' +
               %char(IQDBRTVFD_HEADER_MIN_SIZE) + ' bytes');
       endif;

       if (i_format <> 'RVFD0100');
          kill('Format ' + %trim(i_format) + ' is not valid. +
                The possible formats are: RVFD0100');
       endif;

       monitor;

          sizeFldInf = 1024 * 1024; // 1 MB
          pFldInf = %alloc(sizeFldInf);
          pHeader = %addr(io_fldInf);
          pErrCode = %addr(io_errCode);

          if (i_lenFldInf >= %size(header));
             clear header;
          else;
             %subst(header: 1: i_lenFldInf) = *ALLx'00';
          endif;

          numFields = 0;

          qdbrtvfd(fldInf: sizeFldInf: qFileRet: 'FILD0200'
                  : i_qFile: i_rcdFmt: '0': '*LCL': '*EXT'
                  : io_errCode);

          header.bytAvl = 0;
          header.bytRtn = IQDBRTVFD_HEADER_MIN_SIZE;

          if (i_lenFldInf >= IQDBRTVFD_MIN_SIZE_QFILE_AND_LIB_RTN);
             header.fileRtn = qFileRet.name;
             header.libRtn = qFileRet.lib;
             header.bytRtn += (%size(header.fileRtn) + %size(header.libRtn));
          endif;

          if (errCode.bytAvl = 0);

             offset = %size(header);

             // Get access to "Format Definition Header"
             pQdb_Qddfmt = pFldInf;

             header.bytAvl = %size(header) +
                             Qdb_Qddfmt.Qddffldnum * %size(rvfd0100);

             dow (header.bytRtn <= i_lenFldInf and
                  numFields < Qdb_Qddfmt.Qddffldnum);

                // get access to "Field Header" of first field
                // to get entry length
                if (numFields = 0);
                   pQdb_Qddffld = %addr(Qdb_Qddfmt.Qddffldx);
                else;
                   // get access to next field in list
                   pQdb_Qddffld = pQdb_Qddffld + Qdb_Qddffld.Qddfdefl;
                endif;

                if (header.bytRtn + %size(rvfd0100) <= i_lenFldInf);

                   numFields += 1;
                   header.bytRtn += %size(rvfd0100);

                   // Get access to "Field Text"
                   if Qdb_Qddffld.Qddftxtd = 0;
                      pQdb_Qddfftxt = %addr(BLANK_TEXT);
                   else;
                      pQdb_Qddfftxt = pQdb_Qddffld + Qdb_Qddffld.Qddftxtd;
                   endif;

                   // Determine if the field is a VARYING field
                   if (%bitand(x'20': Qdb_Qddffld.Qddffldst2) = x'20'
                       or Qdb_Qddffld.Qddfftyp = TYPE_VAR_CHAR      // var character Hack until PTF
                       or Qdb_Qddffld.Qddfftyp = TYPE_VAR_GRAPHIC); // var graphic   is available
                      isVarying = *ON;
                   else;
                      isVarying = *OFF;
                   endif;

                   // copy field description
                   pRvfd0100 = pHeader + offset;

                   clear rvfd0100;
                   rvfd0100.name = Qdb_Qddffld.Qddffldi;
                   rvfd0100.type = intToExtDataType(Qdb_Qddffld.Qddfftyp);
                   rvfd0100.use = intToExtUsage(Qdb_Qddffld.Qddffiob);

                   select;
                   when (isDataLink(Qdb_Qddffld.Qddfftyp));
                      if (Qdb_Qddffld.Qddffldb = Qdb_Qddffld.Qddffldb);
                         rvfd0100.length = Qdb_Qddffld.Qddffldb - 24;
                      else;
                         rvfd0100.length = Qdb_Qddffld.Qddffldb;
                      endif;
                   when (isNumeric(Qdb_Qddffld.Qddfftyp));
                      rvfd0100.length = Qdb_Qddffld.Qddffldd;
                   when (isGraphic(Qdb_Qddffld.Qddfftyp));
                      rvfd0100.length = Qdb_Qddffld.Qddffldd;
                   other;
                      if (isVarying);
                         rvfd0100.length = Qdb_Qddffld.Qddffldb - 2;
                      else;
                         rvfd0100.length = Qdb_Qddffld.Qddffldb;
                      endif;
                   endsl;

                   rvfd0100.decPos = Qdb_Qddffld.Qddffldp;
                   rvfd0100.inBuffOffs = Qdb_Qddffld.Qddffibo;
                   rvfd0100.outBuffOffs = Qdb_Qddffld.Qddffobo;
                   rvfd0100.buffLen = Qdb_Qddffld.Qddffldb;
                   rvfd0100.alcLen = Qdb_Qddffld.Qddflalc;
                   rvfd0100.ccsid = qdb_qddffld.Qddfcsid;
                   rvfd0100.isVarying = isVarying;

                   if (isBinary(Qdb_Qddffld.Qddfftyp));
                      rvfd0100.isBinary = '1';
                   else;
                      rvfd0100.isBinary = '0';
                   endif;

                   if (%bitand(x'80': Qdb_Qddffld.Qddffldst2) = x'80');
                      rvfd0100.isNullable = '1';
                   else;
                      rvfd0100.isNullable = '0';
                   endif;

                   rvfd0100.datTimFmt =
                      intToExtDatTimFmt(
                         Qdb_Qddffld.Qddfftyp: Qdb_Qddffld.Qddfdttf);

                   rvfd0100.datTimSep =
                      intToExtDatTimSep(
                         Qdb_Qddffld.Qddfftyp: Qdb_Qddffld.Qddfdtts);

                   rvfd0100.text = Qdb_Qddfftxt.Qddfftxt;

                   if (i_lenFldInf >= IQDBRTVFD_MIN_SIZE_NUM_FLD_RTN);
                      header.numFldRtn += 1;
                   endif;

                   if (header.numFldRtn = 1);
                      if (i_lenFldInf >= IQDBRTVFD_MIN_SIZE_OFFS_FIRST_FLD);
                         header.offsFirstFldD = offset;
                      endif;
                      header.bytRtn += %size(header.numFldRtn);
                      header.bytRtn += %size(header.offsFirstFldD);
                   endif;

                   // set offset to next field description
                   offset += %size(rvfd0100);

                endif;

             enddo;

          endif;

          if (pFldInf <> *null);
             dealloc(n) pFldInf;
          endif;

       on-error;

          if (pFldInf <> *null);
             dealloc(n) pFldInf;
          endif;

          rsnm0100.toStkCnt = 1;
          rsnm0100.toStkEMod = '*NONE';
          rsnm0100.toStkEPgm = '*NONE';
          rsnm0100.lenStkEQual = 10;
          rsnm0100.toStkE = '*PGMBDY';
          qmhrsnem('': lclErrCode: rsnm0100
                   : %size(rsnm0100): 'RSNM0100': '*': 0);
       endmon;

      /end-free
     p                 e

      //*==========================================================================================*
       //* Convert internal data type to external representation
      //*==========================================================================================*
     p intToExtDataType...
     p                 b
     d                 pi             1a
     d  i_intDataType                 2a   const

       // Return value
     d extDataType     s              1a   inz
      *-------------------------------------------------------------------
      /FREE

       select;
       when (i_inTDataType = TYPE_BINARY);
          extDataType = IQDBRTVFD_TYPE_BINARY;

       when (i_inTDataType = TYPE_FLOAT);
          extDataType = IQDBRTVFD_TYPE_FLOAT;

       when (i_inTDataType = TYPE_ZONED);
          extDataType = IQDBRTVFD_TYPE_ZONED;

       when (i_inTDataType = TYPE_PACKED);
          extDataType = IQDBRTVFD_TYPE_PACKED;

       when (i_inTDataType = TYPE_CHAR);
          extDataType = IQDBRTVFD_TYPE_CHAR;

       when (i_inTDataType = TYPE_VAR_CHAR);
          extDataType = IQDBRTVFD_TYPE_CHAR;

       when (i_inTDataType = TYPE_GRAPHIC);
          extDataType = IQDBRTVFD_TYPE_GRAPHIC;

       when (i_inTDataType = TYPE_DBCS);        // not yet supported
          extDataType = IQDBRTVFD_TYPE_ERROR;

       when (i_inTDataType = TYPE_CLOB);
          extDataType = IQDBRTVFD_TYPE_LOB;

       when (i_inTDataType = TYPE_VAR_GRAPHIC);
          extDataType = IQDBRTVFD_TYPE_GRAPHIC;

       when (i_inTDataType = TYPE_VAR_DBCS);    // not yet supported
          extDataType = IQDBRTVFD_TYPE_ERROR;

       when (i_inTDataType = TYPE_DATE);
          extDataType = IQDBRTVFD_TYPE_DATE;

       when (i_inTDataType = TYPE_TIME);
          extDataType = IQDBRTVFD_TYPE_TIME;

       when (i_inTDataType = TYPE_TIMESTAMP);
          extDataType = IQDBRTVFD_TYPE_TMSTMP;

       when (i_intDataType = TYPE_DATALINK);
          extDataType = IQDBRTVFD_TYPE_DATALINK;

       when (i_intDataType = TYPE_DECFLOAT);
          extDataType = IQDBRTVFD_TYPE_DECFLOAT;

       when (i_inTDataType = TYPE_NULL);        // not yet supported
          extDataType = IQDBRTVFD_TYPE_ERROR;

       other;
          extDataType = IQDBRTVFD_TYPE_ERROR;
       endsl;

       return extDataType;

      /end-free
     p                 e

      //*==========================================================================================*
       //* Convert internal usage to to external representation
      //*==========================================================================================*
     p intToExtUsage...
     p                 b
     d                 pi             1a
     d  i_intUsage                    1A   const

       // Return value
     d extUsage        S              1A   inz
      *-------------------------------------------------------------------
      /FREE

       select;
       when (i_intUsage = X'01');
          extUsage = IQDBRTVFD_USE_INPUT;

       when (i_intUsage = X'02');
          extUsage = IQDBRTVFD_USE_OUTPUT;

       when (i_intUsage = X'03');
          extUsage = IQDBRTVFD_USE_BOTH;

       when (i_intUsage = X'04');
          extUsage = IQDBRTVFD_USE_NEITHER;

       other;
          extUsage = IQDBRTVFD_USE_ERROR;
       endsl;

       return extUsage;

      /end-free
     p                 e

      //*==========================================================================================*
       //* Convert internal date/time format to to external representation
      //*==========================================================================================*
      *
     p intToExtDatTimFmt...
     p                 b
     d                 pi            10a   varying
     d  i_intDataType                 2a   const
     d  i_intFmt                      1a   const

     d extFmt          s             10a   varying
      /free

       if (i_intDataType = TYPE_DATE or
           i_intDataType = TYPE_TIME or
           i_intDataType = TYPE_TIMESTAMP);

          select;
          when (i_intFmt = X'FE');
             extFmt = IQDBRTVFD_DTFMT_JOB;

          when (i_intFmt = X'FF');
             extFmt = IQDBRTVFD_DTFMT_QDT;

          when (i_intFmt = X'01');
             extFmt = IQDBRTVFD_DTFMT_USA;

          when (i_intFmt = X'03');
             extFmt = IQDBRTVFD_DTFMT_ISO;

          when (i_intFmt = X'05');
             extFmt = IQDBRTVFD_DTFMT_EUR;

          when (i_intFmt = X'07');
             extFmt = IQDBRTVFD_DTFMT_JIS;

          when (i_intFmt = X'09');
             extFmt = IQDBRTVFD_DTFMT_SAA;

          when (i_intFmt = X'17');
             extFmt = IQDBRTVFD_DTFMT_MDY;

          when (i_intFmt = X'18');
             extFmt = IQDBRTVFD_DTFMT_DMY;

          when (i_intFmt = X'19');
             extFmt = IQDBRTVFD_DTFMT_YMD;

          when (i_intFmt = X'1A');
             extFmt = IQDBRTVFD_DTFMT_JUL;

          when (i_intFmt = X'1B');
             extFmt = IQDBRTVFD_DTFMT_HMS;

          when (i_intFmt = X'25');
             extFmt = IQDBRTVFD_DTFMT_CMDY;

          when (i_intFmt = X'26');
             extFmt = IQDBRTVFD_DTFMT_CDMY;

          when (i_intFmt = X'27');
             extFmt = IQDBRTVFD_DTFMT_CYMD;

          when (i_intFmt = X'28');
             extFmt = IQDBRTVFD_DTFMT_MDYY;

          when (i_intFmt = X'29');
             extFmt = IQDBRTVFD_DTFMT_DMYY;

          when (i_intFmt = X'2A');
             extFmt = IQDBRTVFD_DTFMT_YYMD;

          when (i_intFmt = X'2B');
             extFmt = IQDBRTVFD_DTFMT_YM;

          when (i_intFmt = X'2C');
             extFmt = IQDBRTVFD_DTFMT_MY;

          when (i_intFmt = X'2D');
             extFmt = IQDBRTVFD_DTFMT_YYM;

          when (i_intFmt = X'2E');
             extFmt = IQDBRTVFD_DTFMT_MYY;

          when (i_intFmt = X'30');
             extFmt = IQDBRTVFD_DTFMT_LONGJUL;

          other;
             extFmt = IQDBRTVFD_DTFMT_ERROR;
          endsl;

       else;
          extFmt = '';
       endif;

       return extFmt;

      /end-free
     P                 e

      //*==========================================================================================*
      //* converts the date separator from the internal format to the external representation.     *
      //*==========================================================================================*
     p intToExtDatTimSep...
     p                 b
     d                 pi            10a   varying
     d  i_intDataType                 2a   const
     d  i_intSep                      1a   const

       // Return value
     d extSep          s             10a   varying
      *-------------------------------------------------------------------
      /free

       if (i_intDataType = TYPE_DATE or
           i_intDataType = TYPE_TIME or
           i_intDataType = TYPE_TIMESTAMP);

          select;
          when (i_intSep = X'00');
             extSep = IQDBRTVFD_SEPARATOR_JOB;

          when (i_intSep = X'EE');
             extSep = IQDBRTVFD_SEPARATOR_IMPL;

          when (i_intSep = '/');
             extSep = IQDBRTVFD_SEPARATOR_SLASH;

          when (i_intSep = '-');
             extSep = IQDBRTVFD_SEPARATOR_DASH;

          when (i_intSep = '.');
             extSep = IQDBRTVFD_SEPARATOR_PERIOD;

          when (i_intSep = ' ');
             extSep = IQDBRTVFD_SEPARATOR_BLANK;

          when (i_intSep = ':');
             extSep = IQDBRTVFD_SEPARATOR_COLON;

          other;
             extSep = IQDBRTVFD_SEPARATOR_ERROR;

          endsl;

       else;
          extSep = '';
       endif;

       return extSep;

      /end-free
     p                 e

      //*==========================================================================================*
      //* checks, whether or not a given data type is a binary data type                           *
      //*==========================================================================================*
     p isBinary...
     p                 b
     d                 pi              n
     d  i_intDataType                 2a   const
      /free

       if (i_intDataType = TYPE_BINARY);
          return *ON;
       endif;

       return *OFF;

      /end-free
     p                 e

      //*==========================================================================================*
      //* checks, whether or not a given data type is a datalink data type                         *
      //*==========================================================================================*
     p isDataLink...
     p                 b
     d                 pi              n
     d  i_intDataType                 2a   const
      /free

       if (i_intDataType = TYPE_DATALINK);
          return *ON;
       endif;

       return *OFF;

      /end-free
     p                 e

      //*==========================================================================================*
      //* checks, whether or not a given data type is a graphic data type                          *
      //*==========================================================================================*
     p isGraphic...
     p                 b
     d                 pi              n
     d  i_intDataType                 2a   const
      /free

       if (i_intDataType = TYPE_GRAPHIC or    // graphic
           i_intDataType = TYPE_VAR_GRAPHIC); // varying graphic
          return *ON;
       endif;

       return *OFF;

      /end-free
     p                 e

      //*==========================================================================================*
      //* checks, whether or not a given data type is a numeric data type                          *
      //*==========================================================================================*
     p isNumeric...
     p                 b
     d                 pi              n
     d  i_intDataType                 2a   const
      /free

       if (i_intDataType = TYPE_BINARY or
           i_intDataType = TYPE_FLOAT or
           i_intDataType = TYPE_ZONED or
           i_intDataType = TYPE_PACKED);
          return *ON;
       endif;

       return *OFF;

      /end-free
     p                 e

      //*==========================================================================================*
      //* send an *ESCAPE message to kill the program.                                             *
      //*==========================================================================================*
     p kill...
     p                 b
     d                 pi
     d  i_msgText                   256a   const varying options(*varsize)

     d msgKey          s              4a   inz
     d qMsgF           ds                  likeds(qObj_t) inz
     d errCode         ds                  likeds(errCode_t) inz
      /free

       clear qMsgF;
       qMsgF.name = 'QCPFMSG';
       qMsgF.lib  = 'QSYS';

       clear errCode;
       errCode.bytPrv = %size(errCode);
       QMHSNDPM('CPF9898': qMsgF: i_msgText: %len(i_msgText): '*ESCAPE'
                : '*CTLBDY': 1: msgKey: errCode);

      /end-free
     p                 e