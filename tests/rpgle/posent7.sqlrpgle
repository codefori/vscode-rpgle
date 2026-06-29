     H BNDDIR('QC2LE') DFTACTGRP(*NO) ACTGRP('POS')  EXTBININT(*YES)
     H OPTION(*NODEBUGIO : *SRCSTMT)  FIXNBR(*INPUTPACKED : *ZONED)
     H BNDDIR('AIDBD1' : 'VAT')

      *    <----------- I M P O R A N T   N O T E ------------->
      *
      * When testing for Mod U2804 I found that after you use F11 to
      * bring up the Sub-Field line to key 'Y' for Defective that if
      * you hit F12 after keying 'Y' that when the Order is re-accessed
      * the 'Y' is not in the Defective Field.  In order for the 'Y'
      * to be updated you must hit <Enter> then F12 to suspend the order.
      *

      **  --------------------------------------------------------------
      **  Rev. Dec2014 - By R.Cozzi
      **       Incorporate Value Added TAX (VAT)
      **       Consolidate code to RPG IV as much as possible.
      **       2nd Attempt at "one code base" for POS
      **        - meaning, NAS, BHS, MHS, GTS, RSS as well as FPS all use
      **        - the same program instead of FPS having its own unique ver.
      **  --------------------------------------------------------------
      *
      **  --------------------------------------------------------------
      **  Rev. Dec2004 - By R.Cozzi
      **  2005 Version migrated to RPG IV by R.Cozzi
      **  --------------------------------------------------------------
      *
      * Mod# U2804 06/28/2004 Doug - When a tag is scanned the
      *            defective flag was not being brought over.  The        POSEOD
      *            field was not in the logicals (OEDT1 & OHDT2). Dean    POSEOD
      *            wants all fields brought in, 5 other fields were not   POSEOD
      *            in the logicals and need added, they are; D2DMP,       POSEOD
      *            D2DMT, D2DID, D2SCL, & D2ICL.  These fields will be    POSEOD
      *            moved to their OD fields in the subroutine  M<1UA.     POSEOD
      *            I added a total of 13 fields in the Subroutine. Now    POSEOD
      *            all fields in the logical are being moved.
      *Mod # 031505  charlie Hummel
      *             reset next order # to 1 if order # = 99999 in
      *             company mast  statement 3530.00 (CoORD)
      *
      *
      *Mod # 050205  Charlie Hummel
      *              killed the override error on a return if
      *              it has already been authorised  stmnt # 1375
      *              OHCID <> 0
      *Mod # 050305  Charlie Hummel
      *              Stop the check for past due when customer record is
      *              retrieved. Added OHPID to all checks of past due.
      *
      *Mod # 050505  Charlie  Kill credit checks on return or quote.
      *
      *---------------------------------------------------------------
      * Description: Fix return so same item on order can be returned individually
      *              Fix core return so when item is returned, customer can return
      *              core as well.
      * Mod ID       LC     Lena 5/18/2012   RETURNS MODS
      * Mod ID       LC     Lena             TAG MODS I
      * Mod ID       LCT    Lena             TAG MODS II
      * Mod ID       LCT    Lena             removed line to compare wsord&ohord;
      *
      * Mod ID     LC1      Lena 8/1/12 &    add window to confirm F12 order suspension:
      *                          8/9/12     {exfmt confirm}  ;
      *
      * Mod ID     LCS      Lena       prevent F1 when there are any items scanned into open order;
      * Mod ID     LC       Lena       Allow return for employee accounts without checking for
      *                                overrides;
      * Mod ID     LC       Lena       Allow F12 to return to previous screen  (DSP02W )
      * Mod ID     LC       Lena       CREDIT AND SALE CAN NOT BE IN THE SAME ORDER
      *                                have to test it : not in prod
      *
      * VAT:    RC         Added VAT (value added tax) routines throughout.
      *                    Consolidated the DSPF so that both this version
      *                    and the BONDED (FPS) version of POSENTx use
      *                    the same/identical Display File. The new
      *                    DSPF name is POSENT.
      *---------------------------------------------------------------



         ///////////////////////////////////////////////////////////////////
         // The "AIDSITE" include member, control /DEFINE directives.
         // Based on which system (location) the compiler is compiling,
         // Various site-related variables are defined.
         ///////////////////////////////////////////////////////////////////


.....F*ilename++IPEASFRlen+LKlen+AIDevice+.Keywords++++++++++++++++++++++++++++
     FposEnt    CF   E             WORKSTN INFDS(WSDS)
     F                                     SFILE(ORDDETAIL:sfRRN1)
     F                                     SFILE(returnsDTL:sfRRN2)
     FCMPYMAST  UF   E           K DISK
     FCSHR1     UF A E           K DISK
     FCLSMX     IF   E           K DISK
     FITMCL     IF   E           K DISK
     FMASAR     IF   E           K DISK
     FOECTL     UF A E           K DISK
     FOEHDR     UF A E           K DISK
     FOEHDR3    IF   E           K DISK    Prefix(H3:2)
     F                                     Rename(OEHDRF:OHHD3F)
     FOEDTL     UF A E           K DISK
     FOEDT1     UF   E           K DISK
     F                                     RENAME(OEDTLF:OEDT1F)
     FOETAG     UF A E           K DISK

      **  Order History Files Header/Detail
     FOHHD2     UF   E           K DISK
     FOHDT2     UF   E           K DISK
     FOHDT4     IF   E           K DISK


      ** Active (today's) Orders files
      ** Active order headers
     FOEHDR4    IF   E           K DISK    PREFIX('OE_HDR4.')
     F                                     RENAME(OEHDRF : OEHDRR4)
     FOEHDR5    IF   E           K DISK    PREFIX('OE_HDR5.')
     F                                     RENAME(OEHDRF : OEHDRR5)

      ** Historical Orders files
     FOHHDR4    IF   E           K DISK    PREFIX('OEHHDR4.')
     F                                     RENAME(OHHDRF : OHHDRR4)
     FOHHDR5    IF   E           K DISK    PREFIX('OEHHDR5.')
     F                                     RENAME(OHHDRF : OHHDRR5)

     FOHDTL5    IF   E           K DISK    PREFIX('OEHDTL5.')
     F                                     RENAME(OHDTLF : OHDETLR5)

     FSIGD1     UF   E           K DISK    RENAME(SIGD : SIGD1R)
     FSIGR      UF   E           K DISK    RENAME(SIGRF: SIGRR)

     FSLMAN     IF   E           K DISK
     FSLMA1     IF   E           K DISK
     F                                     RENAME(SLMANF:SLMA1F)
     FUPC2      IF   E           K DISK
     FVNMTX     IF   E           K DISK

     D OERTN         E DS                  extName(OERTN) Inz

     D OEHHDR4       E DS                  ExtName(OHHDR4) QUALIFIED Inz
     D OEHHDR5       E DS                  ExtName(OHHDR5) QUALIFIED Inz
     D OEHDTL5       E DS                  ExtName(OHDTL5) QUALIFIED Inz

     D oh3           E DS                  ExtName(OEHDR3) Qualified Inz
     D OE_HDR4       E DS                  ExtName(OEHDR4) QUALIFIED Inz
     D OE_HDR5       E DS                  ExtName(OEHDR5) QUALIFIED Inz
     D*OE_DTL5       E DS                  ExtName(OEDTL5) QUALIFIED Inz


     D dbbal         E DS                  extname(blnce)    Qualified Inz
     D dbitem        E DS                  extname(mastr)    Qualified Inz
     D dbAD          E DS                  extname(mastrAD)  Qualified Inz
     D dbBond        E DS                  extname(bondmast) Qualified Inz

     D PSDS           SDS
     D  pgmName          *PROC
     D  JobName                      10A   Overlay(PSDS:244)
     D  JobUser                      10A   Overlay(PSDS:254)
     D  JobNbr                        6A   Overlay(PSDS:264)

     D WSDS            DS                  Qualified
     D  FKey                          1A   Overlay(WSDS:369)
     D  sflRRN                        5I 0 Overlay(WSDS:378)

         // Add all "work" or "temp" fields to this data structure.
         // Then use "QTMP.xxxx" to access the field.
         // where xxxx is the subfield's name.
         // DO NOT declare fields in the Calc specs.
         dcl-ds qtmp Qualified Inz;
           bondedStore char(10);
           sqlLoop int(10);
           mySQL varchar(2048);
         end-ds qtmp;

         // These few fields definitions are for compiling in NON-bonded environment.
     D H3BOND          S              5A
     D OHBOND          S              5A
     D ODBOND          S              1A
     D SVBOND          S              1A
     D ODBQTY          S              4S 0
     D ODNQTY          S              4S 0

     D mdy             S              6S 0

     D dynSQL          S           2048A   Varying

     D wkITP           S              1P 0
     D bFoundRtnLineItem...
     D                 S              1N   Inz(*OFF)
 lc  D savven          S                   Like(ODVEN)
 lc  D savitm          S                   Like(ODITM)
 lc  D savitp          S                   Like(ODITP)
 lc  D wrkqtr          S                   Like(ODQTR)
     D wrkType         S              1A

      **************************************************************
      **  These /COPY members contain prototypes
      **  and Named Constants used throughout
      **  the program.
      **************************************************************

      /COPY aidsrc/qcpysrc,aidsys
      /COPY aidsrc/qcpysrc,aidConst
      /COPY aidsrc/qcpysrc,FKEYS
      /COPY cozTools/qcpysrc,joblog
      /COPY cozTools/qcpysrc,cozAppDft

      /COPY aidsrc/qcpysrc,vat

     D NOOP            S              1N   INZ(*OFF)

         dcl-pr loadPOSDefaults;
         end-pr loadPOSDefaults;

         dcl-pr isBondedOrder ind;
           order packed(7:0) Const;
         end-pr isBondedOrder;

         dcl-pr isBondedCashCust ind;
           custNo int(10) Const;
         end-pr isBondedCashCust;

         dcl-pr isBondedCashCustA ind;
           custA char(5) Const;
         end-pr isBondedCashCustA;

         dcl-pr isCashCust ind;
           custNo int(10) Const;
         end-pr isCashCust;

         dcl-pr isCashCustA ind;
           custA char(5) Const;
         end-pr isCashCustA;

         dcl-pr isNotCashCust ind;
           custNo int(10) Const;
         end-pr isNotCashCust;

         dcl-pr isNotCashCustA ind;
           custA char(5) Const;
         end-pr isNotCashCustA;

         dcl-pr getBondedInfo;
         end-pr getBondedInfo;
         dcl-pr getBondedCashCust;
         end-pr getBondedCashCust;
         dcl-pr getBondCust2  extpgm('GETBCUST2');
           lastName char(25) Const;
           bondRecNbr char(6);
           bondNbr char(5);
           bondExpYear char(4);
           custNbr packed(5:0);
         end-pr getBondCust2;

          //  NOTE: This version of POSENT contains VAT tax calculations.
          //        To implement VAT, we modernized the code to use
          //        subprocedures for many of the legacy subroutines.

         dcl-pr prepareOrder;
           bIsAReturn ind Const OPTIONS(*NOPASS);
         end-pr prepareOrder;

         dcl-pr clearNewOrder;
         end-pr clearNewOrder;
         dcl-pr getPrice;
         end-pr getPrice;
         dcl-pr getRegPrice;
         end-pr getRegPrice;
         dcl-pr getOtherPrice;
         end-pr getOtherPrice;
         dcl-pr getVatCharge packed(7:2);
           ordernbr packed(7:0) Const;
           vendor char(3) Const;
           item char(15) Const;
           seqNbr packed(7:0) Const;
           qtySold int(10) Const;
           soldAtPrice packed(7:2) Const;
           corePrice packed(7:2) Const;
         end-pr getVatCharge;

         dcl-pr move_Item_to_OEDTL;
           vendor char(3) Const;
           item char(15) Const;
         end-pr move_Item_to_OEDTL;

         dcl-pr newVatHdr;
         end-pr newVatHdr;
         dcl-pr createNewOrder;
         end-pr createNewOrder;
         dcl-pr sfClear;
         end-pr sfClear;
         dcl-pr deleteOrder;
           orderNbr int(10) Const;
         end-pr deleteOrder;
         dcl-pr deleteLineItem;
           orderNbr int(10) Const;
           orderSeqNbr int(10) Const;
         end-pr deleteLineItem;

         dcl-pr cmdLineWindow  extPgm('QUSCMDLN');
         end-pr cmdLineWindow;

         dcl-pr checkTag ind;
           dcl-parm tag char(10) Const;
           order packed(7:0) Const;
         end-pr checkTag;

         dcl-pr checkItemHist int(10);
           InvNbr  Like(ODINV) Const;
         end-pr checkItemHist;

          // * Summarize quantity for a Item on all related invoices
         dcl-pr sumItemQty int(10);
           InvNbr  Like(ODINV) Const;
           Vendor  Like(ODVEN) Const;
           Item  Like(ODITM) Const;
         end-pr sumItemQty;

          // * Summarize quantity for a Item on an individual invoice
         dcl-pr sumInvQty int(10);
           InvNbr packed(7:0) Const;
           Vendor char(3) Const;
           Item char(15) Const;
         end-pr sumInvQty;

         dcl-pr cstSearch  EXTPGM('CSTSEARCH');
           searchData char(30) Const OPTIONS(*NOPASS);
           rtnCust packed(7:0) OPTIONS(*NOPASS);
         end-pr cstSearch;


     D custNo          S              5P 0
     D custNo7         S              7P 0

        // Counters is a Qualified DS that contains subfields
        // that may be used in looping or array processing.
        // by storing them in this qualified data structure,
        // we "hide" them from other fields in this program
        // and avoid so called "name collision".
     D counters        DS                  Qualified Inz
     D  i                            10I 0
     D  x                            10I 0
     D  y                            10I 0
     D  z                            10I 0


     D option          DS                  Qualified
     D  regItem                       5I 0 Inz(0)
     D  specialOrder                  5I 0 Inz(1)
     D  miscCharge                    5I 0 Inz(2)
     D  coreReturn                    5I 0 Inz(3)
     D  regReturn                     5I 0 Inz(4)
     D  itemReturn                    5I 0 Overlay(regReturn)
     D  specialReturn                 5I 0 Inz(5)
     D  miscReturn                    5I 0 Inz(6)

     D nOrg_Qty        S             10I 0
     D nHist_RtnQty    S             10I 0
     D nTodays_RtnQty  S             10I 0
     D nThis_Qty       S             10I 0
     D bToday_Found    S              1N
     D bHist_Found     S              1N
     D bPrice_Conflict...
     D                 S              1N
     D bBalancing_Error...
     D                 S              1N
     D nRtn_Price      S              7P 2
     D nOrg_CustNo     S                   Like(ODCUS)

     D nOvrInvNbr      S                   Like(ODINV)
     D nInvAge         S             10I 0

     D bReturnsFlag    S              1N   Inz(*OFF)

     D returnInvoice...
     D                 DS                  Qualified Inz
     D  orderType                     1A
     D  orgInvNbr                     6S 0
     D  orgInvDate                    6S 0
     D  invDate                        D   DatFMT(*ISO)
     D  orgInvAge                    10I 0
     D  orgBondNbr                    5A
     D  isBonded                      1A
     D  orgInvoice                    6S 0 Overlay(orgInvNbr)
     D  invAge                       10I 0 Overlay(orgInvAge)

     D userDate        S               D   Datfmt(*ISO) INZ(*JOB)

     D tagInfo_T       DS                  Qualified Inz
     D  amt                          11P 2
     D  qty                           7P 0
     D  ordnbr                        9P 0
     D  invnbr                        9P 0

     D ice             S              9P 2
     D cce             S              9P 2
     D addItem         S              1A

     D wkcmcla         S              9P 0
     D chkAmt          S              9P 0
     D bIsEmployee     S              1N   Inz(*OFF)

     D orderNbr        S              5P 0
     D orderSeqHigh    S              3P 0

        // Subfile Relative Record Number counter for WRITE to SFLDTL
     D sfRRN1          S              4S 0
     D sfRRN2          S              4S 0
     D sfRCDNBR        S              4S 0

     D odutc           S              7P 2
     D odtec           S              7P 2
     D oduts           S              7P 2
     D odtes           S              7P 2
     D odutn           S              7P 2

     D default         DS                  Qualified
     D  max_RTN_AGE                   5I 0 Inz(30)
     D  max_ITEMS                     5I 0 Inz(50)
     D  max_CORE_RTN                  5I 0 Inz(3)
     D  max_ANY_RTN                   5I 0 Inz(4)
     D  ALLOW_CR_RTN                  1N   Inz(*ON)
     D  ALLOW_SPEC_ORD...
     D                                1N   Inz(*OFF)
        // Charge VAT on CORE sales
     D  chargeVATonCore...
     D                                1N   Inz(*OFF)
     D  CHECK_TAG_NAME...
     D                                1N   Inz(*ON)
     D  bondedStore                   1N   Inz(*OFF)
     D  isBondedStore                 1N   Overlay(bondedStore)
     D  isBonded                      1N   Overlay(bondedStore)

     D MODE_SALES      C                   Const('S')
     D MODE_SALESMAN   C                   Const('S')
     D MODE_COUNTERMAN...
     D                 C                   Const('S')

     D MODE_CASHIER    C                   Const('C')

         // Default point of sale "mode" is "Cashier" mode
     D POS_Mode        S              1A   Inz(MODE_CASHIER)

     D defaults        DS                  LikeDS(Default) Based(pDefaults)
     D pDefaults       S               *   Inz(%addr(default))

     D VATH          E DS                  extname(VATHDR) Qualified Inz
     D VATD          E DS                  extname(VATDTL) Qualified Inz
     D VATDID          S             20I 0
     D VATHID          S             20I 0
     D bVATDtl         S              1N   Inz(*OFF)
     D bVATHdr         S              1N   Inz(*OFF)
     D lastOrder       S             10I 0

      ** Legacy array definitions (these were "E" specs)
     D CDO             S              5  0 DIM(20)
     D ERR             S             60    DIM(50) CTDATA PERRCD(1)

     D TDAT            S              8S 0
     D TTIM            S              6S 0

     D szUser          S             10A   Inz(*USER)
     D holdinvnbr      S              6s 0
     D ChkDat          S              8S 0
     D YYMMDD          S              6S 0
     D Today           S               D   Inz(*SYS) DATFMT(*ISO)
     D APP             S             10A   Inz('POS')
     D MENU            S             10A   Inz('ONLINE')
     D OPT             S              3A   Inz('001')
     D Func            S             10A
     D SMN             S              2A
     D SEC             S              1A
     D PRMPT           S              1A   Inz('Y')
     D FINAL           S              1A
     D SCREEN          S             12A
     D WSCID           S              2S 0
     D WSDRAW          S              5P 0
     D HDDRAW          S              5P 0
     D WSYMD           S              6P 0
     D WSNIT           S              3P 0
     D bFailedOvrTest  S              1N   Inz(*OFF)
     D bReturns        S              1N   Inz(*OFF)
     D bCancelGetInv   S              1N   Inz(*OFF)
     D UPCok           S              1A   Inz('N')
     D tagRejected     S              1N   Inz(*OFF)

     D creditReturnCount...
     D                 S              5P 0

     D wsniti          S              3P 0
     D wsnitc          S              3P 0
     D dock1t          S              1A   Inz('N')

     D INVN            S              5P 0
     D Show            S              1A
     D WCopies         S              1P 0
     D PCopies         S              1P 0
     D POrd            S              5A
     D PWSID           S             10A
     D WSPDO           S              1A
     D InvNbrA         S              6A
     D InvNbrn         S              6S 0
     D INVDAT          S              6S 0
     D InvDateA        S              6A
     D dtInvoice       S               D   DatFmt(*ISO)
     D dtOrgInv        S               D   DatFmt(*ISO)
     D nCount          S             10I 0
     D nAge            S             10I 0
     D manualSig       S              1A   Inz('N')
     D inhibitf1       S              1N   Inz(*OFF)
     D pos             S             10I 0
     D rtntype         S              1S 0
     D rtnSeqNbr       S             10I 0

           // This DS with overlapping subfields
           // is used to populate the fields used
           // in Keylists from the Vendor/Item number
           // on the screen. For example, if a TAG
           // is scanned the SALESTAG subfield is
           // used to "chain" out to the files.
           // If a UPC code is scanned, its used
           // as a keyfield to go after the
           // the vendor/item number in the UPC file.
     D                 DS
     D  UPC                    1     18A
     D  TYP                    1      1A
     D  S1VEN                  1      3A
     D  SALESTAG               1      5A
     D  REF                    2      7S 0
     D  S1ITM                  4     18A
     D  IDT                    8     13S 0

     D WSID            S             10A
     D USER            S             10A   Inz(*USER)

     D store           DS                  Qualified
     D  Code                          2S 0
     D  Abbr                          3A
     D  Name                         15A
     D  Loc                          15A
     D  company                      50A

 lc  D bAlreadyUpdated...
 lc  D                 S              1N   Inz(*OFF)
 lc  D bNoUpdateInv...
 lc  D                 S              1N   Inz(*OFF)

     D BOND            DS                  Qualified
     D  isBondOrder                   1A   Inz('N')
     D   wsBond                       1A   Overlay(isBondOrder)
     D  bondID                        5A   Inz('00000')
     D  bondNbr                       5S 0 Overlay(bondID)
     D  bondIDSave                    5A   Inz('00000')
     D  bondNbrSave                   5S 0 Overlay(bondIDSave)
     D  bondedCashCustomer...
     D                                5A   Inz('01001')
     D  bondedCashCust...
     D                                5S 0 Overlay(bondedCashCustomer)
     D  bondedCashCustA...
     D                                5A   Overlay(bondedCashCustomer)
     D  cashCust...
     D                                5S 0 Overlay(bondedCashCustomer)
     D  cashCustA...
     D                                5A   Overlay(bondedCashCustomer)

     D  CASHCUST       S              5S 0 Inz(90000)

        // AID System-wide defaults file
        //    Contains the Key Char(32) and the Value Char(32)
        //    If the value is numeric, use %DEC or %INT to convert it from Char(32) to numeric.
        //    All default values are stored in plain text format.
     D DFT           E DS                  extname(AIDDEFAULT) Inz Qualified

     D entryPList      PI                  extpgm('POSENT7')
     D  operatingMode                 1A   Const Options(*NOPASS)

.....I*ilename++SqNORiPos1+NCCPos2+NCCPos3+NCC...

 7505IOHDT2A        01
 7505IOHDT2B        02
 7505IOHHD2A        01
 7505IOHHD2B        02

      /free
           EXEC SQL SET OPTION NAMING = *SYS, COMMIT = *NONE,
                               CLOSQLCSR =*ENDMOD;

           // Program Body - Loop until exit (i.e., *INLR=*ON)
           dow NOT *INLR;
             if     (screen = 'POS00');
               exsr Dsp00;
             elseif (screen = 'ORDDETAIL');
               exsr Dsp01;
             elseif (screen = 'ORDCONFIRM');
               exsr Dsp02;
             elseif (screen = 'POS03');
               exsr Dsp03;
             elseif (NOT *INLR);
               exsr Dsp00;
             ENDIF;

           enddo;
           return;
           // ENDPGM - Return to caller


      ******************************************************************
      **  Initialization Routine - Called upon Program Entry
      ******************************************************************
         BEGSR *INZSR;
          // AID has a slightly ambiguous nomenclature for POS modes of operation.
          //  Mode='S' Means "Salesman" mode.
          //           More commonly referred to as "Counterman" mode.
          //           This mode performs no sales at all, just quotes.
          //  Mode='C' Means "Cashier" mode.
          //           This mode allows sales, including those from quotes.

           if (%Parms() >= 1);
              if (operatingMode <> '');
               pos_Mode = operatingMode;
             endif;
           endif;

           EXEC SQL SELECT store,ABBR, STORENAME, prov, compname
                INTO :store.Code, :store.Abbr, :store.Name, :store.loc,
                     :store.Company
                FROM aiddata/cmpyinfo
                WHERE dftstore = 'Y'
                FETCH FIRST ROW ONLY;

           if (SQLState >= '02000');
             EXEC SQL SELECT store,ABBR, STORENAME, compname
                    INTO :store.Code, :store.Abbr, :store.Name,
                         :store.loc,:store.Company
                    FROM aiddata/cmpyinfo
                    WHERE abbr = 'NAS'
                    FETCH FIRST ROW ONLY;
           endif;

              // NOTE: If store is NOT FPS, and you need it to be a BONDED Store,
              // then insert an App Defaults Entry of: POS_BONDED = 'Y'
              // and 88 will get set on during loadPOSDefaults.
           *IN88 = %SUBST(store.abbr:1:3) = 'FPS';  // Freeport = BONDED Store
           default.bondedStore = *IN88;

           loadPOSDefaults();

           //  inhibitF1 = *OFF;   // No need to re-init this field here. Already done in D specs.

           wsid = jobName;         // Use Job Name as Workstation ID (WSID)
           TDAT = *ALL'9';
           TTIM = *ALL'9';
           chkdat = %dec( %Date() : *ISO);
           yymmdd = %dec( %Date() : *YMD);

           clear totalLine;
           clear vathid;
           clear vatdid;
           clear vath;
           clear vatd;

           if (pos_Mode = MODE_CASHIER);
              opt = '002';
              EXSR getSec;
              monitor;
                if (smn <> '');
                  wsSmn = %int(smn);
                else;
                  wsSmn = 0;
                endif;
              on-error;
                wsSmn = 0;
              endmon;
              EXSR getSalesMan;
              wsmDes = 'Cashier';
              screen = 'POS00';
           else;
             wsmDes = 'Counter';
             wsord = 0;
             S1CUS = *ALL'9';
             CLEAR OEHDRF;
             CLEAR totalLine;
             wstyp = 'Q';
             orddes = 'REQUEST\QUOTE\RETURN';
             *IN69 = *ON;
           screen = 'ORDDETAIL';
           ENDIF;
           WSDraw = 0;
           wsYMD = %dec(%date() : *YMD);
           EXSR CKDRAWER;
     CSR   endInzSr      endSR


     C**************************
     C* CHECK SCREEN 1
     C**************************
     CSR   CheckSFL      BEGSR
           *IN90 = *OFF;
           *IN48 = *OFF;
           clear wsErr;
           for i = 1 to 10;
           //  MUST CREATE ORDER here.
           IF (WSORD = 0);
             createNewOrder();
             ODORD = WSORD;
             OHORD = WSORD;
             OCORD = WSORD;
           endif;
          end;

           //  CHECK CUSTOMER FIELDS FOR Blanks/Zeros
           IF (S1CUS = *BLANKS or S1CUS = *ZEROS);
             S1CUS = *ALL'9';
             wsErr = Err(29);
             *IN90 = *ON;
             LeaveSR;
           ENDIF;

           //  Customer Number has changed?
           IF (SVCUS <> S1CUS);
              EXSR CHKCUS;
              IF *IN90;
                LeaveSR;
              endif;

            // Changed the bond reset to only occur
            // when the customer was previously assigned
            // and it was not 99999 or 0 or blanks
             if (defaults.isBondedStore);
               If (svcus <> *zeros and
                   svcus <> *ALL'9' and
                   svcus <> *blanks);
                 // Customer has changed reset bonded flag for invoice to 'N'
                 wsbond = 'N';
                 svbond = wsbond;
                 wsbond# = *blanks;
               endIf;
             endIf;
            exsr clearAutDSP;  // Clear overrides
      /end-free

      **  MOVE FIELDS/UPDATE HEADER
     C                   MOVEL     CMNME         S1NME

     C     WSORD         CHAIN     OEHDR                              92
     C     WSORD         CHAIN     OECTL                              93

     C                   if        %Found(OEHDR)
     C                   Exsr      cpyAutFromDBF
     C                   endif

     C                   If        NOT %FOUND(OEHDR)
     C                   eval      OHINV = 0
     C                   eval      OHORD = WSORD
     C                   eval      OHTYP = WSTYP
     C                   eval      OHODT = WSYMD
     C                   ENDIF


     C                   MOVEL     WSTYP         OHTYP
     C                   MOVEL     WSTYS         OHTYS
     C                   MOVEL     S1CUS         OHCUS
     C                   MOVEL     CMNME         OHCNM
     C                   MOVE      CMRPO         OHRPO
     C                   MOVE      WSPDO         OHPDO

      /free
             if (defaults.isBondedStore);
               if wsbond  = 'Y';
                 ohbond = wsbond#;
               else;
                 ohbond = *blanks;
                 wsbond# = *blanks;
               endif;
             endif;

             ohTme = %dec(%Time());
             If %FOUND(OEHDR);
               UPDATE OEHDRF;
             ENDIF;

             EXSR RETOTAL;
             EXSR FillSFL;
             svCus = S1CUS;
             LeaveSR;
           endif;

              // BONDED INVOICE?
           if (defaults.isBondedStore);
               // check to see if request for bonded invoice has been made
             if  (wsbond <> svbond);
                if (wsBond = 'Y');
                  getBondedInfo();   // Get Bond Number for this customer/Order
                else;
                  wsBond = 'N';
                  wsBond# = '';
                  svbond = wsbond;
                endif;

                chain (wsord) oehdr;
                if %Found(OEHDR);
                   // Using Dynamic SQL here since the OEHDR and OHHDR files have
                   // different fields depending on where this is being run.
                   // for example, on FPS there is an OHBOND field at the end
                   // of OEHDR/OHHDR but on NAS there is no such field (as of this writing).
                  dynSQL = 'UPDATE OEHDR SET OHBOND = ''' + wsBond# + ''' ' +
                           'WHERE OHORD = ' + %char(wsord);
                  EXEC SQL EXECUTE IMMEDIATE :dynSQL;

                  exsr reTotal;
                  exsr FillSFL;
                  eval svBond = wsBond;
                  leaveSR;
                endif;
             endif;
           endif;

           CLEAR OEDTLF;
           tagRejected = *OFF;

           READC ORDDETAIL;
           // CHECK FOR TAG (sales slip) SCAN
           if (A=B);
           if NOT %EOF();
             if S1LAST = 'Y';
               EXSR CHK1U;
               IF UPCOK = 'Y';
                 EXSR RETOTAL;
                 EXSR FillSFL;
                 LeaveSR;
               elseif tagRejected;
                 LeaveSR;
               endif;
             endif;
           endif;
           endif;

          // CHECK SUBFILE RECORDS
       DOW NOT %EOF(posent);

         // To remove/delete a line item from "this" order,
         // type a zero quantity sold into the line item and press Enter.
         IF (S1QTS = 0);
           deleteLineItem(odord:odseq);   // Delete "this" Line Item from Order
         else;
           //  IF UPC=BLANKS THEN NO ITEM KEYED/SCANNED
           IF UPC <> *BLANKS;

            for i = 1 to 10;
              chain(e) custmast;
              if %EOF();
                return;
              endif;
              clear inDs;
            endfor;

             //*  If the subfile rec previously contained
             //*  a credit/return, and now it no longer does,
             //*  then decrement the Credit/Return counter.
             if (HODITP >= 3 and HODITP <= 6)
                   and (ODITP < 3 or ODITP > 6)
                   and (creditReturnCount > 1);
               creditReturnCount -= 1;
               clear HODITP;
             endif;

             //  +++++ START: Per Jason Watson 28Dec2014
             //  DO NOT ALLOW options 1 and 2
             if (ODITP > 6);
                 *IN70 = *ON;
                 *IN71 = *OFF;
                 *IN72 = *OFF;
                 *IN73 = *OFF;
                 *IN74 = *OFF;
                 *IN78 = *OFF;
                 *IN79 = *OFF;
                 *IN90 = *ON;
                 WSERR = 'Option ' + %char(oditp) + ' out of range.';
                 CLEAR ODITP;
                 UPDATE ORDDETAIL;
                 *IN70 = *OFF;
                 leaveSR;
             endif;

           if NOT (default.ALLOW_SPEC_ORD);  // Special Orders options 1 and 2 disabled?
             if (ODITP = option.specialOrder or ODITP = option.miscCharge);
                 *IN70 = *ON;
                 *IN71 = *OFF;
                 *IN72 = *OFF;
                 *IN73 = *OFF;
                 *IN74 = *OFF;
                 *IN78 = *OFF;
                 *IN79 = *OFF;
                 *IN90 = *ON;
                 WSERR = 'Option ' + %char(oditp) + ' disabled.';
                 CLEAR ODITP;
                 UPDATE ORDDETAIL;
                 *IN70 = *OFF;
                 leaveSR;
             endif;
           endif;

             //  +++++ START: Per Jason Watson 20Feb2012
             //  DO NOT ALLOW options 3,4,5,6 from ORDENTRY/ORDDETAIL
           if NOT (default.allow_CR_RTN);  // Credit Returns NOT allowed for opt 3,4,5,6?
             if (ODITP >= 3 and ODITP <= 6);
                 *IN70 = *ON;
                 *IN71 = *OFF;
                 *IN72 = *OFF;
                 *IN73 = *OFF;
                 *IN74 = *OFF;
                 *IN78 = *OFF;
                 *IN79 = *OFF;
                 *IN90 = *ON;
                 WSERR = 'Option ' + %char(oditp) + ' prohibited.';
                 CLEAR ODITP;
                 UPDATE ORDDETAIL;
                 *IN70 = *OFF;
                 leaveSR;
             ENDIF;
             // +++++ ENDMOD: Per Jason. Disallow Options 3,4,5,6
           endif;

           // +++++ START: Check for Returns/Credit for Authorization Override
           if (ODITP >= 3 and ODITP <= 6)
                 and creditReturnCount >= 0;
             creditReturnCount += 1;
             HODITP = ODITP;
             exsr CheckCreditAut;
             if bFailedOvrTest = *ON;
               WSERR = ERR(39);
               *IN48 = *ON;
             endif;
           endif;
           // +++++ END:  Check Returns/Credit for Authorization Override
      /END-FREE

      ** CAN NOT DO CREDIT ON CHARGE INVOICE
     C                   IF        OHTYS = 'G' and ODITP > 3 and WSNITI <> 0
     C                   MOVE      ERR(27)       WSERR
     C                   MOVE      *ON           *IN90
     C                   MOVE      *ON           *IN70
     C                   MOVE      *OFF          *IN71
     C                   MOVE      *OFF          *IN72
     C                   MOVE      *OFF          *IN73
     C                   MOVE      *OFF          *IN74
     C                   MOVE      *OFF          *IN78
     C                   MOVE      *OFF          *IN79
     C                   UPDATE    ORDDETAIL
     C                   MOVE      *OFF          *IN70
     C                   LeaveSR
     C                   ENDIF

     C* CAN NOT DO INVOICE ITEM ON CREDIT MEMO FOR CHARGE
     C                   IF        OHTYS = 'G'
      /FREE
           IF  (ODITP = option.regItem) or
              ((ODITP = option.specialOrder or ODITP = option.miscCharge) and
                default.ALLOW_SPEC_ORD);
      /END-FREE
     C                   IF        WSNITC <> 0
     C                   MOVE      ERR(30)       WSERR
     C                   MOVE      *ON           *IN90
     C* 70 = KEY ITP,VEN,ITM
     C* 71 = KEY DES
     C* 72 = UIN, UCN
     C* 73 = CEXG
     C* 74 = DEF
     C* 78 = ITEM IS A RETURN
     C* 79 = ITEM FROM TAG NOT YET SOLD.
     C                   MOVE      *ON           *IN70
     C                   MOVE      *OFF          *IN71
     C                   MOVE      *OFF          *IN72
     C                   MOVE      *OFF          *IN73
     C                   MOVE      *OFF          *IN74
     C                   MOVE      *OFF          *IN78
     C                   MOVE      *OFF          *IN79
     C                   UPDATE    ORDDETAIL
     C                   MOVE      *OFF          *IN70
     C                   LeaveSR
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF

     C* NEW ITEM/ ITEM SEARCH
     C                   IF        S1LAST = 'Y'
     C* SET UP SCREEN INDY FOR LAST ITEM
     C                   MOVE      *ON           *IN70                          LA
     C                   MOVE      *OFF          *IN71                          LA
     C                   MOVE      *OFF          *IN72                          LA
     C                   MOVE      *OFF          *IN73                          LA
     C                   MOVE      *OFF          *IN74                          LA
     C* REG. ITEM                                          LA
      /FREE
           IF ODITP = option.regItem or ODITP = option.regReturn;
             // CHECK UPC FILE FOR MATCH
             IF (UPC <> ' ');
               uxUPC = UPC;
               CHAIN (UXUPC) UPC2;
               IF %Found(UPC2);
                 s1Ven = UXVND;
                 s1Itm = UXITEM;
               endif;
             endif;
           ENDIF;
      /END-FREE
     C     s1ven         ifeq      *blank
     C     s1itm         andne     *blank
     C                   movel     s1itm         wksrch           15
     C                   call      'GETPART'
     C                   parm                    wksrch
     C                   parm                    wkven             3
     C                   parm                    wkitm            15
     C                   movel     wkven         s1ven
     C                   movel     wkitm         s1itm
     C                   endif

      /free

             //  MISC. ITEM CHARGE

             IF ODITP = option.specialReturn or
                (ODITP = option.specialOrder
                 and default.ALLOW_SPEC_ORD);
               ODSCL = 13;
               ODICL = 999;
               s1Des = 'MISC.';
               IF S1ITM = *BLANKS;
                 s1Itm = s1Des;
               endif;
               EXSR DSP08W;
               IF *IN90 = *ON;
                 UPDATE ORDDETAIL;
                 LeaveSR;
               endif;

             endif;


             // SPECIAL ITEM CHARGE
             if  (ODITP = option.miscReturn or
                 (ODITP = option.miscCharge and
                    default.ALLOW_SPEC_ORD));
               ODSCL = 13;
               ODICL = 999;
               s1Des = 'SPECIAL ';
               IF S1ITM = *BLANKS;
                 S1ITM = S1DES;
               endif;

               EXSR DSP08W;
               IF *IN90 = *ON;
                 UPDATE ORDDETAIL;
                 LeaveSR;
               endif;
             endif;
              // CORE RETURN
             IF ODITP = option.coreReturn;
               s1Des = 'CORE RET';
             endif;

        endif;

           IF (ODORD = 0);
              ODORD = WSORD;
           endif;

           // CHECK TO SEE IF ON A TAG
           IF S1LAST = 'Y'
                 and DOCK1T = 'Y'
                 or S1OQT <> S1QTS;
             EXSR CHK1T;
           ENDIF;

           //  RETRIEVE ORDER Detail line item
           chain (odord:odseq) oedtl;

           if (VATHID > 0);
             EXEC SQL SELECT vatdid INTO :VATDID
                 FROM  VATDTL
                 WHERE VATHID = :VATHID and
                       ORDSEQ = :ODSEQ  and
                       VENDOR = :ODVEN  and
                       ITEM   = :ODITM
                 FETCH FIRST ROW ONLY;
             bVatDtl = (SQLState < '02000');  // Found?
           else;
             bVatDtl = *OFF;  // Obviously, not found.
           endif;

           odven = s1Ven;
           oditm = s1itm;
           odqts = s1qts;

           if (s1Last <> 'Y');
             odQTO = s1qts;
           endif;

           if (defaults.isBondedStore);
             odbond = s1bond;
           endif;

           ODUIN  = S1UIN;
           ODUCN  = S1UCN;
           ODDES  = S1DES;
           ODCEXG = S1CEXG;
           ODDEF  = S1DEF;
           ODSMN  = S1SMN;

           if (oditp = option.regItem or ODITP = option.coreReturn or
               oditp = option.regReturn);
             odven = s1ven;
             oditm = s1itm;
             EXEC SQL SELECT imDes
                       INTO  :dbItem.imDes
                       FROM  mastr
                       WHERE (imvnd,imitem) = (:odven, :oditm)
                               and imdel <> 'D'
                         FETCH FIRST ROW ONLY;

              *IN90 =  (SQLState >= '02000');  // Item Master rec not found?

              IF (SQLState < '02000');         // If Item Found...
                if (defaults.isBondedStore);
                   exec sql Select bland, bondp, poType, hazmat
                            INTO  :dbAD.bland,  :dbAD.bondP,
                                  :dbAD.poType, :dbAD.hazmat
                            FROM mastrAD
                            WHERE (advnd,aditem) = (:odven,:oditm)
                            FETCH FIRST ROW ONLY;
                endif;
                exec sql Select ibqoh, ibqoo, iblord
                         INTO   :dbbal.ibqoh, :dbbal.ibqoo, :dbbal.iblord
                         From   blnce
                         where  (ibvnd,ibitem) = (:odven,:oditm)
                                and ibdel <> 'D'
                         FETCH FIRST ROW ONLY;
             endif;

             if (SQLState >= '02000' or *IN90);   // Not found?
                clear dbItem;
                clear dbBal;
                clear s1Des;
                UPDATE ORDDETAIL;   // Update Subfile Detail Record (POS01)
                wsErr = %trimR(err(1)) + ' ' +
                        %trimR(odven)  + ' ' + %trimR(odItm);
                LeaveSR;
             endif;

             clear lstOrdered;
             clear onHand;
             monitor;
               mdy = dbBal.iblOrd;
               test(DE) *MDY mdy;
               if NOT %ERROR();
                 lstOrdered = %date( mdy : *MDY);
               endif;
             on-error;
                clear lstOrdered;
             endmon;

             monitor;
               onHand = %char(dbbal.IBQOH) + '/' + %char(dbbal.IBQOO);
             on-error;
                clear onHand;
             endmon;

             s1des = dbItem.imdes;   // Long (full) description
             s1sDs = dbItem.imdes;   // Short description

             Move_Item_to_OEDTL(odven:oditm);
           endif;

           IF (ODITP >= 3 and ODQTS > 0)
                 or
              (ODITP <  3 and ODQTS < 0);
               //  On returns/credits make quantity negative
              ODQTS = 0 - %abs(ODQTS);
           endif;

          // Deprecated "CORE EXCHANGE" routine:
          if (ODCEXG = 'Y' and ODUCN > *ZEROS);
            // Nothing to do; there is a comment only: "Verify if it is ok"
          endif;

           // This code seems to be out of place.
           // It was ported directly from Freeport's POSENT5.
           // It basicallys says:
           //  "If bonded Cash Customer (i.e., 01001) and
           //   there is a Discount, then tell the User
           //   that you can't mix charge/credit on same
           //   sales."
           // This obviously is not right, so I'm commenting
           // it out in the consolidated/merged POSENT6 version.
           // if (NoOp);  // Do not do; ever!
             if (defaults.isBondedStore);
               IF (S1DKY <> 0  AND isBondedCashCustA(S1cus));
                 wsErr = Err(50);
                 *IN90 = *ON;
                 LeaveSR;
               endif;
             endif;
           // endif;   // end NoOp
      /end-free

     C* VERIFY THAT S1DKY (DISCOUNT KEYED) IS VALID

     C                   IF        S1DKY <> 0

      **************************************************************
      **  Verify DISCOUNT AUTHORITY
      **************************************************************
     C                   if        OHDID = 0 and WSDID = 0
     C                   eval      wsErr = Err(46)
     C                   eval      *IN90 = *ON
     C                   LeaveSR
     C                   endif


     C* DKY WAS KEYED
     C                   IF        S1DKY > 500
     C                              and S1DKY <> 999
     C* VERIFY IF CAN U                                    DK >
     C                   MOVE      *ON           *IN90                          DK >
     C                   MOVEL     ERR(7)        WSERR                          DK >
     C                   UPDATE    ORDDETAIL                                        DK >
     C                   LeaveSR
     C                   endif

     C* .999 = MANUAL KEY PRICE                            DK
     C                   if        S1DKY = 999
     C                   EXSR      DSP08W                                       DK M
     C                   IF        *IN90 = *ON
     C                   UPDATE    ORDDETAIL                                        DK M
     C                   LeaveSR
     C                   ENDIF                                                  DK M
     C                   ENDIF                                                  DK M END

     C* MULT TO GET TO 3/3 FIELD NOT 3/0                   DK
     C                   if        S1DKY <> *ZEROS
end  C     S1DKY         MULT      .001          ODDKY
     C                   ENDIF                                                  DK G
     C                   ENDIF                                                  DK END
      /free
               getPrice();

               *IN78 = (ODITP >= 3);

               if %found(oedtl);
                 update oedtlf;
               else;
                 write  oedtlf;
               endif;
               update ORDDETAIL;
              endif;
             endif;
             readc ORDDETAIL;
           enddo;

           exsr fillSFL;

      /end-free

     c                   IF        OHWON = 0
      **  VALIDATE TRANSACTIONS - CHARGE CAN ONLY BE CREDIT OR INVOICE
      **  HARD ERROR - DO NOT LET PASS
     C                   IF        OHTYS = 'G'
     C                   IF        WSNITI <> *ZEROS
     C                   IF        WSNITC <> *ZEROS
      **  CAN'T USE THIS CUSTOMER FOR MIXED SALE TRANSACTIONS
     C                   eval      *IN90 = *ON
     C                   eval      wsErr = Err(27)
     C                   LeaveSR
     C                   endif
     C                   endif
     C                   endif

     C     OKNIT         TAG                                                    REDO SCRN
      **  Change WSTYP to: C or I
     C                   IF        WSNITI <> *ZEROS
     C                              and WSTYP = 'C'
     C                   MOVE      'I'           WSTYP
     C                   ENDIF

     C                   IF        WSNITC <> *ZEROS
     C                              and WSTYP = 'I'
     C                   MOVE      'C'           WSTYP
     C                   ENDIF
     C                   ENDIF

     CSR   ECHK1         ENDSR

     C**************************
     C* CHECK SCREEN 1 - CHECK TAG FILE FOR MATCH RECORDS
     CSR   CHK1T         BEGSR
     C**************************
     C*
     C* CHECK FOR ITM MATCH
     C                   IF        WSTYP = 'I'
     C* RC: 22Nov2013 - Although it says "OETAG FILE..." there is no
     C*                 referrence to OETAG in this subroutine.
     C* GOOD REC , ADD TO OETAG FILE
     C* MOVE FIELDS
     C     S1QTS         SUB       S1OQT         UNUSED            6 0
     C                   IF        UNUSED > 0
     C     D1KEY2        SETLL     OEDT1
     C     D1KEY2        READE     OEDT1                                  31
     C                   DOW       *IN31 = *OFF

     C                   IF        D1QTO > D1QTS

     C     D1QTO         SUB       D1QTS         AVAIL             6 0
     C                   IF        AVAIL > UNUSED
     C     D1QTO         SUB       UNUSED        D1QTO
     C     UNUSED        SUB       UNUSED        UNUSED
     C                   ELSE
     C     D1QTO         SUB       AVAIL         D1QTO
     C     UNUSED        SUB       AVAIL         UNUSED
     C                   ENDIF
     C                   MOVE      D1SMN         S1SMN
     C                   Eval      S1UIN = D1UIN
     C                   Eval      S1UCN = D1UCN
     C                   MOVE      D1DKY         S1DKY
     C                   UPDATE    OEDT1F
     C                   ENDIF
     C                   READE     OEDT1                                  31
     C                   ENDDO
     C                   UNLOCK    OEDT1
     C* DONE UPDATING, ANY QTY LEFT? IF SO WRITE A DET REC
     C                   Eval      ODQTO = 0
     C                   ENDIF
     C                   ENDIF
     C*
     C     ECHK1T        ENDSR

     C**************************
     C* CHECK SCREEN 1 - UPC TAG SCAN???
     CSR   CHK1U         BEGSR
     C**************************
     C                   MOVE      'N'           UPCOK
     C*  If Type = 'Q' we're scanning a sales "TAG"
     C*  So load this tag's order and items.
     C*  NOTE: "TYP" represents the first character
     C*        of the Vendor/Item number field on
     C*        the screen.
 - A C                   IF        (TYP = 'Q') and
|    C                              (WSTYP <> 'Q' or
|    C                               POS_Mode=MODE_SALESMAN)
|LCT c     SALESTAG      SETLL     OEHDR3
|LCT c     SALESTAG      READE     OEHDR3
|LCT c                   DOW       NOT %EOF(OEHDR3)

     c                   clear                   oh3
     c                   eval      oh3.ohnet = h3Net
     c                   eval      oh3.ohord = h3Ord
     c                   eval      oh3.ohinv = h3Inv
     c                   eval      oh3.ohtyp = h3Typ
     c                   eval      oh3.ohtag = h3Tag

|    C* GOOD REC , ADD TO OETAG FILE
|    C     OTKEY         CHAIN     OETAG                              99
|  F C                   IF        NOT %FOUND()
      /free
           tagRejected = *OFF;
           if  NOT checkTag( salesTag : wsOrd);
               upcOK = 'N';
               tagRejected = *ON;
               WSERR = ERR(50);
               *IN90 = *ON;
               leaveSR;
           endif;
      /end-free

|    C* MOVE FIELDS
|    C                   MOVE      'Y'           UPCOK             1
|    C                   MOVE      WSORD         OTORD
|    C                   MOVE      H3TYP         OTTYP
|    C                   MOVE      H3INV         OTREF
|    C                   Z-ADD     H3IDT         OTIDT
|    C                   WRITE     OETAGF
|    C* UPDATE HEADER
|    C     WSORD         CHAIN     OEHDR                              99
      /free
           if (s1Cus = *ALL'9') or
              (defaults.isBondedStore and h3Bond <> '');
      /end-free
|    C                   MOVE      H3CUS         OHCUS
|    C                   MOVE      H3CUS         S1CUS
|    C                   EXSR      GETCUS
|    C                   MOVE      WSTYS         OHTYS
|    C                   MOVE      H3CUS         SVCUS
|  X C                   endif
|
|    C                   MOVE      H3fill        OHfill
|    C                   MOVE      H3PON         OHPON
|    C                   MOVE      H3SHP         OHSHP
|    C                   MOVE      H3LSM         OHOSM
|    C                   MOVE      H3CSM         OHCSM
|    C                   MOVE      H3PDO         OHPDO
|    C                   MOVE      H3STR         OHSTR
|    C                   MOVE      H3LWS         OHOWS
|    C                   MOVE      H3RPO         OHRPO
|    C                   move      h3won         ohwon
|    C                   MOVE      H3DID         OHDID
|    C                   MOVE      H3LID         OHLID
|    C                   MOVE      H3PID         OHPID
|    C                   MOVE      H3CID         OHCID
|    C                   MOVE      H3AIN         OHAIN
|     /free
                 if (defaults.isBondedStore);
                    ohBond  = h3Bond;
                    wsBond# = h3Bond;
                    if (ohBond <> '');
                      wsBond = 'Y';
                      svBond = wsBond;
                    endif;
                 endif;

                 if %found(oehdr);
                   update oehdrf;
                 endif;
                 exsr  chk1uA;
                 leaveSR;
               endif;
               readE (salesTag) OEHDR3;
             enddo;
           endif;

            if (Typ = 'I' and wsTyp = 'C');
              chain (typ : ref : idt) ohhd2;
              if %Found();
              exec SQL SELECT otOrd
                       INTO  :otOrd
                       FROM   oeTag
                       WHERE  otTyp = :h2Typ and
                              otRef = :h2Inv and
                              otIDT = :h2IDT
                       FETCH FIRST ROW ONLY;
                if  (SQLState >= '02000');
                  upcOK = 'Y';
                  EXEC SQL INSERT INTO OETAG (OTORD, OTTYP, OTREF, OTIDT)
                                  VALUES(:wsORD, :h2Typ, :h2Inv, :h2idt);
                  leaveSR;
                ENDIF;
              ENDIF;
      /end-free
     C                   if        (SQLState >= '02000')
|  | C                   MOVE      'Y'           UPCOK             1
|  | C                   MOVE      WSORD         OTORD
|  | C                   MOVE      H2TYP         OTTYP
|  | C                   MOVE      H2INV         OTREF
|  | C                   Z-ADD     H2IDT         OTIDT
|  | C                   WRITE     OETAGF
|  | C                   LeaveSR
|  W C                   ENDIF
 -Y  C                   ENDIF                                                  ENDTYI=Q

 ----C                   IF        TYP = 'Q'
|    C                              and WSTYP = 'Q'
|    C                              and WSNIT = *ZEROS
|     **  QUOTE CAN IMPORT
|    C     SALESTAG      CHAIN     OEHDR3                             99
|   -C                   IF        %Found()
|  |  **  GOOD REC, CHANGE QUOTE TO THIS QUOTE
|  |  **  MOVE FIELDS
|  | C                   Eval      WSORD = H3ORD
|  | C                   MOVE      H3CUS         SVCUS
|  | C                   MOVE      'Y'           UPCOK             1
|  | C                   LeaveSR
|   -C                   ENDIF
 ----C                   ENDIF                                                  ENDTYP=Q
     C*
     C     ECHK1U        ENDSR

     C**********************************************************
     CSR   CHK1UA        BEGSR
     C************************************************************
     C* ADD RECS TO ORDER FROM TAG AND ALLOCATE QTYS

     C* READ ALL DETAIL RECS FROM TAG SCANNED ORDER
     C     D2PKY         SETLL     OHDT2
     C     D2PKY         READE(N)  OHDT2                                  31
     C                   DOW       *IN31 = *OFF
     C                   Z-ADD     D2QTS         AVAIL             6 0
     C                   IF        D2ITP < 3
     C     D1KEY         SETLL     OEDT1
     C     D1KEY         READE     OEDT1                                  32
     C                   DOW       *IN32 = *OFF
     C                              and AVAIL > *ZEROS

     C                   IF        D1QTO < D1QTS
     C                   Eval      D1QTO = D1QTO + AVAIL

     C                   IF        D1QTO > D1QTS
     C                   Eval      AVAIL = D1QTO - D1QTS
     C                   Eval      D1QTO = D1QTS
     C                   ENDIF
     C                   MOVE      D2SMN         D1SMN
     C                   Eval      D1UIN = D2UIN
     C                   Eval      D1UCN = D2UCN
     C                   MOVE      D2DKY         D1DKY
     C                   UPDATE    OEDT1F
     C                   ENDIF
     C                   READE     OEDT1                                  32
     C                   ENDDO
     C                   UNLOCK    OEDT1
     C                   ENDIF

     C*  DONE UPDATING, ANY QTY LEFT? IF SO WRITE A DETAIL REC
     C                   IF        AVAIL <> *ZEROS
     C                   MOVE      WSORD         ODORD
     C                   Eval      ODSEQ = WSLSEQ
     C                   Eval      WSLSEQ = WSLSEQ + 1
     C                   Eval      ODSEQ = WSLSEQ

     C                   IF        D2ITP < 3
     C                   Eval      ODQTO = AVAIL
     C                   Eval      ODQTS = *ZEROS
     C                   ELSE                                                   CREDITMEMO
     C                   Eval      ODQTO = AVAIL
     C                   Eval      ODQTS = AVAIL
     C                   ENDIF

     C                   MOVE      D2ITP         ODITP

     C                   MOVE      D2VEN         ODVEN
     C                   MOVE      D2ITM         ODITM
     C                   MOVE      D2DES         ODDES
     C                   MOVE      D2SMN         ODSMN
     C                   Eval      ODUIN = D2UIN
     C                   Eval      ODUCN = D2UCN
     C                   MOVE      D2DKY         ODDKY
      *
U2804C                   MOVE      D2DEF         ODDEF
     C                   MOVE      D2DMP         ODDMP
U2804C                   MOVE      D2DMT         ODDMT

      **  Salesperson ID from TAG file
      **  (Authorization is kept at the detail level for discounts?)
     C                   Eval      ODDID = D2DID
     C                   Eval      ODSCL = D2SCL
     C                   Eval      ODICL = D2ICL

U2804C                   MOVE      D2RETR        ODRETR
U2804C                   MOVE      D2CEXG        ODCEXG

      /free
             if (defaults.IsBondedStore);
                 ohBond = wsBond;
             endif;
             write oedtlf;
           endif;
      /end-free
     C                   If        NOT %EOF(OHDT2)
     C                   IF        *IN01
     C                   update    ohdt2a                               12
     C                   elseif    *IN02
     C                   update    ohdt2b                               12
     C                   endif
     C                   endif

     C     D2PKY         READE(N)  OHDT2                                  31
     C                   ENDDO

     CSR   ECKU1A        ENDSR

     C**************************
     CSR   CHK2          BEGSR
     C**************************
     C*
     C* RESET ERROR INDICATOR
     C*
     C                   eval      *IN48 = *OFF
     C                   eval      *IN90 = *OFF
     C* CHECK CUSTOMER FIELDS FOR CHANGE/SEARCH
     C                   IF        S1CUS = *BLANKS
     C                              or S1CUS = '00000'
     C                   MOVEL     ERR(29)       WSERR
     C                   MOVE      *ON           *IN90
     C                   LeaveSR
     C                   ENDIF

     C                   IF        SVCUS <> S1CUS
     C                   EXSR      CHKCUS

      /free
           if (defaults.isBondedStore);
              wsBond = 'N';
              wsBond# = '';
              svBond = wsBond;
           endif;
      /end-free

     C* BAD      CUSTOMER #
     C                   IF        *IN90 = *ON
     C                   LeaveSR
     C                   ENDIF
     C* GOOD CUSTOMER , DO REQD CHANGES
     C* MOVE FIELDS/UPDATE HEADER
     C                   MOVEL     CMNME         S1NME
     C                   ENDIF
     C*
     C     wsord         CHAIN     OEHDR                              92
     C                   IF        NOT %Found(OEHDR)
     C                   Eval      OHINV = *ZEROS
     C                   eval      OHORD = WSORD
     C* MOVED TO ALWAYS    MOVELWSTYP     OHTYP
     C                   Eval      OHODT = WSYMD
     C                   endif

     C                   MOVEL     WSTYP         OHTYP
     C                   MOVEL     WSTYS         OHTYS
     C                   MOVEL     S1CUS         OHCUS
     C                   MOVEL     CMNME         OHCNM
     C                   MOVE      CMRPO         OHRPO
     C                   MOVE      WSPDO         OHPDO
     C                   eval      ohTme = %dec(%Time())
     C                   exsr      cpyAutFromDSP
     C                   MOVE      WSPON         OHPON
      /free
           if (defaults.isBondedStore);
              ohbond = wsBond#;
           endif;
      /end-free

     C* 92 = NEW POS HEADER RECORD
     C                   if        %Found(OEHDR)
     C                   UPDATE    OEHDRF
     C                   endif

     C                   IF        SVCUS <> S1CUS
     C                   EXSR      RETOTAL
     C                   MOVE      S1CUS         SVCUS             5
     C                   LeaveSR
     C                   ENDIF

     C* HARD ERROR - DO NOT LET PASS
     C                   IF        OHTYS = 'G'
     C                   IF        WSNITI = *ZEROS
     C                   GOTO      OKNIT2                                       REDO SCRN
     C                   ENDIF
     C                   IF        WSNITC = *ZEROS
     C                   GOTO      OKNIT2                                       REDO SCRN
     C                   ENDIF
     c                   IF           OHWON <> 0
     C                   GOTO      OKNIT2                                       REDO SCRN
     C                   ENDIF
     C* CAN'T USE THIS CUSTOMER FOR MIXED SALE TRANSACTIONS
     C                   eval      *IN90 = *ON
     C                   MOVEL     ERR(27)       WSERR
     C                   LeaveSR
     C     OKNIT2        TAG                                                    REDO SCRN
     C                   ENDIF

      /free
           // CHECK CUSTOMER OVERRIDES
           //  RC: 03Sept2012 - Change to support J.Watson request
           //                   "Do not require override/authorization
           //                   when House Charge and Credit"

             // ohCID = Credit Password/Authorization
             //         Means okay to return items???

             // ohLID = Credit Limit Password/Authorization
             //         Means customer can exceed their credit limit.

             // ohPID = Past Due Password/Authorization
             //         Means customer can purchase even though Past Due

           // if  (ODITP >= 3          // Returns?
           //      and ODQTS > 0
           //      or ODITP < 3
           //      and ODQTS < 0 ); // Is it a Return?

           // if (wsPAYT = 'H' and ohNet < 0);  replaced on 22April2014 with below code
           //         relocated "if wsQTE <> 'Y') to here to avoid missing it.

          if (wsQTE <> 'Y');   // Not a quote?
           if ( (wsPAYT = 'H' and ohNet < 0) or
                (cmcus >= 90000 and ohNet < 0) );
             bReturns = *ON;
           else;
             bReturns = *OFF;
           endif;
           bIsEmployee = (ohCus >= 6000 and ohCus <= 6999);
           for counters.i = 1 to 1;
         //    if (bReturns);  // Credit or Return?  AUT39       lc
         //        leave;      // Do nothing.                    lc
         //     endif;                                           lc

                // The variable below "newCreditLimit" is no longer used.
                // So get rid of it.
            //  newCreditLimit = (cmcla - (wsNow + wsRem));

            //    if (NOT bIsEmployee)  ;
            //      if (ohlid = 0 and newCreditLimit <= 0);
            //          leave;
            //      endIf;
            //      if (ohpid = 0) and (wsAge > 0);
            //          leave;
            //      endIf;
            //    endif;
                   // END: J. Watson "Do not check for overrides
                   //       when House Charge or Sale is a Credit
              IF (OHCID = 0 and creditReturnCount > 0);
                wsErr = Err(39);
                *IN90 = *ON;
                LeaveSR;
              ENDIF;

              IF (OHPDO = *BLANKS
                    and WSAGE > 0
                    and ohpid = 0
                    and bReturns = *OFF);
                // ACCOUNT IS PAST DUE W/NO OVERRIDE
                *IN90 = *ON;
                wsErr = err(21);
                LeaveSR;
              ENDIF;

              // Is the CHARGE LIMIT Exceeded?
               chkamt = cmcla - (wsNow+wsRem);

               IF (OHLID = 0 and CHKAMT <= 0 and bReturns = *OFF);
                 // ACCOUNT IS OVER CREDIT LIMIT
                 *IN90 = *ON;
                 wsErr = err(22);
                 LeaveSR;
               ENDIF;
             endfor;
           endif;

             // I changed the OHPON test, so that it compares
             // to *ZEROS and *BLANKS because OHPON is type *CHAR.
           IF OHRPO = 'Y' and (OHPON = *ZEROS or OHPON = ' ');
             // P.O. Number REQUIRED?
             *IN90 = *ON;
             wsErr = ERR(23);
             LeaveSR;
           ENDIF;
      /END-FREE
     C                   IF        WSPAYT <> 'C'
     C                              and WSPAYT <> 'H'
     C                              and WSPAYT <> 'R'
     C                              and WSPAYT <> 'K'
     C                              and WSPAYT <> 'V'
     C* INVAILD PAY TYPE
     C                   eval      *IN90 = *ON
     C                   MOVEL     ERR(4)        WSERR
     C                   LeaveSR
     C                   ENDIF

     C                   IF        isNotCashCust(OHCUS)
     C                                and WSPAYT <> 'H'
     C                   eval      wsPayT = 'H'
     C* INVAILD PAY TYPE  - MUST BE H FOR HOUSE ACCOUNT
     C                   eval      *IN90 = *ON
     C                   MOVEL     ERR(7)        WSERR
     C                   LeaveSR
     C                   ENDIF
     C*
     C* IF CASH, CALCULATE CHANGE
     C                   IF        WSPAYT = 'C'
     C                   IF        WSAMT >= WSREM
     C     WSAMT         SUB       WSREM         WSCGV
     C                   ELSE
     C* INVAILD CASH COLLECTED
     C                   eval      *IN90 = *ON
     C                   MOVEL     ERR(3)        WSERR
     C                   LeaveSR
     C                   ENDIF
     C                   ENDIF
     C*
     C* DON'T ALLOW CHARGE ON CASH ACCOUNT
     C                   IF        WSPAYT = 'H'and isCashCust(ohcus)
     C                   eval      *IN90 = *ON
     C                   MOVEL     ERR(15)       WSERR
     C                   LeaveSR
     C                   ENDIF

     C* DISPLAY CHECK # IF PPAYT = K
     C                   IF        WSPAYT = 'K'
     C                   EXSR      DSP02W
     C                   IF        wsds.fKey = F1 or wsds.fKey = F3 or
     C                                wsds.fKey = F12
     C                   eval      *IN90 = *OFF
     C                   LeaveSR
 7508C                   ENDIF
     C                   ENDIF
     C*
     C* CHECK TO SEE IF REDISPLAY OR PRINT INVOICE
     C                   IF        WSPAYT = 'C'
     C                              and WSAMT <> SVAMT
     C                   MOVE      *ON           *IN72                          PC ON AMT
     C                   MOVE      *OFF          *IN70                          PC ON CUS
     C                   MOVE      *OFF          *IN71                          PC ON TYP
     C                   Z-ADD     WSAMT         SVAMT             9 2
     C                   CALL      'POSOPEN'
     C                   LeaveSR
     C                   ENDIF
     C* VOUCHER
     C                   IF        WSPAYT = 'V'
     C                   eval      *IN90 = *ON
     C                   MOVEL     ERR(19)       WSERR
     C                   LeaveSR
     C                   ENDIF

      *************************************************************
      *  PRINT INVOICE
      *************************************************************
      /FREE
           EXSR printInvoice;

           IF SDACPT = 'N';
             *IN90 = *ON;
             wsErr = Err(8);
             LeaveSR;
           ENDIF;

           // DELETE QUOTES USED FOR THIS INVOICE
           EXSR delQuote;
           // F10 = DISPLAY INVOICE SCREEN ACCEPTED/FINISHED, CLEAR WORK FIELDS
           clearNewOrder();

           Screen = 'ORDDETAIL';
           IF POS_Mode <> MODE_SALESMAN;
             wsTyp = 'Q';
           endif;
      /END-FREE
     CSR   ECHK2         ENDSR


     C**************************
     C*  Check 3
     C**************************
     CSR   CHK3          BEGSR

      /free
            *IN90 = *OFF;
            if (wsTag <> '');  // Validate syntax of call TAG (allow blanks)
               if (%subst(WSTAG:1:1) <> 'Q') or
                 (NOT (%subst(WSTAG:2:1) >= 'A' and %subst(WSTAG:2:1) <= 'Z'))
                 or (%CHECK(DIGITS:%subst(WSTAG:3))>0) or
                     (%subst(WSTAG:3)='000');
                   *IN90 = *ON;
                 wsErr = 'TAG SYNTAX must be QAnnn. e.g., QA238, QB038, QC012';
                   leaveSR;
               ENDIF;
            ENDIF;
      /end-free
     C* CHECK CUSTOMER FIELDS FOR CHANGE/SEARCH
     C                   IF        (%check('0 ':S1CUS) = 0)
     C                   MOVEL     ERR(29)       WSERR
     C                   MOVE      *ON           *IN90
     C                   LeaveSR
     C                   ENDIF

     C                   IF        SVCUS <> S1CUS
     C                   EXSR      CHKCUS
     C* BAD CUSTOMER #
     C                   IF        *IN90 = *ON
     C                   LeaveSR
     C                   ENDIF
     C* GOOD CUSTOMER, DO REQD CHANGES
     C* MOVE FIELDS/UPDATE HEADER
     C                   MOVEL     CMNME         S1NME
     C                   ENDIF

     C     WSORD         CHAIN     OEHDR                              92
     C                   IF        NOT %Found(OEHDR)
     C                   Z-ADD     *ZEROS        OHINV
     C                   MOVEL     WSORD         OHORD
     C                   MOVEL     WSTYP         OHTYP
     C                   Z-ADD     WSYMD         OHODT
     C                   ENDIF

     C                   MOVEL     WSTYP         OHTYP
     C                   MOVEL     WSTYS         OHTYS
     C                   MOVEL     S1CUS         OHCUS
     C                   MOVEL     CMNME         OHCNM
     C                   MOVE      CMRPO         OHRPO
     C                   MOVE      WSPDO         OHPDO
     C                   exsr      cpyAutFromDSP
      /free
           ohpon = %TRIML(wsPON);
           ohtme = %dec(%time());

           if (defaults.isBondedStore);
              if (wsBond = 'Y');
                ohBond = wsBond#;
              else;
                ohBond = '';
              endif;
           endif;
      /end-free
     C* Update POS HEADER RECORD
     C                   if        %Found(OEHDR)
     C                   UPDATE    OEHDRF
     C                   endif

     C* AFTER, CHECKS IF CUS CHANGED RESHOW
     C                   IF        S1CUS <> SVCUS
     C                   EXSR      RETOTAL
     C                   MOVE      S1CUS         SVCUS             5
     C                   LeaveSR
     C                   ENDIF
     C* HARD ERROR - DO NOT LET PASS
     C                   IF        OHTYS = 'G'
     C                   IF        WSNITI = *ZEROS
     C                   GOTO      OKNIT3                                       REDO SCRN
     C                   ENDIF
     C                   IF        WSNITC = *ZEROS
     C                   GOTO      OKNIT3                                       REDO SCRN
     C                   ENDIF
     C                   IF        OHWON <> 0
     C                   GOTO      OKNIT3                                       REDO SCRN
     C                   ENDIF
     C* CAN'T USE THIS CUSTOMER FOR MIXED SALE TRANSACTIONS
     C                   eval      *IN90 = *ON
     C                   MOVEL     ERR(27)       WSERR
     C                   LeaveSR
     C     OKNIT3        TAG                                                    REDO SCRN
     C                   ENDIF

      /free
           bReturns = *OFF;
           if (wsQte <> 'Y');   // If not a quote, then continue...

           if ((wsPAYT = 'H' or wsPayT = '') and ohNet < 0);
             bReturns = *ON;    // It is a RETURN
           else;
             bReturns = *OFF;   // It is NOT a RETURN
           endif;
           if (NOT bReturns) or // If not a return validate the TAG syntax
              (POS_Mode <> MODE_CASHIER); // RC: 09Aug2014 - Added "or Counterman" per J.Watson
                                         // Note: "not cashier" is equal to "Is Counterman".
             if (wsTag = '');
               wsErr = ERR(47);
               *IN90 = *ON;
               LeaveSR;
             ENDIF;
           endif;

           //  RC: 12Nov2012 - Change to support J.Watson request
           //                   "Do not require override/authorization
           //                   when House Charge and Credit"
           //  lc: 02/04/2013   "Do not require override/authorization
           //                   when it is an employee account which are
           //                   in 6000 series"

           for counters.i = 1 to 1;

           //if (wsQte = 'N' and wsTag <>' ');
           if (WSINVTOT < 0  or bReturns);
              if (ohpid = 0) and (wsAge > 0);
                  leave;
              endIf;
              if (ohcus >= 06000) and (ohcus <= 07000);
                  leave;  // Skip employee accounts
              endIf;
           endif;  // END: J. Watson "Do not check for overrides
                   //       when House Charge and Sale is a Credit
      /end-free

     C* CHECK CUSTOMER OVERRIDES
     C                   IF        OHPDO = *BLANKS
     C                              and WSAGE > *ZEROS
     C                              and ohpid = *ZEROS
     C                              and bReturns = *OFF
      *added ohpid 050503 ch
     C* ACCOUNT IS PAST DUE W/NO OVERRIDE
     C                   MOVEL     ERR(21)       WSERR
     C                   eval      *IN90 = *ON
     C                   LeaveSR
     C                   ENDIF

     C* Charge LIMIT exceeded?
     C                   eval      chkamt = cmcla - (wsNow+wsRem)
     C                   IF        OHLID = 0 and CHKAMT <= 0 and bReturns = *OFF
     C
     C* ACCOUNT IS OVER CREDIT/CHARGE LIMIT
     C                   eval      *IN90 = *ON
     C                   MOVEL     ERR(22)       WSERR
     C                   LeaveSR
     C                   ENDIF
      /free
            endfor;
          endif;
      /end-free

     C                   IF        creditReturnCount > 0 and ohcid = 0
     C***  AUT39                    and NOT bReturns
     C                   eval      *IN90 = *ON
     C                   eval      wsErr = Err(39)
     C                   LeaveSR
     C                   ENDIF

      **  CHECK OTHER DATA
     C                   IF        OHRPO = 'Y'
     C                              and (WSPON = *ZEROS or WSPON = ' ')
     C                   eval      *IN90 = *ON
     C                   MOVEL     ERR(23)       WSERR
     C                   LeaveSR
     C                   ENDIF
      **  CHECK PRINT QTE MUST BE Y/N
     C                   IF        WSQTE <> 'Y'
     C                              and WSQTE <> 'N'
     C                   eval      *IN90 = *ON
     C                   MOVEL     ERR(25)       WSERR
     C                   LeaveSR
     C                   ENDIF
      **  IF TAG SCAN then MAKE SURE UNIQUE (today)
     C                   IF        WSTAG <> *BLANKS
     C     WSTAG         CHAIN     OEHDR3                             99
     C                   IF        *IN99 = *OFF
     C                              and H3ORD <> OHORD
     C                   eval      *IN90 = *ON
     C                   MOVEL     ERR(24)       WSERR                          TAG IN USE
     C                   LeaveSR
     C                   ENDIF
     C                   ENDIF
      **  UPDATE TAG IN HEADER HERE WHEN IT IS COMPLETE
     C     WSORD         CHAIN     OEHDR                              92
     C                   exsr      cpyAutFromDSP
     C                   MOVE      WSPON         OHPON
     C                   MOVE      WSTAG         OHTAG
      ** 92 = NEW POS HEADER RECORD
     C                   eval      ohTme = %dec(%Time())
     C                   if        %Found()
     C                   UPDATE    OEHDRF
     C                   endif
      **  PRINT INVOICE
     C                   IF        WSQTE = 'Y'
     C                   EXSR      printInvoice
     C                   ELSE
     C                   EXSR      GETINV
     C                   EXSR      UPDINV
     C                   ENDIF
      **  SHOW NEXT QUOTE/SCREEN
      /free
           clearNewOrder();
      /end-free
     C                   eval      Screen = 'ORDDETAIL'
     CSR                 ENDSR

      ** ***********************************************************
      **  AUTHORIZATION OVERRIDE ROUTINES
      ** ***********************************************************
     CSR   CHK9          BEGSR
     C*  VERIFY BADGE SCANS
     C*  INITIALIZE SECURITY CHECK FIELDS
     C*  CLEAR ERROR INDICATOR
     C                   eval      *IN90 = *OFF
     C                   MOVEL     'POS'         APP              10
     C                   MOVEL     'ONLINE  '    MENU             10
     C                   MOVEL     '001'         OPT               3
     C                   MOVEL     *BLANKS       FUNC

     C                   if        POS_Mode = MODE_CASHIER
     C                   MOVE      '002'         OPT
     C                   endif

     C* CHECK FOR PROGRAM ACCESS SECURITY AND INITIAL PROMPT FOR BADGE
     C                   MOVE      *BLANKS       SMN                            SLSMAN
     C                   MOVE      *BLANKS       SEC               1            SEC CLRNCE
     C                   MOVE      'N'           PRMPT             1            PRMPT FOR BADGE

      ** +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
     C** ++  CHECK PID PAST DUE OVERRIDE                          ++
      ** +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
     C                   IF        PIDBDG <> *BLANKS
     C* BADGE SCANNED
     C                   eval      WSPID = 0
     C*
     C     PIDBDG        CHAIN     SLMA1                              90
     C                   IF        *IN90 = *ON
     C                   MOVEL     ERR(32)       WSERR
     C                   LeaveSR
     C                   ENDIF
     C                   MOVE      SMSMN         SMN
     C                   MOVEL     'PSTDUE '     FUNC
     C* CHECK SECURITY
     C                   EXSR      GETSEC
     C* FAILED
     C                   if        SEC = 'Y'
     C                   MOVE      SMN           WSPID
     C                   eval      WSPDO = 'P'
     C                   ELSE
     C                   MOVEL     ERR(33)       WSERR
     C                   MOVE      *ON           *IN90
     C                   ENDIF
      **  END OF PID CHECK
     C                   ENDIF
      ** +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


      ** +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      ** ++  CHECK LID OVER CHARGE LIMIT                          ++
      ** +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
     C                   IF        LIDBDG <> *BLANKS
      **  BADGE SCANNED
     C                   Eval      WSLID = 0

     C     LIDBDG        CHAIN     SLMA1                              90
     C                   IF        *IN90 = *ON
     C                   MOVEL     ERR(34)       WSERR
     C                   LeaveSR
     C                   ENDIF
     C                   MOVE      SMSMN         SMN
     C                   MOVEL     'OVRLMT '     FUNC
      **  CHECK FOR EMPLOYEE'S DIFFERENT
     C                   IF        OHCUS >= 06000
     C                              and OHCUS <= 07000
      **  EMPLOYEE ACCOUNT
     C                   MOVEL     'EOVRLMT'     FUNC
     C                   ENDIF
      **  CHECK SECURITY
     C                   EXSR      GETSEC
      **  FAILED
     C                   IF        SEC = 'Y'
     C                   MOVE      SMN           WSLID
     C                   ELSE
     C                   MOVEL     ERR(35)       WSERR
     C                   MOVE      *ON           *IN90
     C                   IF        FUNC = 'ECRDLMT '
     C                   MOVEL     ERR(36)       WSERR
     C                   ENDIF
     C                   ENDIF
      ** +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
     C* END OF LID CHECK
     C                   ENDIF
      ** +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


      ** +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      ** ++  CHECK CID CREDIT/RETURNS                             ++
      ** +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
     C                   IF        CIDBDG <> *BLANKS
      **  BADGE SCANNED
     C                   Eval      WSCID = 0

     C     CIDBDG        CHAIN     SLMA1                              90
     C                   IF        NOT %FOUND()
     C                   eval      WSERR = Err(37)
     C                   LeaveSR
     C                   endif

     C                   move      '1'           override          1
     C                   MOVE      SMSMN         SMN
     C                   eval      Func = 'CNOINV'
     C                   EXSR      GETSEC
     C     SEC           ifne      'Y'
     C                   eval      Func = 'CKEY'
     C                   move      ' '           override          1
     C                   endif

     C                   z-add     0             hldcount          5 0
      **  CHECK SECURITY
     C                   EXSR      GETSEC
      **  Successfull?
     C                   if        SEC = 'Y'
     C                   eval      hldcount = creditReturnCount
     C                   eval      creditReturnCount = -1
     C                   move      SMN           WSCID
     C                   else
     C                   eval      *IN90 = *ON
     C                   eval      WSERR = Err(38)
     C                   ENDIF
      ** +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
     C* END OF CID CHECK
     C                   ENDIF
      ** +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      ** +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      ** ++  CHECK DID OVERRIDE Discount                          ++
      ** +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
     C                   if        DIDBDG <> *BLANKS
      **  BADGE SCANNED
     C                   Eval      WSDID = 0

     C     DIDBDG        CHAIN     SLMA1                              90
     C                   IF        *IN90 = *ON
     C                   MOVEL     ERR(37)       WSERR
     C                   GOTO      ECHK9
     C                   ENDIF
     C                   MOVE      SMSMN         SMN
     C                   eval      FUNC = 'IDSCNT'
      **  CHECK SECURITY
     C                   EXSR      GETSEC
      **  FAILED
     C                   IF        SEC = 'Y'
     C                   MOVE      SMN           WSDID
     C                   ELSE
     C                   MOVEL     ERR(38)       WSERR
     C                   MOVE      *ON           *IN90
     C                   ENDIF
      ** +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
     C* END OF DID CHECK
     C                   ENDIF
      ** +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

     C                   if        *IN90 = *OFF
     C                             and (PIDBDG = *BLANKS
     C                              and LIDBDG = *BLANKS
     C                              and CIDBDG = *BLANKS
     C                              and DIDBDG = *BLANKS)
     C                             and (wsDID = 0
     C                              and wsLID = 0
     C                              and wsPID = 0
     C                              and wsCID = 0)
     C                   eval      wsErr = Err(45)
     C                   eval      *IN90 = *ON
     C                   endif


     CSR   ECHK9         ENDSR


     C**************************
     C* CUSTOMER SEARCH
     C     CHKCUS        BEGSR
     C**************************
     C* CHKCUS = ALPHA DO CUST SEARCH
     C                   IF        WSNITC <> *ZEROS
     C                   MOVEL     ERR(28)       WSERR
     C                   MOVE      *ON           *IN90
     C                   MOVE      SVCUS         S1CUS
     C                   LeaveSR
     C                   ENDIF
      **  If all digits, then right-justify and zero fill.
      **  Otherwise, do a customer search/lookup.
     C     ' 0123456789' CHECK     S1CUS
     C                   if        NOT %Found()
     C                   evalR     S1CUS = '00000' + %Trim(S1CUS)
     C                   else
      /free
            pos = %scan(' ' : s1cus);
            if (pos > 1);          // Clean up "W 345' type of entries
              %subst(s1cus:pos) = '';
            ENDIF;
             // Use new Search program directly.
             // Avoid "stub" CMSRCH which simply calls CSTSEARCH
            custNo7 = custNo;
            cstSearch( %trimR(S1CUS) : custNo7 );
            if (custNo > 0);
                monitor;
                custNo = CustNo7;
                on-error;
                  // do nothing
                  custNo = 0;
                endmon;
                evalR s1Cus = %editC(custNo : 'X');
            endif;
      /end-free
     C**                  MOVEL     S1CUS         CMSRCH
     C**                  CALL      'CMSRCH'
     C**                  PARM                    CMSRCH           30
     C**                  PARM                    PCUS              5
     C*                   IF        PCUS <> *BLANKS
     C*                  MOVEL     PCUS          S1CUS
     C*                  ENDIF
     C                   ENDIF
     C* GET CUS DATA
     C                   EXSR      GETCUS
     C     ECHKCU        ENDSR
     C**************************
     C**************************
     C* CHECK FOR OPEN DRAWER
     CSR   CKDRAWER      BEGSR
     C**************************
     C                   MOVE      *BLANKS       PAYT
     C                   Eval      DRAW = 0
     C     CRKEY         SETLL     CSHR1
     C                   READ      CSHR1                                  30
     C  N30              UNLOCK    CSHR1
     C                   DOW       *IN30 = *OFF
     C                              and CSLWS = JobName
     C                              and CSIDT = WSYMD

     C                   IF        CSPAY = 'A'
     C                   Z-ADD     CSDRAW        DRAW              5 0
     C                   MOVE      'Z'           PAYT
     C     CRKEY         CHAIN     CSHR1                              99
     C  N99              UPDATE    CSHRF
     C                   IF        *IN99 = *ON
     C* OPEN REGISTER
     C                   Eval      WSDRAW = CSDRAW
     C     wssmn         ifne      cssmn
     C                   move      '1'           chgslm            1
     C                   else
     C                   move      *blank        chgslm
     C                   end
     C                   Eval      WSsmn  = CSsmn
     C                   GOTO      ECKDRA
     C                   ENDIF
     C                   ENDIF
     C                   READ      CSHR1                                  30
     C  N30              UPDATE    CSHRF
     C                   ENDDO
     C*
     CSR   ECKDRA        ENDSR

     C**************************
     C* CLEAR ACTIVE ORDER Control Fields
     CSR   CLRCTL        BEGSR
      /free
           chain (wsord) oectl;
           OCORD = wsord;
           clear OCSMN;
           clear OCSCD;
           if %found(oectl);
             update oectlf;
           else;
             write oectlf;
           endif;
      /end-free
     C     ECLRCT        ENDSR

     C**************************
     C* CLEAR SIGNATURE REQUEST
     CSR   DELSIGR       BEGSR
      /free
            // For the record  (R.Cozzi,Jr.)
            // This code/logic makes no sense to me.
            // I don't understand why we don't "chain"
            // to the SIGR file by the invoice number
            // rather than just get today's date/time/wsid
            // and then read the first invoice number for
            // that period. We already know the invoice
            // number we want to delete.  Very odd code!
           SETLL (wsid:tDat:tTim) SIGR;
           read  SIGR;
           dow NOT %EOF();
             if (srInvN <> '' and WSORD = %int(srInvN));
                delete SIGR;
             endif;
             read  SIGR;
           enddo;
      /end-free
     C     ECLRSI        ENDSR

     C**************************
     C* DELETE RECORD
     CSR   DELSIGD       BEGSR
      /free
              // For some reason we delete the captured signature.
              // This seems very odd to me.
           CHAIN ( %editC(wsord:'X') ) SIGD1;
           if %FOUND();  // Delete the signature record itself.
                delete SIGD1;
           endif;
      /end-free
     CSR   EDELSI        ENDSR

     C**************************
     C* DELETE QUOTES THAT PRINTED
     C**************************
     CSR   delQuote      begsr
      /free
         if (ohTyp = 'Q');
         EXEC SQL DELETE FROM OEHDR
                          WHERE OHTYP = :otTyp and
                                OHINV = :otRef and ohIDT = :otIDT;
         EXEC SQL DELETE FROM OHHDR
                          WHERE OHTYP = :otTyp and
                                OHINV = :otRef and ohIDT = :otIDT;

         EXEC SQL DELETE FROM OEDTL
                          WHERE ODTYP = :otTyp and
                                ODINV = :otRef and ODIDT = :otIDT;
         EXEC SQL DELETE FROM OHDTL
                          WHERE ODTYP = :otTyp and
                                ODINV = :otRef and ODIDT = :otIDT;
         endif;
      /end-free
     CSR   ENDDltTAG     endsr

     CSR   OldDelTag     begSR
         // RC: 27Jan2015
         //     Changed the READE to READ(N) no-lock so that
         //     the TAG file isn't locked during this routine.
         //     D2PKY = (OTTYP : OTREF : OTIDT)
     C     WSORD         SETLL     OETAG
     C     WSORD         READE(N)  OETAG                                  31
 7506C* MOD SDD 9/22/04 DELETE QUOTES THAT HAVE BEEN INVOICED.
     C                   DOW       *IN31 = *OFF
     C     D2PKY         CHAIN     OHHD2                              99
     C                   IF        *IN99 = *OFF
     C                              and H2TYP = 'Q'
     C* PROCESS DETAIL AND DELETE
     C     D2PKY         SETLL     OHDT2
     C                   setoff                                       0102
     C     D2PKY         READE     OHDT2                                  32
     C                   DOW       *IN32 = *OFF
     C* DELETE DETAIL
     C   01              DELETE    OHDT2A                               12
     C   02              DELETE    OHDT2B                               12
     C                   setoff                                       0102
     C     D2PKY         READE     OHDT2                                  32
     C                   ENDDO
     C* DELETE HEADER --LAST
     C   01              DELETE    OHHD2A                               12
     C   02              DELETE    OHHD2B                               12
     C* END OF HEADER
     C                   ENDIF
     C*
     C     WSORD         READE(N)  OETAG                                  31
     C                   ENDDO

     CSR   EDELQU        ENDSR


     C**************************
     CSR   DSP00         BEGSR
     C**************************
     C                   move      chgslm        chgsav            1
     C                   EXSR      CKDRAWER
     C* INITIALIZE FIELDS
     C                   MOVE      *OFF          *IN90
     C* DISPLAY SCREEN POS00
     C                   DOW       SCREEN = 'POS00'
     C                   clear                   strOpt
     C*SDD                 EXSR CLRCTL
     C                   EXFMT     POS00

     C                   eval      *IN48 = *OFF
     C                   eval      *IN90 = *OFF

     C* CHECK COMMAND KEYS
     C* F3 = EXIT PROGRAM
     C                   IF        wsds.fKey = F3
     C                   eval      screen = ''
     C                   MOVE      *ON           *INLR
     C                   RETURN
     C                   ENDIF
     C*
     C* F10 = OPEN DRAWER
     C                   IF        wsds.fKey = F10 or StrOpt = 10
     C* OPNDRAW = OPEN DRAWER
     C                   MOVEL     'OPNDRAW '    FUNC
     C                   EXSR      SECCHK
     C                   IF        SEC <> 'Y'
     C                   Iter
     C                   ENDIF
     C*** NOT ERE          MOVE XXSMN     WSSMN
     C                   EXSR      getSalesMan                                       CHG SMN
     C*
     C                   CALL      'POSOPEN'
     C                   ENDIF

      **  F6=Create new order
     C                   IF        (wsds.fKey = F6 and POS_Mode = mode_cashier)
     C                              or (strOpt = 6 and POS_Mode = mode_cashier)
     c
     C                   exsr      inzOrder

     C                   MOVEL     'IKEY    '    FUNC
     C                   MOVE      'N'           PRMPT
     C                   EXSR      SECCHK
     C                   IF        SEC <> 'Y'
     C                   GOTO      ECHK00
     C                   ENDIF

     C                   EXSR      getSalesMan                                       CHG SMN

     C                   IF        WSDRAW = *ZEROS
     C                   MOVEL     ERR(11)       WSERR
     C                   MOVE      *ON           *IN90
     C                   GOTO      ECHK00
     C                   ENDIF
     C                   if        chgsav <> ''
     C                   MOVEL     ERR(13)       WSERR
     C                   MOVE      *ON           *IN90
     C                   GOTO      ECHK00
     C                   ENDIF
     C                   Eval      WSORD = 0
     C                   eval      S1CUS = *ALL'9'
     C* TEST SDD 12/7      EXSR NEWORD
     C                   CLEAR                   OEHDRF
     C                   MOVE      'I'           WSTYP             1            ORDER TYP
     C                   MOVEL     'INVOICE'     ORDDES                         ORDER DESCR
     C                   MOVE      *OFF          *IN69                          SHOW ORDDES
     C                   eval      screen = 'ORDDETAIL'
     C                   ENDIF

      **  F8 = VOUCHER/CREDITMEMO Authorize
     C                   if        wsds.fKey = F8
     C                   MOVEL     'CKEY    '    FUNC
     C                   MOVE      'N'           PRMPT
     C                   EXSR      SECCHK
     C                   if        SEC = 'Y'
     C                   eval      creditReturnCount = -1
     C                   endif
     C                   IF        SEC <> 'Y'
     C                   LeaveSR
     C                   ENDIF

     C                   EXSR      getSalesMan                                       CHG SMN

     C                   Eval      WSORD = 0
     C                   eval      S1CUS = *ALL'9'
     C***  TEST SDD 12/7  EXSR NEWORD
     C                   CLEAR                   OEHDRF
     C                   MOVE      'C'           WSTYP
     C                   MOVE      *BLANKS       ORDDES
     C***  'VOUCHER\'    CAT       'CREDIT':0    ORDDES
     C                   eval      ORDDES = 'VOUCHER\CREDIT'
     C                   MOVE      *ON           *IN69
     C                   eval      screen = 'ORDDETAIL'
     C                   endif

      **   F9=New QUOTATION
     C                   if        wsds.fKey = F9 or StrOpt = 9
     C                   eval      FUNC = 'QKEY'
     C* SDD                MOVE 'Y'       PRMPT
     C*                    EXSR SECCHK
     C*          SEC       IFNE 'Y'
     C*                    GOTO ECHK00
     C*                    ENDIF
     C                   Eval      WSORD = 0
     C                   eval      S1CUS = *ALL'9'
     C* TEST SDD 12/7      EXSR NEWORD
     C                   CLEAR                   OEHDRF
     C                   eval      WSTYP = 'Q'
     C                   eval      ORDDES = 'REQUEST\QUOTE\RETURN'

     C                   eval      *IN69 = *ON
     C                   eval      SCREEN = 'ORDDETAIL'
     C                   ENDIF

      * F12 = Show my Suspened Orders (from initial POS display)
     C                   if        wsds.fKey = F12 or StrOpt = 12
     C* IKEY  = ORDER ROLL
     C                   MOVEL     'IKEY    '    FUNC
     C                   MOVE      'N'           PRMPT
     C                   EXSR      SECCHK
     C                   IF        SEC <> 'Y'
     C                   GOTO      ECHK00
     C                   ENDIF
     C                   IF        WSDRAW = *ZEROS
     C                   MOVEL     ERR(11)       WSERR
     C                   MOVE      *ON           *IN90
     C                   GOTO      ECHK00
     C                   ENDIF
     C                   MOVE      'I'           WSTYP             1            ORDER TYP
     C* SHOW ORDERS
     C                   EXSR      rollOrder
     C                   GOTO      EDSP00

     C                   ENDIF

     C* F2    OPEN DRAWER
     C                   IF        wsds.fKey = F2 or StrOpt = 2

     C                   MOVEL     'IKEY    '    FUNC                           KEY A INVOIC
     C                   EXSR      SECCHK
     C                   IF        SEC <> 'Y'
     C                   MOVEL     'CKEY    '    FUNC                           KEY A CRDMEM
     C                   EXSR      SECCHK
     C                   IF        SEC <> 'Y'
     C                   GOTO      ECHK00
     C                   ENDIF
     C                   ENDIF

     C                   IF        WSDRAW <> *ZEROS
     C                   MOVEL     ERR(12)       WSERR
     C                   MOVE      *ON           *IN90
     C                   GOTO      ECHK00
     C                   ENDIF
     C                   EXSR      GETDRW
     C                   EXSR      DSP06W
     C                   ENDIF
     C*
     C*  F4 = CLOSE DRAWER/PRINT REPORT
     C                   IF        wsds.fKey = F4 or strOpt = 4
     C                   IF        WSDRAW = *ZEROS
     C                   MOVEL     ERR(13)       WSERR
     C                   MOVE      *ON           *IN90
     C                   GOTO      ECHK00
     C                   ENDIF
     C                   EXSR      DSP07W
     C                   ENDIF
     C     ECHK00        TAG

     C                   ENDDO
     C     EDSP00        ENDSR
     C* PROTECT DESCRIPTION
     C**************************
     CSR   DSP01         BEGSR
     C**************************
     C* INITIALIZE INDICATORS
     C                   MOVE      *OFF          *IN61
     C                   MOVE      *OFF          *IN62
     C                   MOVE      *OFF          *IN63
     C* PROTECT DESCRIPTION
     C                   MOVE      *ON           *IN70
     C* INITIALIZE FIELDS
     C                   CLEAR                   ORDDETAIL
     C* INITIALIZE FIELDS, NEW ORDER
     C                   IF        WSORD = 0
     C                   eval      creditReturnCount = 0
     C* CLEAR NEW ORDER FIELDS
     C                   MOVE      *BLANKS       WSPDO
     C                   ExSr      clearAutDSP
     C                   Z-ADD     *ZEROS        WSAGE
     C                   Z-ADD     *ZEROS        WSNOW
     C                   eval      S1CUS = *ALL'9'

      /free
           if (defaults.isBondedStore);
             wsBond = 'N';
             svBond = wsBond;
             wsBond#= ' ';
           endif;
      /end-free
     C* IF IN SALES/Counterman MODE, PROMPT FOR BADGE
     C                   IF        WSTYP = 'Q'
     C* GET SECURITY
     C                   MOVEL     'QKEY    '    FUNC
     C                   MOVE      'Y'           PRMPT
     C                   EXSR      SECCHK
     C                   IF        *IN90 = *ON
     C                   eval      screen = 'POS00'
     C                   LeaveSR
     C                   ENDIF

     C                   EXSR      getSalesMan
     C                   eval      wsord = 0
     C                   eval      S1CUS = *ALL'9'
     C* TEST SDD 12/7 - EXSR NEWORD
     C                   CLEAR                   OEHDRF
     C                   MOVE      'Q'           WSTYP
     C                   MOVE      *BLANKS       ORDDES
     C     'REQUEST\'    CAT       'QUOTE\':0    ORDDES
     C     ORDDES        CAT       'RETURN':0    ORDDES
     C                   eval      *IN69 = *ON
     C                   ENDIF

     C                   ENDIF

      ** REFRESH THE SUBFILE
     C                   EXSR      FillSFL

     C                   MOVE      S1CUS         SVCUS             5


      **  SHOW SCREEN 1 WHILE DSPLY1 = ON
     C                   DOW       SCREEN = 'ORDDETAIL'
      **  INITIALIZE FIELDS
     C                   WRITE     POS99
      /free
                  // wsORDTOT is Sales Total before tax
                  // wsVATTOT is VAT Tax
                  // wsINVTOT is Invoice total (Sales Total + VAT tax)

             clear wsDsp;
             if (wsOrdTot <> 0 or wsVatTot <> 0);   // Order (Item) Total
               wsDsp = 'Subtotal: ' + %TrimL(%editC(wsOrdTot:'J'));
             endif;

             if (wsVatTot <> 0);   // VAT Tax Total
               wsDsp = %TRIMR(wsDsp) + '  ' +
                        'VAT: ' + %TrimL(%editC(wsVatTot:'J'));
             endif;
             if (wsInvTot <> 0);   // Invoice Total (Order Total + VAT Total)
               evalR wsDsp = %TRIMR(wsDsp) + '  ' +
                              'Total: ' + %TrimL(%editC(wsInvTot:'J'));
             endif;

             WRITE totalLine;

             *IN61 = *ON;
             *IN75 = (ohWon <> 0);
        //////////////////////////////////////////////////////
        // Primary POS Workstation I/O routine.             //
        // Prompt Cashier for Item being sold.              //
        // Process item, add it to the order.               //
        //////////////////////////////////////////////////////
             exfmt ordCtrl;

            *IN48 = *OFF;      // Error Message(WSERR) COLOR(WHITE)
            *IN90 = *OFF;      // Error Message(WSERR) COLOR(RED)
            *IN61 = *OFF;      // SFLDSP
            *IN78 = *OFF;      // Quantity Sold/Ext. COLOR(WHITE)
            *IN79 = *OFF;      // Vendor/Item COLOR(BLUE)
                               // Quantity Sold COLOR(RED)
      /END-FREE
     C* F3=Exit?
     C                   IF        wsds.fKey = F3
     C* ASK IF ITEMS HAVE BEEN KEYED
     C                   IF        WSORD <> 0
     C                   EXSR      confirmDelete
     C                   IF        wsds.fKey = F4
     C                   eval      screen = 'POS00'
     C                   leaveSR
     C                   ENDIF
     C                   ELSE
     C                   eval      screen = 'POS00'
     C                   leaveSR
     C                   ENDIF
     C                   ENDIF

      ** F5=Refresh Display
     C                   IF        wsds.fKey = F5
     C                   EXSR      FillSFL
     C                   ENDIF

      ** F8=Work with Order Authorization Overrides
     C                   IF        wsds.fKey = F8
     C                   eval      hldCount = creditReturnCount
     C                   EXSR      DSP09W
     C                   LeaveSR
     C                   ENDIF

      ** F11=Subfile Fold (rarely used)
     C                   IF        wsds.fKey = F11
     C                   eval      *IN11 = NOT *IN11
     C                   ENDIF
     C
      ** F17 (s+F5) = Receive a Payment
     C                   IF        wsds.fKey = F17
     C                   CALL      'EPAYJN'
     C                   PARM                    PASS              2
     C                   ENDIF

      ** F19 (s+F7) = Lookup an Invoice
     C                   IF        wsds.fKey = F19
     C                   CALL      'INVINQN'
     C                   ENDIF

      /free
           // F1=Return one or more items.
           if (wsds.fKey = F1 and inhibitf1 = *off);
              setll (wsord) oedtl;
              if %Equal(oedtl);
                iter;
              endif;

              exsr returnItems;

              if (wsds.fKey = f12 or wsds.fKey = F3); // Return cancelled?
                iter;
              endif;

              prepareOrder(*ON);  // *ON = This is a RETURN

           endif;

           // F12=SUSPEND Order / Show "my" suspended orders
           If (wsds.fKey = F12);
             exfmt confirm;
             if (wsds.fKey=F12 or wsds.fKey=F3 or
                (wsds.fKey = Enter and CONTINUE<>'Y'));
               LeaveSR;
             endif;
             IF (WSORD > 0);
               EXSR RETOTAL;
               EXSR CLRCTL;
             endif;
             screen = 'POS00';
             EXSR rollOrder;
             LeaveSR;
           ENDIF;

             //  CHECK ORDDETAIL (subfile record)
             //  "Check" is a misnomer. It is really "Process Subfile and update Order".
             //  Use checkSFL (formerly CHK1) for errors and Override Auth

            EXSR  checkSFL;

            if (*IN90 or *IN48);
              iter;
            ENDIF;


            // DISPLAY INVOICE SCREEN
               IF (wsds.fKey = F10 and *IN90 = *OFF);

                 prepareORDER();

                 IF (addItem = 'N');
                   SCREEN = 'ORDCONFIRM';
                   IF (WSTYP = 'Q');
                     screen = 'POS03';
                   endif;
                   LeaveSR;
                 ENDIF;

                 EXSR FillSFL;

                 wsErr = Err(26);
                 *IN90 = *ON;
               ENDIF;

           ENDDO;
      /END-FREE

     CSR   EDSP01        ENDSR

     C**************************
     CSR   ConfirmDelete BEGSR
     C**************************
     C* INITIALIZE INDICATORS
     C* INITIALIZE FIELDS
     C                   EVAL      SHOW = 'Y'
     C*
     C* SHOW SCREEN 1 WHILE DSPLY1 = ON
     C*
     C                   DOW       SHOW = 'Y'
     C* SDD                EXSR CLRCTL
     C                   EXFMT     confirmDel
     C* EXIT SCREEN
     C                   IF        wsds.fKey = F1 or wsds.fKey = F3 or
     C                               wsds.fKey = F12
     C                   eval      Show = ' '
     C                   LeaveSR
     C                   endif
     C* DELETE ENTIRE ORDER
     C                   IF        wsds.fKey = F4
     C                   callp      deleteOrder(wsord)
     C                   eval      Show = ' '
     C                   LeaveSR
     C                   ENDIF
     C*
     C                   ENDDO
     C*
     CSR   EDSP0W        ENDSR

     C**************************
     CSR   DSP01W        BEGSR
     C**************************
     C* INITIALIZE INDICATORS
     C* INITIALIZE FIELDS
     C                   eval      Show = 'Y'
     C                   eval      *IN90 = *OFF

     C                   DOW       SHOW = 'Y'
     C                   EXFMT     POS01W
     C* EXIT SCREEN
     C                   IF        wsds.fKey = F1 or wsds.fKey = F12
     C                   eval      Show = ' '
     C                   LeaveSR
     C                   endif

      **  Check for Errors
     C                   IF        ODDEF = 'N'
     C                              or ODDEF = 'Y'
     C                   eval      Show = ' '
     C                   LeaveSR
     C                   endif

     C                   eval      *IN90 = *ON
     C                   eval      WswErr = Err(5)

     C                   ENDDO

     CSR   EDSP1W        ENDSR

     C**************************
     CSR   DSP02         BEGSR
     C**************************
      /FREE
          // INITIALIZE FIELDS
           SVAMT = 0;
           WSCHQ = 0;
           WSREM = *ZEROS;
           WSAMT = *ZEROS;
           WSCGV = *ZEROS;
           WSPAYT = 'C';

           *IN70 = *OFF;
           *IN71 = *ON;
           *IN72 = *OFF;

           // PREPARE ORDER FOR PRINTING
           prepareORDER();

           // CALULATE REMAINING:  Invoice Total less Voucher(s) Total
           WSREM = WSINVTOT;

           wsPON = OHPON;

           IF (OHTYS = 'G');
             IF isBondedCashCust(ohCus);
                 wsPayT = 'C';
             else;
                 wsPayT = 'H';
             endif;
           endif;

           // SHOW SCREEN 2 WHILE
           DOW (SCREEN = 'ORDCONFIRM');

               //////////////////////////////////////////////////////////
               // Order Confirmation/Completion/Invoice the Order screen
               //////////////////////////////////////////////////////////
             EXFMT ORDConfirm;

              // Return to POS display
             IF (wsds.fKey = F1 or wsds.fKey = F3);
                screen = 'ORDDETAIL';
                LeaveSR;
             ENDIF;

             *IN90 = *OFF;
              // DISPLAY OVERRIDE SCREEN  F8
             IF (wsds.fKey = F8);
                hldcount = creditReturnCount;
                EXSR DSP09W;
                if (wsds.fKey = F12);
                  *IN90 = *on;
                  iter;
                endif;
                LeaveSR;   // Is this correct?
             endif;

             if (wsds.fKey = F12);  // Suspend Order?
               exfmt confirm;
               if wsds.fKey=F12 or wsds.fKey=F3 or
                   (wsds.fKey = Enter and CONTINUE<>'Y');
                  LeaveSR;
               endif;

               EXSR CLRCTL;
               screen = 'POS00';
               EXSR rollOrder;
               LeaveSR;
             endif;

             EXSR CHK2;
           ENDDO;
           wsTyp = 'I';
           wsOrd = 0;
           S1CUS = *ALL'9';

      /END-FREE
     C     EDSP02        ENDSR

     C**************************
     CSR   DSP02W        BEGSR
     C**************************
      /free
               clear wschq;
               dow (wsds.fKey <> F12 and wsds.fKey <> F3 and
                       (wschq = 0 or (wsamt <> wsrem)));
                 exfmt pos02w;

                 if (wsds.fKey=F12 or wsds.fKey = F3);
                    LeaveSR;   // Exit receive payment (check) screen
                 endif;
                 if (wschq = 0);
                    wswErr = err(10);
                 endif;
                 if (wsamt <> wsrem);
                    wswErr = err(16);
                 endif;
               enddo;
      /end-free
     C     EDSP2W        ENDSR

     C**************************
     CSR   DSP03         BEGSR
     C**************************
     C* INITIALIZE FIELDS
     C                   Z-ADD     *ZEROS        SVAMT             9 2
     C                   Z-ADD     *ZEROS        WSREM
     C                   Z-ADD     *ZEROS        WSAMT
     C                   Z-ADD     *ZEROS        WSCGV
     C*                   Z-ADD     *ZEROS        WSVTOT            9 2
     C                   MOVE      *BLANKS       WSPAYT
     C                   MOVE      *BLANKS       WSPON
     C                   MOVE      *BLANKS       WSTAG
     C                   Z-ADD     *ZEROS        WSCHQ
     C                   MOVE      *OFF          *IN70                          POSCURSR S1CUSN
     C*
     C* PREPARE ORDER FOR PRINTING
      /free
           prepareORDER();
      /end-free
     C* CALULATE REMAINING
     C                   eval      WSREM = WSINVTOT
     C                   MOVE      'N'           WSQTE
     C                   MOVE      OHPON         WSPON
 lc  c                   MOVE      *BLANK        WSTAG
     C*
     C* SHOW SCREEN 2 WHILE
     C                   DOW       SCREEN = 'POS03'
     C* INITIALIZE FIELDS
     C                   EXFMT     POS03

     C* Return without completing or suspending order
     C                   IF        wsds.fKey = F3 or wsds.fKey = F1
     C                   eval      screen = 'ORDDETAIL'
     C                   LeaveSR
     C                   ENDIF

      ** DISPLAY the Badge Override SCREEN  F8
     C                   IF        wsds.fKey = F8
     C                   eval      hldcount = creditreturncount
     C                   EXSR      DSP09W
     C                   Iter
     C                   endif

     C* F12 = SUSPEND Order
     C                   IF        wsds.fKey = F12
 lc1  *
      /free
               exfmt confirm;
               if (wsds.fKey=F12 or wsds.fKey=F3 or
                   (wsds.fKey = Enter and CONTINUE<>'Y'));
                  LeaveSR;
               endif;
      /end-free
 lc1  *
     C                   EXSR      CLRCTL
     C                   eval      screen = 'POS00'
     C                   EXSR      rollOrder
     C                   LeaveSR
     C                   ENDIF
     C                   eval      ohpon = wspon
      **  Error check Screen POS03
     C                   EXSR      CHK3
     C                   ENDDO

     C                   Eval      WSORD = 0
     C                   eval      S1Cus = *ALL'9'

     CSR   EDSP03        ENDSR

     C**************************
     CSR   DSP03W        BEGSR
     C**************************
     C     TOPD3W        TAG
     C* INITIALIZE INDICATORS
     C                   MOVE      *OFF          *IN90
     C                   eval      manualSig = 'N'
     C* INITIALIZE FIELDS
     C                   eval      Show = 'Y'
     C                   eval      nCount = 0
     C                   MOVEL     WSORD         SGKEY            10


     C* SHOW SCREEN 1 WHILE SHOW   = Y
     C                   DOW       SHOW = 'Y'

     C     SGKEY         CHAIN     SIGD1                              99

      **  Wait for signature for upto 12 iterations with 4 second delays; up to 1 min wait
     C                   DOW       NOT %FOUND()
     C                              and (OHPAY = 'H'
     C                                   and nCount < 12)

      **  Sleep for 4 seconds before trying to access signature capture record again.
      **  NOTE: We only do the SLEEP operation when a real Cashier user is = 'AIDPOS'.
      **        Otherwise we are probably in debug/development/test mode
     C                   IF        szUser = 'AIDPOS'
     C                   callp     sleep(4)
     C                   ENDIF
     C                   eval      nCount += 1
     C                   TIME                    LSTTIM            6 0
     C     SGKEY         CHAIN     SIGD1                              99
     C                   ENDDO

     C* SIGNATURE DONE& APPROVED, CONTINUE PROCESS
     C                   if        %Found()

     C                   if        SDACPT = 'Y'
     C                   LeaveSR
     C                   ENDIF

     C* SIGNATURE DONE & NOT GOOD, SHOW WARNING
     C                   IF        SDACPT = 'N'
     C* DISPLAY SCREEN POS04W, failed request
     C                   EXSR      DSP04W

     C                   IF        NOT %Found(SIGD1)
     C                   EXSR      DELSIGR
     C                   EXSR      DELSIGD
     C                   MOVEL     ERR(9)        WSERR
     C                   GOTO      EDSP3W
     C                   ELSE
     C                   GOTO      TOPD3W
     C                   ENDIF
     C* END IF, FOR POS04W
     C                   endif
     C                   endif

            // Continue waiting for SigCap tablet to respond?
     C                   EXFMT     POS03W

     C* Cancel out of Signature Capture
     C                   IF        wsds.fKey = F12 or wsds.fKey=F1
     C                   eval      Show = ' '
     C                   MOVE      'N'           SDACPT
     C                   EXSR      DELSIGR
     C                   EXSR      DELSIGD
     C                   MOVE      *ON           *IN90
     C                   MOVEL     ERR(8)        WSERR
     C                   LEAVESR
     C                   ENDIF

     C* If PAD NOT WORKING, PRINT  SIGNATURE MANUAL-SIGN SLIP
     C                   IF        wsds.fKey = F6
     C                   eval      Show = ' '
     C                   EVAL      manualSig = 'Y'
     C                   EXSR      DELSIGR
     C                   EXSR      DELSIGD
     C                   LEAVESR
     C                   ENDIF
     C* CREDIT CARD TRAN IS DONE
     C                   IF        wsds.fKey = F10
     C                              and OHPAY = 'G'
     C                   LEAVESR
     C                   ENDIF
     C*
     C* DECIDE IF GOOD OR BAD REQUEST
     C*
     C                   ENDDO
     C*
     CSR   EDSP3W        ENDSR

     C**************************
     CSR   DSP04W        BEGSR
     C**************************
     C     TOPD4W        TAG
     C* INITIALIZE INDICATORS
     C                   eval      *IN90 = *OFF
     C* INITIALIZE FIELDS
     C                   MOVEL     'Y'           SHOW2             1
     C*
     C* SHOW SCREEN 1 WHILE SHOW   = Y
     C*
     C                   DOW       SHOW2 = 'Y'
     C*
     C* SIGNATURE DONE& NOT GOOD, SHOW WARNING
     C* DISPLAY SCREEN POS04W, FAILED REQUEST
     C                   EXFMT     POS04W

     C* CANCEL Signature capture?
     C                   IF        wsds.fKey = F12 or wsds.fKey = F1
     C                   EXSR      DELSIGR
     C                   EXSR      DELSIGD
     C                   eval      *IN90 = *ON
     C                   MOVEL     ERR(9)        WSERR
     C                   GOTO      EDSP4W
     C                   ENDIF
     C*
     C* F5 RETRY SAME REQUEST
     C                   IF        wsds.fKey = F5 or wsds.fKey = F10
     C                   EXSR      DELSIGR
     C                   EXSR      DELSIGD
     C* SEE IF EXIST
     C                   CALL      'POSSIGR'
     C                   PARM                    WSID
     C                   PARM                    WSORD
     C                   PARM                    WSREM
     C                   PARM                    CMNME
     C                   PARM                    WSPAYT
     C*RECEIVE SIGNATURE NOTIFICATION
     C                   MOVE      *OFF          *IN90
     C                   GOTO      EDSP4W
     C                   ENDIF
     C* END OF DO FOR POS04W
     C                   ENDDO
     C*
     C     EDSP4W        ENDSR



     CSR   DSP05W        BEGSR
     C**************************
     C* GET MANUAL PRICE
     C* INITIALIZE FIELDS
     C                   MOVEL     'Y'           SHOW              1
     C                   MOVE      *OFF          *IN90
     C*
     C* SHOW SCREEN 1 WHILE DSPLY1 = ON
     C*
     C                   DOW       SHOW = 'Y'
     C                   EXFMT     POS05W
     C* EXIT SCREEN
     C                   IF        wsds.fKey = F1
     C                   MOVE      *BLANKS       SHOW
     C                   GOTO      EDSP5W
     C                   ENDIF
     C* SELL AT ZERO PRICE, F10
     C                   IF        wsds.fKey = F10
     C                   Eval      S1UIN = 0
     C                   Eval      S1UCN = 0
     C                   MOVE      *BLANKS       SHOW
     C                   GOTO      EDSP5W
     C                   ENDIF
     C                   IF        S1UIN = *ZEROS
     C                              and S1UCN = *ZEROS
     C                   MOVEL     ERR(6)        WSWERR
     C                   MOVE      *ON           *IN90
     C                   ENDIF
     C* EVRYTHIN OK
     C                   IF        *IN90 = *OFF
     C                   MOVE      'N'           SHOW
     C                   ENDIF
     C*
     C                   ENDDO
     C*
     CSR   EDSP5W        ENDSR

     C**************************
     CSR   DSP06W        BEGSR
     C**************************
     C                   MOVEL     'Y'           SHOW
     C                   MOVE      *OFF          *IN90
     C* DBAL = DRAWER BALANCE --- BEGIN WITH $100.00
     C                   Eval      WSDBAL = 100
     C                   DOW       SHOW = 'Y'
     C                   EXFMT     POS06W
     C* START A DRAWER/SHIFT
     C                   IF        wsds.fKey = F1
     C                   MOVE      *BLANKS       SHOW
     C                   GOTO      EDSP6W
     C                   ENDIF

     C                   IF        WSDRAW = *ZEROS
     C                   MOVEL     ERR(14)       WSERR
     C                   MOVE      *ON           *IN90
     C                   ELSE
     C                   MOVE      *BLANKS       SHOW
     C* WRITE CSHREG REC
     C                   CLEAR                   CSHRF
     C                   Eval      CSORD = *ZEROS
     C                   Eval      CSINV = *ZEROS
     C                   Eval      CSIDT = WSYMD
     C                   TIME                    WKTIME            6 0
     C                   MOVEL     WKTIME        CSTME
     C                   Eval      CSNET = WSDBAL
     C                   MOVE      'A'           CSPAY
     C                   Eval      CSCHQ = *ZEROS
     C                   MOVEL     JobName       CSLWS
     C                   MOVEL     USER          CSUSR
     C                   Eval      CSDRAW = WSDRAW
     C                   Eval      CSCUS = *ZEROS
     C                   Eval      CSSMN = WSSMN

     C                   WRITE     CSHRF
     C                   ENDIF
     C                   ENDDO

     CSR   EDSP6W        ENDSR

     C**************************
     CSR   DSP07W        BEGSR
     C**************************
     C                   MOVE      *OFF          *IN90
     C                   MOVE      'Y'           SHOW

     C                   DOW       SHOW = 'Y'
     C                   EXFMT     POS07W
     C*CHECK RESULTS
     C                   IF        wsds.fKey = F1 or wsds.fKey = F12
     C                   MOVE      *BLANKS       SHOW
     C                   LeaveSR
     C                   ENDIF

     C                   IF        wsds.fKey = F10
     C* WRITE CHSREG END REC
     C                   CLEAR                   CSHRF
     C                   Eval      CSORD = 0
     C                   Eval      CSINV = 0
     C                   Eval      CSIDT = WSYMD
     C                   TIME                    WKTIME            6 0
     C                   MOVEL     WKTIME        CSTME
     C                   Eval      CSNET = 0
     C                   MOVE      'Z'           CSPAY
     C                   Eval      CSCHQ = 0
     C                   MOVEL     JobName       CSLWS
     C                   MOVEL     USER          CSUSR
     C                   Eval      CSDRAW = WSDRAW
     C                   Eval      CSCUS = 0
     C                   Eval      CSSMN = WSSMN
     C*
     C                   WRITE     CSHRF
     C                   MOVE      WSYMD         PYMD              6
     C                   MOVE      WSDRAW        PDRAW             5
     C                   CALL      'POSDEND'
     C                   PARM                    WSID
     C                   PARM                    PYMD
     C                   PARM                    PDRAW
     C                   MOVE      *BLANKS       SHOW
     C                   Eval      WSDRAW = 0
     C                   ENDIF
     C                   ENDDO
     CSR   EDSP7W        ENDSR

     C**************************
     CSR   DSP08W        BEGSR
     C**************************
     C                   eval      Show = 'Y'
     C                   Z-ADD     ODUIN         S1UIN
     C                   Z-ADD     ODUCN         S1UCN

     C                   IF        ODITP = option.regItem
     C                              or oditp = option.regReturn
     C* REG ITEM
     C                   MOVE      *ON           *IN70
     C                   MOVE      *ON           *IN71
     C                   ELSE
     C                   MOVE      *OFF          *IN70
     C                   MOVE      *OFF          *IN71
     C                   ENDIF

     C                   DOW       SHOW = 'Y'
     C                   EXFMT     POS08W
     C                   eval      *IN90 = *OFF
     C*CHECK RESULTS
     C                   IF        wsds.fKey = F12
     C                   MOVE      *ON           *IN90
     C                   MOVEL     ERR(18)       WSERR
     C                   LeaveSR
     C                   ENDIF
     C* VALIDATE DATA
     C                   IF        S1UIN = *ZEROS
     C                              and S1UCN = *ZEROS
     C                              and wsds.fKey = F10
     C                   MOVEL     ERR(6)        WSWERR
     C                   MOVE      *ON           *IN90
     C                   ENDIF
     C* EVRYTHING OK?
     C                   IF        *IN90 = *OFF
     C                   Eval      SHOW = 'N'
     C                   ENDIF

     C                   ENDDO
     CSR   EDSP8W        ENDSR

     C*****************************
     C**  OVERRIDE AUTHORIZE WINDOW
     CSR   DSP09W        BEGSR
     C*****************************
     C* INITIALIZE INDICATORS
     C* INITIALIZE FIELDS
     C                   eval      *IN90 = *OFF
     C                   eval      WSERR = ' '
     C                   eval      Show  = 'Y'

     C                   DOW       SHOW = 'Y'
      **  RETRIEVE SMN Names for OVERRIDES
     C                   EXSR      FILL9
     C                   if        CRDINV = *BLANKS
     C                              and WSCID > 0
     C                   eval      *IN49 = *ON
     C                   else
     C                   eval      *IN49 = *OFF
     C                   endif

     C                   dou       wsds.fKey <> F22
     C                   EXFMT     POS09W
      ** Command Line Window  Shift+F10 = CmdLine Window
     C                   if        wsds.fKey = F22
     C                   callp     cmdLineWindow()
     C                   endif
     C                   enddo

      **  EXIT SCREEN - NO UPDATES
     C                   if        wsds.fKey = F3 or wsds.fKey = F12
     C                   eval      Show = ' '
      *05/05/09
     C                   eval      creditReturnCount = hldcount
     C                   exsr      clearAutDsp
     C                   LeaveSR
     C                   Endif

     C                   if        wsds.fKey = F9 or wsds.fKey = F21
     C                   Exfmt     POS09E
     C                   Iter
     C                   endif

     C                   if        wsds.fKey = F5
      **  CLEAR Authorization Overrides
     C                   exsr      clearAutDsp

     C                   if        OHPDO <> 'C'
      **  Do not clear the PAST DUE OVERRIDE in customer master
     C                   eval      WSPDO = ' '
     C                   endif
      **  If F5, then Iterate back up to the top of the DO loop
     C                   Iter
     C                   endif


      **  -------------------------
      **   Translate customer
      **  -------------------------
     C                   if        S1CUS = *BLANKS
     C                   eval      S1CUS = *ALL'9'
     C                   endif

     C                   if        CRDINV <> *BLANKS
      **  Extract the invoice number from the scanned-in value
     C                   exsr      getInvNbr
     C                   endif

      **  -------------------------
      **  Perform Security Authorization Override
      **  -------------------------
      **  Verify Override Authority Screen data
      **  Returns *IN90=*OFF when security check passes.
      **  Returns *IN90=*ON  when security check fails.
     C                   EXSR      CHK9
     C                   if        *IN90 and CRDINV <> *BLANKS
     C                   Iter
     C                   endif

      **  If security passes, and they end-user pressed F10, bail out|
     C**                 if        wsds.fKey <> F10
      *** No security in place for Invoice Age or Customer not matching.
      *** so as long as the above call the security passes. We be okay.
      **  _FOR NOW_.

     C                   if        override = *blank
      **  Check invoice age
     C                   if        nInvAge > default.MAX_RTN_AGE
     C                   eval      wsErr = 'Invoice is ' + %char(nInvAge) +
     C                               ' days old. Store policy is ' +
     C                               %char(default.MAX_RTN_AGE) + ' days.'
     C                   eval      *IN90 = *ON
     C                   Iter
     C                   endif

     C                   if        nOvrInvNbr > 0
      ** Check for errors (returns 0 when zero errors detected).
     C                   if        CheckItemHist(nOvrInvNbr) > 0

     C                   if        bBalancing_Error
     C                   eval      wsErr = Err(41)
     C                   eval      *IN90 = *ON
     C                   Iter
     C                   endif

     C                   if        %Int(S1CUS) <> nOrg_CustNo
     C                   eval      wsErr = 'Customer: ' + S1CUS +
     C                              ' is not same as: ' +
     C                              %EditC(nOrg_CustNo:'X') +
     C                              ' on original invoice.'
     C                   eval      *in90 = *ON
     C                   endif

     C                   if        bPrice_Conflict = *ON
     C                   eval      wsErr = Err(42)
     C                   eval      *in90 = *ON
     C                   endif

     C                   endif
     C                   endif

      **  END F10 = *ON
     C                   endif

      **  -------------------------
      **  F10=Accept Authorization
      **  -------------------------
      **  If F10 is pressed and the CHK9 security routines
      **  do NOT return an error (*IN90=*OFF), accept and continue.
     C                   if        wsds.fKey = F10 and NOT *IN90
      **  Accept Override and continue; Update Order Header
     C     WSORD         CHAIN     OEHDR                              92
     C                   exsr      CpyAutFromDSP
     C                   if        %Found(OEHDR)
     C                   eval      ohTme = %dec(%Time())
     C                   update    OEHDRF
     C                   ENDIF
     C                   eval      Show = ' '
     C                   LeaveSR
     C                   ENDIF

     C                   ENDDO
     CSR   EDSP9W        ENDSR


     CSR   getInvNbr     BegSR
      **  -------------------------
      **   Clear Prev-Invoice work fields
      **  -------------------------
     C                   clear                   InvNbr
     C                   clear                   InvDate
     C                   clear                   INVDAT
     C                   clear                   nInvAge
     C                   clear                   nOvrInvNbr

      **  INVOICE NUMBER EXTRACTION ROUTINE FOR CREDIT/RETURN
      **  ---------------------------------------------------
      **  If they scanned in an invoice number, then extract
      **  the invoice number and invoice date from the barcode.
      /free
          // Added this code to retain the returning Invoice number/Date
          // throughout the POS process for the Return.
          // I find that occassionally the ODFILL field in the OEDTL record
          // is emptied for a Credit/Return, but it should _always_ contain
          // the original invoice number and invoice date. We need this
          // because the OEHDR records only had space for Original Invoice Number,
          // but not the Original Invoice Date. :(

          // For now, I only save the info here, and do not actually use it yet.
          clear returnInvoice;
          returnInvoice.orderType =  %subst(CRDINV:1:1);
          if (returnInvoice.orderType = 'I');  // Doing a Return?
            monitor;
              returnInvoice.OrgInvNbr = %dec(%subst(CRDINV:2:6) : 6 : 0);
              returnInvoice.OrgInvDate = %dec(%subst(CRDINV:8:6) : 6 : 0);
              test(DE) *YMD returnInvoice.orgInvDate;
              if NOT %Error();
                returnInvoice.invDate = %date(returnInvoice.orgInvDate:*YMD);
                userDate = %date(returnInvoice.orgInvDate:*YMD);
                returnInvoice.orgInvAge =
                         %DIFF(%DATE() : userDate : *DAYS);
              else;
                clear returnInvoice;
              endif;
            on-error;
              clear returnInvoice;
            endmon;
          endif;

      /end-free
     C                   if        CRDINV <> *BLANKS and
     C                              (%subst(CRDINV:1:1) = 'I'
     C                                or %subst(CRDINV:1:1) = 'i')
     C************       eval      InvNbrA = %subst(CrdInv:2:%Size(InvNbrA))
     C                   eval      InvNbrA = %subst(CrdInv:2)
     C     '0123456789'  Check     InvNbrA
     C                   if        %Found()
     C                   eval      InvNbr = 0
     C                   else
      **  Good invoice number for credit/return
     C                   eval      InvNbr = %Int(InvNbrA)
     C                   eval      nOvrInvNbr = InvNbr
      ** Extract the Invoice Date
     C                   eval      invDateA = %subst(crdInv:2+%Size(InvNbrA))
     C     *YMD0         TEST(DE)                InvDateA
     C                   if        %Error()
     C                   eval      InvDate = *Blanks
     C                   else
     C     *YMD0         MOVE      InvDateA      dtInvoice
     C     *USA          MOVE      dtInvoice     InvDate
     C     *YMD          MOVE      dtInvoice     INVDAT
     C     Today         SubDur    dtInvoice     nInvAge:*DAYS
     C                   endif
     C                   endif
     C                   endif

     CSR   endGetInvNbr  ENDSR


     C**************************
     C* FILL SUBFILE ORDENTRY
     CSR   FillSFL       BEGSR
     C**************************
      /free
           ////////////////////////////////////////////////////////
           // RC: 03Jan2015
           //     Added a test for Zero Order number before CHAINing and UPDATEing
           //     There seems to be legacy logic that writes out a record
           //     with a zero order number. This causes all hell to break loose
           //     with the VAT implementation which cannot use a zero order nbr.
           //     Also, when Cashiers were pressing F6 to check-out a customer,
           //     when the zero record existed, it caused a LOCK-WAIT situation here
           //     giving the impressiong the system was looping.
           //     If all cashiers were past this part of the code, then the VAT
           //     system would not work because the initial order number is also
           //     set to zero. Very odd but this "IF wsord=0" test seems to
           //     have helped.
           ////////////////////////////////////////////////////////

           if (wsord = 0);
             chain(n) (wsOrd) oeCtl;
           else;
             chain (wsOrd) oehdr;
             chain (wsOrd) oeCtl;
           endif;

           if (wsord = 0 or NOT %found(oehdr));
             clear ohfill;
           else;
              evalR s1cus = %EditC(ohcus:'X');
              if (defaults.isBondedStore);
                if (ohBond <> '');
                  wsBond# = ohBond;
                  wsBond  = 'Y';
                else;
                  wsBond# = '';
                  wsBond  = 'N';
                endif;
              endif;
              exsr cpyAutFromDBF;
              wsPDO = OHPDO;
           endif;

           ohlsm = wssmn;
           ohlws = wsid;
           ohldt = %dec( %Date() : *YMD);
           OCSCD = 'L';
           OCORD = WSORD;
           OCSMN = WSSMN;

           exsr getCus;

           S1NME=CMNME;
           OHCNM=CMNME;
           ohtme = %dec(%time());

           if %found(oehdr) and ohord > 0;
             update oeHDRf;
           endif;
           if %found(oectl) and ocord > 0;
             update oeCTLf;
           endif;
      /end-free

     C* INITIALIZE INDICATORS

     C* 61 = OFF MEANS DO NOT SHOW SUBFILE
     C* 63 = OFF MEANS DO NOT SHOW *MORE ON SUBFILE
     C* 60 = CLEAR SUBFILE
     C* 75 = Work Order
     C                   eval      *IN75 = (ohwon <> 0)
     C                   MOVE      *OFF          *IN61
     C                   MOVE      *OFF          *IN63
     C                   MOVE      *ON           *IN60

        // Write out subfile Control record to initialize/clear the subfile.
     C                   eval      SFRRN1 = 1
     C                   WRITE     ordCtrl
     C                   MOVE      *OFF          *IN60

     C* INITIALIZE FIELDS

     C                   callp     sfClear()

     C* 70 = KEY ITP,VEN,ITM
     C* 71 = KEY DES
     C* 72 = UIN, UCN
     C* 73 = CEXG
     C* 74 = DEF
     C* 78 = ITEM IS A RETURN
     C* 79 = ITEM FROM TAG NOT YET SOLD.
     C                   MOVE      *OFF          *IN70
     C                   MOVE      *OFF          *IN71
     C                   MOVE      *OFF          *IN72
     C                   MOVE      *OFF          *IN73
     C                   MOVE      *OFF          *IN74
     C                   MOVE      *OFF          *IN78
     C                   MOVE      *OFF          *IN79

      ** SET UP KEY
      ** NOTE: SEQ is Dec(3,0) yet original code used '9999'
      **       as the assignment value here. I've changed it
      **       to '999' to avoid the overflow error.
      *
     C                   eval      SEQ = 999

      * GET LAST SEQ (wsord:seq)
     C     ODKEY         SETLL     OEDTL
     C                   READP     OEDTL                                  30
      * GET LAST ITEM BALNCE REC
      * ADD A BLANK RECORD TO SUBFILE
     C                   Eval      ODORD = WSORD

      * CLEAR BLNCE FIELDS
     C*****              Eval      IBQOH = 0
     C*****              Eval      IBLORD = 0

           // No record found? Create new Order Entry line input
     C                   IF        *IN30 = *ON
     C                              or ODORD <> WSORD
     C                   Eval      ODSEQ = 1
     C                   ELSE
           // Found? Shift the sequence number down one
     C                   Eval      ODSEQ += 1
     C                   endif

     C* LAST SEQ #
     C                   Z-ADD     ODSEQ         WSLSEQ            3 0

     C                   eval      ODITP = option.regItem

     C                   MOVE      *BLANKS       S1VEN
     C                   MOVE      *BLANKS       S1ITM
     C                   MOVE      *BLANKS       S1SDS
     C                   Eval      S1QTS = 1
     C                   Eval      S1QTO = 0
     C                   Eval      S1OQT = 0
     C                   eval      S1DKY = 0
     C                   Eval      S1UIN = 0
     C                   Eval      S1UCN = 0
     C                   Eval      ODTEN = 0
     C                   MOVE      *BLANKS       S1CEXG
     C                   MOVE      *BLANKS       S1DES
     C                   Eval      S1SMN = WSSMN
      /free
           if (defaults.isBondedStore);
             s1Bond = wsBond;
           endif;
      /end-free
     C                   Eval      ODDMP = 0
     C                   Eval      ODUIS = 0
     C                   MOVE      *BLANKS       ODDMT
     C                   MOVE      *BLANKS       S1DEF
     C                   MOVE      'Y'           S1LAST
     C                   MOVE      0             ODITP
     C                   Eval      sfRRN1 += 1
     C     ohfill        ifne      'RETURN'
     C                   MOVE      *ON           *IN70
     C                   else
     C                   move      *off          *in70
     C                   move      *off          *in71
     C                   end
     C     ohwon         ifne      0
     C                   move      *off          *in70
     C                   move      *off          *in71
     C                   move      *off          *in72
     C                   move      *on           *in75
     C                   endif

     C                   WRITE     ORDDETAIL
     C* SET UP KEY
     C                   Eval      SEQ = 999

     C** Read last item in order detail file (wsord:seq)

     C     ODKEY         SETLL     OEDTL
     C                   READP     OEDTL                                  30

     C* END OF BLANK REC CODE

      /FREE
           if (creditReturnCount >= 0);
               creditReturnCount = 0;
           endif;

           // READ ALL RECS IN DETAIL FOR WSORD  =

           DOW (*IN30 = *OFF and WSORD = ODORD);

      /END-FREE
     C* 70 = KEY ITP,VEN,ITM
     C* 71 = KEY DES
     C* 72 = UIN, UCN
     C* 73 = CEXG
     C* 74 = DEF
     C* 78 = ITEM IS A RETURN
     C* 79 = ITEM FROM TAG NOT YET SOLD.

     C                   MOVE      *OFF          *IN70
     C                   MOVE      *OFF          *IN71
     C                   MOVE      *OFF          *IN72
     C                   MOVE      *OFF          *IN73
     C                   MOVE      *OFF          *IN74
     C                   MOVE      *OFF          *IN78
     C                   MOVE      *OFF          *IN79

      **  COUNT INVOICE/CREDIT ITEMS
     C                   IF        ODITP < 3
     C                   ADD       1             WSNITI
     C                   ELSE
     C                   ADD       1             WSNITC
     C                   ENDIF

     C                   IF        ((ODITP = option.specialOrder or
     C                               ODITP = option.miscCharge) and
     C                               default.ALLOW_SPEC_ORD)
     C                              or (ODITP = option.specialReturn or
     C                                  ODITP = option.miscReturn)
     C* MISC,SPC ITEM KEY DES AND PRICE
     C                   MOVE      *ON           *IN71
     C                   endif

     C                   if        ohFill <> 'RETURN'
     C                   MOVE      *ON           *IN72
     C                   MOVE      *off          *IN50
     C                   else
     C                   move      *off          *in72
     C                   MOVE      *on           *IN50
     C                   endif

     C                   if        ohwon <> 0
     C                   move      *off          *in72
     C                   move      *off          *in70
     C                   MOVE      *on           *IN50
     C                   MOVE      *on           *IN75
     C                   endif

      /FREE
           //  NOTE: creditReturnCount is set to -1 when F8=Override has been used.
           if (ODITP >= 3 and ODITP <= 6); // Line Item Type between 3 and 6?
             HODITP = ODITP;
             if (creditReturnCount >= 0);
                creditReturnCount += 1;
             endif;
           endif;
      /END-FREE
     C                   IF        ODITP = option.regItem
     C                              and ODUCN > 0
     C* REG ITEM WITH CORE CHARGE  - ALLOW EXCHANGE
     C                   MOVE      *ON           *IN73
     C                   ENDIF
     C                   eval      *IN74 = (ODITP >= option.regReturn)
     C*                  IF        ODITP >= 4
     C* CREDIT MEMO - ASK IF DEFECTIVE
     C*                  MOVE      *ON           *IN74
     C*                  ENDIF
     C
     C                   IF        ((ODITP = option.specialOrder or
     C                               ODITP = option.miscCharge) and
     C                               default.ALLOW_SPEC_ORD)  or
     C                               (ODITP = option.regItem)

     C                   IF        (ODQTO > ODQTS)
     C* ITEM FROM TAG, NOT FULLY SCANNED
     C                   MOVE      *ON           *IN79
     C                   MOVE      'Y'           DOCK1T
     C                   endif

     C                   ELSE
     C                   MOVE      *ON           *IN78
     C                   ENDIF

     C                   MOVE      ODVEN         S1VEN
     C                   MOVE      ODITM         S1ITM
     C                   MOVEL     ODDES         S1SDS
     C                   Eval      S1OQT = ODQTS
     C                   Eval      S1QTS = ODQTS
     C                   IF        *IN79 = *ON
     C                              and ODQTS = 0
     C                   Eval      S1QTS = ODQTO
     C                   ENDIF

      /free
           S1QTO = ODQTO;
           S1DKY = ODDKY;
           S1UIN = ODUIN;
           S1UCN = ODUCN;
           S1SMN = ODSMN;
           S1CEXG = ODCEXG;
           S1DES = ODDES;
           S1DEF = ODDEF;
           WSNIT += 1;
           clear S1LAST;

           if (defaults.isBondedStore);
              exec sql Select bland, bondp, poType, hazmat
                       INTO  :dbAD.bland,  :dbAD.bondP,
                             :dbAD.poType, :dbAD.hazmat
                       FROM mastrAD
                       WHERE (advnd,aditem) = (:odven,:oditm)
                       FETCH FIRST ROW ONLY;
             if (SQLState >= '02000');
               clear dbAD;
             endif;
             s1bond = odBond;
           endif;


           if (VATHID > 0);
             EXEC SQL SELECT vatdid INTO :VATDID
                 FROM  VATDTL
                 WHERE VATHID = :VATHID and
                       ORDSEQ = :ODSEQ  and
                       VENDOR = :ODVEN
                 FETCH FIRST ROW ONLY;
           endif;

             // Accumulate VAT Tax amount for "this" Order.
           WSVATTOT += getVatCharge(odord:odven:oditm:odseq:odqts:ODUIN:ODUCN);
           WSORDTOT += ODTEN;
           if (wsOrdTot > 0 and wsVatTot < 0);
              wsInvTot = wsordTot + %abs(wsVatTot);
           else;
              wsInvTot = wsordTot + wsVatTot;
           endif;

           if (odDes = '');
             EXEC SQL SELECT imDes
                       INTO  :dbItem.imDes
                       FROM  mastr
                       WHERE imvnd = :odven and imitem = :oditm
                               and imdel <> 'D'
                         FETCH FIRST ROW ONLY;

              IF (SQLState >= '02000');
                *IN99 = *ON;  // Item Master recd not found
                clear dbItem;
                s1Des = '* ERROR *';
              else;
                *IN99 = *OFF;  // Item Master recd found
                s1Des = dbItem.imDes;
                s1SDS = dbItem.imDes;
              endif;
            endif;

            IF ODQTS = 0 and ODQTO = 0;
               DELETE OEDTLF;
               EXEC SQL DELETE FROM VATDTL
                         WHERE ORDNBR  = :ODORD and
                               ORDSEQ  = :ODSEQ and
                               VENDOR  = :ODVEN and
                               ITEM    = :ODITM;
            else;
              sfRRN1 += 1;
              WRITE ORDDETAIL;
            ENDIF;

            READP OEDTL;
            *IN30 = %EOF;
          ENDDO;

      /END-FREE

     C* RELEASE LAST DETAIL
     C                   MOVE      *OFF          *IN70
     C                   MOVE      *OFF          *IN71
     C                   MOVE      *OFF          *IN72
     C                   MOVE      *OFF          *IN73
     C                   MOVE      *OFF          *IN74
     C                   MOVE      *OFF          *IN78
     C                   MOVE      *OFF          *IN79

     C                   IF        *IN30 = *OFF
     C                   UNLOCK    OEDTL
     C                   ENDIF

     C* POSITION CURSOR IN FILE
     C                   eval      SFRCDNBR = 1
     C* 70 = ABILITY TO KEY ITEM
     C* IF RCNO1 > 0 THEN SHOW SUBFILE  61 = SHOW SUBFILE
     C                   IF        sfRRN1 > 0
     C                   MOVE      *ON           *IN61
     C                   ENDIF
     C* IF RCNO1 > 16 THEN SHOW SUBFILE  63 = SHOW *MORE
     C                   IF        sfRRN1 > 16
     C                   MOVE      *ON           *IN63
     C                   ENDIF
     CSR   endFillSFL    endsr


     C**************************
     CSR   FILL9         BEGSR
     C**************************
      ** CLEAR FIELDS

     C                   MOVE      *BLANKS       DIDSMN
     C                   MOVE      *BLANKS       LIDSMN
     C                   MOVE      *BLANKS       PIDSMN
     C                   MOVE      *BLANKS       CIDSMN

     C                   MOVE      *BLANKS       DIDBDG
     C                   MOVE      *BLANKS       LIDBDG
     C                   MOVE      *BLANKS       PIDBDG
     C                   MOVE      *BLANKS       CIDBDG

      **  Credit/Returns
     C                   IF        WSCID <> *ZEROS
     C     WSCID         CHAIN     SLMAN                              99
     C                   IF        %Found()
     C                   MOVE      SMNME         CIDSMN
     C                   eval      creditReturnCount = -1
     C                   ENDIF
     C                   ENDIF

      **  Discount
     C                   IF        WSDID <> *ZEROS
     C     WSDID         CHAIN     SLMAN                              99
     C                   IF        %Found()
     C                   MOVE      SMNME         DIDSMN
     C                   ENDIF
     C                   ENDIF

      **  Charge Limit
     C                   IF        WSLID <> *ZEROS
     C     WSLID         CHAIN     SLMAN                              99
     C                   IF        %Found()
     C                   MOVE      SMNME         LIDSMN
     C                   ENDIF
     C                   ENDIF

      **  Past Due
     C                   IF        WSPID <> *ZEROS
     C     WSPID         CHAIN     SLMAN                              99
     C                   IF        %Found()
     C                   MOVE      SMNME         PIDSMN
     C                   ENDIF
     C                   ENDIF

     C                   IF        WSPDO = 'C'
     C                   MOVEL     'CUS OVR'     PIDSMN
     C                   ENDIF

      **  RE-Acquire SLSMN NAME
     C                   MOVE      WSSMN         SMN
     C                   EXSR      getSalesman

     CSR   EFILL9        ENDSR


     C**************************
     C* GET CUSTOMER DATA/CALUCLATE CUSTOMER BALANCES
     C     GETCUS        BEGSR
     C**************************
     C*
     C* VALIDATE CUSTOMER #
     C     ' 0123456789' CHECK     S1CUS
      /FREE
           if (%Check( ' 0123456789': s1Cus) = 0);
             evalR S1CUS = '00000' + %Trim(S1CUS);
           endif;
           if S1CUS = *BLANKS or S1CUS = *ALL'0';
             S1CUS = *ALL'9';
           endif;

           if (defaults.isBondedStore);
              if (s1Cus = *ALL'9');
                wsBond = 'N';
                wsBond#='';
                svBond = wsBond;
              ENDIF;
           endif;

           monitor;
             custNo = %int(s1Cus);
           on-error;
             custNo = 0;
           endmon;

           chain (custNo) masar;
           *IN90 = (cmDel='D' or NOT %Found(MASAR));

           if (cmDel = 'D' or NOT %Found(masar));
             if (%Found(masar) and cmDel = 'D');  // Deleted customer?
                wsErr = '* Customer exists but has been deleted. *';
             else;
                wsErr = err(2);
             endif;
             CMNME = wsErr;
             leaveSR;
           endif;
      /end-free
     C* INITIALIZE FIELDS
     C                   Z-ADD     0             WSAGE             9 2
     C                   Z-ADD     0             WSNOW             9 2
     C                   MOVE      *BLANKS       WSRDO             1
     C* MOVE CUSTOMER SPEECIFIC FIELDS
     C                   IF        S1CUS < '90000' and
     C                              isNotCashCustA(s1Cus)
     C     CMAG1         ADD       CMAG2         WSAGE             9 2
     C     WSAGE         SUB       CMDPY         WSAGE
     C     CMOWE         SUB       CMDPY         WSNOW             9 2
     C                   IF        CMPDO = 'Y'
     C                   eval      WSPDO = 'P'
     C                   ENDIF
     C                   ENDIF
      *stop the check for past due when customer record is retrieved ch 050305
      *it is allready done at the record level                              05
     C* IF ACCOUNT PAST DUE
     C*                  IF        WSPDO = *BLANKS
     C*                             and WSAGE > *ZEROS
     C** REQUEST SCAN      EXSR DSP10W
     C*                  MOVEL     ERR(21)       WSERR
     C*                  ENDIF
     C                   IF        OHWON  <> 0
     C                   move      ohtys         wstys
     C                   GOTO      EGETCU
     C                   ENDIF
     C                   IF        CMCUS < 90000
     C                   IF        WSNITI = *ZEROS
     C                              or WSNITC = *ZEROS
     C                   MOVE      'G'           WSTYS                          F-CASH G-CHG
     C*                  ELSE
     C* CAN'T USE THIS CUSTOMER FOR MIXED SALE TRANSACTIONS
     C*                  MOVE      *ON           *IN90
     C*                  MOVEL     ERR(27)       WSERR
     C                   ENDIF
     C* HOUSE CHARGE
     C                   ELSE
     C                   MOVE      'F'           WSTYS             1
     C                   ENDIF
     C     EGETCU        ENDSR


      **************************
      * SECURITY CHECK - Routine
     CSR   GETSEC        BEGSR
      **************************
     C                   MOVE      SMN           SECSMN            3
     C                   CALL      'XASECCL'
     C                   PARM                    SECSMN            3
     C                   PARM                    APP              10
     C                   PARM                    MENU             10
     C                   PARM                    OPT               3
     C                   PARM                    FUNC             10
     C                   PARM                    SEC               1
     C                   PARM                    PRMPT             1                 Y/N
     C                   MOVE      SECSMN        SMN
     CSR                 ENDSR

     C**************************
     C* PREPARE AND PRINT
     C**************************
     CSR   printInvoice  BEGSR
     C*PREPARE DATA
     C                   MOVE      'Y'           SDACPT
     C* UPDATE HEADER WITH FINAL INVOICE DATA
     C     WSORD         CHAIN     OEHDR                              99
     C                   IF        %Found(OEHDR)
     C                   MOVE      WSSMN         OHLSM
     C                   MOVEL     WSID          OHLWS
 7502C                   MOVE      S1CUS         OHCUS
     C* CASH COLLECTION DATA
     C                   MOVEL     WSPAYT        OHPAY
      /FREE
           OHNET = WSORDTOT;
           OHAMT = WSAMT;
           OHCGV = WSCGV;
           OHCHQ = WSCHQ;
           test(de) *YMD ohidt;
           if (%error() or ohidt = 0);
             ohidt = %dec(%Date():*YMD);
           endif;
      /END-FREE

     C                   IF        WSPAYT = 'H'
     C                   MOVE      'G'           OHTYS
     C                   ELSE
     C                   MOVE      'F'           OHTYS
     C                   endif
     C                   eval      ohTme = %dec(%Time())

     C* CALC INVOICE # HERE
     C                   UPDATE    OEHDRF
      **  ENDIF %Found(OEHDR)
     C                   ENDIF

     C* INITIALIZE FIELDS
     C                   EVAL      manualSig = 'N'
     C* WRITE REQUEST
     C                   IF        WSPAYT = 'H'
     C                              and WSTYP = 'I'
     C*  Write signature Requested record to SIGR to evoke SigCap3 PC program
     C                   CALL      'POSSIGR'
     C                   PARM                    WSID
     C                   PARM                    WSORD
     C                   PARM                    WSAMT
     C                   PARM                    CMNME
     C                   PARM                    WSPAYT

     C*  RECEIVE SIGNATURE NOTIFICATION
     C                   EXSR      DSP03W

     C                   IF        SDACPT = 'N'
     C                   LeaveSR
     C                   ENDIF

     C                   ENDIF

     C                   EXSR      GETINV

     C*  PRINT ACTUAL INVOICE
     C*  Print two copies when manual signature is required,
     C*  Otherwise, one copy is good enough.
     C                   IF        manualSig = 'Y'
     C                   eval      WCopies = 2
     C                   ELSE
     C                   eval      WCopies = 1
     C                   ENDIF

     C                   EXSR      UPDINV

     C                   eval      PCopies = 1

     C                   DOW       PCopies <= WCopies
     C                   Eval      POrd = %EditC(WSOrd:'X')
     C                   Eval      WSDRAW = WSDRAW
     C                   eval      PWsid = OHLWS

         // Note: Freeport Version of POSENT (Bonded)
         // would also pass OHBOND to POSPRINT.
         // However, POSPRINT no longer needs/uses OHBOND
     C                   CALL      'POSPRINT'
     C                   PARM                    PORD
     C                   PARM                    WSDRAW
     C                   PARM                    manualSig
     C                   PARM                    OHPAY
     C                   PARM                    PWSID            10
     C                   PARM                    OHTYP
     C                   PARM                    OHBOND

      **  Increment number of copies printed
     C                   eval      PCopies += 1
     C                   ENDDO

      /free
           chain(n) (wsord) oehdr;
           if %Found();
             CLEAR   CSHRF;
             CSORD = OHORD;
             CSINV = OHINV;
             CSIDT = OHIDT;
             cstme = %Int(ohtme / 100);  // Dorks used Pkd(4,0) for CSTME and Pkd(6,0) for OHTME

                // Last minute change: Need to store OHNET + VAT tax in CSNET in CSHR1 file.
             EXEC SQL SELECT ORDTOTAL, VATTOTAL
                       INTO   :vath.ordtotal, :vath.vattotal
                       FROM   VATHDR
                       WHERE  ordnbr = :csord and invnbr = :csinv and
                              invDate = iQuery.cvtdate(:csidt,'ymd')
                        FETCH FIRST ROW ONLY;
             if (SQLState < '02000');
                csNet = ohNet + vath.VatTotal;
             else;
                csNet = ohNet;
             endif;
             csPay = ohPay;
             CSCHQ = OHCHQ;
             cslws = JobName;   // Move the job name instead of OHLWS to get full 10-char wsid
             csUsr = User;
             CSDRAW = WSDRAW;
             CSSMN = WSSMN;
             CSCUS = OHCUS;
             IF WSTYP <> 'Q';
               WRITE CSHRF;
             ENDIF;
           endif;
           exsr UPDInv;
      /end-free
     CSR   EINVOICE      ENDSR

     C**************************
     C* RE-PRINT AN INVOICE
     CSR   ReprintInv    BEGSR
     C**************************
     C*PREPARE DATA
     C* RePRINT 1 copy of invoice
     C                   eval      manualSig = 'N'
     C                   eval      WCopies = 1
     C                   eval      PCopies = 1

     C                   DOW       PCopies  <= WCopies

     C                   Eval      POrd = %EditC(WSOrd:'X')
     C                   eval      PWsid = OHLWS
     C                   Eval      hdDRAW = wsdraw
     C                   Eval      WSDRAW = *ZEROS

     C                   CALL      'POSPRINT'
     C                   PARM                    PORD
     C                   PARM                    WSDRAW
     C                   PARM                    manualSig
     C                   PARM                    OHPAY
     C                   PARM                    PWSID            10
     C                   PARM                    OHTYP
     C                   PARM                    OHBOND

     C                   eval      PCopies += 1

     C                   Eval      wsDRAW = hddraw
     C                   ENDDO
     CSR   EndReprint    ENDSR

     C**************************************************
     CSR   GETDRW        BEGSR
     C*****************************************************
     C* ASSIGN DRAWER  #
     C     store.code    CHAIN     CMPYMAST                           98
0103 C                   ADD       1             CODRAW                   99    LST DRWR       INVOI
     C   99              ADD       1             CODRAW                         WAS = 0
     C                   Eval      WSDRAW = CODRAW
     C*
     C  N98              UPDATE    CMPYMASF
     CSR   EGETDR        ENDSR


     CSR   GETINV        BEGSR
     C*****************************************************
     C     WSORD         CHAIN     OEHDR                              99
     C* ASSIGN INVOICE #
     C                   IF        OHINV = *ZEROS
     C     store.code    CHAIN     CMPYMAST                           98
     C                   IF        WSTYP = 'I'
     C                              or WSTYP = 'C'

         // Cash invoice and CMPYMAST field CONOSY = 'Y'?
         // then use two different invoice schemes.
         // otherwise just use COCHIN for next invoice number.
     C                   IF        CONOSY = 'Y' and OHTYS = 'F'
     C                   Eval      COCAIN += 1
     C                   if        coCain = 0 or coCain = 999999
     C                   Eval      COCAIN = 1
     C                   endif

     C                   Eval      OHINV = COCAIN

0106 C                   else
     C                   Eval      COCHIN += 1
     C                   if        cochin = 0 or cochin = 999999
     C                   Eval      COCHIN = 1
     C                   endif
     C                   Eval      OHINV = COCHIN
0109 C                   ENDIF
0114 C                   ENDIF

     C                   IF        WSTYP = 'Q'
     C* IS A QUOTATION
     C                   ADD       1             COQTE                    99
     C   99              ADD       1             COQTE                          WAS = 0
     C                   Eval      OHINV = COQTE
     C                   ENDIF

     C  N98              UPDATE    CMPYMASF
     C                   endif                                                 INV = 0

     C                   eval      ohTme = %dec(%Time())
     C                   UPDATE    OEHDRF
     CSR   endGetInv     ENDSR

     C**************************
     C* GET SMAN RECORD
     C**************************
     CSR   getSalesMan   BEGSR
     C                   MOVE      SMN           WSSMN                          ONLY PLACE
     C                   MOVE      WSSMN         SMSMN
     C     SMSMN         CHAIN     SLMAN                              99
      **  If not found or deleted, then return '* Error'
     C                   IF        *IN99 = *ON
     C                              or SMDEL = 'D'
     C                   eval      SMNME = '* ERROR'
     C                   ENDIF
     CSR   endSalesMan   ENDSR


     C**************************
     C*  RE-TOTAL AN ORDER, VARIOUS REASONS, CUSTOMER CHANGE?
     C**************************
     CSR   RETOTAL       BEGSR
      /free
           final = 'N';   // Let prepare order, this is NOT the final pass
           prepareORDER();
           clear final;
      /end-free
     CSR   endReTotal    ENDSR


     C*********************************************
     CSR   rollOrder     BEGSR
     C*********************************************
     C                   MOVE      USER          WSPERS           10
     C                   MOVE      *ZEROS        WSORD             5 0
     C                   MOVE      WSTYP         PTYP              1
     C                   MOVE      WSSMN         PSMN              3

     C                   CALL      'POSORD'
     C                   PARM                    PSMN
     C                   PARM                    PTYP
     C                   PARM                    WSORD
     C                   PARM                    WSID

     C                   IF        WSORD <> *ZEROS
     C                   IF        WSORD = 99999
     C* NEW ORDER F6 PRESSED IN POSORD
     C                   Eval      WSORD = 0
     C                   ENDIF

     C* DETERMINE IF IT IS A REPPRINT
     C     WSORD         CHAIN(N)  OEHDR                              92
     C                   IF        %Found()
     C                              and OHICD = 'I'
     C* ORDER IS A REPRINT
     C                   EXSR      ReprintInv
     C                   Eval      WSORD = 0
     C                   endif
     C                   eval      screen = 'ORDDETAIL'
     C                   endif
     CSR   endROLLORD    ENDSR

     C**************************
     C* SECURITY CHECK
     CSR   SECCHK        BEGSR
     C**************************
     C                   Z-ADD     *ZEROS        XXSMN             2 0
     C* CHECK FOR SECURITY ON A FUNCTION - FILL FUNC FROM CALLING SUBR
     C                   MOVE      WSSMN         SMN
     C                   MOVE      *BLANKS       SEC               1
     C* PASS THIS PARM     MOVE 'N'       PRMPT
     C                   EXSR      GETSEC
     C*                    MOVE *BLANKS   SMN     3
     C                   IF        SEC = 'R'
     C* GET AUTHORZATION FROM ANOTHER USER
     C                   DOW       SEC = 'R'
     C                   MOVE      'Y'           PRMPT
     C                   MOVE      *BLANKS       SMN               2
     C                   EXSR      GETSEC
     C                   ENDDO
     C* CHECK FOR VAILD MANAGER OVRRIDE
     C                   IF        SEC = 'Y'
     C                   MOVE      SMN           XXSMN
     C                   MOVE      WSSMN         SMN
     C                   ELSE
     C                   MOVE      'N'           SEC
     C                   Z-ADD     *ZEROS        XXSMN
     C                   MOVE      WSSMN         SMN
     C                   ENDIF
     C                   ENDIF
     C* MOVE SMN  TO XXSMN FOR OVRRID
     C                   IF        SEC = 'Y'
     C                   MOVE      SMN           XXSMN
     C                   ELSE
     C* BAD SECURITY
     C                   MOVEL     ERR(20)       WSERR
     C                   MOVEL     SMN           WSERR2           14
     C                   MOVE      FUNC          WSERR2
     C                   MOVE      WSERR2        WSERR
     C                   MOVE      *ON           *IN90
     C                   ENDIF

     C                   MOVE      'Y'           PRMPT
     CSR   endSECCheck   ENDSR

     CSR   updateVATHDR  BEGSR
      /free
          WSORDTOT = 0;
          WSVATTOT = 0;
          WSINVTOT = 0;

          monitor;
            test(de) *YMD ohodt;
            if NOT %Error();
              vath.ordDate = %date(ohodt:*YMD);
            else;
              vath.ordDate = %date();
            endif;
          on-error;
            vath.ordDate = %date();
          endmon;
          monitor;
            test(de) *YMD ohidt;
            if NOT %Error();
              vath.invdate = %date(ohidt:*YMD);
            else;
              vath.invdate = %date();
            endif;
          on-error;
            vath.invdate = %date();
          endmon;

          vath.invNbr  = OHINV;
          vath.ordType = OHTYP;
          vath.Charge  = (ohtys = 'G');

          if (vathid > 0);  // 31May2015 - RC removed ORDTOTAL and VATTOTAL updates
           EXEC SQL UPDATE VATHDR    -- Update the VAT Tax Order Header Record
                     set   ordnbr  = :ohord,
                           invnbr  = :vath.invnbr,
                           cstnbr  = :ohcus,
                           ordType = :vath.ordType,
                           charge  = :vath.charge,
                           orddate = :vath.ordDate,
                           invdate = :vath.invDate
                         --  ordTotal = 0,
                         --  vatTotal = 0      -- VatTotal will be reCalced later
                     WHERE vathid  = :vathid;
         else;
           dou (SQLState < '02000' or qtmp.sqlLoop > 10);
             EXEC SQL INSERT INTO VATHDR (vathid,
                       ordnbr,invnbr,cstnbr,ordType,charge,orddate,invdate)
                  VALUES(default,:ohord,:ohinv,:ohcus,
                         :vath.ordType, :vath.charge,
                         :vath.orddate,:vath.invdate);
             qtmp.sqlLoop += 1;
           enddo;
           EXEC SQL VALUES IDENTITY_VAL_LOCAL() INTO :vatHID;
         endif;
      /end-free
     CSR   endUpdVATHDR  endsr

     CSR   updateVATDTL  BEGSR
      /free
           if (odten = 0);
                // On a return, the ODTEN (Item Net Amount) is always zero.
                // So I have to use "Item Net" instead of "Extended Item Net"
             WSORDTOT += (odqts*(ODUIN+oducn));
           else;
             WSORDTOT += ODTEN;
           endif;

           WSVATTOT += getVatCharge(odord:odven:oditm:odseq:odqts:ODUIN:ODUCN);
           if (wsOrdTot > 0 and wsVatTot < 0);
              wsInvTot = wsordTot + %abs(wsVatTot);
           else;
              wsInvTot = wsordTot + wsVatTot;
           endif;

              // VAT Detail Line Item Processing
            if (vatHID > 0);
              EXEC SQL SELECT  vatdid INTO :vatDID
                       FROM  VATDTL
                       WHERE vathid = :vathid and
                             ordSeq = :odseq and
                             vendor = :odven and item = :oditm;
            endif;
            if (SQLState < '02000' and vatHID > 0);
              exec SQL UPDATE VATDTL set VATAMT = :vatd.vatAmt,
                                         VATRATE= :vatd.vatRate
                        WHERE VATDID = :vatdid;
            else;
              exec SQL INSERT INTO VATDTL
                             (vathid,ordnbr,ordSeq,vendor, item,
                              vatRate, vatAmt)
                             VALUES(:vathid,:odord,:odseq,:odven, :oditm,
                                    :vatd.vatRate, :vatd.vatAmt);
            endif;
            EXEC SQL UPDATE vathdr set ordTotal = ordTotal + :odten,
                                       vatTotal = vatTotal + :vatd.vatAmt
                         WHERE  VATHID = :vathid;
      /end-free
     CSR   endUpdVATDTL  ENDSR

     CSR   updateRETURNS BEGSR
      /free

           if NOT bReturnsFlag;
              leaveSR;
           endif;

           if (oditp = 4);
             rtntype = 0;
           elseif (oditp = 5);
             rtntype = 1;
           elseif (oditp = 6);
             rtntype = 2;
           elseif (oditp = 3);
             rtntype = 3;
           endif;

           wrkType = 'I';
           // odqtr  += odqts;  // Add this return's quantity to quantity sold.

           odvenr    = s1VenR;
           oditmr    = s1ItmR;
           rtnSeqNbr = s1seq;   // Seq nbr of item in original invoice
           odqtr     = s1qtr;    // Qty being returned this time.
           odqtsr    = S1QTSR;   // Qty originally sold

           monitor;
             rtninv = %int(%subst(odfill:1:6));
             odidr  = %int(%subst(odfill:7:6));
           on-error;
             leaveSR;
           endmon;


           // get the origional RETURNS record

            EXEC SQL   SELECT ODDEL,ODIDR,ODVENR,ODITMR,ODSEQR,
                              ODQTR,ODQTSR,RTNINV,ODTYPR
                       INTO  :oddel, :odidr, :odvenr, :oditmr, :odseqr,
                             :odqtr, :odqtsr, :rtninv, :odtypr
                       FROM   OERTN
                       WHERE  rtninv = :rtninv and odidr=:odidr and
                              odvenr = :odvenr and oditmr = :oditmr and
                              ODTYPR = :rtnType and odseqr = :rtnSeqNbr and
                              ODDEL <> 'D';

           if (SQLState < '02000');   // FOUND prior Return record, then update it?
             odqtr += %abs(QTYRTN) * -1;
             if  (%abs(odqtr) <= %abs(odQTSR));  // If Qty being returned,
                EXEC SQL UPDATE OERTN           // plus prior returns is <= Sold QTY, continue.
                                  SET odqtsr = :odqtsr, odqtr = :odqtr,
                                      ODTYPR  = :rtnType
                    WHERE  rtninv = :rtninv and odidr=:odidr and
                           odvenr = :odvenr and oditmr = :oditmr and
                           ODTYPR = :rtnType and odseqr = :rtnSeqNbr;
             endif;

           else;  // Return NOT Found? then add new record.
             EXEC SQL SELECT odinv, odven,oditm,odseq,odqts
                       into :rtnInv, :odvenr, :oditmr, :odseqR,:odqtsr
                       FROM  OEDTL
                       WHERE odtyp = 'I' and
                             odinv = :rtnInv and odidt = :odidr and
                             odven = :odvenr and oditm = :oditmr
                             AND ODSEQ = :rtnSeqNbr
                       FETCH FIRST ROW ONLY;
             IF (SQLState >= '02000');  // Not found in OEDTL file?
               EXEC SQL SELECT odinv, odven,oditm,odseq,odqts
                         into :rtnInv, :odvenr, :oditmr, :odseqR,:odqtsr

                         FROM  OHDTL
                         WHERE odtyp = 'I' and
                               odinv = :rtnInv and ODIDT = :ODIDR and
                               odven = :odvenr and oditm = :oditmr
                               AND ODSEQ = :rtnSeqNbr
                         FETCH FIRST ROW ONLY;
             endif;

             if (SQLState < '02000');  // Found it in OEDTL or OHDTL?
                odqtr  = %abs(QTYRTN) * -1;  // Qty to be returned.
                odtypr = rtntype;  // Set current return type.
               if (%abs(odqtr) <= %abs(odQTSR));  // If Qty being returned,
                  EXEC SQL INSERT INTO  OERTN
                             (RTNINV,ODIDR,
                              ODVENR,ODITMR,ODSEQR,
                              ODQTR,ODQTSR,ODTYPR)
                           VALUES(:rtninv, :odidr,
                                  :odvenr, :oditmr, :rtnSeqNbr,
                                  :odqtr, :odqtsr, :odtypr);
               endif;
             endif;

           endif;
      /end-free
     CSR   endUPDReturns ENDSR

     C**************************
     C* UPDATE HEADER/DETAIL W/INVOICE Number
     C**************************
     CSR   UPDINV        BEGSR

     C     WSORD         CHAIN     OEHDR                              92
     C                   IF        OHTYP = 'Q'
     C                   MOVE      'I'           OHICD
     C                   MOVE      'Q'           OHQTE
     C                   MOVE      WSTAG         OHTAG
     C                   Z-ADD     WSYMD         OHIDT
     C                   eval      ohTme = %dec(%Time())
     C                   ENDIF
     C* UPDATE HEADER
     C                   if        %Found(OEHDR)
     C                   UPDATE    OEHDRF
     C                   Endif
      /free
           exsr  updateVATHDR;  // Create/Update VAT Header record for this order

           SEQ = 1;
           setll (wsord:seq) oedtl;
           reade (wsord) oedtl;

           clear savVen;
           clear savItm;
           clear savITP;


           DOW ((WSORD = ODORD) and NOT %EOF(OEDTL));

             ODTYS = OHTYS;
             ODINV = OHINV;
             ODIDT = OHIDT;
             ODTYP = OHTYP;

             UPDATE oedtlf;

             exsr   updateVATDTL;  // Create update VAT Detail record for this line item.


             // Update the RETURNed QTY counts: NOTE: This call  may no longer be needed.
             IF (oditp >= 3 AND oditp <= 6 AND odfill <> *blank);
               exsr  updateReturns;   // Update the OERTN record for this line item/
             endif;

             savVEN = odven;
             savItm = oditm;
             savitp = oditp;
             read OEDTL;
           enddo;

           //  RELEASE LAST DETAIL
           IF NOT %EOF(OEDTL);
             UNLOCK OEDTL;
           endif;
      /end-free
     CSR   endUpdInvoice ENDSR


     CSR   checkCreditAutBegSR
      /free
           bFailedOvrTest = *OFF;
           if ODITP >= 3;
             if OHCID = 0 and WSCID = 0 or OHINV <= 0;
               bFailedOvrTest = *ON;
             else;
               creditReturnCount = -1;
             endif;
           endif;
      /end-free
     CSR   ENDChkAut     EndSR


     CSR   cpyAutFromDsp BegSR
      /free
           //*  Past Due
           OHPID = WSPID;
           //*  Discounted
           OHDID = WSDID;
           //*  Charge Limit
           OHLID = WSLID;
           //*  Credit/Returns
           OHCID = WSCID;
           //*  Credit/Returns invoice number
           OHAIN = invNbr;
      /end-free
     CSR   endCpyAutDSp  EndSr


     CSR   CpyAutFromDBF BegSR
      /free
           //  Past Due
           WSPID = OHPID;
           //  Discounted
           WSDID = OHDID;
           //  Charge Limit
           WSLID = OHLID;
           //  Credit/Returns
           WSCID = OHCID;

             // RC: 03Jan2015 -- Do not copy in OHAIN when NOT a return or Credit
           if (ohFill = 'RETURN' or (OHAIN > 0 and OHTYP='C'));
             //  Credit/Returns invoice number
             if (ohain = 0);
               ohAIN = holdInvNbr;
             else;
               invNbr = OHAIN;
               nOvrInvNbr = OHAIN;
             endif;
           endif;

      /end-free
     CSR   EndCpyAutDBF  EndSr



     CSR   ClearAutDsp   BegSR
     C                   eval      wspid = 0
     C                   eval      wsdid = 0
     C                   eval      wslid = 0
     C                   eval      wscid = 0
     C                   Clear                   nOvrInvNbr
     C                   Clear                   InvNbr
     C                   Clear                   InvDate
     C                   Clear                   CrdInv
           // Added to resolve an issue with QTY SOLD being locked
           // during new order after a TAG or Credit was issued.
     C                   eval      *IN50 = *OFF
     CSR   EndClearOvr   EndSR

     CSR   InzOrder      BegSr
      /free
           clearNewOrder();
           exsr clearAutDsp;
      /END-FREE
     CSR   endInzOrder   EndSr

      **************************
      * process Item Return(s)
      **************************
     CSR   returnItems   begsr
      /FREE
           exsr getTheInv; // Ask user for invoice & invoice date
           if (invNbr > 0 and NOT bCancelGetInv);
             exsr doReturnInvoiceItems;
           endif;
      /END-FREE
     CSR   endReturns    endsr


      *------------------------------------------------------------------------
      *Check the validity of the Return Request  (assign Invoice)
      *------------------------------------------------------------------------
     CSR   getTheInv     begsr
      /free
          //////////////////////////////////////////////
          // Get the Invoice Number to start a Return
          //////////////////////////////////////////////
           crdInv = '';
           cidSMN = '';
           bCancelGetInv = *OFF;

           DOW 1 = 1;    // Classic "Do Forever" loop
             EXFMT POS0AW;    // Prompt for Returned Invoice/Date (scanned in)
             if wsds.fKey =  F12;
               bCancelGetInv = *ON;
               leaveSR;
             endif;

             exsr getInvNbr;
             if invnbr = 0;  // No invoice number? then try again until valid invnbr or F12
               iter;
             endif;

             holdInvNbr = invnbr;  // Save the Invoice number to use as the OHAIN value later.

             if (1 = 0);  // NOOP: Bypass the CID check for Credit/Returns
        // +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        // ++  CHECK CID CREDIT/RETURNS                             ++
        // +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
              IF (CIDBDG <> *BLANKS);
                //*  BADGE SCANNED
                WSCID = 0;
                cidsmn = '';
                CHAIN CIDBDG SLMA1;
                *IN90 = NOT %FOUND;
                IF NOT %FOUND();
                  WSERR = Err(37);
                  iter;
                endif;

                prmpt = 'N';
                evalR smn = %editC(SMSMN : 'X');
                Func = 'CKEY';

                EXSR GETSEC;  //  CHECK SECURITY

                //  Successfull?
                if SEC = 'Y';
                  cidsmn = smnme;
                  wscid = %int(smn);
                else;
                  *IN90 = *ON;
                  WSERR = Err(38);
                ENDIF;
              ENDIF;
             endif;   // ENDIF for Bypass condition++++++++++++++++++++

             //  If security passes, and the end-user pressed F10, bail out.
             //  No security in place for Invoice Age or Customer not matching.
             //  So as long as the above call to security passes. We be okay.
             //   _FOR NOW_.
             //  Check invoice age
             if (nInvAge > default.MAX_RTN_AGE);
               wsErr = 'Invoice is ' + %char(nInvAge) +
                   ' days old. Store policy is ' +
                   %char(default.MAX_RTN_AGE) + ' days.';
               *IN90 = *ON;
               Iter;
             endif;
             if wsds.fKey = f10;
                inhibitF1 = *ON;
                leaveSR;
             endif;
           enddo;

      /end-free
     CSR   endGetTheInv  endsr


      *------------------------------------------------------------------------
      * Display the original invoice that contains items that are candidates to be returned.
      *------------------------------------------------------------------------
      /free
            begsr doReturnInvoiceItems;
      /end-free
     C     INVkey        klist
     C                   kfld                    itype             1
     C                   kfld                    invnbrn
     C                   kfld                    invdat

      /free
          /////////////////////////////////////////////////////
          // RETURNS ONLY: Display the Original Invoice and allow Returns
          /////////////////////////////////////////////////////
          bAlreadyUpdated = *OFF;
          sfRRN2 = 0;
          *IN60 = *OFF;
          *IN61 = *OFF;
          write returnsCTL; // Write/Clear/Initialize Returned Invoice items subfile
          iType = 'I';
          invNbrN = invNbr;

           chain(n) INVkey ohhd2;
           If %found(ohhd2);

             // If this is a Bonded Store, then retrieve the original invoice's
             // bond number. If there is a bond number, then this was a bonded sale.
             // Otherwise it was a non-bonded sale.
             // First check today's OEHDR file if the invoice is not found there,
             // then check the OHHDR (order history) header file.

             if (defaults.isBondedStore);

                qtmp.mySQL = 'SELECT OHBOND FROM OEHDR +
                               WHERE OHINV = ' + %char(invnbr);
                EXEC SQL PREPARE dynPrepTODAY FROM :qtmp.mySQL;
                EXEC SQL DECLARE todayORD CURSOR FOR dynPrepTODAY;
                EXEC SQL OPEN    todayORD;
                EXEC SQL FETCH   todayORD INTO :returnInvoice.orgBondNbr;
                if (SQLState < '02000' and returnInvoice.orgBondNbr <> '');
                   returnInvoice.isBonded = 'Y';
                else;
                   returnInvoice.isBonded = 'N';
                endif;
                IF (SQLState >= '02000');
                  EXEC SQL CLOSE todayORD;
                  qtmp.mySQL = 'SELECT OHBOND FROM OHHDR +
                            WHERE OHINV = ' + %char(invnbr) +
                          ' and   OHIDT = ' + %char(invdat);

                  EXEC SQL PREPARE dynPrepHIST FROM :qtmp.mySQL;
                  EXEC SQL DECLARE histORD CURSOR FOR dynPrepHIST;
                  EXEC SQL OPEN    histORD;
                  EXEC SQL FETCH   histORD INTO :returnInvoice.orgBondNbr;
                  if (SQLState < '02000' and returnInvoice.orgBondNbr <> '');
                    returnInvoice.isBonded = 'Y';
                  else;
                    returnInvoice.isBonded = 'N';
                  endif;
                  EXEC SQL CLOSE   histORD;
                endif;
                EXEC SQL CLOSE   histORD;
                EXEC SQL CLOSE   todayORD;
             endif;

             wsmdesr =  cidsmn;
             s1cusr  =  %EDITC(h2cus:'X');
             chain h2cus masar;
             if %Found(masar);
               s1nmer = cmnme;
             else;
               clear s1nmer;
             endif;
             wsordr = h2ord;

             clear orgInvDate;
             smnmer = cidsmn;
             wssmnr = wscid;

             s1invR = h2inv;
             test(DE) *YMD h2idt;
             if NOT %ERROR();
               orgInvDate = %date(h2idt:*YMD);
             endif;

      /end-free
     C     invkey        setll     ohdt2
     C     invkey        reade     ohdt2
     C                   dow       NOT %EOF(ohdt2)
     C                   move      *blank        scselb
     C                   move      d2ord         odordr
     C                   move      d2seq         odseqr
 LC  C                   z-add     d2seq         wrkseq            3 0
     C                   move      d2smn         s1smnr
     C                   move      d2itp         hoditpr
     C                   move      d2itp         s1typr
     C
      /free
           // Protect select if Return is on origional invoice
           if (d2itp >= 3);
              *in50 = *on;
              s1qtr = d2qts;
           else;
              *in50 = *off;
              odidr = h2idt;
              wkitp = d2itp;
              s1qtr = 0;

           // Read the returned items from the OERTN ("Returns") file
           // All returned line items and the Qty returned are retained in OERTN.
           EXEC SQL   SELECT ODDEL,ODIDR,ODVENR,ODITMR,ODSEQR,
                             ODQTR,ODQTSR,RTNINV,ODTYPR
                      INTO  :oddel, :odidr, :odvenr, :oditmr, :odseqr,
                            :odqtr, :odqtsr, :rtninv, :odtypr
                      FROM   OERTN
                      WHERE  rtninv = :s1invR and odidr = :odidr and
                             odvenr = :d2ven and oditmr = :d2itm and
                             ODTYPR = :wkitp and odseqr = :d2Seq;
             if (SQLState >= '02000');  // Return NOT found?
               clear odvenr;
               clear oditmr;
               clear odseqr;
               clear odqtsr;
               clear odqtr;
               clear rtninv;
               clear odtypr;
             else;
               *IN50 = ((odqtsr + odqtr) <= 0);
             endif;

           endif;

           s1qtr  = odqtr; // Previous quantity returned (if any)
           s1venr = d2ven;
           s1itmr = d2itm;
           s1sdsr = d2des;
           s1qtsr = d2qts;
           s1uinr = d2uin;
           s1ucnr = d2ucn;
           s1seq  = d2seq;
           ODSEQR = d2seq;
           *in60  = *on;

           sfRRN2 += 1;
           write returnsDTL;

          // See if item requires a core exchange
          // d2uin         ifne      0
          // d2ucn         andne     0
          // invkey3       klist
          //               kfld                    wrkType
          //               kfld                    d2inv
          //               kfld                    d2idt
          //               kfld                    d2ven
          //               kfld                    d2itm
          //               kfld                    wkitp             1 0

             if (d2uin <> 0 and d2ucn <> 0);
               wrkType = 'I';
               wkItp  = 3;
               chain (wrkType : d2inv : d2idt : d2ven :d2itm : wkitp) OHDT4;

               if NOT %Found(OHDT4);
                 EXEC SQL   SELECT ODDEL,ODIDR,ODVENR,ODITMR,ODSEQR,
                                   ODQTR,ODQTSR,RTNINV,ODTYPR
                      INTO  :oddel, :odidr, :odvenr, :oditmr, :odseqr,
                            :odqtr, :odqtsr, :rtninv, :odtypr
                      FROM   OERTN
                      WHERE  rtninv = :s1invR and odidr = :odidr and
                             odvenr = :d2ven and oditmr = :d2itm and
                             ODTYPR = :wkitp and odseqr = :d2Seq;
                 if (SQLState >= '02000');  // Return NOT found?
                    *IN50 = *OFF;
                    s1qtr = 0;
                    s1uinr = 0;
                    sfRRN2 += 1;
                    write returnsDTL;
                 endif;
               endif;
             endif;
             // reade (iType : invNBRn : invDat) ohdt2;
             reade invKey ohdt2;
           enddo;

      /END-FREE
     C                   move      *on           *in61

     C                   write     returnsFTR
     C                   EXFMT     RETURNSCTL

     C                   if        wsds.fKey= f12
     C                   leavesr
     C                   endif

     C                   Move      *off          *in51

     C                   readc     returnsDTL                           5151

  lc c                   dow       NOT *IN51
  lc c                   if        scselB <> ' '
  lc c                   if        (s1qtsr + s1qtr) <= 0
     C                   movel     err(48)       wserr1
     C                   move      *on           *in90
  lc C                   endif
      /free
           //  *IN74 = (HODITP >= option.regReturn);
           qtyRtn = 0;
           qtyNOTRTN = %abs(s1QTSR) - %abs(s1QTR);
      /end-free
     C     *in90         doueq     *off
  lc.c                   move      *blank        s1deft
     C                   exfmt     RETURNITEM
     C                   move      *off          *in90
     C                   if        wsds.fKey = f12
     C                   leaveSR
     C                   endif
     C     s1qtsr        add       s1qtr         qtyleft           4 0
     C     qtyleft       iflt      QTYRTN
     C                   z-add     0             QTYRTN
     C                   movel     err(49)       wserr1
     C                   move      *on           *in90
     C                   endif
 lc. C                   enddo
      /free
             if (NOT *IN74); // Not regular item return? (Core Return?)
               clear s1DEFT;
             endif;

             exsr addReturnLineItem;

             bReturnsFlag  = *ON;
             exsr updateReturns;    // Update Returns file for each line item.
             bReturnsFlag  = *OFF;

      /end-free
     C                   endif
  lc c
  lc c                   readc     returnsDTL                           5151
  lc C                   endDo

     C                   endif
      /free
           exsr UPDINV;    // Update Returns file for each line item.
           bNoUpdateInv = bAlreadyUpdated;
      /end-free
     CSR                 endsr

      /free
          begSR  addReturnLineItem;
             *IN90 = *OFF;
             clear  wsErr;

           //*  MUST CREATE ORDER here.

           IF WSORD = 0;
             callp createNewOrder();
             ODORD = WSORD;
             OHORD = WSORD;
             OCORD = WSORD;
             ohinv = 0;
           endif;
           S1CUS = s1cusr;

           //*  BAD CUSTOMER?
           IF SVCUS <> S1CUS;
             EXSR CHKCUS;
             IF *IN90 = *ON;
               LeaveSR;
             endIF;
           endif;

             //*  CLEAR CUSTOMER-SPECIFIC OVERRIDE CODES
           exsr clearAutDSP;

           s1nme = cmnme;
      /END-FREE

     C     WSORD         CHAIN     OEHDR                              92
     C     WSORD         CHAIN     OECTL                              93

      /FREE
           if %Found(OEHDR);
             exsr cpyAutFromDBF;
             ohFill = 'RETURN';
           endif;

           If NOT %FOUND(OEHDR);
             OHINV = 0;
             OHORD = WSORD;
             OHTYP = WSTYP;
             OHODT = WSYMD;


               if (OHAIN = 0 and holdInvNbr <> 0 and OHFILL = 'RETURN');
                   OHAIN = holdInvNbr;
               endif;
           ENDIF;
      /end-free


     C                   MOVEL     WSTYP         OHTYP
     C                   MOVEL     WSTYS         OHTYS
     C                   MOVEL     S1CUS         OHCUS
     C                   MOVEL     CMNME         OHCNM
     C                   MOVE      CMRPO         OHRPO
     C                   MOVE      WSPDO         OHPDO
     C                   eval      ohTme = %dec(%Time())
      /free
           if (defaults.isBondedStore);
             ohBond = wsBond#;
           endif;
           If %FOUND(OEHDR);
             UPDATE OEHDRF;
           ENDIF;

             // Capture the origional invoice and inv date in ODFILL
           odFill = %EditC(InvNbrN:'X') + %editC(invdat:'X');
              // RC: 12Jan2015 Changed test from "IF = 'Y'"
              //     to "IF NOT Negative"
           if (s1DEFT <> '' and s1DEFT <> 'N' and s1DEFT <> '0');
             oddef = 'Y';   // Defective flag?
           else;
             clear oddef;
           endif;

           EXSR RETOTAL;

               // I think its doing FILLSFL to build the return order/credit memo.
               // One returned item at a time.
           EXSR FillSFL;

           svcus = S1CUS;
           if (defaults.isBondedStore);
             svBond = wsBond;
           endif;
      /end-free

           // Initialize the Order Detail record
     C                   clear                   oedtlf

         // RC:  Always move 1 into SeqNbr for first (empty) order entry subfile record
     C                   move      ohord         odord
     C                   z-add     1             odseq

     C                   MOVE      S1VENR        ODVEN
     C                   MOVE      S1ITMR        ODITM
     C                   Eval      ODQTS = QTYRTN
     C                   z-sub     odqts         odqts


     C                   Eval      ODUIN = S1UINR
     C                   Eval      ODUCN = S1UCNr
     C                   MOVE      S1DESr        ODDES
     C                   MOVE      S1CEXGr       ODCEXG
     C                   MOVE      S1DEFr        ODDEF
     C                   Eval      ODSMN = S1SMN

     C                   select
     C     s1typr        wheneq    0
      *core return record  (do we have a core return)
     C     s1uinr        ifeq      0
     C     s1ucnr        andgt     0
     C                   z-add     3             oditp
     C                   z-add     oducn         oduin
     C                   z-add     0             oducn
     C     oddes         cat       'core':1      oddes
     C                   else
     C                   z-add     4             oditp
     C                   endif
     C     s1typr        wheneq    1
     C                   z-add     5             oditp
     C     s1typr        wheneq    2
     C                   z-add     6             oditp
     C                   endsl
      /free
             // Capture the origional invoice and inv date in ODFILL
           odFill = %EditC(InvNbrN:'X') + %editC(invdat:'X');
              // RC: 12Jan2015 Changed test from "IF = 'Y'"
              //     to "IF NOT Negative"
           if (s1DEFT <> '' and s1DEFT <> 'N' and s1DEFT <> '0');
             oddef = 'Y';   // Defective flag?
           else;
             clear oddef;
           endif;

           write  oeDTLF;

      /end-free
     CSR                 ENDSR

      ******************************************************************
      **  Nothing but Keylists
      **  (and sadly some legacy field declarations)
      ******************************************************************
     CSR   keyLists      Begsr

0047 C     CRKEY         KLIST
     C                   KFLD                    WSID
     C                   KFLD                    WSYMD             6 0
     C                   KFLD                    DRAW              5 0
     C                   KFLD                    PAYT              1

0047 C     CXKEY         KLIST
     C                   KFLD                    custNo
     C                   KFLD                    ICL               3 0
0047 C     IBKEY         KLIST
     C                   KFLD                    ODVEN
     C                   KFLD                    ODITM

0047 C     IMKEY         KLIST
     C                   KFLD                    ODVEN
     C                   KFLD                    ODITM

0047 C     D1KEY         KLIST
     C                   KFLD                    WSORD
     C                   KFLD                    D2VEN
     C                   KFLD                    D2ITM
     C***
0047 C     D1KEY2        KLIST
     C                   KFLD                    WSORD
     C                   KFLD                    S1VEN
     C                   KFLD                    S1ITM
     C***
0047 C     D2KEY         KLIST
     C                   KFLD                    OTTYP
     C                   KFLD                    OTREF
     C                   KFLD                    OTIDT
     C                   KFLD                    S1VEN
     C                   KFLD                    S1ITM
     C***
0047 C     D2PKY         KLIST
     C                   KFLD                    OTTYP
     C                   KFLD                    OTREF
     C                   KFLD                    OTIDT
     C***
0047 C     H2KEY         KLIST
     C                   KFLD                    TYP
     C                   KFLD                    REF
     C                   KFLD                    IDT
     C***
0047 C     OHKEY         KLIST
     C                   KFLD                    WSORD
           // KList(OHKEY)  = (wsord)
           // KList(ODKEY)  = (wsord:seq)
           // KList(ODKEY2) = (odord:odseq)

0047 C     ODKEY         KLIST
     C                   KFLD                    WSORD
     C                   KFLD                    SEQ               3 0

     C     ODKEY2        KLIST
     C                   KFLD                    ODORD
     C                   KFLD                    ODSEQ


0047 C     OTKEY         KLIST
     C                   KFLD                    WSORD
     C                   KFLD                    H3TYP
     C                   KFLD                    H3INV
     C                   KFLD                    H3IDT
     C***
     C***
0047 C     VXKEY         KLIST
     C                   KFLD                    ODVEN
     C                   KFLD                    DMT               1
     C                   KFLD                    ITM              15

     CSR   endKeyList    endsr

      **********************************************************
      **  This routine summarizes the quantity sold for the   **
      **  given item on all associated Invoices.              **
      **  It does this by reading all existing returned item  **
      **  invoice numbers and then summarizing the item for   **
      **  each of those invoice number.                       **
      **********************************************************
     P SumItemQty      B
     D SumItemQty      PI            10I 0
     D  InvNbr                             Like(ODINV) Const
     D  Vendor                             Like(ODVEN) Const
     D  Item                               Like(ODITM) Const

     D nItemQty        S             10I 0 Inz
     D Invoice         S                   Like(OHAIN)


     C     PrevInvoice   KList
     C                   KFld                    InvNbr

     C                   eval      bHist_Found = *OFF
     C                   eval      bToday_Found = *OFF

      ** --------------------------
      ** HISTORICAL ORDERS
      ** --------------------------

      ** Find ORIGINAL INVOICE receipt record.
      ** (Is it in the order history file?)
     C     prevInvoice   SETLL     OHHDR4
     C                   if        %Equal()
     C                   eval      bHist_Found = *ON
     C     prevInvoice   READE     OHHDR4


     C                   Dow       NOT %EOF(OHHDR4)
     C                   eval      Invoice = OEHHDR4.OHINV

      **  Check the original invoice's age.
     C                   if        OEHHDR4.OHDEL <> 'D' and OEHHDR4.OHORD <> 0
     C                              and OEHHDR4.OHIDT = INVDAT
      **  Save the original invoice's customer number
     C                   eval      nOrg_CustNo=OEHHDR4.OHCUS
     C                   eval      nItemQty += SumInvQty(Invoice: Vendor:Item)
     C                   endif

     C     PrevInvoice   READE     OHHDR4
     C                   enddo
     C                   endif

      ** Find all the previous returns (aka, "applied invoice")
     C     PrevInvoice   SETLL     OHHDR5
     C                   if        %Equal()
     C     PrevInvoice   READE     OHHDR5

     C                   Dow       NOT %EOF(OHHDR5)
      **   We an applied invoice is found use that record's
      **   original invoice number to hunt down this return.
     C                   if        OEHHDR5.OHDEL <> 'D' and OEHHDR5.OHORD <> 0
     C     *YMD          move      OEHHDR5.OHIDT dtOrgInv
     C     Today         SubDur    dtOrgInv      nAge:*DAYS
     C                   if        nAge <= default.MAX_RTN_AGE and
     C                              OEHHDR5.OHINV > 0
     C                   eval      Invoice = OEHHDR5.OHINV
     C                   eval      nItemQty += SumInvQty(Invoice: Vendor:Item)
     C                   endif
     C                   endif

     C     PrevInvoice   READE     OHHDR5
     C                   enddo
     C                   endif

      ** --------------------------
      ** TODAY's CURRENT ORDERS
      ** --------------------------
      **  Check today's Active Invoices
      **  Today's Active invoices
     C     PrevInvoice   SETLL     OEHDR4
     C                   if        %Equal()
     C                   eval      bToday_Found = *ON
     C     PrevInvoice   READE     OEHDR4
     C                   if        nOrg_CustNo = 0
      **  Save the original customer number
     C                   eval      nOrg_CustNo=OE_HDR4.OHCUS
     C                   endif
     C                   Dow       NOT %EOF(OEHDR4)
     C                   if        OEHHDR4.OHDEL <> 'D'
     C                   eval      Invoice = OE_HDR4.OHINV
     C                   eval      nItemQty += SumInvQty(Invoice: Vendor:Item)
     C                   endif
     C     PrevInvoice   READE     OEHDR4
     C                   enddo
     C                   endif


      **  Today's APPLIED Invoices (if any)
     C     PrevInvoice   SETLL     OEHDR5
     C                   if        %Equal()
     C     PrevInvoice   READE     OEHDR5
     C                   Dow       NOT %EOF(OEHDR5)
      **  We found an applied invoice, so use that record's
      **  original invoice number to hunt down this return.
     C                   if        OE_HDR5.OHDEL <> 'D' and OE_HDR5.OHINV > 0
     C                   eval      Invoice = OE_HDR5.OHINV
     C                   eval      nItemQty += SumInvQty(Invoice: Vendor:Item)
     C                   endif
     C     PrevInvoice   READE     OEHDR5
     C                   enddo
     C                   endif


     C                   return    nItemQty
     P SumItemQty      E

      **********************************************************
      **  This routine summarizes the quantity sold for the   **
      **  given item on a single Invoice.  It includes        **
      **  sales and credits/records. It gives use the Delta   **
      **  of sales combined with returns for the item.        **
      **********************************************************
     P SumInvQty       B
     D SumInvQty       PI            10I 0
     D  InvNbr                        7P 0 Const
     D  Vendor                        3A   Const
     D  Item                         15A   Const

     D sumQtySold      S             10I 0 Inz
     D itemType        S              5I 0
     D itemQtySold     S             10I 0
     D itemPrice       S             11P 2
      /free
           EXEC SQL DECLARE IH CURSOR for
                    SELECT ODITP, odqts, oduin
                    FROM   OHDTL
                    WHERE  odinv = :invNbr and
                           (odven,oditm) = (:vendor,:item);
           EXEC SQL OPEN IH;
           IF (SQLState < '02000');
             EXEC SQL FETCH IH into :itemType, :itemQtySold, :itemPrice;
             dow (SQLState < '02000');
               sumQtySold += itemQtySold;
               if (itemType < 3);
                 nOrg_QTY += itemQtySold;
                 if (itemPrice < nRtn_Price);
                   bPrice_Conflict = *ON;
                 ENDIF;
               ENDIF;
               EXEC SQL FETCH IH into :itemType, :itemQtySold, :itemPrice;
             ENDDO;
             EXEC SQL CLOSE IH;
           endif;

           EXEC SQL DECLARE II CURSOR for
                    SELECT ODITP, odqts, oduin
                    FROM   OEDTL
                    WHERE  odinv = :invNbr and
                           (odven,oditm) = (:vendor,:item);
           EXEC SQL OPEN II;
           EXEC SQL FETCH II into :itemType, :itemQtySold, :itemPrice;
           dow (SQLState < '02000');
             sumQtySold += itemQtySold;
             if (itemType < 3);
               nOrg_QTY += itemQtySold;
               if (itemPrice < nRtn_Price);
                 bPrice_Conflict = *ON;
               ENDIF;
             ENDIF;
             EXEC SQL FETCH II into :itemType, :itemQtySold, :itemPrice;
           ENDDO;
           EXEC SQL CLOSE II;

           return sumQtySold;
      /end-free
          // NOTE: Everything below here is deprecated
      **  See if this invoice is in Today's activity
     C*    InvItem       SETLL     OEDTL5
     C*                  if        %Equal()
     C*    InvItem       READE     OEDTL5
     C*                  Dow       NOT %EOF(OEDTL5)
     C*                  eval      nItemQty += OE_DTL5.ODQTS
      *
      **  Original Sales quantity from today?
     C*                  if        OE_DTL5.ODITP < 3
     C**                 eval      nOrg_QTY += OE_DTL5.ODQTS
     C*                  if        OE_DTL5.ODUIN < nRtn_Price
     C*                  eval      bPrice_Conflict = *ON
     C*                  endif
      **  Today's Returns
     C*                  else
     C**                 eval      nTodays_RtnQty += OE_DTL5.ODQTS
     C*                  endif


     C*    InvItem       READE     OEDTL5
     C*                  enddo
     C*                  endif
      *
     C*                  return    sumQtySold
     P SumInvQty       E

     P CheckItemHist   B
     D CheckItemHist   PI            10I 0
     D  InvNbr                             Like(ODINV) Const

     D nRRN            S             10I 0
     D nStartingLine   S             10I 0 Inz(2)
     D bNotFound       S              1N   Inz(*OFF)
     D nErrorCount     S             10I 0


      **  Check the item quantity in this order
      **  to make sure there is there is sufficient quantity
      **  on the original Invoice to cover the return.

     C                   eval      bPrice_Conflict  = *OFF
     C                   eval      bBalancing_Error = *OFF
     C                   eval      bToday_Found     = *OFF
     C                   eval      bHist_Found      = *OFF
     C                   eval      nErrorCount      = 0
     C                   eval      OrgQty = 0
     C                   eval      HstQty = 0
     C                   eval      TodQty = 0
     C                   eval      RtnQty = 0
     C                   eval      BalQty = 0
     C                   eval      nRtn_Price = 0
     C                   eval      nOrg_CustNo = 0

     C                   eval      nOrg_Qty = 0
     C                   eval      nHist_RtnQty = 0
     C                   eval      nTodays_RtnQty = 0
     C                   eval      nThis_Qty = 0

     C                   for       nRRN = nStartingLine to default.MAX_ITEMS
     C     nRRN          Chain     ORDDETAIL
     C                   if        NOT %Found() OR  S1Ven = *Blanks
     C                   if        NOT %Found()
     C                   eval      bNotFound = *ON
     C                   endif
     C                   leave
     C                   endif

      ** If this is a return, then verify that the quantity returned
      ** is not greater than the originally quantity sold.
      ** ** CORE RETURNS ARE NOT CHECKED.
      ** ** IF THEY NEED TO BE CHECKED, change the ODITP compare to 3.
     C                   if        ODITP >= 3 and ODITP <=6
      **  If qty sold + qty rtn < 0, then too many items being returned.
      **      1      +      -4 = -3  (not Okay|)
      **      1      +      -1 =  0  (Okay)
      **      2      +      -1 =  1  (Okay)
     C                   eval      nThis_Qty = S1QTS

      **  Save the credit/return price for this item.
     C                   eval(R)   nRtn_Price = S1UIN - ((S1DKY/10)*S1UIN)

      **  Calculate the Delta of prior sales and returns
      **  If negative, sorry but there are too many being returned.
     C                   if        (SumItemQty(InvNbr:s1Ven:s1Itm) + S1QTS) < 0
     C                   eval      nErrorCount += 1
     C                   endif

      **  Save the results for our hidden F9 display ;)
     C                   eval      OrgQty = nOrg_Qty
     C                   eval      HstQty = nHist_RtnQty
     C                   eval      TodQty = nTodays_RtnQty
     C                   eval      RtnQty = nThis_Qty
     C                   eval      BalQty = OrgQty+HstQty+TodQty+RtnQty
     C                   eval      bBalancing_Error = (BalQty < 0)
     C                   eval      RTNVEND = s1Ven
     C                   eval      RTNITEM = s1Itm

     C                   endif
     C                   endfor
     C                   return    nErrorCount
     P CheckItemHist   E

     P checkTag        B
     D checkTag        PI             1N
     D  tag                          10A   Const
     D  newOrder                      7P 0 Const


     D T1              DS                  LikeDS(tagInfo_T) Inz
     D T2              DS                  LikeDS(tagInfo_T) Inz

      /free
           if (tag = '');  // No tag? Then don't check anything
              return *ON;
           endif;
           clear t1;
           clear t2;
           EXEC SQL SELECT ohOrd, ohinv, ohNet
                    INTO   :t1.ordNbr, :t1.invNbr, :t1.amt
                    FROM   OEHDR
                    WHERE  upper(ohtag) = upper(:tag)
                    FETCH FIRST ROW ONLY;
           if (SQLState >= '02000');  // Tag not found in OE Hdr file?
              return *off;  // Fail
           endif;

              // Check "this" TAG's amt to any previous TAGs used.
           EXEC SQL SELECT otOrd, otRef
                    INTO   :t2.ordNbr, :t2.invNbr
                    FROM   OETAG
                    WHERE  otOrd = :newOrder
                    ORDER BY otord,otidt desc
                    FETCH FIRST ROW ONLY;

              EXEC SQL SELECT sum(ODTEN)
                    INTO   :t2.amt
                    FROM   OEDTL
                    WHERE  odORD = :newOrder
                    GROUP BY ODORD
                    FETCH FIRST ROW ONLY;

              if (SQLState >= '02000');
                 return *ON;  // Not found? That is okay too.
              endif;

           // NOTE: We may (also) be able to compare
           // the Order Type and/or Type of Sales fields
           // of the TAG to the new Order.
           if ((t1.amt > 0 and t2.amt > 0) or // Both positive amounts?
               (t1.amt < 0 and t2.amt < 0) or // Both negative amounts?
               (t1.amt = 0));  // Prior stmt was comparing t2. to t2.
              return *on;
           ENDIF;
           return *OFF;   // False! Not okay TAG to use
      /end-free
     P checkTag        E

     P prepareORDER    B
     D prepareORDER    PI
     D   bIsAReturn                   1N   Const OPTIONS(*NOPASS)
     D  bIsReturn      S              1N   Inz(*OFF)
      /free
           if (%Parms() >= 1 and %addr(bIsAReturn) <> *NULL);
             bIsReturn = (bIsAReturn = *ON);
           endif;

           // PREPARE ORDER FOR INVOICING/PRINTING
              CHAIN WSORD OEHDR;
                // NEW ORDER?
              IF NOT %FOUND(OEHDR);
                 OHTYP = WSTYP;
                 ODORD = WSORD;
                 monitor;
                   ohcus = %int(s1Cus);
                 on-error;
                   ohcus = 0;
                   s1cus = *ZEROS;
                 endmon;
                 OHODT = WSYMD;
                 OHINV = 0;  // Invoice number is assigned later
              ENDIF;

           evalR s1cus = %editC(ohcus:'X');

           // GET CUSTOMER
           // SETUP DATA IN HEADER
           OHLSM = WSSMN;
           OHLDT = WSYMD;
           OHTYP = WSTYP;
           OHSTR = store.code;

           // INITIALIZE FIELDS
           if (bIsReturn);
              if (defaults.isBondedStore);
                if (returnInvoice.isBonded = 'Y' and
                    returnInvoice.orgBondNbr <> '');
                  wsBond# = returnInvoice.orgBondNbr;
                  wsBond  = 'Y';
                else;
                  wsBond# = '';
                  wsBond  = 'N';
                endif;
                ohbond = wsBond#;
                odBond = wsBond;
              endif;
           endif;

           wsOrdTot = 0;
           wsVatTot = 0;
           wsInvTot = 0;

           WSNIT = 0;
           OHCST = 0;
           OHINE = 0;
           OHCNE = 0;
           OHICE = 0;
           OHCCE = 0;
           OHNET = 0;     // Clear OHNET to avoid Accumulated Totalling

           clear totalLine;
           clear vath;
           clear vatd;

           // SET UP KEY
           addItem = 'N';
           SEQ = 1;

           SETLL (wsord:seq) OEDTL;
           READE (wsord) OEDTLF;

           test(de) *YMD ohodt;
           if %error();
             EXEC SQL SELECT vathid INTO :vatHID
                     FROM  vathdr
                     WHERE ordnbr = :wsord
                     order by ordnbr, crtdate desc
                     FETCH FIRST ROW ONLY;
           else;
             EXEC SQL SELECT vathid INTO :vatHID
                     FROM  vathdr
                     WHERE ordnbr = :wsord and
                           orddate = iQuery.cvtdate(:ohodt,'ymd')
                     order by ordnbr, orddate desc, crtdate desc
                     FETCH FIRST ROW ONLY;
           endif;

           // READ ALL RECS IN DETAIL FOR WSORD = ODORD

           if (defaults.isBondedStore);
              if (ohBond = ' ');
                odBond = 'N';
              else;
                odBond = 'Y';
              endif;
           endif;

           DOW NOT %EOF(OEDTL);
             // REGULAR ITEM
             IF (ODITP = option.regItem or ODITP = option.coreReturn or
                 oditp = option.regReturn);
               Move_Item_to_OEDTL(odven:oditm);
             ENDIF;

             IF FINAL <> 'N' and ODQTO <> 0 and ODQTS < ODQTO;
               ODQTS = ODQTO;
               addItem = 'Y';
             endif;

             // CALCULATE TOTAL EXTENDED COST
           getPrice();  // Get item's price and VAT Tax

             // ACCUM HEADER TOTALS
           ice    = odqts * oduic;
           cce    = odqts * oducc;
           ohice += ice;
           ohcce += cce;
           ohcst += odtec;
           ohine += odein;
           ohcne += odecn;
           ohnet += odten;

           wsOrdTot += ODTEN;
           WSVATTOT += getVatCharge(odord:odven:oditm:odseq:odqts:ODUIN:ODUCN);
           if (wsOrdTot > 0 and wsVatTot < 0);
               wsVatTot = %abs(wsVatTot);
           endif;
           WSINVTOT = wsOrdTot + wsVatTot;

                  // COUNT NUMBER OF Line Items
             wsnit += 1;
             odcus = ohcus;
             odTyp = ohTyp;

             UPDATE OEDTLF;

             READE (wsord) OEDTL;

           ENDDO;

             // RELEASE LAST DETAIL
           if NOT %EOF(OEDTL);
             UNLOCK(e) OEDTL;
           endif;

           //  UPDATE TOTALS
           OHNIT = WSNIT;

           // UPDATE_ADD Order Header record

           if NOT (wsnit = 0 and ohord = 0);  // Skip "Zero Order Number" BUG
             ohTme = %dec(%Time());
             IF %FOUND(OEHDR);
               UPDATE OEHDRF;
             else;
               WRITE OEHDRF;
             endif;
           endif;

      /end-free
     P PrepareORDER    E


     P getPrice        B
     D getPrice        PI

     C* PRICE ROUTINE

     C***** Rev. 2004Dec
     C*  IF ODDKY = O THEN USE MATRIX
     C                   IF        ODDKY <> *ZEROS
     C                   MOVE      'M'           ODDMT
     C                   Eval      ODDMP = ODDKY
     C                   if        ODDKY = .999
     C*  MANUAL KEY PRICE
     C                   Eval      ODDMP = 0
     C                   endif
     C                   ELSE
     C
     C                   IF        OHWON = 0
     C                   Eval      ODDMP = 0
     C                   Eval      ODDMT = ''

     C**GET CUST.CLASS OR VENDOR DISCOUNT IF NOT A WORK ORDER

     C**R&E TAM 2/22/96 ADDED TO CHAIN TO ITEM CLASS FILE WITH ITEM CLASS
     C** TO VERIFY IF ITEM CAN HAS GLOBAL DISCOUNTING

     C                   clear                   icGDS
     C     ODICL         CHAIN     ITMCL                              99
     C                   if        (NOT %FOUND()) or
     C                               (%Found() and ICDel = 'D')
     C                   eval      ICGDS = 'N'
     C                   endif

     C
     C                   If        ICGDS <> 'N'
     C                   Eval      ICL = *ZEROS
     C     CXKEY         CHAIN     CLSMX                              99

     C                   IF        *IN99 = *OFF
     C                              and CXDEL <> 'D'
     C* GLOBAL DISCNT BY CUST, CLASS
     C                   eval      ODDMP = CXDIS
     C                   eval      ODDMT = 'C'
     C                   ENDIF
     C                   ENDIF

     C                   eval      ICL = ODICL
     C     CXKEY         CHAIN     CLSMX                              99
     C                   IF        *IN99 = *OFF
     C                              and CXDEL <> 'D'
     C* DISCOUNT BY CUS,ICL  - MOST SPECIFIC
     C                   Eval      ODDMP = CXDIS
     C                   MOVE      'C'           ODDMT
     C                   ENDIF

     C********* PROMOTINAL/SALE ITEMS ***************************
     C** RESD (KEVIN)  4-10-96  ADDED CODING TO CHECK FOR AN ITEM DISCOUNT
     C**      OR A VENDOR DISCOUNT.
     C                   MOVE      'V'           DMT
     C                   MOVE      *BLANKS       ITM
     C     VXKEY         CHAIN     VNMTX                              99
     C                   IF        *IN99 = *OFF
     C                              and VXDEL <> 'D'
     C                              and CHKDAT < VXEDT
     C                              and CHKDAT >= VXBDT
     C                              and VXDIS >= ODDMP
     C                   Eval      ODDMP = VXDIS
     C                   MOVE      DMT           ODDMT
     C                   ENDIF

     C* CHECK FOR AN ITEM LEVEL DISCNT
     C                   MOVE      'I'           DMT
     C                   MOVE      ODITM         ITM
     C     VXKEY         CHAIN     VNMTX                              99
     C                   IF        *IN99 = *OFF
     C                              and VXDEL <> 'D'
     C                              and CHKDAT < VXEDT
     C                              and CHKDAT >= VXBDT
     C                              and VXDIS >= ODDMP
     C                   Eval      ODDMP = VXDIS
     C                   MOVE      DMT           ODDMT
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C**CALC.PRICES NOW
      /free
           IF (ODITP = option.regItem or oditp = option.regReturn);
             getRegPrice();
           else;
             getOtherPrice();
           endif;
      /end-free
     p getPrice        E

     P getRegPrice     B
     D getRegPrice     PI
      /free
           if (oditp = option.regReturn);  // If Core Return, clear out Core Cost, Sell & Net
             ODUCC = 0;
             ODUCS = 0;
             ODUCN = 0;
           endif;

           monitor;
           odutc = oduic + oducc;  // Total Cost = Item Cost + Core Cost
           odtec = odutc * odqts;  // Total Extended Cost = Total Cost * Qty Sold
           oduts = oduis + oducs;
           odtes = oduts * odqts;
           on-error;
             odutc = 0;
             odtec = 0;
             oduts = 0;
             odtes = 0;
             return;
           endmon;

           IF (ohfill <> 'RETURN');
             IF (dbItem.IMADSC = ' ');  // "Allow Discount"?
               oduin = oduis - %DecH(oduis * oddmp: 9:2);
             else;
               oduin = oduis;
             endif;
           endif;

           monitor;
           odutn = oduin + oducn;    // Item + Core = net per item
           odten = odutn * odqts;    // (Item + Core) * Qty Sold = Net Total per item
           odein = oduin * odqts;    // Ext Item Net
           odecn = oducn * odqts;    // Ext Core Net
           on-error;
             odutn = 0;
             odten = 0;
             odein = 0;
             odecn = 0;
             return;
           endmon;
      /end-free
     P getRegPrice     E


     P getOtherPrice   B
     D getOtherPrice   PI
      /free
           IF  (ODITP = option.coreReturn);
             oducc = oducn * .5;  // Core Cost is 1/2 Core Net Price.
             oduis = 0;
             oduin = 0;
             oduic = 0;
           else;
             oduic = oduin;
             oducc = 0;
           endif;

           odtec = oduic * odqts;

           // CALC SELL PRICE
           ODUIS = ODUIN;
           ODUCS = 0;
           ODUTS = 0;
           ODTES = ODUIS * ODQTS;

           // Calc Core Net
           ODUTN = ODUIN + ODUCN;
           ODTEN = ODUTN * ODQTS;
           ODEIN = ODUIN * ODQTS;
           ODECN = ODUCN * ODQTS;
      /end-free
     p getOtherPrice   E


     P getVatCharge    B
     D getVatCharge    PI             7P 2
     D  orderNbr                      7P 0 Const
     D  vendor                        3A   Const
     D  item                         15A   Const
     D  seqNbr                        7P 0 Const
     D  qtySold                      10I 0 Const
     D  soldAtPrice...
     D                                7P 2 Const
     D  corePrice...
     D                                7P 2 Const

     D  invDat         S              6S 0
     D  invDate        S               D   Datfmt(*ISO) INZ(*SYS)

     D coreUnitCost    S              7P 2
     D itemPrice       S              7P 2
     D vatVendor       S              3A
     D vat             DS                  LikeDS(VAT_T) Inz
      /free
           clear VAT;
           clear VATD;

           if isBondedOrder( orderNbr );
                // If the order is Bonded, then normally all items are VAT excempt.
                // If, however, the Vendor code appears in our VATVEND file,
                // then that item is VAT-able <g> and VAT tax should be charged anyway.

                // NOTE: We may end up having to add a "BONDED" field to the VATVEND file
                //       I'm not sure why right now, but it seems like it would make
                //       it possible to have vendor codes taxed when BONDED and Vendor
                //       codes taxed when not bonded. However, I may be over thinking it.
              EXEC SQL SELECT vendor
                        INTO  :vatVendor
                        FROM  VATVEND
                        WHERE vendor = :vendor
                        FETCH FIRST ROW ONLY;

               if (SQLState >= '02000' or vatVendor = '');
                  // If the vendor code does NOT appear in the "NON-Except VAT tax" file
                  // when this is a Bonded Order, then bail out, no VAT tax.
                  return 0;
               endif;
           endif;
           invDat = ohidt;

           if (ohFill = 'RETURN' or (OHAIN > 0 and OHTYP='C'));
               EXEC SQL SELECT OHIDT
                        INTO   :invDat
                        FROM   OEHDR
                        WHERE  OHINV = :ohAIN and OHTYP = 'I'
                        ORDER BY OHINV, OHIDT DESC
                        FETCH  FIRST ROW ONLY;
             if (SQLState >= '02000');
               EXEC SQL SELECT OHIDT
                        INTO   :invDat
                        FROM   OHHDR
                        WHERE  OHINV = :ohAIN and OHTYP = 'I'
                        ORDER BY OHINV, OHIDT DESC
                        FETCH  FIRST ROW ONLY;
               if (SQLState >= '02000');  // Not from today?
                  invDat = %dec(%Date() : *YMD);
               endif;
             endif;
           endif;
           if (invDat = 0);
             if (ohidt <> 0);
               invDat = ohidt;
             endif;
           endif;
           test(DE) *YMD invDat;
           if NOT %Error();
             invDate = %Date( invDat : *YMD );
           endif;

           if (defaults.chargeVATonCore);
             coreUnitCost = corePrice;
           else;
             coreUnitCost = 0;
           endif;

           itemPrice = soldAtPrice;

           if (ODITP = option.coreReturn);
              vat = getVatAmt(vendor:item:qtySold : 0 : coreUnitCost :
                              invDate);

           elseif (ODITP = option.regReturn);
              vat = getVatAmt(vendor:item:qtySold: itemPrice : 0 :
                              invDate);
           else;
              vat = getVatAmt(vendor:item:qtySold: itemPrice : coreUnitCost :
                              invDate);
           endif;

            // VAT Calculation is:
            //    vatPerUnit = Price of 1 unit of "this" Item * VAT Rate;
            //    vatThisLineItem = "vatPerUnit" * Quantity Sold.
            vatd.vatRate = vat.rate;
            vatd.vatAmt  = vat.vatAmt;

            return vat.vatAmt;


      /end-free
     P getVatCharge    E


     P move_Item_to_OEDTL...
     P                 B
     D move_Item_to_OEDTL...
     D                 PI
     D  vendor                        3A   Const
     D  item                         15A   Const
     D sellPrice       S              7P 2
      /free
          // ODUCC is 1/2 the IMUCS (Core Sell Price)
          // ODUCS is the Core Sell Price from the Item Mastr file
          // ODUCN is the same as ODUCS (Core Sell Price) used by Work Orders Only

           odstr = store.Code;    // 2-digit Store code
           EXEC SQL SELECT    imicl,  imscl,
                              imuic,  imucs,
                              imuis,  imucs,
                              imadsc, imdes
                       INTO   :dbItem.imicl,  :dbItem.imscl,
                              :dbItem.imuic,  :dbItem.imucs,
                              :dbItem.imuis,  :dbItem.imucs,
                              :dbItem.imadsc, :dbItem.imdes
                       FROM  mastr
                       WHERE imvnd = :vendor and imitem = :item;
           if (SQLState < '02000');  // Found?
              odicl = dbItem.imicl;      // Item Class
              odscl = dbItem.imscl;      // Item Sales Class/Category
              oduic = dbItem.imuic;      // Item Cost
              oducc = dbItem.imucs/2;    // Core "Cost" (1/2 CORE Sell Price)
              sellPrice = dbItem.imuis;  // Item sell price.
              // Suppress reload of Sales price on a labor Work Order item(s)
              // if a Service (VENDOR=SRV) and Work Order Number > 0, then price out.
              IF NOT (odven = 'SRV' AND OHWON <> 0);
                ODUIS = dbItem.imuis;  // Item sell price.
                ODUCS = dbItem.imucs;  // Core Sell price.
                ODUCN = dbItem.imucs;  // Core Net  price (same as sell price?)
              endif;
           endif;

           if (defaults.isBondedStore);
              exec sql Select bland, bondp, poType, hazmat
                       INTO  :dbAD.bland,  :dbAD.bondP,
                             :dbAD.poType, :dbAD.hazmat
                       FROM mastrAD
                       WHERE (advnd,aditem) = (:vendor,:item)
                       FETCH FIRST ROW ONLY;

             if (SQLState < '02000');  // Found, then use Bonded Sell/Landed
              if (ohBond = ' ');
                  odBond = 'N';
              else;
                 odBond = 'Y';
                 oduis = dbAD.bondp;
                 oduic = dbAD.bland;
              endif;
             endif;
           endif;

      /end-free
     P move_Item_to_OEDTL...
     P                 E

     P newVatHdr       B
     D newVatHdr       PI
     D  sqlLoop        S             10I 0
      /free
           test(DE) *YMD ohidt;
           if %Error();
             vath.invDate = %Date();
           endif;

           if (VATHID = 0);
             clear vath;
             clear vatd;
             clear totalLine;
             EXEC SQL SELECT  vathid,  ordnbr,  cstnbr,  invNbr, invDate
                      INTO   :vathid, :vath.ordnbr, :vath.cstnbr,
                             :vath.invnbr, :vath.invdate
                      FROM  VATHDR
                      WHERE ordnbr = :ohord
                            and cstnbr =  CASE :ohcus WHEN 0 THEN cstnbr
                                          ELSE :ohcus end
                            and invnbr =  CASE :OHINV WHEN 0 THEN invnbr
                                          ELSE :OHINV end
                            and invDate = CASE :OHIDT WHEN 0 THEN invDate
                                          ELSE iQuery.cvtdate(:ohidt,'ymd') end
                      ORDER BY ORDNBR,INVDATE DESC
                      FETCH FIRST ROW ONLY;
           else;
             EXEC SQL SELECT vathid, ordnbr, cstnbr, invnbr
                      INTO  :vathid, :vath.ordnbr, :vath.cstnbr,
                            :vath.invnbr
                      FROM  VATHDR
                      WHERE VATHID = :vatHID;
           endif;

           bVatHdr = (SQLState < '02000');   // Found?
           vath.cstnbr = ohcus;
           vath.invnbr = ohinv;
           vath.ordnbr = ohord;
           test(DE) *YMD ohidt;
           if %Error();
             vath.invDate = %Date();
           else;
             vath.invDate = %date(ohidt:*YMD);
           endif;

           test(DE) *YMD ohodt;
           if %Error();
             vath.ordDate = %Date();
           else;
             vath.ordDate = %Date(ohodt:*YMD);
           endif;

           if (bVatHdr);  // If VAT Header found
             EXEC SQL UPDATE VATHDR
                        set  ordnbr  = :ohord,
                             invnbr  = :ohinv,
                             cstnbr  = :ohcus,
                             orddate = :vath.ordDate,
                             invdate = :vath.invDate
                      WHERE  vathid  = :vathid;
           else;
             dou (SQLState < '02000' or sqlLoop > 10);
               EXEC SQL INSERT INTO VATHDR
                         (ordnbr,invnbr,cstnbr,orddate,invdate)
                   VALUES(:ohord,:ohinv,:ohcus,:vath.orddate,:vath.invdate);
               sqlLoop += 1;
             enddo;
             if (sqlState < '02000');
                EXEC SQL VALUES IDENTITY_VAL_LOCAL() INTO :vatHID;
             endif;
           endif;
      /end-free
     P newVatHdr       E

     P createNewOrder  B
     D createNewOrder  PI
      /free

           clearNewOrder();   // Needed?

           // Get next order number from AID Company Informatiom file.
           chain (store.code) cmpymast;
           if %Found();
             if (coOrd >= 99999 or coOrd <= 0);
               coOrd = 1;
             else;
               coOrd += 1;
             endif;
             update CMPYMASF %fields(coOrd);
           endif;

           VATHID = 0;  // RC: 31May2015 - Added for "missing records" issue.

           wsOrd = coOrd;
           orderNbr = coOrd;

                // If we are creating a new order, clear all prior totals and stored information.
           clear s1ven;
           clear s1itm;
           clear vathid;
           clear vatdid;
           clear vath;
           clear vatd;
           clear totalLine;

           sfClear();   // Clear other Subfile Display stuff as inr original POS app

           chain (wsord) oehdr;
           ohlsm = wssmn;
           ohlws = wsid;
           ohldt = %Dec(%date():*YMD);
           ohcnm = cmnme;
           ohTme = %dec(%Time());

           if NOT %FOUND(OEHDR);
             clear vathid;     // VAT: Clear last/previous VAT Hdr ID.
             OHORD = WSORD;
             OHSTR = store.code;
             OHINV = 0;

             monitor;
               if (%Check(digits: s1Cus) = 0);
                 ohcus = %int(s1Cus);
               else;
                 ohcus = 99999;
               endif;
             on-error;
               ohcus = 99999;
             endmon;

             if (defaults.isBondedStore);
                wsBond# = '';
                wsBond  = 'N';
                svBond  = wsBond;
                ohBond  = wsBond#;
             endif;

             ohows = wsid;
             ohtyp = wstyp;
             ohosm = wssmn;
             ohodt = %Dec(%date():*YMD);
             if (ohord > 0);
               WRITE OEHDRF;
             else;
               wsErr = 'Zero Order number detected. Order Creation failed.';
               *IN90 = *ON;
               return;
             endif;
           else;
             UPDATE OEHDRF;
           endif;

           chain (wsord) oectl;
           OCSCD = 'L';
           OCORD = WSORD;
           OCSMN = WSSMN;
           if %Found(OECTL);
             UPDATE OECTLF;
           else;
             WRITE OECTLF;
           endif;
      /end-free
     P createNewOrder  E

     P sfClear         B
     D sfClear         PI
      /free
           *in74  = *OFF;
           sfRCDNBR = 1;
           sfRRN1   = 0;
           wsOrdTot = 0;
           wsVatTot = 0;
           wsInvTot = 0;
           wsnIt     = 0;
           dock1t    = 'N';
           wsniti    = 0;
           wsnitc    = 0;

      /end-free
     P sfClear         E

     P clearNewOrder   B
     D clearNewOrder   PI
      /free
              //  Clear for New Order after finishing off the prior Order
           clear wsord;
           clear vathid;
           clear vatdid;
           clear vath;
           clear vatd;
           clear bVATHdr;
           clear bVATDtl;

           clear ohain;  // Doing this just because I can!

           clear inhibitf1;
           clear InvDate;
           clear InvNbr;
           clear nOvrInvNbr;
           clear holdinvnbr;
           clear bNoUpdateInv;

           CLEAR OEHDRF;
           CLEAR OEDTLF;
           CLEAR OECTLF;
           CLEAR OHHD3F;
           CLEAR OEHDRR4;
           CLEAR OEHDRR5;
           CLEAR OEHHDR4;
           CLEAR OEHHDR5;
           CLEAR OEHDTL5;
           CLEAR OH3;

           creditReturnCount = 0;
           bFailedOvrTest = *OFF;
           s1Cus = *ALL'9';
      /end-free
     P clearNewOrder   E


     P deleteOrder     B
     D deleteOrder     PI
     D   orderNbr                    10I 0 Const

     D ordNbr          S             10I 0
     D invNbr          S             10I 0
     D cstNbr          S             10I 0
     D invDate         S              6S 0
     D vatID           S             20I 0 Inz
      /free
           exec SQL SELECT  ohord, ohinv, ohidt, ohcus
                     INTO   :ordNbr, :invNbr, :invdate, :cstnbr
                     FROM   OEHDR
                     WHERE  ohord = :orderNbr
                     ORDER BY ohord, ohidt desc
                     FETCH FIRST ROW ONLY;
           if (SQLState < '02000');
               // We use invoice date here because order date and invoice
               // date are the same except on rare occasions on SHELL.
             exec SQL SELECT vathid  INTO :vatID
                       FROM  VATHDR
                       WHERE ordnbr = :ordNbr and invnbr = :invnbr and
                             cstnbr = :cstnbr
                       ORDER BY ORDNBR, ORDDATE DESC
                      FETCH FIRST ROW ONLY;
             if (sqlState >= '02000');
               clear vatID;
             endif;
           endif;
           delete (orderNbr) oehdr;
           delete (orderNbr) oectl;
           exec SQL DELETE FROM OEDTL where ODORD = :orderNbr;

           if (SQLState < '02000' and vatID > 0);
              exec SQL DELETE FROM VATHDR where vathid = :vatID;
              exec SQL DELETE FROM VATDTL where vathid = :vatID;
           endif;
           VATHID = 0;
           VATDID = 0;
           clearNewOrder();
      /end-free
     p deleteOrder     E

     P deleteLineItem  B
     D deleteLineItem  PI
     D  delOrdNbr                    10I 0 Const
     D  delOrdSeq                    10I 0 Const
     D yesterday       S               Z    Inz(*SYS)
      /free
           delete (delOrdNbr:delOrdSeq) oeDtlf;

           if (vathid = 0);
              yesterDay += %Days(1);
              exec SQL DELETE FROM VATDTL
                        where ORDNBR = :delOrdNbr and ordSeq = :delOrdSeq and
                              crtDate between :yesterday and CURRENT_TIMESTAMP;

           else;
              exec SQL DELETE FROM VATDTL
                        WHERE vathid = :vathid and ordSeq = :delOrdSeq;
           endif;
      /end-free
     P deleteLineItem  E


     P isCashCust      B
     D isCashCust      PI             1N
     D  custNo                       10I 0 Const
      /free
        if (defaults.isBondedStore);
          return (custNo >= CASHCUST or custNo = bond.bondedCashCust);
        else;
          return (custNo >= CASHCUST);
        endif;
      /end-free
     P isCashCust...
     P                 E

     P isBondedCashCust...
     P                 B
     D isBondedCashCust...
     D                 PI             1N
     D  custNo                       10I 0 Const
      /free
        if (defaults.isBondedStore);
          return (custNo = bond.bondedCashCust);
        endif;
        return *OFF;
      /end-free
     P isBondedCashCust...
     P                 E

     P isBondedCashCustA...
     P                 B
     D isBondedCashCustA...
     D                 PI             1N
     D  custA                         5A   Const
     D  custNo         S              5S 0
      /free
        if (%Check(DIGITS:custA) = 0);
          custNo = %int(custA);
        else;
          return *OFF;
        endif;

        if (defaults.isBondedStore);
          return (custNo = bond.bondedCashCust);
        endif;
        return *OFF;
      /end-free
     P isBondedCashCustA...
     P                 E


     P isCashCustA     B
     D isCashCustA     PI             1N
     D  custA                         5A   Const
     D  custNo         S              5S 0
      /free
        if (%Check(DIGITS:custA) = 0);
          custNo = %int(custA);
        else;
          return *OFF;
        endif;

        if (defaults.isBondedStore);
          return (custNo >= CASHCUST or custNo = bond.bondedCashCust);
        else;
          return (custNo >= CASHCUST);
        endif;
      /end-free
     P isCashCustA     E

     P isNotCashCustA...
     P                 B
     D isNotCashCustA...
     D                 PI             1N
     D  custA                         5A   Const
     D  custNo         S              5S 0
      /free
        if (%Check(DIGITS:custA) = 0);
          custNo = %int(custA);
        else;
          return *ON;  // Return *ON to indicate "NOT Cash Cust"
        endif;

        if (defaults.isBondedStore);
          return NOT (custNo >= CASHCUST or custNo = bond.bondedCashCust);
        else;
          return NOT (custNo >= CASHCUST);
        endif;
      /end-free
     P isNotCashCustA...
     P                 E

     P isNotCashCust...
     P                 B
     D isNotCashCust   PI             1N
     D  custNo                       10I 0 Const
      /free
        if (defaults.isBondedStore);
          return NOT (custNo >= CASHCUST or custNo = bond.bondedCashCust);
        else;
          return NOT (custNo >= CASHCUST);
        endif;
      /end-free
     P isNotCashCust...
     P                 E

     P getBondedCashCust...
     P                 B
     D getBondedCashCust...
     D                 PI

     D lastName        S             25A
     D bd              DS                  Qualified Inz
     D  KeyA                          6A
     D  Key                           6S 0 Overlay(KeyA)
     D  Nbr                           5A
     D  Year                          4A
     D  cust                          5P 0
      /free
           wsBond# = '';
           getBondCust2(lastName : bd.KeyA : bd.Nbr: bd.Year : bd.cust);
           if (bd.Year = '' or bd.Year < %char(*YEAR));
               evalR scBdyr = %char(*YEAR);
               exfmt posGW;
               wsBond# = '';
               wsBond  = 'N';
               svBond  = ' ';
               bd.Nbr = '';
           endif;
           if (bd.Nbr <> '');
             scbs = 'BC';
             monitor;
               bd.Key = %int(bd.Nbr);
             on-error;
               bd.Key = 0;
             endmon;
             wsBond# = bd.Nbr;
           endif;

      /end-free
     P getBondedCashCust...
     P                 E

     P getBondedInfo...
     P                 B
     D getBondedInfo...
     D                 PI
     D custNo          S             10I 0
         // Validate Bond Number
      /free
           wsbond# = *blank;
           wsbond  = 'N';
           monitor;
           custNo = %int(s1Cus);
           on-error;
             custno = 0;
           endmon;
           if IsNotCashCustA(s1Cus);
              EXEC SQL SELECT bdCust, bdNbr, bondYR
                        INTO  :dbBond.bdCust, :dbBond.bdNbr, :dbBond.bondYR
                        FROM  BONDMAST
                        WHERE aidCust = :custNo
                        ORDER BY bdCust, bondYR DESC
                        FETCH FIRST ROW ONLY;
              if (SQLState < '02000');    // Bond Record found?
                wsBond# = dbBond.bdnbr;
                wsBond  = 'Y';
                if (dbBond.bondYR < *YEAR);
                  scBdyr = %Char(*YEAR);
                  exfmt posGW;
                  wsBond# = '';
                  wsBond  = 'N';
                  svBond  = ' ';
                endif;
                scbs = '';
              endif;
            endif;
            if isBondedCashCust(cmcus) and wsbond# = '';
                getBondedCashCust();
                WSPDO = 'P';
                if wsbond# <> '';
                   wsbond = 'Y';
                endif;
              endif;
      /END-FREE
     P getBondedInfo...
     P                 E

     P isBondedOrder   B
     D isBondedOrder   PI             1N
     D   order                        7P 0 Const
      /free
           if (defaults.isBondedStore);
             if (order > 0);
                return (OHBOND <> '');
             endif;
           endif;
           return *OFF;
      /end-free
     P isBondedOrder   E

     P LoadPOSDefaults...
     P                 B
     D LoadPOSDefaults...
     D                 PI
      /free
           default.MAX_RTN_AGE =
                    getappDftInt('MAX_RTN_DAYS':default.MAX_RTN_AGE);
           default.MAX_ITEMS =
                    getappDftInt('MAX_ITEMS':default.MAX_ITEMS);
           default.MAX_CORE_RTN =
                    getappDftInt('MAX_CORE_RTN':default.MAX_CORE_RTN);
           default.MAX_ANY_RTN =
                    getappDftInt('MAX_ANY_RTN':default.MAX_ANY_RTN);
           default.ALLOW_CR_RTN =
                    getappDftLgl('ALLOW_CR_RETURNS':default.ALLOW_CR_RTN);
           *IN64 = default.ALLOW_CR_RTN;
           default.ALLOW_SPEC_ORD =
                    getappDftLgl('ALLOW_SPEC_ORD':default.ALLOW_SPEC_ORD);
           *IN65 = default.ALLOW_SPEC_ORD;
           defaults.chargeVATonCore =
                    getappDftLgl('POS_CORE_VAT': defaults.ChargeVATonCore );

           // protect CORE entry on the Special Order screen.
           *IN55 = getAppDftLgl('POS_SPCORD_CORE_PROTECT' : 'Y');

              // Normally only FPS is a Bonded Store.
              // App Defaults allows any store to be Bonded.
              //  POS_BONDED = 'Y'  means Bonded Override.
              //  If not 'Y' then assume Store Abbr (e.g., 'FPS')
              //  is used to determine bonded store status.

           qtmp.bondedStore = getAppDft('POS_BONDED' : '?');
           if (qtmp.bondedStore = 'Y') or
              (qtmp.bondedStore = 'y') or
              (qtmp.bondedStore = '1') or
              (qtmp.bondedStore = 'YES');  // Only seton *IN88 if Bonded.
             default.bondedStore = *ON;
             *IN88 = *ON;                   // Do not setoff *IN88 here.
           endif;
           return;

      /end-free
     P LoadPOSDefaults...
     P                 E


**CTDATA ERR
01  INVALID ITEM NUMBER PLEASE RE-KEY  *
02  INVALID CUSTOMER NUMBER PLEASE RE-KEY  *
03  CASH COLLECTED MUST BE GREATER THAN INOVICE TOTAL  *
04  PAY TYPE MUST BE 'C' 'H' 'K' 'R' OR 'V'  *
05  DEFECTIVE MUST Y OR N    *
06  F10=SELL AT ZERO PRICE   *
07  PAY TYPE MUST BE H FOR CREDIT MEMO RETURNS
08  REQUEST CANCELED, RETRY
09  AUTHORIZATION FAILED, RETRY PAYMENT
10  CHECK NUMBER MAY NOT BE BLANK
11  CASH DRAWER NOT OPEN: F2=START A DRAWER
12  PREVIOUS DRAWER NOT CLOSED: F4=CLOSE DRAW
13  NO PREVIOUS DRAWER STARTED: F2=START A DRAWER
14  NO DRAWER SCANNED, LABEL ON BOTTOM OF TRAY
15  ACCOUNT CAN NOT BE A HOUSE CHARGE
16  CHECK MUST = REMAINING AMT
17  CORE RETURN ONLY ON CREDIT MEMO
18  MISC/SPECIAL REQUIRE PRICE
19  VOUCHER PAYMENT NOT ACTIVE
20  SECURITY VIOLATION - ** ACTIONS LOGGED **
21  ACCOUNT PAST DUE - F8=OVVERRIDE
22  ACCOUNT OVER CREDIT LIMIT - F8=OVVERRIDE
23  P.O. NUMBER REQUIRED
24  TAG IN USE, DO NOT USE TODAY.  ALREADY USED ONCE
25  PRINT QUOTE MUST BE A Y/N.
26  ** ITEMS ADDED ** DOUBLE CHECK INVOICE
27  CUSTOMER INVALID FOR PENDING TRANSACTIONS, START OVER
28  CANNOT CHANGE CUSTOMERS ON A CREDIT
29  CUSTOMER CAN NOT BE BLANK/ZERO
30  ITEM TYPE MUST BE A 3,4,5,6 (CREDIT MEMO)
31  CUSTOMER PAST DUE, MUST HAVE AUTHORIZATION
32  INVALID BADGE FOR PAST DUE OVERRIDE
33  BADGE NOT AUTHORIZED, PASTDUE OVERRIDE
34  INVALID BADGE FOR OVERLIMIT OVERRIDE
35  BADGE NOT AUTHORIZED, OVERLIMIT OVERRIDE
36  BADGE NOT AUTHORIZED FOR EMPLOYEES
37  Invalid badge for override.
38  Badge not authorized for override.
39  Returns/credit require Authorization. F8=Authorize
40   ** You are about to end your shift and print your daily summary.
41   ** Qty being returned is GREATER than quantity sold.
42   ** Return item price variance. Verify price being credited.
43   ** Badge not authorized for Credit/Return w/no invoice.
44   ** Customer number not same as original invoice customer.
45   ** Can't apply override. Must scan badge.
46   ** Discount requires authorization. F8=Authorize
47   ** You must scan tag.
48   ** Item is fully returned.
49   ** Too many returned items.
50   ** CANNOT MIX CREDIT/INVOICE TAGS on same order.
