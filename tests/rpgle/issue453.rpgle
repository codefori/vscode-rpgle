      *
          dcl-f Custmast disk(*ext) usage(*input) KEYED USROPN INFDS(INFDS);
         dcl-f BALANCE disk(80) usage(*input) KEYED(*CHAR:7);
         dcl-f INVENTRY disk usage(*input:*output);
          dcl-f itemmast disk(*ext) usage(*input) KEYED USROPN
          EXTFILE('AIDDATA/MASTR');
          dcl-f Custmast disk(*ext) usage(*input) KEYED USROPN
            INFDS(INFDS);
          dcl-f balance disk(80) usage(*input) KEYED(*CHAR:7);
         dcl-f INVENTRY disk usage(*input:*output);
          dcl-f itemmast disk(*ext) usage(*input) KEYED USROPN
            EXTFILE('AIDDATA/MASTR');

     D saved           DS                  Qualified
     D  max_RTN_AGE                   5I 0 Inz(30)
     D  max_ITEMS                     5I 0 Inz(50)
     D  max_CORE_RTN                  5I 0 Inz(3)
     D  max_ANY_RTN                   5I 0 Inz(4)
     D  ALLOW_CR_RTN                  1N   Inz(*ON)
     D  ALLOW_SPEC_ORD...
     D                                1N   Inz(*OFF)
     D  shortRound                    1S 0
     D  secondShort                   3P 0
       // Charge VAT on CORE sales
     D  chargeVATonCore...
     D                                1N   Inz(*OFF)
     D  CHECK_TAG_NAME...
     D                                1N   Inz(*ON)
     D  bondedStore                   1N   Inz(*OFF)
     D  isBondedStore                 1N   Overlay(bondedStore)
     D  isBonded                      1N   Overlay(bondedStore)
     D  MAX_BIG_QTY                   7P 0 Inz(10000)

     D  chargeVATonCore...
     D                 S              1N   Inz(*OFF)
     D  CHECK_TAG_NAME...
     D                 S              1N   Inz(*ON)

     D default         DS                  Qualified
     D  max_RTN_AGE                   5I 0 Inz(30)
     D  max_ITEMS                     5I 0 Inz(50)
         // Fake comment 2
     D  max_CORE_RTN                  5I 0 Inz(3)
     D  max_ANY_RTN                   5I 0 Inz(4)
     D  ALLOW_CR_RTN                  1N   Inz(*ON)
             // fake comments
     D  ALLOW_SPEC_ORD...
     D                                1N   Inz(*OFF)
     D  shortRound                    1S 0
     D  secondShort                   3P 0
       // Charge VAT on CORE sales
     D  chargeVATonCore...
     D                                1N   Inz(*OFF)
     D  CHECK_TAG_NAME...
     D                                1N   Inz(*ON)
     D  bondedStore                   1S 0 Inz(0) BASED(ptr_veryLong...
     D                                      namethatisAlsoAPointer)
     D  isBondedStore                 1N   Overlay(bondedStore)
     D  isBonded                      1N   Overlay(bondedStore)
     D  MAX_BIG_QTY                   7P 0 Inz(10000)

     D  chargeVATonCore...
     D                 S              1N   Inz(*OFF)
     D  CHECK_TAG_NAME...
     D                 S              1N   Inz(*ON)


      * Variable MyString is a Java String object.
          dcl-s MyString   OBJECT(*JAVA :'java.lang.String');
          dcl-pr bdcreate OBJECT  EXTPROC(*JAVA: 'java.math.BigDecimal'
            :*CONSTRUCTOR);
          end-pr;

          dcl-c MyConst CONST('These are the times to remember cause they will +
            not last forever. These arethe days to hold onto.');
          dcl-s Variable varchar(256)  INZ('Gerkins');
         dcl-ds legacyPSDS PSDS;
         pgmname *PROC;
         status *STATUS;
         intJOBID int(10) POS(9);
         intSale4 bindec(5:2) POS(5);
         intSale2 int(5) POS(5);
         prevStatus zoned(3:0) POS(3);
         instNo packed(5:0) POS(5);
         exceptionData char(10) POS(10);
         exceptionID char(5) POS(5);
         jobName char(10) POS(10);
         wsid char(10) overlay(PSDS_LARGE_T:244);
         jobUser char(10) overlay(PSDS_LARGE_T:254);
         user char(10) overlay(PSDS_LARGE_T:254);
         jobNbr char(6) overlay(PSDS_LARGE_T:264);
         jobNo zoned(6:0) overlay(PSDS_LARGE_T:264);
         jobNumber zoned(6:0) overlay(PSDS_LARGE_T:264);
     D ThisIsAVeryLongDataStructure2...
     D extra                         10A
     D ThisIsAVeryLongDataStructureName...
     D                               10A
     D  ThisIsAVeryLongDataStructureNameext...
     D                 DS
     D                                       Qualified Inz
     D custNbr                        7p 0
     D custName                      10A

     D custName        S             10A
     D custName
     D                 S             10A
          dcl-ds custInfo EXTNAME('CUSTMAST') INZ;
          name  EXTFLD INZ('UNKNOWN');
     D address       E                     EXTFLD(adr)
          id_number  EXTFLD('ID') INZ(-1);

          dcl-ds *n PSDS;
          // What the actual fuck? Over.
          status pos(*STATUS);
          prevStatus zoned(5:0) overlay(PSDS_LARGE_T:16 );
          runLib char(10) overlay(PSDS_LARGE_T:81 );
          exceptionData char(80) overlay(PSDS_LARGE_T:91 );
          exceptionID char(4) overlay(PSDS_LARGE_T:171);
          jobName char(10) overlay(PSDS_LARGE_T:244);
          wsid char(10) overlay(PSDS_LARGE_T:244);
          jobUser char(10) overlay(PSDS_LARGE_T:254);
          user char(10) overlay(PSDS_LARGE_T:254);
          jobNbr char(6) overlay(PSDS_LARGE_T:264);
          jobNo zoned(6:0) overlay(PSDS_LARGE_T:264);
          jobNumber zoned(6:0) overlay(PSDS_LARGE_T:264);
          pgmCrtDate char(6) overlay(PSDS_LARGE_T:288);
          pgmCrtTime char(6) overlay(PSDS_LARGE_T:294);
          osvxry char(4) overlay(PSDS_LARGE_T:300);
          crtSrcFile char(10) overlay(PSDS_LARGE_T:304);
          crtSrcLib char(10) overlay(PSDS_LARGE_T:314);
          crtSrcMbr char(10) overlay(PSDS_LARGE_T:324);
          Module char(10) overlay(PSDS_LARGE_T:344);
          dcl-subf select char(10);
          dcl-subf chain char(10);
          end-ds;

         dcl-ds LDA_DS1a DTAARA(*LDA);
         SUBFLD char(600) POS(1);
         end-ds LDA_DS1a;

         dcl-ds LDA_DS1b DTAARA(*LDA:*AUTO:*USRCTL);
         SUBFLD char(50) POS(1);
         end-ds LDA_DS1b;

         dcl-ds LDA_DS2a DTAARA(*LDA);
         SUBFLD char(600) POS(1);
         end-ds LDA_DS2a;

         dcl-ds LDA_DS2b DTAARA(*LDA:*AUTO:*USRCTL);
         SUBFLD char(50) POS(1);
         end-ds LDA_DS2b;


          // *
         dcl-ds ALONE INZ;
         BASEPD zoned(8:3) POS(1);
         end-ds ALONE;
          // *
         dcl-ds *n DTAARA(*AUTO);
         WINV char(7) POS(2);
         WSTR char(2) POS(100);
         end-ds;
         dcl-s UINV packed(7:0); // Calc Spec work-field;
         dcl-s USTR packed(2:0); // Calc Spec work-field;

         EVALR UINV = WINV;
         EVALR USTR = WSTR;
           // Calc Spec work-field

         CHAIN CUSTNO CUSTMAST ;
         *IN32 = NOT %FOUND(CUSTMAST);

         X = 0;


         dcl-proc system EXPORT;
         dcl-pi system int(10) extproc(*CWIDEN:'system');
         command varchar(256);
         end-pi system;
         end-proc;