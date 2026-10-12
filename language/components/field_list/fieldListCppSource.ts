/**
 * Raw FIELD_LIST C++ source synced from open-UDTF.
 * No base64 encoding is used; source is uploaded as plain text.
 */
export function getFieldListCPPSrc(): string {
    return `

// @author BobCozzi

#ifndef __STDC_WANT_DEC_FP__
#define __STDC_WANT_DEC_FP__
#endif

#ifndef __POSIX_LOCALE__
#define __POSIX_LOCALE__
#endif

#pragma datamodel(P128)

#include <stdlib.h>
#include <stdarg.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <p_time.h>
#include <time.h>

#include <langinfo.h>
#include <QP0LSTDI.h>
#include <QP0ZTRC.h>

#include <sys/stat.h>
#include <sys/types.h>

#include <time.h>
#include <errno.h>
#include <ctype.h>
#include <inttypes.h>
#include <except.h>
#include <decimal.h>

#include <iconv.h>
#include <QTQICONV.h>

#include <LECOND.h>

#include <QUSEC.h>
#include <QUSGEN.h>
#include <QUSCRTUS.h>
#include <QUSCUSAT.h>
#include <QUSPTRUS.h>

#include <QLGCASE.h>
#include <QWCCVTDT.h>

#include <QMH.h>
#include <QMHSNDPM.h>
#include <QMHRCVPM.h>
#include <QMHRMVPM.h>

#include <QUSRJOBI.h>

#include <QUSLFLD.h>
#include <QUSLRCD.h>
#include <QLIDLTO.h>


#include <mih/micommon.h>
#include <mih/matpgmnm.h>
#include <mih/cpybytes.h>
#include <mih/triml.h>

#include <cstring>
#include <cctype>
#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <memory>
#include <ctime>

#include <algorithm>
#include <vector>

#include <SQL.h>
#include <SQLUDF.h>

using namespace std;
// Copyright (c) 2026 by R. Cozzi, Jr.

const int coz_MAXUSRSPACESIZE = 16711568;
const int coz_MAXMEM15M = 15662992;

/* Case-conversion direction constants */
#ifndef _TOUPPER
#define _TOUPPER 0
#endif
#ifndef _TOLOWER
#define _TOLOWER 1
#endif

#define isEmpty(_s) ((::triml( _s, ' ')==0) ? 1 : 0)

#define inChar(_v) \
    char *in##_v = \
        (char *)coz_getNextParmIf(p, argc, argv)
#define inInt(_v) \
    int *in##_v = (int *)coz_getNextParmIf(p, argc, argv)
#define inShort(_v) \
    short *in##_v = (short *)coz_getNextParmIf(p, argc, argv)
#define inSmallInt(_v) \
    short *in##_v =    \
        (short *)coz_getNextParmIf(p, argc, argv)
#define inBigInt(_v)    \
    long long *in##_v = \
        (long long *)coz_getNextParmIf(p, argc, argv)

#define outChar(_v) \
    char *out##_v = \
        (char *)coz_getNextParmIf(p, argc, argv, 1)

#define outVARCHAR(_v) \
    struct sqludf_vc_fbd *out##_v = \
        (struct sqludf_vc_fbd*) \
        coz_getNextParmIf(p,argc,argv,2)

#define outVARBINARY(_v) \
        SQLUDF_VARBINARY* out##_v = \
        (SQLUDF_VARBINARY*)coz_getNextParmIf(p, argc, argv,2)

    // VARGRAPHIC uses a 2-byte length prefix.
#define outVARGRAPHIC(_v) \
        struct sqludf_vg* out##_v = \
       (struct sqludf_vg*) coz_getNextParmIf(p, argc, argv,2)

#define outCLOB(_v)        \
    SQLUDF_CLOB *out##_v = \
        (SQLUDF_CLOB *)coz_getNextParmIf(p, argc, argv, 4)
#define outInt(_v) \
    int *out##_v = \
        (int *)coz_getNextParmIf(p, argc, argv, 4)

#define outSmallInt(_v) \
    short *out##_v =    \
        (short *)coz_getNextParmIf(p, argc, argv, 2)

#define inIndy(_v)      \
    short *indyIn##_v = \
        (short *)coz_getNextParmIf(p, argc, argv, 2)
#define outIndy(_v)   \
    short *indy##_v = \
        (short *)coz_getNextParmIf(p, argc, argv, 2)

typedef struct tag_QUS_EC
{
    union
    {
        int Bytes_Provided;
        int length;
    };
    union
    {
        int Bytes_Available;
        int Bytes_Returned;
    };
    union
    {
        char Exception_Id[7];
        char msgid[7];
    };
    char Reserved;
    union
    {
        char Exception_Data[255];
        char msgdata[255];
    };
} qusec_t;

///////  BEGIN iconv

class CiConv {
private:
    iconv_t hConv;
    QtqCode_T toCode;
    QtqCode_T fromCode;

public:
    CiConv();
    CiConv(int to, int from);
    ~CiConv();

    void clear();
    iconv_t open();
    iconv_t open(int to, int from);
    void close();
    int  isOpen();
    void setFromCCSID(int ccsid = 0);
    void setToCCSID(int ccsid = 0);
    int  getFromCCSID();
    int  getToCCSID();
    void setCCSID(int toCCSID, int fromCCSID);
    int  xlate(char* pOutput,
               int outLen = -1,
               const char* pInput = NULL,
               int inLen = -1,
               int* pBytesProduced = NULL,
               int* pBytesConsumed = NULL);
    int  convert(char* pOutput, const char* pInput);

};

CiConv::CiConv()
{
   clear();
   hConv.return_value = -1;
}
CiConv::CiConv(int to, int from)
{
   clear();
   hConv.return_value = -1;
   toCode.CCSID = to;
   fromCode.CCSID = from;
   open();
}
CiConv::~CiConv()
{
   close();
}

void CiConv::clear()
{
  memset(&hConv,0x00, sizeof(hConv));
  memset(&toCode, 0x00, sizeof(toCode));
  memset(&fromCode, 0x00, sizeof(fromCode));
}

iconv_t CiConv::open()
{
   if (hConv.return_value == -1)
   {
     hConv = QtqIconvOpen( &toCode, &fromCode );
   }
   return hConv;
}
iconv_t CiConv::open(int to, int from)
{
   if (hConv.return_value == -1)
   {
     clear();
     toCode.CCSID = to;
     fromCode.CCSID = from;
     hConv = QtqIconvOpen( &toCode, &fromCode );
   }
   return hConv;
}

void CiConv::close()
{
   if (hConv.return_value != -1)
   {
      iconv_close(hConv);
      memset(&hConv,0x00, sizeof(hConv));
      hConv.return_value = -1;
   }
}

int CiConv::isOpen()
{
    return (hConv.return_value == -1) ? 0 : 1;
}

void CiConv::setFromCCSID(int ccsid)  // Default to "Job CCSID")
{
   fromCode.CCSID = ccsid;
  // if (ccsid == 1200 || ccsid == 13488) fromCode.shift_alternative = 1;
}
void CiConv::setToCCSID(int ccsid)  // Default to "Job CCSID")
{
   toCode.CCSID = ccsid;
}
void CiConv::setCCSID(int toCCSID, int fromCCSID)  // Default to "Job CCSID")
{
   setToCCSID(toCCSID);
   setFromCCSID(fromCCSID);
}

int CiConv::getFromCCSID()
{
   return fromCode.CCSID;
}
int CiConv::getToCCSID()
{
   return toCode.CCSID;
}

int CiConv::xlate(
    char* pOutput,
    int outLen,
    const char* pInput,
    int inLen,
    int* pBytesProduced,
    int* pBytesConsumed)
{
    const int outputTargetMultiplier = 4;  // (4x length of input by default)

    char* inputPtr = NULL;
    char* outputPtr = pOutput;


    size_t inputSize  = 0;
    size_t outputSize = 0;

    if (pInput == NULL)
    {
        inputPtr = const_cast<char *>(pOutput);
    }
    else
    {
        inputPtr = const_cast<char *>(pInput);
    }

    if (inLen <= 0)
    {
      inputSize = strlen(inputPtr);
    }
    else
    {
      inputSize = inLen; // Adjust this based on your needs
    }
    if (outLen <= 0)
    {
      outputSize = inputSize * outputTargetMultiplier;
    }
    else
    {
      outputSize = outLen;
    }
    const size_t initialInputSize = inputSize;
    const size_t initialOutputSize = outputSize;

    size_t result = iconv(hConv, &inputPtr, &inputSize, &outputPtr, &outputSize);

    const int bytesProduced = (int)(initialOutputSize - outputSize);
    const int bytesConsumed = (int)(initialInputSize - inputSize);

    if (pBytesProduced != NULL)
    {
        *pBytesProduced = bytesProduced;
    }
    if (pBytesConsumed != NULL)
    {
        *pBytesConsumed = bytesConsumed;
    }

    // Return -1 on conversion failure; otherwise return output bytes produced.
    return ((int)result == -1) ? -1 : bytesProduced;
}
int CiConv::convert( char* pOutput, const char* pInput )
{
   return xlate( pOutput, -1, pInput, -1);
}

///////  END iconv

typedef struct tagFieldData
{
    Qdb_Lfld_Header_t *pListHdr;
    Qdb_Lfld_FLDL0300_t *pField;
} fieldData_t;

/* ============================================= */
/* Helper function prototypes                    */
/* ============================================= */
inline void copyPad(char *t, const char *s, int padLen = 10, char padChar = ' ');
inline int copyUntil(char *t, const char *s, int maxLen = 10, const char *stopChars = NULL, bool bTrim = true);
inline int coz_toUpper(char *szData, int inLen = -1, int ccsid = 0);
inline int coz_nameUpper(char *szData, int inLen = -1);
inline char *coz_getNextParmIf(int &pC, int &argc, char **argv, int ioFlag = 0);
static int getJobInfo(Qwc_JOBI0400_t* pJobDesc);

void coz_resignalMsg(qusec_t *ec);

inline int RMVMSG(const char* msgtype, const char* msgkey=NULL);
inline void coz_DLTOBJ(const char *qualObjName, const char *objType, const char *ASPName = "*");
char *coz_TEMP_USRSPACE(char *rtnUSName, const char *objAttr = "SQLTOOLS", int initSize = 4096);
char *coz_getPtrUsrSpace(const char *p2PartUsrSpaceName);
void coz_makeAPIObjName(char *objName, const char *qualObj, const char *dftLib = "*LIBL");
int getNextFmt(char *rcdfmt, const char *szUserSpace, int index);
fieldData_t *getNextField(const char *szUserSpace, int &index);
static void dumpField(Qdb_Lfld_FLDL0100_t* pField);
static int extractDFT(const unsigned char *defaultPtr,
                    int defaultLen,
                    char dataType,
                    int digits,
                    char* result);

static CiConv  colHdr_to_jobCCSID;
static CiConv  fldText_to_jobCCSID;
static CiConv  edtWord_to_jobCCSID;

typedef _Packed struct tagScratch
{
    int length;          // Scratch Pad length
    int LOG;             // Non-zero to log info to joblog
    int eof;             // End of File flag
    int jobCCSID;        // Job Default CCSID
    int onlyRCDFMT;      // Only one rcdfmt requested?
    int nextRCDFMT;      // Next record format to get
    int nextFIELD;       // Next field to return
    int offsetFIELD;     // Cumulative Offset to "next" field
    int FIELDCOUNT;      // Number of fields in this format

    char FILENAME[21];   // Name of File whose fields are returned
    char OVR[2];         // Respect OVERRIDEs '1' or '0'
    char RCDFMTLIST[21]; // List of Record Formats User space
    char FIELDLIST[21];  // List of Fields User space
    char RCDFMTNAME[11]; // User-specfied RCDFMT parameter value
    char RCDFMT[11];     // Record format being used
    Qwc_JOBI0400_t jobInfo;
} scratch_t;

scratch_t scratch;

int main(int argc, char *argv[])
{
    int p = 0;

    _MI_Time mt;
    time_t epochTime;

    //////////////////////////////////////////////
    //  INPUT Parameters
    //////////////////////////////////////////////
    inChar(LIBRARY);  // Library containing the file
    inChar(FILENAME); // File name
    inChar(RCDFMT);   // Record format name  DEFAULT *FIRST
    inChar(OVR);      // Override processing DEFAULT *NO
    inChar(LOG);      // Write info to joblog?

    //////////////////////////////////////////////
    //  OUTPUT Columns for DSPFFD
    //  These mirror the members of Qdb_Lfld_FLDL0300_t.
    //////////////////////////////////////////////
    outChar(LIBRARY);
    outChar(FILENAME);
    outChar(FILETYPE); // PF, LF, DSPF, PRTF, DDMF, etc.
    outChar(RCDFMT);
    outInt(FIELDCOUNT);   // Number of fields in format

    outInt(SEQNBR);   // Field's "ORDINAL_POSITION"
    outChar(FIELDNAME);

    outChar(SQLDATATYPE);
    outChar(RPGDATATYPE);

    outInt(FIELD_LENGTH);
    outChar(DATATYPE);
    outInt(DEC_POS);
    outInt(FIELD_BUFFER_LENGTH);

    outInt(INPUT_BUFFER_POS);
    outInt(OUTPUT_BUFFER_POS);

    outChar(USAGE); // I=Input, O=Output, B=Both/Combined, N=None
    outInt(CCSID);

    outChar(SQLDEFN);
    outChar(RPGDEFN);

    outChar(COLUMN_HEADING1);
    outChar(COLUMN_HEADING2);
    outChar(COLUMN_HEADING3);
    outChar(INTERNAL_FIELD_NAME);
    outChar(LONG_COLUMN_NAME);

    outChar(FIELD_TEXT);
    outChar(RCDFMT_TEXT);

    outChar(EDIT_CODE);
    outChar(EDIT_WORD);
    outChar(DATE_TIME_FORMAT);
    outChar(DATE_TIME_SEP);

    outInt(UCS2_DISPLAYED_LENGTH);
    outInt(NUMBER_DBCS_CHARACTERS);
    outInt(MAXIMUM_LARGE_OBJECT_LENGTH);
    outInt(LARGE_OBJECT_PAD_LENGTH);

    outChar(UDT_NAME);
    outChar(UDT_LIBNAME);

    outInt(ROWNBR); // DSPF/PRTF Row Number
    outInt(COLNBR); // DSPF/PRTF Column Number

    outChar(IS_NULLABLE);
    outChar(IS_UPDATEABLE);
    outChar(IS_VARCHAR);
    outChar(IS_HIDDEN);
    outChar(IS_ROWID);
    outChar(IS_IDENTITY);
    outChar(IS_TIMESTAMP);
    outChar(HAS_DEFAULT);
    outChar(GENWHEN); // isROWID, isIDENTITY, isTIMESTAMP generated ALWAYS or BY DEFAULT
    outChar(GENFOR); // NULL=Never or ROWID< IDENTITY, ROWCHGDTS, ROWBEGIN, ROWEND, TRANSID, EXPRESSION

    outChar(FIELD_PROC_PGMNAME);
    outChar(FIELD_PROC_LIBNAME);
    outSmallInt(TIMESTAMP_PREC);
    outChar(DFTVALUE);

    //////////////////////////////////////////////
    //  Input Parameter Indicators
    //////////////////////////////////////////////
    inIndy(LIBRARY);
    inIndy(FILENAME);
    inIndy(RCDFMT);
    inIndy(OVR);
    inIndy(LOG);

    //////////////////////////////////////////////
    //  Output Column Indicators
    //////////////////////////////////////////////
    outIndy(LIBRARY);
    outIndy(FILENAME);
    outIndy(FILETYPE); // PF, LF, DSPF, PRTF, DDMF, etc.;
    outIndy(RCDFMT);
    outIndy(FIELDCOUNT);   // Number of fields in format

    outIndy(SEQNBR);

    outIndy(FIELDNAME);
    outIndy(SQLDATATYPE);
    outIndy(RPGDATATYPE);

    outIndy(FIELD_LENGTH);
    outIndy(DATATYPE);
    outIndy(DEC_POS);
    outIndy(FIELD_BUFFER_LEN);

    outIndy(INPUT_BUFFER_POS);
    outIndy(OUTPUT_BUFFER_POS);

    outIndy(USAGE); // I=Input, O=Output, B=Both/Combined, N=None
    outIndy(CCSID);

    outIndy(SQLDEFN);  // Full SQL column type (e.g., Dec(7, 2))
    outIndy(RPGDEFN);  // Full SQL column type (e.g., Packed(7 : 2))

    outIndy(COLUMN_HEADING1);
    outIndy(COLUMN_HEADING2);
    outIndy(COLUMN_HEADING3);
    outIndy(INTERNAL_FIELD_NAME);
    outIndy(LONG_COLUMN_NAME);

    outIndy(FIELD_TEXT);
    outIndy(RCDFMT_TEXT);

    outIndy(EDIT_CODE);
    outIndy(EDIT_WORD);
    outIndy(DATE_TIME_FORMAT);
    outIndy(DATE_TIME_SEP);

    outIndy(UCS2_DISPLAYED_LENGTH);
    outIndy(NUMBER_DBCS_CHARACTERS);
    outIndy(MAXIMUM_LARGE_OBJECT_LENGTH);
    outIndy(LARGE_OBJECT_PAD_LENGTH);

    outIndy(UDT_NAME);
    outIndy(UDT_LIBNAME);

    outIndy(ROWNBR); // DSPF/PRTF Row Number
    outIndy(COLNBR); // DSPF/PRTF Column Number

    outIndy(IS_NULLABLE);
    outIndy(IS_UPDATEABLE);
    outIndy(IS_VARCHAR);
    outIndy(IS_HIDDEN);
    outIndy(IS_ROWID);
    outIndy(IS_IDENTITY);
    outIndy(IS_TIMESTAMP);
    outIndy(HAS_DEFAULT);

    outIndy(GENWHEN); // isROWID, isIDENTITY, isTIMESTAMP generated ALWAYS or BY DEFAULT
    outIndy(GENFOR); // NULL=Never or ROWID, IDENTITY, ROWCHGDTS, ROWBEGIN, ROWEND, TRANSID, EXPRESSION

    outIndy(FIELD_PROC_PGMNAME);
    outIndy(FIELD_PROC_LIBNAME);
    outIndy(TIMESTAMP_PREC);
    outIndy(DFTVALUE);

    //////////////////////////////////////////////
    //  SQL-specific scratch parameters
    //////////////////////////////////////////////
    char *sqlstate = (char *)coz_getNextParmIf(p, argc, argv);
    char *funcName = (char *)coz_getNextParmIf(p, argc, argv);
    char *specificName = (char *)coz_getNextParmIf(p, argc, argv);
    char *sqlmsgtext = (char *)coz_getNextParmIf(p, argc, argv);
    char *scratchPad = (char *)coz_getNextParmIf(p, argc, argv);
    int *sqlOpCode = (int *)coz_getNextParmIf(p, argc, argv);

    ////////////////////////////////////////////////////////////
    //  BEGIN main() body (after parms starts here)
    ////////////////////////////////////////////////////////////

    scratch_t *pScratch = (scratch_t *)scratchPad;
    char *pBuffer = NULL;
    char *pFieldList = NULL;
    char *pRcdFmt = NULL;
    char *pRcdFmtList = NULL;
    char qualFile[21];
    char overrides[2] = "0";
    char FMTAPI[] = "RCDL0100";
    qusec_t ec;

    if (*sqlOpCode == SQLUDF_TF_OPEN)
    {
        // one-time STUFF GOES HERE

        // Initialize Scratch pad
        memset(pScratch, 0x00, sizeof(scratch));
        pFieldList = coz_TEMP_USRSPACE(pScratch->FIELDLIST, "CODE4I");
        pRcdFmt = coz_TEMP_USRSPACE(pScratch->RCDFMTLIST, "CODE4I");

        if (*indyInLOG >= 0 && strlen(inLOG) > 0)
        {
            coz_toUpper(inLOG);
            if (*inLOG == '*') inLOG++;  // Skip leading Asterisk (if any)
            switch (*inLOG)
            {
                case 'Y':  // Yes
                case 'L':  // Log=>'Log'
                case '1':  // Log=>1
                case 'D':  // Log=>'Debug' / Log=>'Dump'
                case 'J':  // Log=>'Joblog'
                case 'S':  // Log=>'Si'
                pScratch->LOG = 1;
                break;
            }
        }
        if (getJobInfo(&pScratch->jobInfo))
        {
            pScratch->jobCCSID = pScratch->jobInfo.Default_Coded_Char_Set_Id;
            colHdr_to_jobCCSID.setToCCSID(pScratch->jobCCSID);
        }

        coz_nameUpper(inFILENAME);
        coz_nameUpper(inLIBRARY);
        memset(qualFile, ' ', sizeof(qualFile)-1);
        _CPYBYTES(qualFile, inFILENAME, strlen(inFILENAME));
        _CPYBYTES(qualFile + 10, inLIBRARY, strlen(inLIBRARY));
        _CPYBYTES(pScratch->FILENAME, qualFile, sizeof(qualFile)-1);

        if (pScratch->LOG)
        {
            Qp0zLprintf("FIELD_LIST: Job Default CCSID(%d)\n",pScratch->jobCCSID);
            Qp0zLprintf(
                "FIELD_LIST(LIBRARY_NAME=>'%s', "
                "FILE_NAME=>'%s', "
                "RCDFMT=>'%s', OVR='%s', "
                "LOG=>'%s')\n",
                inLIBRARY, inFILENAME, inRCDFMT, inOVR, inLOG);
        }

        strcpy(pScratch->OVR, "0");
        if (*indyInOVR >= 0 && strlen(inOVR) > 0)
        {
            coz_toUpper(inOVR);
            if (*inOVR == '*') inOVR++;  // Skip leading Asterisk (if any)
            switch (*inOVR)
            {
                case '1':  // '1'
                case 'Y':  // Yes
                case 'O':  // Override
                case 'R':  // Respect
                case 'S':  // Si
                strcpy(pScratch->OVR, "1");
                break;
            }
        }

        if (*indyInRCDFMT >= 0 && strlen(inRCDFMT) > 0)
        {
            coz_toUpper(inRCDFMT);
            strcpy(pScratch->RCDFMTNAME, inRCDFMT);
        }
        else
        {
            strcpy(pScratch->RCDFMTNAME, "*ALL");
        }

        // If RcdFmt name is *FIRST, *LAST or *ALL the enumerate the list of RCDFMT names
        if (pScratch->RCDFMTNAME[0] == '*')
        {
            memset(&ec, 0x00, sizeof(ec));
            ec.Bytes_Provided = sizeof(ec);
            QUSLRCD(pScratch->RCDFMTLIST, FMTAPI, pScratch->FILENAME, overrides, &ec);
            if (ec.Bytes_Returned > 0)
            {
                coz_resignalMsg( &ec );
            }
        }
    }

    if (*sqlOpCode == SQLUDF_TF_FETCH && pScratch->eof == 1)
    {
        strcpy(sqlstate, "02000");
    }

    // ------------------------------------------------------
    // F E T C H   Operation
    // ------------------------------------------------------
    if (*sqlOpCode == SQLUDF_TF_FETCH && pScratch->eof == 0)
    {
        fieldData_t *pFieldData = NULL;
        Qdb_Lfld_Header_t *pHeader = NULL;
        Qdb_Lfld_FLDL0100_t *pField = NULL;
        Qdb_Lfld_FLDL0300_t *pField300 = NULL;

        char FFDAPI[] = "FLDL0300";
        char override[2];
        override[0] = pScratch->OVR[0];
        override[1] = 0x00;

        do {
            if (pScratch->nextFIELD == 0 && pScratch->onlyRCDFMT != 1)
            {
                if (strcmp(pScratch->RCDFMTNAME, "*LAST") == 0)
                {
                    pScratch->nextRCDFMT = -1;
                }
                if (strcmp(pScratch->RCDFMTNAME, "*FIRST") == 0 ||
                    strcmp(pScratch->RCDFMTNAME, "*ONLY") == 0)
                {
                    pScratch->nextRCDFMT = 0;
                }
                if (strcmp(pScratch->RCDFMTNAME, "*ALL") != 0)
                {
                    pScratch->onlyRCDFMT = 1;
                }
                if (pScratch->RCDFMTNAME[0] == '*')
                {
                    pScratch->nextRCDFMT = getNextFmt(pScratch->RCDFMT,
                                                      pScratch->RCDFMTLIST,
                                                      pScratch->nextRCDFMT);
                    if (pScratch->nextRCDFMT < 0)
                    {
                        pScratch->eof = 1;
                        strcpy(sqlstate, "02000");
                        return 0;
                    }
                }

                memset(&ec, 0x00, sizeof(ec));
                ec.Bytes_Provided = sizeof(ec);
                QUSLFLD(pScratch->FIELDLIST, /* User Space and Library name    */
                        FFDAPI,              /* API Format name                */
                        pScratch->FILENAME,  /* File name and library          */
                        pScratch->RCDFMT,    /* record format name             */
                        override,            /* override processing            */
                        &ec);                /*   Error code                   */
                if (ec.Bytes_Returned > 0)
                {
                    coz_resignalMsg( &ec );
                    pScratch->eof = 1;
                    strcpy(sqlstate, "02000");
                    return 0;
                }
                else
                {
                    memset(&ec, 0x00, sizeof(ec));
                    ec.Bytes_Provided = sizeof(ec);
                    QUSPTRUS(pScratch->FIELDLIST, &pBuffer, &ec);

                    Qus_Generic_Header_0100_t* pGenHdr = (Qus_Generic_Header_0100_t *)pBuffer;
                    pScratch->FIELDCOUNT = pGenHdr->Number_List_Entries;

                }

            }

            if ((pFieldData = getNextField(pScratch->FIELDLIST, pScratch->nextFIELD)) == NULL)
            {
                if (pScratch->onlyRCDFMT != 1)
                {
                    pScratch->nextFIELD = 0;
                    // pScratch->nextRCDFMT++;  // This is auto-incremented from getNextFmt();
                    continue;
                }
                pScratch->eof = 1;
                strcpy(sqlstate, "02000");
                return 0;
            }
            break;
        } while (0 != 1);

        pField300 = pFieldData->pField;
        pHeader = pFieldData->pListHdr;
        pBuffer = (char*)pField300;
        pField = (Qdb_Lfld_FLDL0100_t*)(pBuffer + pField300->Offset_to_Qdb_Lfld_FLDL0100);

        // Qp0zLprintf("getNextField: %d:RCDFMT(%10s)\n",pScratch->nextRCDFMT, pHeader->Record_Format_Name);

        // Copy the data from the pField pointer to the output fields.

        copyUntil(outFILENAME, pHeader->File_Name_Used);
        copyUntil(outLIBRARY,  pHeader->Library_Name);
        copyUntil(outFILETYPE, pHeader->File_Type);
        copyUntil(outRCDFMT,   pHeader->Record_Format_Name);
        copyUntil(outRCDFMT_TEXT, pHeader->Record_Text_Description, 50);
        *outFIELDCOUNT = pScratch->FIELDCOUNT;

        *outSEQNBR = pScratch->nextFIELD;

        copyUntil(outFIELDNAME, pField->Field_Name);
        sprintf(outDATATYPE, "%c", pField->Data_Type);

        char sql_type[64];
        char rpg_type[64];
        char IS_VARCHAR = pField->Variable_Length_Field_Ind;
        int  BUFFER_LENGTH = pField->Field_Length_Bytes;
        memset(sql_type,0x00, sizeof(sql_type));
        memset(rpg_type,0x00, sizeof(rpg_type));

        *indyNUMBER_DBCS_CHARACTERS = -1;
        *indyMAXIMUM_LARGE_OBJECT_LENGTH = -1;
        *indyLARGE_OBJECT_PAD_LENGTH = -1;



        //// BEGIN ChatGPT generated SWITCH BLOCK
        switch (pField->Data_Type)
        {
            case 'D':   /* Digits only */
            case 'E':   /* DBCS or alphanumeric */
            case 'I':   /* Inhibit entry */
            case 'M':   /* Numeric only */
            case 'N':   /* Numeric shift */
            case 'O':   /* Both DBCS and alphanumeric */
            case 'X':   /* Alphabetic only */
            case 'Y':   /* Numeric only */
                strcpy(sql_type, "CHAR");
                break;

            case 'A':   /* Alphanumeric */
            case 'H':   /* BINARY (aka "HEX")*/
                if (IS_VARCHAR=='1')
                    strcpy(sql_type, "VARCHAR");
                else
                    strcpy(sql_type, "CHAR");
                // dumpField(pField);
                break;

            case 'B':   /* Binary/integer */
                strcpy(sql_type, "INT");
                break;

            case 'F':   /* Floating point */
                strcpy(rpg_type, "FLOAT");
                if (BUFFER_LENGTH == 4)
                {
                    strcpy(sql_type, "FLOAT");
                }
                else if (BUFFER_LENGTH == 8)
                {
                    strcpy(sql_type, "DOUBLE");
                }
                break;

            case 'G':   /* Graphic */
                *indyNUMBER_DBCS_CHARACTERS = 0;
                if (IS_VARCHAR=='1')
                {
                    strcpy(sql_type, "VARGRAPHIC");
                    strcpy(rpg_type, "VARGRAPH");
                    pField->Digits = 0; // Fixes a bug in QUSLFLD
                }
                else
                {
                    strcpy(sql_type, "GRAPHIC");
                    strcpy(rpg_type, "GRAPH");
                }
                // dumpField(pField);
                break;

            case 'J':   /* DBCS only */
            case 'W':   /* Katakana */
                *indyNUMBER_DBCS_CHARACTERS = 0;
                if (IS_VARCHAR=='1')
                {
                    strcpy(sql_type, "VARGRAPHIC");
                    strcpy(rpg_type, "VARGRAPH");
                }
                else
                {
                    strcpy(sql_type, "GRAPHIC");
                    strcpy(rpg_type, "GRAPH");
                }
                // dumpField(pField);
                break;

            case 'L':   /* Date */
                strcpy(sql_type, "DATE");
                break;

            case 'P':   /* Packed decimal */
                strcpy(sql_type, "DEC");
                break;

            case 'S':   /* Zoned decimal */
                strcpy(sql_type, "NUM");
                break;

            case 'T':   /* Time */
                strcpy(sql_type, "TIME");
                break;

            case 'Z':   /* Timestamp */
                strcpy(sql_type, "TIMESTAMP");
                break;

            case '1':   /* BLOB */
                strcpy(sql_type, "BLOB");
                break;

            case '2':   /* CLOB */
                strcpy(sql_type, "CLOB");
                break;

            case '3':   /* DBCLOB */
                strcpy(sql_type, "DBCLOB");
                break;

            case '4':   /* DATALINK */
                strcpy(sql_type, "DATALINK");
                break;

            case '5':   /* Binary character */
                if (IS_VARCHAR=='1')
                    strcpy(sql_type, "VARBINARY");
                else
                    strcpy(sql_type, "BINARY");
                break;

            case '6':   /* Decimal floating point */
                strcpy(sql_type, "DECFLOAT");
                break;

            case '7':   /* XML */
                strcpy(sql_type, "XML");
                break;

            case '8':   /* Boolean - V7R5+ */
                strcpy(sql_type, "BOOLEAN");
                break;

            default:
                *sql_type = '\0';
                *rpg_type = '\0';
                break;
        }


        *outFIELD_BUFFER_LENGTH = pField->Field_Length_Bytes;
        if (IS_VARCHAR == '1') pField->Field_Length_Bytes -= 2;  // subtract 2 for actual length

        // Qp0zLprintf("Initial Data_Type: SQL: %s  RPG: %s  DIGITS: %d\n",
        //                sql_type, rpg_type, pField->Digits);

        if (strlen(rpg_type) > 0)
        {
            strcpy(outRPGDEFN, rpg_type);
        }

        // if (strlen(sql_type) > 0)
        // {
        //     sprintf(outSQLDEFN, "%s", sql_type);
        // }

        if (pField->Digits > 0) // Is Numeric or Float?
        {
            if (strcmp(sql_type, "INT") == 0)
            {
                if (pField->Decimal_Positions > 0)
                {
                    sprintf(outRPGDEFN, "BINDEC(%d : %d)", pField->Digits, pField->Decimal_Positions);
                    sprintf(outSQLDEFN, "DEC(%d : %d)", pField->Digits, pField->Decimal_Positions);
                }
                else
                {   if (pField->Field_Length_Bytes == 2)
                    {
                        strcpy(outRPGDEFN, "INT(5)");
                        strcpy(outSQLDEFN, "SMALLINT");
                    }
                    else if (pField->Field_Length_Bytes == 4)
                    {
                        strcpy(outRPGDEFN, "INT(10)");
                        strcpy(outSQLDEFN, "INT");
                    }
                    else if (pField->Field_Length_Bytes == 8)
                    {
                        strcpy(outRPGDEFN, "INT(20)");
                        strcpy(outSQLDEFN, "BIGINT");
                    }
                    else
                    {
                        sprintf(outRPGDEFN, "INT(%d)", pField->Digits);
                    }
                }

            }
            else if (strcmp(sql_type, "DEC") == 0)
            {
                sprintf(outSQLDEFN, "%s(%d, %d)", sql_type, pField->Digits, pField->Decimal_Positions);
                sprintf(outRPGDEFN, "Packed(%d : %d)", pField->Digits, pField->Decimal_Positions);
            }
            else if (strcmp(sql_type, "NUM") == 0)
            {
                sprintf(outSQLDEFN, "%s(%d, %d)", sql_type, pField->Digits, pField->Decimal_Positions);
                sprintf(outRPGDEFN, "Zoned(%d : %d)", pField->Digits, pField->Decimal_Positions);
            }
            else if (strcmp(sql_type, "FLOAT") == 0 || strcmp(sql_type, "DOUBLE") == 0)
            {
                sprintf(outRPGDEFN, "FLOAT(%d)", pField->Field_Length_Bytes);
            }
            *outFIELD_LENGTH = pField->Digits;
        }
        else
        {
            if (strcmp(sql_type, "GRAPHIC") == 0 || strcmp(sql_type, "VARGRAPHIC") == 0)
            {
                sprintf(outRPGDEFN, "%s(%d)", rpg_type, pField->Number_DBCS_Characters);
                sprintf(outSQLDEFN, "%s(%d)", sql_type,pField->Number_DBCS_Characters);
                *outFIELD_BUFFER_LENGTH = pField->Field_Length_Bytes;
                *indyNUMBER_DBCS_CHARACTERS = 0;
            }
            else if (strcmp(sql_type, "TIMESTAMP") == 0)
            {
                sprintf(outRPGDEFN, "%s(%d)", sql_type,pField->Timestamp_precision);
                sprintf(outSQLDEFN, "%s(%d)", sql_type, pField->Timestamp_precision);
            }
            else if (strcmp(sql_type, "DATE") == 0 || strcmp(sql_type, "TIME") == 0)
            {
                strcpy(outRPGDEFN, sql_type);
                strcpy(outSQLDEFN, sql_type);
            }
            else if (strcmp(sql_type, "CLOB") == 0 || strcmp(sql_type, "BLOB") == 0)
            {
                sprintf(outRPGDEFN, "SQLTYPE(%s:%d)", sql_type,pField->Maximum_Large_Object_Length);
                *outMAXIMUM_LARGE_OBJECT_LENGTH = pField->Maximum_Large_Object_Length;
                *indyMAXIMUM_LARGE_OBJECT_LENGTH = 0;
                *outLARGE_OBJECT_PAD_LENGTH = pField->Large_Object_Pad_Length;
                *indyLARGE_OBJECT_PAD_LENGTH = 0;
                pField->Field_Length_Bytes = pField->Maximum_Large_Object_Length;

            }
            else if (strcmp(sql_type, "DBCLOB") == 0)
            {
                sprintf(outRPGDEFN, "SQLTYPE(%s:%d)", sql_type,pField->Maximum_Large_Object_Length);
                *outMAXIMUM_LARGE_OBJECT_LENGTH = pField->Maximum_Large_Object_Length;
                *indyMAXIMUM_LARGE_OBJECT_LENGTH = 0;
                *outLARGE_OBJECT_PAD_LENGTH = pField->Large_Object_Pad_Length;
                *indyLARGE_OBJECT_PAD_LENGTH = 0;
                pField->Field_Length_Bytes = pField->Maximum_Large_Object_Length;
            }
            else if (strlen(rpg_type)==0)
            {
                sprintf(outRPGDEFN, "%s(%d)", sql_type,  pField->Field_Length_Bytes);
            }
            else
            {
                sprintf(outRPGDEFN, "%s(%d)", rpg_type,  pField->Field_Length_Bytes);
            }

            *outFIELD_LENGTH = pField->Field_Length_Bytes;

        }
        if (strlen(outSQLDEFN)==0)
        {
            sprintf(outSQLDEFN, "%s(%d)", sql_type, pField->Field_Length_Bytes);
        }


        copyUntil(outSQLDATATYPE, outSQLDEFN, 16, "(", true);
        copyUntil(outRPGDATATYPE, outRPGDEFN, 16, "(", true);

        // Qp0zLprintf("Finalized Data_Type: SQL: %s  RPG: %s\n",
        //                outSQLDATATYPE, outRPGDATATYPE);
        *outIS_UPDATEABLE = '0';
        switch (pField->Use)
        {
        case 'I':
            strcpy(outUSAGE, "INPUT");
            break;
        case 'O':
            strcpy(outUSAGE, "OUTPUT");
            break;
        case 'B':
            strcpy(outUSAGE, "BOTH");
            *outIS_UPDATEABLE = '1';
            break;
        default: // 'N' or any other value should set USAGE to NULL
            strcpy(outUSAGE, "NONE");
            // *indyUSAGE = -1; // Set field to NULL
            break;
        }
        if (pField->Digits > 0)
        {
            *outDEC_POS = pField->Decimal_Positions;
        }
        else
        {
            *indyDEC_POS = -1;
        }
        *outOUTPUT_BUFFER_POS = pField->Output_Buffer_Position;
        *outINPUT_BUFFER_POS = pField->Input_Buffer_Position;

        *outCCSID = pField->Field_Data_CCSID;

        copyUntil(outEDIT_CODE, pField->Edit_Code, 2);
        *indyEDIT_CODE = isEmpty(outEDIT_CODE) ? -1 : 0;
        if (pField->Edit_Word_Length > 0)
        {
            if (pField->Field_Edit_Words_CCSID == pScratch->jobCCSID ||
                pField->Field_Edit_Words_CCSID == 0)
            {
                copyUntil(outEDIT_WORD, pField->Edit_Word, pField->Edit_Word_Length);
            }
            else
            {
                if (!edtWord_to_jobCCSID.isOpen() ||
                    (edtWord_to_jobCCSID.getFromCCSID() != pField->Field_Edit_Words_CCSID))
                {
                    if (edtWord_to_jobCCSID.isOpen()) edtWord_to_jobCCSID.close();
                    edtWord_to_jobCCSID.open(pScratch->jobCCSID, pField->Field_Edit_Words_CCSID);
                }
                edtWord_to_jobCCSID.xlate(outEDIT_WORD, 50, pField->Edit_Word, 50);
            }
        }
        *indyEDIT_WORD = isEmpty(outEDIT_WORD) ? -1 : 0;

        copyUntil(outDATE_TIME_FORMAT, pField->Date_Time_Format, 4);
        *indyDATE_TIME_FORMAT = isEmpty(outDATE_TIME_FORMAT) ? -1 : 0;
        *outDATE_TIME_SEP = pField->Date_Time_Separator;
        *indyDATE_TIME_SEP = *outDATE_TIME_SEP==' ' ? -1 : 0;

        // *outFIELD_TEXT_CCSID = pField->Field_Description_CCSID;
        if (pField->Field_Description_CCSID == pScratch->jobCCSID ||
            pField->Field_Description_CCSID == 0)
        {
            copyUntil(outFIELD_TEXT, pField->Field_Text_Description, 50);
        }
        else
        {
            if (!fldText_to_jobCCSID.isOpen() ||
                (fldText_to_jobCCSID.getFromCCSID() != pField->Field_Description_CCSID))
            {
                if (fldText_to_jobCCSID.isOpen()) fldText_to_jobCCSID.close();
                fldText_to_jobCCSID.open(pScratch->jobCCSID, pField->Field_Description_CCSID);
            }
            fldText_to_jobCCSID.xlate(outFIELD_TEXT, 50, pField->Field_Text_Description, 50);
        }



        // *outFIELD_COLUMN_HEADING_CCSID = pField->Field_Column_Heading_CCSID;
        if (pField->Field_Column_Heading_CCSID == pScratch->jobCCSID ||
            pField->Field_Column_Heading_CCSID == 0)
        {
            copyUntil(outCOLUMN_HEADING1, pField->Column_Heading1, 20);
            copyUntil(outCOLUMN_HEADING2, pField->Column_Heading2, 20);
            copyUntil(outCOLUMN_HEADING3, pField->Column_Heading3, 20);
        }
        else
        {
            if (!colHdr_to_jobCCSID.isOpen() ||
                (colHdr_to_jobCCSID.getFromCCSID() != pField->Field_Column_Heading_CCSID))
            {
                if (colHdr_to_jobCCSID.isOpen()) colHdr_to_jobCCSID.close();
                colHdr_to_jobCCSID.open(pScratch->jobCCSID, pField->Field_Column_Heading_CCSID);
            }
            colHdr_to_jobCCSID.xlate(outCOLUMN_HEADING1, 20, pField->Column_Heading1, 20);
            colHdr_to_jobCCSID.xlate(outCOLUMN_HEADING2, 20, pField->Column_Heading2, 20);
            colHdr_to_jobCCSID.xlate(outCOLUMN_HEADING3, 20, pField->Column_Heading3, 20);
        }

        memset(outLONG_COLUMN_NAME,0x00, 255);
        if (pField->Length_Alternate_Field > 0 && pField->Length_Alternate_Field<= 30)
        {
            copyUntil(outLONG_COLUMN_NAME, pField->Alternate_Field_Name, pField->Length_Alternate_Field);
        }
        else if (pField->Length_Alternate_Field > 30)
        {
            char* pLongName = ((char*)pField300) + pField300->Offset_to_Alternate_Field_Name_Long;
            copyUntil(outLONG_COLUMN_NAME, pLongName, pField->Length_Alternate_Field);
        }
        else
        {
            copyUntil(outLONG_COLUMN_NAME, pField->Field_Name, 10);
        }

        // Default Value
        *outHAS_DEFAULT = '0';
        *indyDFTVALUE = -1;
       // outDFTVALUE->length = 0;
        if (pField300->Length_of_DefaultVal > 0)
        {
            const unsigned char* pDefault = (unsigned char*)(pBuffer + pField300->Offset_to_DefaultVal);
          //  outDFTVALUE->length = extractDFT( pDefault,
          //              pField300->Length_of_DefaultVal,
          //              pField->Data_Type,
          //              pField->Digits,
          //              outDFTVALUE->data);
          //  if (outDFTVALUE->length > 0)
          //  {
                *outHAS_DEFAULT = '1';
                *indyDFTVALUE = 0;
                const int MAX_DFTLEN = 2000;
                int maxDftLen = std::min<int>(pField300->Length_of_DefaultVal, MAX_DFTLEN);
                _CPYBYTES(outDFTVALUE, pDefault, maxDftLen);
                outDFTVALUE[maxDftLen] = 0x00;
           // }
        }

        copyUntil(outINTERNAL_FIELD_NAME, pField->Internal_Field_Name, 10);

        if (pField->Number_DBCS_Characters > 0)
        {
            *outNUMBER_DBCS_CHARACTERS = pField->Number_DBCS_Characters;
        }

        *indyROWNBR = (pField->Row_Number < 0) ? -1 : 0;
        *outROWNBR = pField->Row_Number;

        *indyCOLNBR = (pField->Column_Number < 0) ? -1 : 0;
        *outCOLNBR = pField->Column_Number;

        *indyUCS2_DISPLAYED_LENGTH = (pField->UCS2_Displayed_Length == 0) ? -1 : 0;
        *outUCS2_DISPLAYED_LENGTH  =  pField->UCS2_Displayed_Length;

        sprintf(outIS_NULLABLE, "%c", pField->Null_Values_Allowed);
        sprintf(outIS_VARCHAR, "%c", pField->Variable_Length_Field_Ind);
        sprintf(outIS_IDENTITY, "%c", pField->Identity_Column);
        sprintf(outIS_ROWID, "%c", pField->ROWID_Column);
        sprintf(outIS_HIDDEN, "%c", pField->Implicitly_Hidden);
        if (pField->Data_Type == 'Z')
        {
            *outIS_TIMESTAMP = '1';
            *indyTIMESTAMP_PREC = 0; // non-NULL
            *outTIMESTAMP_PREC = pField->Timestamp_precision;
        }
        else
        {
            *outIS_TIMESTAMP = '0';
            *indyTIMESTAMP_PREC = -1;  // NULL
            *outTIMESTAMP_PREC = 0;
        }

        strcpy(outGENWHEN, ((pField->GENERATED_BY== '1') ? "DEFAULT" :
                           (pField->GENERATED_BY == '2') ? "ALWAYS" : "NONE"));

        *indyGENWHEN = (strcmp(outGENWHEN,"NONE")==0) ? -1 : 0;

        switch (pField->Generated_column)
        {
        case 0:
            strcpy(outGENFOR, "NONE");
            *indyGENFOR = -1;
            break;
        case 1:
            strcpy(outGENFOR, "ROWID");
            break;
        case 2:
            strcpy(outGENFOR, "IDENTITY");
            break;
        case 3:
            strcpy(outGENFOR, "ROW CHANGE TIMESTAMP");
            // Row Chg Tmstmp is either always or by default which mearns:
            // when no user-provided value is specified
            strcpy(outGENWHEN, ((pField->Row_Change_Timestamp_Generated_By== '1') ? "DEFAULT" :
                                (pField->Row_Change_Timestamp_Generated_By == '2') ? "ALWAYS" : "NONE"));
            *indyGENWHEN = 0;

            break;
        case 4:
            strcpy(outGENFOR, "ROW-BEGIN");
            break;
        case 5:
            strcpy(outGENFOR, "ROW-END");
            break;
        case 6:
            strcpy(outGENFOR, "TRANS START ID");
            break;
        case 7:
            strcpy(outGENFOR, "EXPRESION");
            break;
        default:
            strcpy(outGENFOR, "NONE");
            *indyGENFOR = -1;
            break;
        }
        copyUntil(outFIELD_PROC_PGMNAME, pField->Field_Procedure_Program_Name);
        copyUntil(outFIELD_PROC_LIBNAME, pField->Field_Procedure_Library_Name);
        *indyFIELD_PROC_PGMNAME = (::triml(outFIELD_PROC_PGMNAME, ' ')==0) ? -1 : 0;
        *indyFIELD_PROC_LIBNAME = (::triml(outFIELD_PROC_LIBNAME, ' ')==0) ? -1 : 0;
        copyUntil(outUDT_NAME, pField->UDT_Name, pField->UDT_Name_Length);
        copyUntil(outUDT_LIBNAME, pField->UDT_Library_Name);
        *indyUDT_NAME = (::triml(outUDT_NAME, ' ')==0) ? -1 : 0;
        *indyUDT_LIBNAME = (::triml(outUDT_LIBNAME, ' ')==0) ? -1 : 0;
    }

    if (*sqlOpCode == SQLUDF_TF_CLOSE)
    {
        coz_DLTOBJ(pScratch->FIELDLIST, "*USRSPC");
        coz_DLTOBJ(pScratch->RCDFMTLIST, "*USRSPC");
    }
    if (*sqlOpCode == SQLUDF_TF_FINAL ||
        *sqlOpCode == SQLUDF_TF_FINAL_CRA)
    {
        // no final call code for now
    }
}

/* ============================================= */
/* copyPad                                   */
/* Copy s to t, blank-padding to padLen bytes.   */
/* ============================================= */
inline void copyPad(
    char *t,
    const char *s,
    int padLen,
    char padChar)
{
    int slen = (s != NULL) ? (int)strlen(s) : 0;
    memset(t, padChar, padLen);
    if (slen > 0)
        _CPYBYTES(t, s,
                  (slen < padLen) ? slen : padLen);
}

/* ============================================= */
/* coz_DLTOBJ                                    */
/* Delete an IBM i object via QLIDLTO API.       */
/* ============================================= */
inline void coz_DLTOBJ(
    const char *qualObjName,
    const char *objType,
    const char *ASPName)
{
    char szObjType[11];
    char szASP[11];
    char rmvMsg[1] = {'1'};
    qusec_t ec;
    if (qualObjName == NULL ||
        qualObjName[0] == ' ' ||
        qualObjName[0] == 0x00)
        return;
    copyPad(szObjType, objType, 10);
    copyPad(szASP, ASPName, 10);
    memset(&ec, 0x00, sizeof(ec));
    QLIDLTO((char *)qualObjName,
            szObjType, szASP,
            rmvMsg, &ec);
}
/* ============================================= */
/* coz_toUpper / coz_toLower                     */
/* In-place EBCDIC case conversion.              */
/* ============================================= */
inline int coz_toUpper(
    char *szData,
    int inLen,
    int ccsid)
{
    Qlg_CCSID_ReqCtlBlk_T frcb;
    qusec_t ec;
    long len = (inLen <= 0)
                   ? (long)strlen(szData)
                   : inLen;
    memset((char *)&frcb, 0x00, sizeof(frcb));
    frcb.Type_of_Request = 1;
    frcb.Case_Request = _TOUPPER;
    frcb.CCSID_of_Input_Data = ccsid;
    memset(&ec, 0x00, sizeof(ec));
    ec.Bytes_Provided = sizeof(ec);
    if (len > 0)
        QlgConvertCase((char *)&frcb,
                       szData, szData,
                       &len, (char *)&ec);
    return (int)len;
}

/* ============================================= */
/* coz_nameUpper                                 */
/* Converts non-quoted IBM i object name to      */
/* upper case in-place.                          */
/* ============================================= */
inline int coz_nameUpper(
    char *szData,
    int inLen)
{
    const char q = '"';
    int len = (inLen <= 0)
                  ? (int)strlen(szData)
                  : inLen;
    if (szData[0] == q)
        return inLen;
    return coz_toUpper(szData, len);
}

inline char *coz_getPtrUsrSpace(
    const char *p2PartUsrSpaceName)
{
    void *pUS = NULL;
    qusec_t ec;
    memset(&ec, 0x00, sizeof(ec));
    ec.Bytes_Provided = sizeof(ec);

    QUSPTRUS((char *)p2PartUsrSpaceName,
             &pUS, &ec);
    if (ec.Bytes_Returned == 0 && pUS != NULL)
    {
        return (char *)pUS;
    }
    return NULL;
}

/* ============================================= */
/* coz_TEMP_USRSPACE                             */
/* Create a temporary *USRSPC in QTEMP.          */
/* Returns a pointer to user-space.              */
/* stores 20-char API name in rtnUSName[20].     */
/* ============================================= */
inline char *coz_TEMP_USRSPACE(
    char *rtnUSName,
    const char *objAttr,
    int initSize)
{
    char qualName[21];
    char extAttr[11];
    char pubAut[11];
    char text[51];
    char repl[11];
    char domain[11];
    char initVal = 0x00;
    char *pUS = NULL;
    qusec_t ec;
    std::string nm;
    size_t pos = 0;
    memset(qualName, ' ', 20);
    qualName[20] = 0x00;
    nm = tmpnam(NULL);
    pos = nm.find_last_of("/\\");
    if (pos != std::string::npos)
        nm.erase(0, pos + 1);
    if (nm.length() > 10)
        nm.resize(10);

    coz_makeAPIObjName(
        qualName, nm.c_str(), "QTEMP");

    coz_nameUpper(qualName, 20);

    copyPad(extAttr, objAttr, 10);
    copyPad(pubAut, "*LIBCRTAUT", 10);
    copyPad(text,"FIELD_LIST UDTF Work Space", 50);

    copyPad(repl, "*NO", 10);
    copyPad(domain, "*DEFAULT", 10);

    memset(&ec, 0x00, sizeof(ec));
    ec.Bytes_Provided = sizeof(ec);

    QUSCRTUS(qualName, extAttr, initSize,
             &initVal, pubAut, text,
             repl, &ec, domain);
    if (ec.Bytes_Returned == 0)
    {
        RMVMSG("*COMP",NULL);   // Swallow the Ownership has changed msg

        /* Enable auto-extend so the space grows   */
        /* automatically when APIs write past the  */
        /* initial size (e.g. QDBRTVFD).           */
        /* key=3 is AutoExtend; key=1=size,        */
        /* key=2=InitChar, key=4=TransferSize.     */
        typedef _Packed struct
        {
            int key;
            int dataLen;
            union
            {
                int size;
                int transferSize;
                char autoExtend;
                char initValue;
            };
        } coz_spaceKey_t;
        typedef _Packed struct
        {
            int count;
            coz_spaceKey_t attr[1];
        } coz_spaceAttr_t;
        coz_spaceAttr_t spaceAttr;
        spaceAttr.count = 1;
        spaceAttr.attr[0].key = 3;
        spaceAttr.attr[0].dataLen = 1;
        spaceAttr.attr[0].autoExtend = '1';
        char rtnLib[10];
        memset(rtnLib, ' ', sizeof(rtnLib));

        memset(&ec, 0x00, sizeof(ec));
        ec.Bytes_Provided = sizeof(ec);

        QUSCUSAT(rtnLib, qualName, &spaceAttr, &ec);
        /* Non-fatal if QUSCUSAT fails \Z space     */
        /* will still work, just won't auto-grow.  */
        memset(&ec, 0x00, sizeof(ec));
        ec.Bytes_Provided = sizeof(ec);
        QUSPTRUS(qualName, (void **)&pUS, &ec);
    }
    if (pUS != NULL && rtnUSName != NULL)
        _CPYBYTES(rtnUSName, qualName, 20);
    return pUS;
}

/* ============================================= */
/* coz_makeAPIObjName                            */
/* Build 20-char IBM i API object name from      */
/* qualified name (LIB/OBJ or OBJ).              */
/* ============================================= */
inline void coz_makeAPIObjName(
    char *objName,
    const char *qualObj,
    const char *dftLib)
{
    char OBJNAME[11];
    char LIBNAME[11];
    memset(objName, ' ', 20);
    memset(OBJNAME, ' ', sizeof(OBJNAME));
    memset(LIBNAME, ' ', sizeof(LIBNAME));
    OBJNAME[10] = 0x00;
    LIBNAME[10] = 0x00;
    const char *slashPos = strchr(qualObj, '/');
    if (slashPos != NULL)
    {
        int len = (int)(slashPos - qualObj);
        int olen = (int)strlen(slashPos + 1);
        if (olen > 10)
            olen = 10;
        if (len > 10)
            len = 10;
        _CPYBYTES(OBJNAME, slashPos + 1, olen);
        _CPYBYTES(LIBNAME, qualObj, len);
    }
    else
    {
        int olen = (int)strlen(qualObj);
        if (olen > 10)
            olen = 10;
        _CPYBYTES(OBJNAME, qualObj, olen);
        if (dftLib != NULL)
        {
            int llen = (int)strlen(dftLib);
            if (llen > 10)
                llen = 10;
            _CPYBYTES(LIBNAME, dftLib, llen);
        }
        else
        {
            _CPYBYTES(LIBNAME, "*LIBL", 5);
        }
    }
    _CPYBYTES(objName, OBJNAME, 10);
    _CPYBYTES(objName + 10, LIBNAME, 10);
}

/* ============================================= */
/* copyUntil                                     */
/* Copy s to t (null-terminating t), stopping at */
/* maxLen, NULL, or any char in stopChars.        */
/* ============================================= */
inline int copyUntil(
                    char *t,
                    const char *s,
                    int maxLen,
                    const char *stopChars,
                    bool bTrim)
{
    int bStop = 0;
    int rtnLen = 0;
    int i = 0;
    if (t == NULL || s == NULL)
        return 0;
#pragma exception_handler(coz_cu_exc,                            \
                          0, 0,                                  \
                          _C2_MH_ESCAPE | _C2_MH_FUNCTION_CHECK, \
                          _CTLA_HANDLE)
    memset(t, ' ', maxLen);
    while (s[i] != 0x00 && i < maxLen)
    {
        if (stopChars != NULL)
        {
            size_t j;
            for (j = 0; j < strlen(stopChars); j++)
            {
                if (s[i] == stopChars[j])
                {
                    bStop = 1;
                }
            }
        }
        if (bStop)
            break;
        t[i] = s[i];
        i++;
    }
    if (bTrim)
    {
        t[i] = 0x00;
        if (i > 0)
            t[::triml(t, ' ')] = 0x00;
        rtnLen = (int)strlen(t);
    }
    else
    {
        rtnLen = i;
    }
#pragma disable_handler
    return rtnLen;
coz_cu_exc:;
    return rtnLen;
}

/* ============================================= */
/* coz_resignalMsg                               */
/* Re-send an IBM i API error as a program msg.  */
/* ============================================= */
void coz_resignalMsg(qusec_t *ec)
{
    char msgfile[21];
    char msgtype[11];
    char msgkey[4];
    char topgmq[11];
    char msgPrefix[3];
    char msgType[3];
    qusec_t fc;

    if (ec->Bytes_Returned == 0)
    {
        return;
    }

    memset((char *)&fc, 0x00, sizeof(fc));
    fc.Bytes_Provided = sizeof(fc);
    memset(msgkey, ' ', sizeof(msgkey));
    memset(msgfile, ' ', sizeof(msgfile));
    memset(topgmq, ' ', sizeof(topgmq));
    copyPad(topgmq, "*", 10);
    _CPYBYTES(msgPrefix, ec->msgid, 2);
    _CPYBYTES(msgType, ec->msgid + 2, 1);
    msgType[1] = 0x00;
    msgPrefix[2] = 0x00;
    if (strcmp(msgPrefix, "CP") == 0)
        coz_makeAPIObjName(msgfile,
                           "QCPFMSG", "*LIBL");
    else if (strcmp(msgPrefix, "RN") == 0)
        coz_makeAPIObjName(msgfile,
                           "QRPGLEMSG", "QDEVTOOLS");
    else if (strcmp(msgPrefix, "HT") == 0)
        coz_makeAPIObjName(msgfile,
                           "QHTTPMSG", "QHTTPSVR");
    else if (strcmp(msgPrefix, "CE") == 0)
        coz_makeAPIObjName(msgfile,
                           "QCEEMSG", "QSYS");
    else if (strcmp(msgPrefix, "GU") == 0)
        coz_makeAPIObjName(msgfile,
                           "QGUIMSG", "QSYS");
    else if (strcmp(msgPrefix, "IW") == 0)
        coz_makeAPIObjName(msgfile,
                           "QIWSMSG", "QSYS");
    else
        coz_makeAPIObjName(msgfile,
                           "QCPFMSG", "*LIBL");
    switch (msgType[0])
    {
    case 'F':
    case 'I':
        copyPad(msgtype, "*INFO", 10);
        break;
    case 'E':
        copyPad(msgtype, "*ESCAPE", 10);
        break;
    case 'D':
        copyPad(msgtype, "*DIAG", 10);
        break;
    case 'C':
        copyPad(msgtype, "*COMP", 10);
        break;
    default:
        copyPad(msgtype, "*INFO", 10);
        break;
    }
    QMHSNDPM(ec->msgid, msgfile,
             ec->msgdata,
             ec->Bytes_Returned - 16,
             msgtype, topgmq, 1,
             msgkey, &fc);
}

// inline int RMVMSG(const char* msgtype, const char* msgkey=NULL);
inline int RMVMSG(const char* msgtype, const char* msgkey)
{
     qusec_t      ec;

     int             callStackCounter = 0;
     char            callStack[10];
     char            msgType[11];
     char            msgKey[4];
     Qmh_Rcvpm_RCVM0100_t   msgInfo;

     memset((char*)callStack,' ',sizeof(callStack));
     memset((char*)msgType,' ',  sizeof(msgType));
     memset((char*)msgKey,0x00,  sizeof(msgKey));
     memset((char*)&msgInfo, 0x00, sizeof(msgInfo));

     callStack[0] ='*';
     callStackCounter = 0;
     if (msgtype != NULL && strlen(msgtype) > 4)
     {
       _CPYBYTES(msgType,msgtype,strlen(msgtype));
     }
     {
       _CPYBYTES(msgType,"*PRVJLMSG ",10);
     }
     if (msgkey != NULL)
     {
       _CPYBYTES(msgKey,msgkey,4);
     }

     QMHRCVPM(&msgInfo, sizeof(msgInfo), QMH_FMT_RCVM0100,
              callStack, callStackCounter, msgType, msgKey, 0,  // wait time is zero
              QMH_MSGACT_SAME, &ec);

     if (ec.Bytes_Returned  > 0)
     {
        return 0;
     }
     if (memcmp(msgInfo.Message_Id,"CPC2206",7)==0)  // Ownership change message?
     {
        QMHRMVPM(callStack, callStackCounter,msgInfo.Message_Key,"*BYKEY    ",&ec);
     }
     return 1;
}

/* ============================================= */
/* coz_getNextParmIf                             */
/* Advance parameter counter and return argv[n]. */
/* If ioFlag>0, memset output parm to 0x00.      */
/* ============================================= */
inline char *coz_getNextParmIf(
    int &pC,
    int &argc,
    char **argv,
    int ioFlag)
{
    char *pRtn = NULL;
    if (argc > pC + 1)
    {
        pRtn = argv[++pC];
        if (ioFlag > 0)
            memset(pRtn, 0x00, ioFlag);
    }
    return pRtn;
}

int getNextFmt(char *rcdfmt, const char *szUserSpace, int index)
{
    char *pBuffer = NULL;
    Qus_Generic_Header_0100_t *pGenHdr = NULL;
    Qdb_Lrcd_Header_t *pListHdr = NULL;
    Qdb_Lrcd_RCDL0100_t *pRcdFmt = NULL;
    qusec_t ec;

    int fmtCount = 0;
    int fmtNbr = 0;
    int fmtSize = 0;
    int listOffset = 0;

    memset(&ec, 0x00, sizeof(ec));
    ec.Bytes_Provided = sizeof(ec);

    QUSPTRUS((char *)szUserSpace, &pBuffer, &ec);
    if (ec.Bytes_Returned > 0 || pBuffer == NULL)
    {
        return -1;
    }

    pGenHdr = (Qus_Generic_Header_0100_t *)pBuffer;
    fmtCount = pGenHdr->Number_List_Entries;
    fmtSize = pGenHdr->Size_Each_Entry;
    if (index >= fmtCount)
    {
        return -1;
    }
    pListHdr = (Qdb_Lrcd_Header_t *)(((unsigned char *)pGenHdr) +
                                     pGenHdr->Offset_Header_Section);

    // Get offset to list of rcdfmt names from start of User Space
    listOffset = pGenHdr->Offset_List_Data;
    // Get ptr to requested record format name
    if (index < 0) // *LAST?
    {
        index = fmtCount - 1;
    }
    pRcdFmt = (Qdb_Lrcd_RCDL0100_t *)(pBuffer + listOffset +
                                      (fmtSize * index));

    copyPad(rcdfmt, pRcdFmt->Format_Name, 10);
    return index + 1; // Increment the row counter
}

fieldData_t *getNextField(const char *szUserSpace, int &index)
{
    char *pBuffer = NULL;
    Qus_Generic_Header_0100_t *pGenHdr = NULL;
    Qdb_Lfld_Header_t *pListHdr = NULL;
    Qdb_Lfld_FLDL0300_t *pField = NULL;
    qusec_t ec;
    int fieldCount = 0;
    int fmtSize = 0;
    int listOffset = 0;
    int fieldOffset = 0;
    static fieldData_t fieldData;
    fieldData.pField = NULL;
    fieldData.pListHdr = NULL;

    memset(&ec, 0x00, sizeof(ec));
    ec.Bytes_Provided = sizeof(ec);

    QUSPTRUS((char *)szUserSpace, &pBuffer, &ec);
    if (ec.Bytes_Returned > 0 || pBuffer == NULL)
    {
        return NULL;
    }

    pGenHdr = (Qus_Generic_Header_0100_t *)pBuffer;
    fieldCount = pGenHdr->Number_List_Entries;
    fmtSize = pGenHdr->Size_Each_Entry;
    if (index >= fieldCount)
    {
        return NULL;
    }

    fieldData.pListHdr = (Qdb_Lfld_Header_t *)(((unsigned char *)pGenHdr) +
                                     pGenHdr->Offset_Header_Section);

    // Get offset to the requested Field name
    listOffset = pGenHdr->Offset_List_Data;
    char *pEntry = (pBuffer + listOffset);
    for (int i = 0; i < index ; i++)
    {
        pField = (Qdb_Lfld_FLDL0300_t*) pEntry;
        pEntry += pField->Length_of_FLDL0300;
    }
    index++;
    fieldData.pField = (Qdb_Lfld_FLDL0300_t*) pEntry;
    return &fieldData;
}

/*
 * Extract the default value returned by QUSLFLD.
 *
 * Character:
 *     7D <EBCDIC data> 7D [00 padding]
 *
 * Graphic:
 *     C7 7D <graphic data> 7D [00 padding]
 *
 * Numeric:
 *     QUSLFLD returns an EBCDIC-oriented representation, but the
 *     exact representation varies by numeric field type.  This
 *     routine currently handles the common forms and leaves the
 *     raw value available for anything not recognized.
 */


static int extractDFT(const unsigned char *defaultPtr,
                    int defaultLen,
                    char dataType,
                    int digits,
                    char *result)
{
    int start = 0;
    int end = defaultLen;
    int i;
    size_t out = 0;

    if (result == NULL) {
        return 0;
    }

    result[0] = '\0';

    if (defaultPtr == NULL || defaultLen <= 0) {
        return 0;
    }
    Qp0zLprintf("Extracting default value\n");
    /*
     * ------------------------------------------------------------
     * Character / VARCHAR
     *
     *     7D <value> 7D 00...
     * ------------------------------------------------------------
     */
    if (dataType == 'A') {

        if (defaultLen >= 2 &&
            defaultPtr[0] == 0x7D) {

            start = 1;

            /*
             * Find the terminating 7D.  Don't simply use
             * defaultLen - 1 because QUSLFLD may return padding.
             */
            end = start;

            while (end < defaultLen &&
                   defaultPtr[end] != 0x7D) {
                end++;
            }

            if (end < defaultLen &&
                defaultPtr[end] == 0x7D) {

                for (i = start; i < end; i++) {
                    result[out++] = defaultPtr[i];
                }

                result[out] = '\0';
                return (int)out;
            }
        }

        return 0;
    }

    /*
     * ------------------------------------------------------------
     * Graphic / VARGRAPHIC
     *
     *     C7 7D <value> 7D 00...
     *
     * C7 appears to identify the graphic/DBCS literal.
     * ------------------------------------------------------------
     */
    if (dataType == 'G') {

        if (defaultLen >= 3 &&
            defaultPtr[0] == 0xC7 &&
            defaultPtr[1] == 0x7D) {

            start = 2;
            end = start;

            while (end < defaultLen &&
                   defaultPtr[end] != 0x7D) {
                end++;
            }

            if (end < defaultLen &&
                defaultPtr[end] == 0x7D) {
                for (i = start; i < end; i++) {
                    result[out++] = defaultPtr[i];
                }

                result[out] = '\0';
                return (int)out;
            }
        }

        return 0;
    }

    /*
     * ------------------------------------------------------------
     * Numeric
     *
     * Digits > 0 identifies numeric fields.
     *
     * QUSLFLD appears to return EBCDIC digits, decimal points,
     * and sign information rather than the native storage format.
     *
     * Examples observed:
     *
     *     0       -> F0 7D
     *     42      -> F4 F2
     *     42.50   -> F4 F2 4B F5 F0
     *
     * We therefore translate the recognizable EBCDIC characters.
     * ------------------------------------------------------------
     */
    if (digits > 0) {

        for (i = 0; i < defaultLen; i++) {

            unsigned char c = defaultPtr[i];

            /*
             * Zero padding terminates the useful data.
             */
            if (c == 0x00) {
                break;
            }

            /*
             * EBCDIC digits F0-F9
             */
            if (c >= 0xF0 && c <= 0xF9) {
                result[out++] = (char)('0' + (c - 0xF0));
                continue;
            }

            /*
             * EBCDIC decimal point
             */
            if (c == 0x4B) {
                result[out++] = '.';
                continue;
            }

            /*
             * EBCDIC minus sign.
             *
             * This is deliberately left as a placeholder until
             * the exact QUSLFLD numeric representation is confirmed.
             */
            if (c == 0x60) {
                result[out++] = '-';
                continue;
            }

            /*
             * Positive sign / zoned-decimal sign byte.
             *
             * 7D is observed for positive zero in zoned numeric
             * defaults.  It isn't part of the displayed number.
             */
            if (c == 0x7D) {
                continue;
            }

            /*
             * If we encounter something unexpected, stop rather
             * than manufacture a potentially incorrect value.
             */
            break;
        }

        result[out] = '\0';
        return (int)out;
    }

    return 0;
}

static void dumpField(Qdb_Lfld_FLDL0100_t* pField)
{
    Qp0zLprintf(
            "Field_Name=%-10.10s "
            "Data_Type=%c "
            "IS_VARLEN=%c "
            "Field_Length_Bytes=%d "
            "Digits=%d "
            "Dec_Pos=%d\n",
            pField->Field_Name,
            pField->Data_Type,
            pField->Variable_Field_Indicator,
            pField->Field_Length_Bytes,
            pField->Digits,
            pField->Decimal_Positions);
}

static int getJobInfo(Qwc_JOBI0400_t* pJobDesc)
{
    char* pJobInfo = NULL;
    char APIFMT[] = "JOBI0400"; // Buffer with CCSID and Default CCSID
    char jobName[26]; // Buffer for time zone abbreviation
    char jobINT[16]; // Buffer for time zone abbreviation
    qusec_t ec;
    memset((char*)&ec,0x00,sizeof(ec));
    ec.Bytes_Provided = sizeof(ec);
    int len = sizeof(Qwc_JOBI0400_t);

    memset(jobName,' ',sizeof(jobName));
    memset(jobINT,' ',sizeof(jobINT));
    jobName[0] = '*';
    pJobInfo = (char*) pJobDesc;

    QUSRJOBI(pJobInfo, len, APIFMT, jobName, jobINT, &ec);
    return (ec.Bytes_Returned == 0) ? 1 : 0;
}

#pragma datamodel(pop)

`;
}
