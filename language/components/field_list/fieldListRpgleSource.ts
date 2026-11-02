/**
 * Embedded source for FIELD_LIST.RPGLE (RPG IV rewrite of the original C++ UDTF program).
 *
 * Source origin: /Users/cozzi/Downloads/projects/open-UDTF/src/FIELD_LIST/FIELD_LIST.RPGLE
 * Stored as plain text (no base64 encoding); uploaded to the IFS and compiled
 * with CRTBNDRPG during component install.
 */
export function getFieldListRPGLESrc(): string {
    return String.raw`**free

// SPDX-License-Identifier: Apache-2.0
// Copyright (c) 2026 by R. Cozzi, Jr.

ctl-opt main(main) dftactgrp(*no) actgrp(*caller) option(*srcstmt);

dcl-c SQL_FETCH 0;
dcl-c SQL_OPEN -1;
dcl-c SQL_CLOSE 1;

// Global joblog flag controlled by the LOG parameter (set during SQL_OPEN).
dcl-s g_logToJoblog ind inz(*off);


dcl-s tinyInt  int(3);
dcl-s smallInt int(5);
dcl-s SQLSMALLINT int(5);
dcl-s SQLINDY     int(5);
dcl-s SQLINT      int(10);
dcl-s SQLBIGINT   int(20);
dcl-s BIGINT      int(20);


dcl-ds QUSEC_t qualified template;
    bytes_Provided int(10) inz(%size(QUSEC_t));
    bytes_Returned int(10) inz;
    msgID     char(7) inz;
    reserved1 char(1) inz(X'00');
    msgData   char(1024) inz;
end-ds;

dcl-pr QUSListRcdFmt extpgm('QUSLRCD');
    UsrSpace char(20) const;
    Format char(8) const;
    FileName char(20) const;
    bOvride ind const;
    api_error likeds(QUSEC_t) options(*varsize:*nopass);
end-pr;

dcl-pr QUSListFields extpgm('QUSLFLD');
    UsrSpace char(20) const;
    Format char(8) const;
    FileName char(20) const;
    RcdFmt char(10) const;
    bOvride ind const;
    api_error likeds(QUSEC_t) options(*varsize:*nopass);
end-pr;

dcl-pr QusCrtUsrSpace extpgm('QUSCRTUS');
    UsrSpace char(20) const;
    ExtAttr char(10) const;
    nSize int(10) const;
    InitValue char(1) const;
    PubAuth char(10) const;
    szTextDesc char(50) const;
    Replace char(10) const;
    api_error likeds(QUSEC_t) options(*varsize:*nopass);
    domain char(10) const options(*nopass);
    transferPages int(10) const options(*NOPASS); // DFT(0) 0-32
    alignSpace    char(1) const options(*NOPASS); // DFT('1')
    autoExtended  char(1) const options(*NOPASS); // DFT('0') '1'=AutoExtend
end-pr;

        // I mostly use this to add Auto-Extend on
        // systems running IBM i prior to V7R4
        // which is when they added the AutoExtended parameter
        // to the QUSCRTUS API. Prior to V7R4 we have to
        // cal QUSCUSAT to set that attribute
dcl-pr QusChgUsrSpaceAttr extpgm('QUSCUSAT');
    RtnLibName char(10);
    UsrSpace char(20) const;
    USAttr char(64) options(*varsize);
    api_error likeds(QUSEC_t) options(*varsize);
end-pr;

dcl-pr QusDltUS extpgm('QUSDLTUS');
    userSpace char(20) const;
    api_error likeds(QUSEC_t) options(*varsize:*nopass);
end-pr;

dcl-pr QUSPTRUS extpgm('QUSPTRUS');
    usrSpace char(20) const;
    pRtnPtrVar pointer;
    api_error likeds(QUSEC_t) options(*varsize:*nopass);
end-pr;

dcl-pr RTVJOBA extpgm('QUSRJOBI');
    rtnJobInfo char(6000) options(*varsize);
    rtnBufferSize int(10) const;
    apiFormat char(8) const;
    Job char(26) const;
    InternalJobID char(16) const;
    apiError likeds(QUSEC_t) options(*varsize);
end-pr;

dcl-pr QLIDLTO extpgm('QLIDLTO');
    QualObj char(20) const;
    ObjType char(10) const;
    ASP char(10) const;
    rmvMessages char(1) const;
    apiError likeds(QUSEC_t) options(*varsize);
end-pr;

dcl-pr CEEGPID  OPDESC EXTPROC('CEEGPID') ;
    rtnVerRel      Int(10);
    rtnPlatform    Int(10);
    feedback       Char(12)   OPTIONS(*OMIT);
end-pr;

dcl-ds Qwc_JOBI0400_t qualified template;
    Bytes_Return int(10);
    Bytes_Avail int(10);
    Job_Name char(10);
    User_Name char(10);
    Job_Number char(6);
    Int_Job_ID char(16);
    Job_Status char(10);
    Job_Type char(1);
    Job_Subtype char(1);
    Reserved_001 char(240);
    CCSID int(10);
    Default_Coded_Char_Set_Id int(10);
end-ds;

            // Converted from: <QSYSINC/H/QUSGEN>
dcl-ds Qus_Generic_Header_0100_T  Qualified Inz TEMPLATE;
    User_Area CHAR(64);
    Size_Generic_Header INT(10);
    Structure_Release_Level CHAR(4);
    Format_Name CHAR(8);
    Api_Used CHAR(10);
    Date_Time_Created CHAR(13);
    Information_Status CHAR(1);
    Size_User_Space INT(10);
    Offset_Input_Parameter INT(10);
    Size_Input_Parameter INT(10);
    Offset_Header_Section INT(10);
    Size_Header_Section INT(10);
    Offset_List_Data INT(10);
    Size_List_Data INT(10);
    Number_List_Entries INT(10);
    Size_Each_Entry INT(10);
    CCSID_List_Ent INT(10);
    Country_ID CHAR(2);
    Language_ID CHAR(3);
    Subset_List_Indicator CHAR(1);
    Reserved CHAR(42);
end-ds;  // Qus_Generic_Header_0100_T

    // Converted from: <QSYSINC/H/QUSLFLD>
dcl-ds Qdb_Lfld_Header_T  Qualified Inz TEMPLATE;
    File_Name_Used CHAR(10);
    Library_Name CHAR(10);
    File_Type CHAR(10);
    Record_Format_Name CHAR(10);
    Record_Length INT(10);
    Record_Format_Id CHAR(13);
    Record_Text_Description CHAR(50);
    Reserved CHAR(1);
    Record_Description_CCSID INT(10);
    Variable_Length_Fields CHAR(1);
    Graphic_Fields_Indicator CHAR(1);
    Date_Time_Fields_Indicator CHAR(1);
    Null_Capable_Fields_Indicator CHAR(1);
end-ds;  // Qdb_Lfld_Header_T

            // Converted from: <QSYSINC/H/QUSLRCD>
dcl-ds Qdb_Lrcd_Header_T  Qualified Inz TEMPLATE;
    File_Name_Used CHAR(10);
    Library_Name CHAR(10);
    File_Type CHAR(10);
    File_Text_Description CHAR(50);
    File_Description_CCSID_H INT(10);
    File_Creation_Date CHAR(13);
end-ds;  // Qdb_Lrcd_Header_T

            // Converted from: <QSYSINC/H/QUSLRCD>
dcl-ds Qdb_Lrcd_RCDL0100_T  Qualified Inz TEMPLATE;
    Format_Name CHAR(10);
end-ds;  // Qdb_Lrcd_RCDL0100_T

dcl-ds rcdFormat Qualified Inz;
    genHdr  likeDs(Qus_Generic_Header_0100_T) inz;
    apiHdr  likeDs(Qdb_Lfld_Header_T) inz;
    buffer  char(2048);
end-ds;

            // Converted from: <QSYSINC/H/QUSLFLD>
dcl-ds Qdb_Lfld_FLDL0100_T  Qualified Inz TEMPLATE;
    Field_Name CHAR(10);
    Data_Type CHAR(1);
    Use CHAR(1);
    Output_Buffer_Position INT(10);
    Input_Buffer_Position INT(10);
    Field_Length_Bytes INT(10);
    Digits INT(10);
    Decimal_Positions INT(10);
    Field_Text_Description CHAR(50);
    Edit_Code CHAR(2);
    Edit_Word_Length INT(10);
    Edit_Word CHAR(64);
    Column_Heading1 CHAR(20);
    Column_Heading2 CHAR(20);
    Column_Heading3 CHAR(20);
    Internal_Field_Name CHAR(10);
    Alternate_Field_Name CHAR(30);
    Length_Alternate_Field INT(10);
    Number_DBCS_Characters INT(10);
    Null_Values_Allowed CHAR(1);
    Variable_Field_Indicator CHAR(1);
    Date_Time_Format CHAR(4);
    Date_Time_Separator CHAR(1);
    Variable_Length_Field_Ind CHAR(1);
    Field_Description_CCSID INT(10);
    Field_Data_CCSID INT(10);
    Field_Column_Heading_CCSID INT(10);
    Field_Edit_Words_CCSID INT(10);
    UCS2_Displayed_Length INT(10);
    Field_Data_Encoding_Scheme INT(10);
    Maximum_Large_Object_Length INT(10);
    Large_Object_Pad_Length INT(10);
    UDT_Name_Length INT(10);
    UDT_Name CHAR(128);
    UDT_Library_Name CHAR(10);
    Datalink_Control CHAR(1);
    Datalink_Integrity CHAR(1);
    Datalink_Read_Permission CHAR(1);
    Datalink_Write_Permission CHAR(1);
    Datalink_Recovery CHAR(1);
    Datalink_Unlink_Control CHAR(1);
    Row_Number INT(10);
    Column_Number INT(10);
    ROWID_Column CHAR(1);
    Identity_Column CHAR(1);
    GENERATED_BY CHAR(1);
    IC_Cycle CHAR(1);
    IC_Orig_Start_With Packed(31 : 0);
    IC_Curr_Start_With Packed(31 : 0);
    IC_Increment_By INT(10);
    IC_MINVALUE Packed(31 : 0);
    IC_MAXVALUE Packed(31 : 0);
    IC_CACHE UNS(10);
    IC_ORDER CHAR(1);
    Implicitly_Hidden CHAR(1);
    Row_Change_Timestamp_Generated_By CHAR(1);
    Field_Procedure_Program_Name CHAR(10);
    Field_Procedure_Library_Name CHAR(10);
    Timestamp_precision UNS(5);
    Generated_column CHAR(1);
    Reserved CHAR(2);
end-ds;  // Qdb_Lfld_FLDL0100_T


dcl-ds fieldList Qualified based(pFieldList);
    genHdr  likeDs(Qus_Generic_Header_0100_T);
    apiHdr  likeDs(Qdb_Lrcd_Header_T);
end-ds;

dcl-ds fieldInfo LikeDS(Qdb_Lfld_FLDL0100_T) based(pField);


// RPG equivalent of C++ fieldData_t
dcl-ds fieldData_t qualified template;
    pListHdr pointer;
    pField pointer;
end-ds;

// Minimal FLDL0300 layout needed for list walking.
dcl-ds Qdb_Lfld_FLDL0300_t qualified template;
    Length_of_FLDL0300 int(10);
    Offset_to_Qdb_Lfld_FLDL0100 int(10);
    Offset_to_Alternate_Field_Name_Long int(10);
    Offset_to_DefaultVal int(10);
    Length_of_DefaultVal int(10);
end-ds;

dcl-ds scratch_t qualified template;
    length int(10);
    eof int(10);
    IBMi_vrm   int(10);  // VxRyMz as VRM (numeric value, e.g., 730)
    onlyOneFmt int(10);
    usedSpecific int(10);
    nextFmt int(10);
    nextField int(10);
    fieldCount int(10);
    seqNbr int(10);
    useOverride ind;
    qualFile char(20);
    rcdFmt char(10);
    curFmt char(10);
    fmtUS char(20);
    fldUS char(20);
    jobCCSID int(10);
end-ds;

dcl-proc main;
    dcl-pi main extpgm('FIELD_LIST');

        inLIBRARY_NAME varchar(128) const;
        inFILE_NAME varchar(128) const;
        inRCDFMT varchar(10) const;
        inOVR varchar(10) const;
        inLOG varchar(10) const;


        outLibrary_name varchar(10);
        outFile_name varchar(10);
        outFILE_TYPE varchar(10);
        outRCDFMT varchar(10);
        outCOLUMN_COUNT int(10);
        outORDINAL_POSITION int(10);

        outCOLUMN_NAME varchar(10);
        outSQL_DATA_TYPE varchar(16);
        outRPG_DATA_TYPE varchar(16);
        outCOLUMN_LENGTH int(10);
        outDATA_TYPE char(1);
        outDEC_POS int(10);
        outBUFFER_LENGTH int(10);
        outINPUT_BUFFER_POS int(10);
        outOUTPUT_BUFFER_POS int(10);
        outUSAGE varchar(10);
        outCOLUMN_CCSID int(10);
        outSQL_DEFN varchar(64);
        outRPG_DEFN varchar(64);
        outCOLHDG1 varchar(20);
        outCOLHDG2 varchar(20);
        outCOLHDG3 varchar(20);
        outINTERNAL_NAME varchar(10);
        outLONG_NAME varchar(255);
        outCOLUMN_TEXT varchar(50);
        outRCDFMT_TEXT varchar(50);
        outEDIT_CODE varchar(2);
        outEDIT_WORD varchar(64);
        outDATE_TIME_FMT varchar(4);
        outDATE_TIME_SEP char(1);
        outUCS2_DSP_LENGTH int(10);
        outDBCS_CHARS int(10);
        outLOB_MAX_LENGTH int(10);
        outLOB_PAD_LENGTH int(10);
        outUDT_NAME varchar(128);
        outUDT_LIBNAME varchar(10);
        outDSP_ROW_NBR int(10);
        outDSP_COL_NBR int(10);
        outIS_NULLABLE char(1);
        outIS_UPDATEABLE char(1);
        outIS_VARCHAR char(1);
        outIS_HIDDEN char(1);
        outIS_ROWID char(1);
        outIS_IDENTITY char(1);
        outIS_TIMESTAMP char(1);
        outHAS_DEFAULT char(1);
        outGENERATED_WHEN char(10);
        outGENERATED_FOR varchar(32);
        outPROC_PGMNAME varchar(10);
        outPROC_LIBNAME varchar(10);
        outTIMESTAMP_PRECISION int(5);
        outDEFAULT_VALUE varchar(2000);

        indy_inLIBRARY_NAME like(SQLINDY);
        indy_inFILE_NAME like(SQLINDY);
        indy_inRCDFMT like(SQLINDY);
        indy_inOVR like(SQLINDY);
        indy_inLOG like(SQLINDY);

        indy_outLibrary_name like(SQLINDY);
        indy_outFile_name like(SQLINDY);
        indy_outFILE_TYPE like(SQLINDY);
        indy_outRCDFMT like(SQLINDY);
        indy_outCOLUMN_COUNT like(SQLINDY);
        indy_outORDINAL_POSITION like(SQLINDY);
        indy_outCOLUMN_NAME like(SQLINDY);
        indy_outSQL_DATA_TYPE like(SQLINDY);
        indy_outRPG_DATA_TYPE like(SQLINDY);
        indy_outCOLUMN_LENGTH like(SQLINDY);
        indy_outDATA_TYPE like(SQLINDY);
        indy_outDEC_POS like(SQLINDY);
        indy_outBUFFER_LENGTH like(SQLINDY);
        indy_outINPUT_BUFFER_POS like(SQLINDY);
        indy_outOUTPUT_BUFFER_POS like(SQLINDY);
        indy_outUSAGE like(SQLINDY);
        indy_outCOLUMN_CCSID like(SQLINDY);
        indy_outSQL_DEFN like(SQLINDY);
        indy_outRPG_DEFN like(SQLINDY);
        indy_outCOLHDG1 like(SQLINDY);
        indy_outCOLHDG2 like(SQLINDY);
        indy_outCOLHDG3 like(SQLINDY);
        indy_outINTERNAL_NAME like(SQLINDY);
        indy_outLONG_NAME like(SQLINDY);
        indy_outCOLUMN_TEXT like(SQLINDY);
        indy_outRCDFMT_TEXT like(SQLINDY);
        indy_outEDIT_CODE like(SQLINDY);
        indy_outEDIT_WORD like(SQLINDY);
        indy_outDATE_TIME_FMT like(SQLINDY);
        indy_outDATE_TIME_SEP like(SQLINDY);
        indy_outUCS2_DSP_LENGTH like(SQLINDY);
        indy_outDBCS_CHARS like(SQLINDY);
        indy_outLOB_MAX_LENGTH like(SQLINDY);
        indy_outLOB_PAD_LENGTH like(SQLINDY);
        indy_outUDT_NAME like(SQLINDY);
        indy_outUDT_LIBNAME like(SQLINDY);
        indy_outDSP_ROW_NBR like(SQLINDY);
        indy_outDSP_COL_NBR like(SQLINDY);
        indy_outIS_NULLABLE like(SQLINDY);
        indy_outIS_UPDATEABLE like(SQLINDY);
        indy_outIS_VARCHAR like(SQLINDY);
        indy_outIS_HIDDEN like(SQLINDY);
        indy_outIS_ROWID like(SQLINDY);
        indy_outIS_IDENTITY like(SQLINDY);
        indy_outIS_TIMESTAMP like(SQLINDY);
        indy_outHAS_DEFAULT like(SQLINDY);
        indy_outGENERATED_WHEN like(SQLINDY);
        indy_outGENERATED_FOR like(SQLINDY);
        indy_outPROC_PGMNAME like(SQLINDY);
        indy_outPROC_LIBNAME like(SQLINDY);
        indy_outTIMESTAMP_PRECISION like(SQLINDY);
        indy_outDEFAULT_VALUE like(SQLINDY);

        outSQLSTATE char(5);
        inFuncName varchar(517) const;
        inSpecName varchar(128) const;
        outSQLMSG varchar(70);
        scratchPad likeds(scratch_t);
        inSQLOpCode like(SQLINT) const;
    end-pi;

    dcl-s saveLen int(10);
    dcl-ds ec likeds(QUSEC_t) inz(*likeds);
    dcl-ds jobInfo likeds(Qwc_JOBI0400_t);
    dcl-s pFieldData pointer;
    dcl-ds fieldData likeds(fieldData_t) based(pFieldData);
    dcl-s pHdr pointer;
    dcl-ds fldHdr likeds(Qdb_Lfld_Header_t) based(pHdr);
    dcl-s pFld300 pointer;
    dcl-ds fld300 likeds(Qdb_Lfld_FLDL0300_t) based(pFld300);
    dcl-s pFld0100 pointer;
    dcl-ds fld likeDS(Qdb_Lfld_FLDL0100_t) based(pFld0100);
    dcl-s pFldUs pointer;
    dcl-ds fldUsHdr likeds(Qus_Generic_Header_0100_t) based(pFldUs);

    dcl-s pRaw pointer;
    dcl-ds rawData Qualified based(pRaw);
        bytes char(32767);
    end-ds;

    dcl-s SQL_DATATYPE varchar(16);
    dcl-s RPG_DATATYPE varchar(16);
    dcl-s workSql varchar(64);
    dcl-s workRpg varchar(64);
    dcl-s baseType varchar(16);
    dcl-s p int(10);
    dcl-s effLen int(10);
    dcl-s maxDftLen int(10);

    if (inSQLOpCode = SQL_OPEN);

        saveLen = scratchPad.length;
        clear scratchPad;
        scratchPad.length = saveLen;

        if (indy_inFILE_NAME < 0 or %trim(inFILE_NAME) = '');
            outSQLSTATE = '38701';
            outSQLMSG = 'FILE_NAME is required';
            return;
        endif;

            // convert the file and library name to "short" "system" object names
        scratchPad.qualFile = getShortName( inFILE_NAME : inLIBRARY_NAME );

        if (indy_inRCDFMT >= 0 and %trim(inRCDFMT) <> '');
            scratchPad.rcdFmt = %UPPER(inRCDFMT);
        else;
            scratchPad.rcdFmt = '*ALL';
        endif;

        scratchPad.useOverride = parseYesNo(inOVR : indy_inOVR);

        // LOG parameter gates informational joblog messages.
        g_logToJoblog = parseYesNo(inLOG : indy_inLOG);


        if (getJobInfo(jobInfo));
            scratchPad.jobCCSID = jobInfo.Default_Coded_Char_Set_Id;
        endif;

        if (createTempUserSpace(scratchPad.fmtUs : 'CODE4I') = *off);
            outSQLSTATE = '38701';
            outSQLMSG = 'Unable to create RCDFMT user space';
            return;
        endif;
        if (createTempUserSpace(scratchPad.fldUs : 'CODE4I') = *off);
            outSQLSTATE = '38701';
            outSQLMSG = 'Unable to create FIELD user space';
            return;
        endif;

        if (%subst(scratchPad.rcdFmt : 1 : 1) = '*');
            reset ec;
            //  Create List of Record Format names
            QUSListRcdFmt(
                scratchPad.fmtUs :
                'RCDL0100' :
                scratchPad.qualFile :
                scratchPad.useOverride :
                ec
            );
            if (ec.bytes_Returned > 0);
                outSQLSTATE = '38701';
                outSQLMSG = 'QUSLRCD failed: ' + ec.msgID;
                return;
            endif;
            if (scratchPad.rcdFmt = '*LAST');
                scratchPad.nextFmt = -1;
            endif;
            if (scratchPad.rcdFmt = '*FIRST' or scratchPad.rcdFmt = '*ONLY');
                scratchPad.onlyOneFmt = 1;
            endif;
        else;
            scratchPad.onlyOneFmt = 1;
            scratchPad.curFmt = scratchPad.rcdFmt;
        endif;

        scratchPad.eof = 0;

    elseif (inSQLOpCode = SQL_FETCH);
        if (scratchPad.eof = 1);
            outSQLSTATE = '02000';
            return;
        endif;

        dow (*on);
            // Load/reload field list when this is the first field in a format.
            if (scratchPad.nextField = 0 and
                (scratchPad.onlyOneFmt <> 1 or scratchPad.fieldCount = 0));
                if (scratchPad.rcdFmt = '*LAST');
                    scratchPad.nextFmt = -1;
                endif;
                if (scratchPad.rcdFmt = '*FIRST' or scratchPad.rcdFmt = '*ONLY');
                    scratchPad.nextFmt = 0;
                endif;
                if (scratchPad.rcdFmt <> '*ALL');
                    scratchPad.onlyOneFmt = 1;
                endif;

                if (%subst(scratchPad.rcdFmt : 1 : 1) = '*');
                    scratchPad.nextFmt = getNextFmt(scratchPad.curFmt :
                                                    scratchPad.fmtUs :
                                                    scratchPad.nextFmt);
                    if (scratchPad.nextFmt < 0);
                        scratchPad.eof = 1;
                        outSQLSTATE = '02000';
                        return;
                    endif;
                else;
                    scratchPad.curFmt = scratchPad.rcdFmt;
                endif;

                reset ec;
                QUSListFields(
                    scratchPad.fldUs :
                    'FLDL0300' :
                    scratchPad.qualFile :
                    scratchPad.curFmt :
                    scratchPad.useOverride :
                    ec
                );
                if (ec.bytes_Returned > 0);
                    scratchPad.eof = 1;
                    outSQLSTATE = '02000';
                    outSQLMSG = 'QUSLFLD failed: ' + ec.msgID;
                    return;
                endif;

                pFldUs = getPtrUsrSpace(scratchPad.fldUs);
                if (pFldUs = *null);
                    scratchPad.eof = 1;
                    outSQLSTATE = '02000';
                    return;
                endif;
                scratchPad.fieldCount = fldUsHdr.Number_List_Entries;
            endif;

            pFieldData = getNextField(scratchPad.fldUs : scratchPad.nextField);
            if (pFieldData = *null);
                if (scratchPad.onlyOneFmt <> 1);
                    scratchPad.nextField = 0;
                    iter;
                endif;
                scratchPad.eof = 1;
                outSQLSTATE = '02000';
                return;
            endif;
            leave;
        enddo;

        pHdr = fieldData.pListHdr;
        pFld300 = fieldData.pField;
        pFld0100 = pFld300 + fld300.Offset_to_Qdb_Lfld_FLDL0100;

        outFile_name = %trimr(fldHdr.File_Name_Used);
        outLibrary_name = %trimr(fldHdr.Library_Name);
        outFILE_TYPE = %trimr(fldHdr.File_Type);
        outRCDFMT = %trimr(fldHdr.Record_Format_Name);
        outRCDFMT_TEXT = %trimr(fldHdr.Record_Text_Description);
        outCOLUMN_COUNT = scratchPad.fieldCount;
        outORDINAL_POSITION = scratchPad.nextField;

        outCOLUMN_NAME = %trimr(fld.Field_Name);
        outDATA_TYPE = fld.Data_Type;

        SQL_DATATYPE = '';
        RPG_DATATYPE = '';
        workSql = '';
        workRpg = '';
        effLen = fld.Field_Length_Bytes;

        indy_outDBCS_CHARS = -1;
        indy_outLOB_MAX_LENGTH = -1;
        indy_outLOB_PAD_LENGTH = -1;
        indy_outDEC_POS = -1;
        indy_outTIMESTAMP_PRECISION = -1;
        indy_outDEFAULT_VALUE = -1;
        indy_outGENERATED_WHEN = -1;
        indy_outGENERATED_FOR = -1;

        select;
            when (fld.Data_Type in %list('D':'E':'I':'M':'N':'O':'X':'Y'));
                SQL_DATATYPE = 'CHAR';
            when (fld.Data_Type in %list('A':'H'));
                if (fld.Variable_Length_Field_Ind = '1');
                    SQL_DATATYPE = 'VARCHAR';
                else;
                    SQL_DATATYPE = 'CHAR';
                endif;
            when (fld.Data_Type = 'B');
                SQL_DATATYPE = 'INT';
            when (fld.Data_Type = 'F');
                RPG_DATATYPE = 'FLOAT';
                if (effLen = 4);
                    SQL_DATATYPE = 'FLOAT';
                elseif (effLen = 8);
                    SQL_DATATYPE = 'DOUBLE';
                endif;
            when (fld.Data_Type in %list('G':'J':'W'));
                indy_outDBCS_CHARS = 0;
                if (fld.Variable_Length_Field_Ind = '1');
                    SQL_DATATYPE = 'VARGRAPHIC';
                    RPG_DATATYPE = 'VARGRAPH';
                else;
                    SQL_DATATYPE = 'GRAPHIC';
                    RPG_DATATYPE = 'GRAPH';
                endif;
            when (fld.Data_Type = 'L');
                SQL_DATATYPE = 'DATE';
            when (fld.Data_Type = 'P');
                SQL_DATATYPE = 'DEC';
            when (fld.Data_Type = 'S');
                SQL_DATATYPE = 'NUM';
            when (fld.Data_Type = 'T');
                SQL_DATATYPE = 'TIME';
            when (fld.Data_Type = 'Z');
                SQL_DATATYPE = 'TIMESTAMP';
            when (fld.Data_Type = '1');
                SQL_DATATYPE = 'BLOB';
            when (fld.Data_Type = '2');
                SQL_DATATYPE = 'CLOB';
            when (fld.Data_Type = '3');
                SQL_DATATYPE = 'DBCLOB';
            when (fld.Data_Type = '4');
                SQL_DATATYPE = 'DATALINK';
            when (fld.Data_Type = '5');
                if (fld.Variable_Length_Field_Ind = '1');
                    SQL_DATATYPE = 'VARBINARY';
                else;
                    SQL_DATATYPE = 'BINARY';
                endif;
            when (fld.Data_Type = '6');
                SQL_DATATYPE = 'DECFLOAT';
            when (fld.Data_Type = '7');
                SQL_DATATYPE = 'XML';
            when (fld.Data_Type = '8');
                SQL_DATATYPE = 'BOOLEAN';
            other;
                SQL_DATATYPE = '';
                RPG_DATATYPE = '';
        endsl;

        outBUFFER_LENGTH = fld.Field_Length_Bytes;
        if (fld.Variable_Length_Field_Ind = '1');
            effLen -= 2;
        endif;

        if (fld.Digits > 0);
            if (SQL_DATATYPE = 'INT');
                if (fld.Decimal_Positions > 0);
                    workRpg = 'BINDEC(' + %char(fld.Digits) + ' : ' +
                                    %char(fld.Decimal_Positions) + ')';
                    workSql = 'DEC(' + %char(fld.Digits) + ' : ' +
                                    %char(fld.Decimal_Positions) + ')';
                else;
                    if (effLen = 2);
                        workRpg = 'INT(5)';
                        workSql = 'SMALLINT';
                    elseif (effLen = 4);
                        workRpg = 'INT(10)';
                        workSql = 'INT';
                    elseif (effLen = 8);
                        workRpg = 'INT(20)';
                        workSql = 'BIGINT';
                    else;
                        workRpg = 'INT(' + %char(fld.Digits) + ')';
                        workSql = 'INT(' + %char(fld.Digits) + ')';
                    endif;
                endif;
            elseif (SQL_DATATYPE = 'DEC');
                workSql = SQL_DATATYPE + '(' + %char(fld.Digits) + ', ' +
                                          %char(fld.Decimal_Positions) + ')';
                workRpg = 'PACKED(' + %char(fld.Digits) + ' : ' +
                                        %char(fld.Decimal_Positions) + ')';
            elseif (SQL_DATATYPE = 'NUM');
                workSql = SQL_DATATYPE + '(' + %char(fld.Digits) + ', ' +
                                            %char(fld.Decimal_Positions) + ')';
                workRpg = 'ZONED(' + %char(fld.Digits) + ' : ' +
                                            %char(fld.Decimal_Positions) + ')';
            elseif (SQL_DATATYPE = 'FLOAT' or SQL_DATATYPE = 'DOUBLE');
                workRpg = 'FLOAT(' + %char(effLen) + ')';
            endif;
            outCOLUMN_LENGTH = fld.Digits;
            outDEC_POS = fld.Decimal_Positions;
            indy_outDEC_POS = 0;
        else;
            if (SQL_DATATYPE = 'GRAPHIC' or SQL_DATATYPE = 'VARGRAPHIC');
                workRpg = RPG_DATATYPE + '(' + %char(fld.Number_DBCS_Characters) +
                                    ')';
                workSql = SQL_DATATYPE + '(' + %char(fld.Number_DBCS_Characters) +
                                    ')';
                outBUFFER_LENGTH = fld.Field_Length_Bytes;
                indy_outDBCS_CHARS = 0;
            elseif (SQL_DATATYPE = 'TIMESTAMP');
                workRpg = SQL_DATATYPE + '(' + %char(fld.Timestamp_precision) +
                                    ')';
                workSql = SQL_DATATYPE + '(' + %char(fld.Timestamp_precision) +
                                    ')';
            elseif (SQL_DATATYPE = 'DATE' or SQL_DATATYPE = 'TIME');
                workRpg = SQL_DATATYPE;
                workSql = SQL_DATATYPE;
            elseif (SQL_DATATYPE = 'CLOB' or
                    SQL_DATATYPE = 'BLOB' or
                    SQL_DATATYPE = 'DBCLOB');
                workRpg = 'SQL_DATATYPE(' + SQL_DATATYPE + ':' +
                                %char(fld.Maximum_Large_Object_Length) +
                                 ')';
                outLOB_MAX_LENGTH = fld.Maximum_Large_Object_Length;
                indy_outLOB_MAX_LENGTH = 0;
                outLOB_PAD_LENGTH = fld.Large_Object_Pad_Length;
                indy_outLOB_PAD_LENGTH = 0;
                effLen = fld.Maximum_Large_Object_Length;
            elseif (RPG_DATATYPE = '');
                workRpg = SQL_DATATYPE + '(' + %char(effLen) + ')';
            else;
                workRpg = RPG_DATATYPE + '(' + %char(effLen) + ')';
            endif;
            outCOLUMN_LENGTH = effLen;
        endif;

        if (%trim(workSql) = '');
            workSql = SQL_DATATYPE + '(' + %char(effLen) + ')';
        endif;
        if (%trim(workRpg) = '');
            workRpg = workSql;
        endif;

        outSQL_DEFN = workSql;
        outRPG_DEFN = workRpg;

        p = %scan('(' : workSql);
        if (p > 0);
            baseType = %trim(%subst(workSql : 1 : p - 1));
        else;
            baseType = %trim(workSql);
        endif;
        outSQL_DATA_TYPE = baseType;

        p = %scan('(' : workRpg);
        if (p > 0);
            baseType = %trim(%subst(workRpg : 1 : p - 1));
        else;
            baseType = %trim(workRpg);
        endif;
        outRPG_DATA_TYPE = baseType;

        select;
            when (fld.Use = 'I');
                outUSAGE = 'INPUT';
            when (fld.Use = 'O');
                outUSAGE = 'OUTPUT';
            when (fld.Use = 'B');
                outUSAGE = 'BOTH';
                outIS_UPDATEABLE = '1';
            other;
                outUSAGE = 'NONE';
        endsl;
        if (fld.Use <> 'B');
            outIS_UPDATEABLE = '0';
        endif;

        outOUTPUT_BUFFER_POS = fld.Output_Buffer_Position;
        outINPUT_BUFFER_POS = fld.Input_Buffer_Position;
        outCOLUMN_CCSID = fld.Field_Data_CCSID;

        outEDIT_CODE = %trimr(fld.Edit_Code);
        if (%Len(%TRIMR(outEDIT_CODE))>0);
            indy_outEDIT_CODE = 0;
        else;
            indy_outEDIT_CODE = -1;
        endif;
        if (%trim(outEDIT_CODE) = '');
            indy_outEDIT_CODE = -1;
        else;
            indy_outEDIT_CODE = 0;
        endif;
        if (fld.Edit_Word_Length > 0);
            outEDIT_WORD = %trimr(%subst(fld.Edit_Word : 1 :
                                         fld.Edit_Word_Length));
        else;
            outEDIT_WORD = '';
        endif;
        if (%trim(outEDIT_WORD) = '');
            indy_outEDIT_WORD = -1;
        else;
            indy_outEDIT_WORD = 0;
        endif;

        outDATE_TIME_FMT = %trimr(fld.Date_Time_Format);
        if (%trim(outDATE_TIME_FMT) = '');
            indy_outDATE_TIME_FMT = -1;
        else;
            indy_outDATE_TIME_FMT = 0;
        endif;
        outDATE_TIME_SEP = fld.Date_Time_Separator;
        if (outDATE_TIME_SEP = ' ');
            indy_outDATE_TIME_SEP = -1;
        else;
            indy_outDATE_TIME_SEP = 0;
        endif;

        outCOLUMN_TEXT = %trimr(fld.Field_Text_Description);
        outCOLHDG1 = %trimr(fld.Column_Heading1);
        outCOLHDG2 = %trimr(fld.Column_Heading2);
        outCOLHDG3 = %trimr(fld.Column_Heading3);

        if (fld.Length_Alternate_Field > 0 and
            fld.Length_Alternate_Field <= 30);
            outLONG_NAME = %trimr(%subst(fld.Alternate_Field_Name : 1 :
                                         fld.Length_Alternate_Field));
        elseif (fld.Length_Alternate_Field > 30);
            pRaw = pFld300 + fld300.Offset_to_Alternate_Field_Name_Long;
            outLONG_NAME = %trimr(%subst(rawData.bytes : 1 :
                                         fld.Length_Alternate_Field));
        else;
            outLONG_NAME = %trimr(fld.Field_Name);
        endif;

        outHAS_DEFAULT = '0';
        outDEFAULT_VALUE = '';
        if (fld300.Length_of_DefaultVal > 0);
            maxDftLen = %int(%min(fld300.Length_of_DefaultVal : 2000));
            pRaw = pFld300 + fld300.Offset_to_DefaultVal;
            outDEFAULT_VALUE = %subst(rawData.bytes : 1 : maxDftLen);
            outHAS_DEFAULT = '1';
            indy_outDEFAULT_VALUE = 0;
        endif;

        outINTERNAL_NAME = %trimr(fld.Internal_Field_Name);
        if (fld.Number_DBCS_Characters > 0);
            outDBCS_CHARS = fld.Number_DBCS_Characters;
            indy_outDBCS_CHARS = 0;
        endif;

        outDSP_ROW_NBR = fld.Row_Number;
        if (fld.Row_Number < 0);
            indy_outDSP_ROW_NBR = -1;
        else;
            indy_outDSP_ROW_NBR = 0;
        endif;

        outDSP_COL_NBR = fld.Column_Number;
        if (fld.Column_Number < 0);
            indy_outDSP_COL_NBR = -1;
        else;
            indy_outDSP_COL_NBR = 0;
        endif;

        outUCS2_DSP_LENGTH = fld.UCS2_Displayed_Length;
        if (fld.UCS2_Displayed_Length = 0);
            indy_outUCS2_DSP_LENGTH = -1;
        else;
            indy_outUCS2_DSP_LENGTH = 0;
        endif;

        outIS_NULLABLE = fld.Null_Values_Allowed;
        outIS_VARCHAR = fld.Variable_Length_Field_Ind;
        outIS_IDENTITY = fld.Identity_Column;
        outIS_ROWID = fld.ROWID_Column;
        outIS_HIDDEN = fld.Implicitly_Hidden;

        if (fld.Data_Type = 'Z');
            outIS_TIMESTAMP = '1';
            outTIMESTAMP_PRECISION = fld.Timestamp_precision;
            indy_outTIMESTAMP_PRECISION = 0;
        else;
            outIS_TIMESTAMP = '0';
            outTIMESTAMP_PRECISION = 0;
            indy_outTIMESTAMP_PRECISION = -1;
        endif;

        select;
            when (fld.GENERATED_BY = '1');
                outGENERATED_WHEN = 'DEFAULT';
                indy_outGENERATED_WHEN = 0;
            when (fld.GENERATED_BY = '2');
                outGENERATED_WHEN = 'ALWAYS';
                indy_outGENERATED_WHEN = 0;
            other;
                outGENERATED_WHEN = '';
                indy_outGENERATED_WHEN = -1;
        endsl;

        select;
            when (fld.Generated_column = x'01');
                outGENERATED_FOR = 'ROWID';
                indy_outGENERATED_FOR = 0;
            when (fld.Generated_column = x'02');
                outGENERATED_FOR = 'IDENTITY';
                indy_outGENERATED_FOR = 0;
            when (fld.Generated_column = x'03');
                outGENERATED_FOR = 'ROW CHANGE TIMESTAMP';
                indy_outGENERATED_FOR = 0;
                if (fld.Row_Change_Timestamp_Generated_By = '1');
                    outGENERATED_WHEN = 'DEFAULT';
                    indy_outGENERATED_WHEN = 0;
                elseif (fld.Row_Change_Timestamp_Generated_By = '2');
                    outGENERATED_WHEN = 'ALWAYS';
                    indy_outGENERATED_WHEN = 0;
                endif;
            when (fld.Generated_column = x'04');
                outGENERATED_FOR = 'ROW-BEGIN';
                indy_outGENERATED_FOR = 0;
            when (fld.Generated_column = x'05');
                outGENERATED_FOR = 'ROW-END';
                indy_outGENERATED_FOR = 0;
            when (fld.Generated_column = x'06');
                outGENERATED_FOR = 'TRANS START ID';
                indy_outGENERATED_FOR = 0;
            when (fld.Generated_column = x'07');
                outGENERATED_FOR = 'EXPRESSION';
                indy_outGENERATED_FOR = 0;
            other;
                outGENERATED_FOR = '';
                indy_outGENERATED_FOR = -1;
        endsl;

        outPROC_PGMNAME = %trimr(fld.Field_Procedure_Program_Name);
        outPROC_LIBNAME = %trimr(fld.Field_Procedure_Library_Name);
        if (%trim(outPROC_PGMNAME) = '');
            indy_outPROC_PGMNAME = -1;
        else;
            indy_outPROC_PGMNAME = 0;
        endif;
        if (%trim(outPROC_LIBNAME) = '');
            indy_outPROC_LIBNAME = -1;
        else;
            indy_outPROC_LIBNAME = 0;
        endif;

        if (fld.UDT_Name_Length > 0);
            outUDT_NAME = %trimr(%subst(fld.UDT_Name : 1 :
                                        fld.UDT_Name_Length));
        else;
            outUDT_NAME = '';
        endif;
        outUDT_LIBNAME = %trimr(fld.UDT_Library_Name);
        if (%trim(outUDT_NAME) = '');
            indy_outUDT_NAME = -1;
        else;
            indy_outUDT_NAME = 0;
        endif;
        if (%trim(outUDT_LIBNAME) = '');
            indy_outUDT_LIBNAME = -1;
        else;
            indy_outUDT_LIBNAME = 0;
        endif;

        outSQLSTATE = '00000';

    elseif (inSQLOpCode = SQL_CLOSE);
        if (scratchPad.fmtUs <> *blanks);
            QusDltUS(scratchPad.fmtUs : ec);
        endif;
        if (scratchPad.fldUs <> *blanks);
            QusDltUS(scratchPad.fldUs : ec);
        endif;
        scratchPad.eof = 1;
    endif;

end-proc;

dcl-proc toObjName;
    dcl-pi toObjName char(10);
        inName varchar(128) const;
    end-pi;

    dcl-s wrk varchar(128);
    dcl-s rtn char(10) inz;
    dcl-s useLen int(10);

    wrk = %trim(inName);
    if (wrk = '');
        return rtn;
    endif;

    if (%subst(wrk : 1 : 1) <> '"');
        wrk = %upper(wrk);
    endif;

    useLen = %int(%min(%len(%trim(wrk)) : 10));
    %subst(rtn : 1 : useLen) = %subst(wrk : 1 : useLen);
    return rtn;
end-proc;

dcl-proc buildQualFile;
    dcl-pi buildQualFile char(20);
        inLibraryName varchar(10) const;
        nullIndy int(5) const;
        inFile varchar(10) const;
    end-pi;

    dcl-s outName char(20) inz(*all' ');
    dcl-s lib char(10) inz('*LIBL');

    if (nullIndy >= 0 and %trim(inLibraryName) <> '');
        lib = toObjName(inLibraryName);
    endif;

    %subst(outName : 1 : 10) = toObjName(inFile);
    %subst(outName : 11 : 10) = lib;  // (already did toObjName(lib))
    return outName;
end-proc;


    // YES
    // NO
    // True
    // False
    // REPLACE
    // ADD
    // 1
    // 0
dcl-proc parseYesNo;
    dcl-pi parseYesNo ind;
        inValue varchar(10) const;
        inIndy like(SQLINDY) const;
    end-pi;

    dcl-s wrk varchar(10);

    if (inIndy < 0 or %trim(inValue) = '');
        return *OFF;
    endif;

    wrk = %upper(%trim(inValue));
    if (%subst(wrk : 1 : 1) = '*');
        wrk = %subst(wrk : 2);
    endif;
    return (%subst(wrk : 1 : 1) in %list('1':'Y':'T':'R'));
end-proc;

dcl-proc createTempUserSpace;
    dcl-pi createTempUserSpace ind;
        rtnUserSpace char(20);
        extAttr char(10) const;
    end-pi;

    dcl-s uniqueTM timestamp(12);
    dcl-s uniqueID char(7);
    dcl-ds ec  likeDS(QUSEC_T) inz(*LIKEDS);
    dcl-ds usName qualified;
        name char(10);
        library char(10);
    end-ds;
    dcl-ds autoExtend qualified inz;
        attrCount int(10) inz(1);
        key int(10) inz(3);
        dataLen int(10) inz(1);
        autoExtend char(1) inz('1');
    end-ds;
    dcl-s tfrSize int(10) inz(0);
    dcl-s optAlign char(1) inz('1');
    dcl-s autoExt  char(1) inz('1');

    dcl-s rtnLib char(10);

    // Create unique user space name using the time micro segment
    uniqueTM = %timestamp(*unique);
    uniqueID = %subst(%char(uniqueTM) : 21 : 7);
    usName.name = 'C4I' + uniqueID;
    usName.library = 'QTEMP';

    reset ec;
    if (getOSVer() >= 740);
        QusCrtUsrSpace(
            usName :
            extAttr :
            64000 :
            x'00' :
            '*ALL' :
            'FIELD_LIST UDTF' :
            '*YES' :
            ec :
            '*DEFAULT' : // *DEFAULT = Auto-domain choice
            tfrSize :   // 0 = Auto
            optAlign :  // '1' = Optimum space alignment
            autoExt     // '1' = auto Extend=yes
        );
        if (ec.bytes_Returned > 0);
            if (g_logToJoblog);
                snd-msg *INFO %MSG(ec.msgID : 'QCPFMSG' :
                              %SUBST(ec.msgData : 1 : ec.bytes_Provided - 16));
            endif;

            return *off;
        endif;
    else;  // V7R3 and earlier
        QusCrtUsrSpace(
            usName :
            extAttr :
            64000 :
            x'00' :
            '*ALL' :
            'FIELD_LIST UDTF' :
            '*YES' :
            ec :
            '*DEFAULT' : // *DEFAULT = Auto-domain choice
            tfrSize :    // 0 = Auto
            optAlign     // '1' = Optimum space alignment
        );
        if (ec.bytes_Returned > 0);
            if (g_logToJoblog);
                snd-msg *INFO %MSG(ec.msgID : 'QCPFMSG' :
                            %SUBST(ec.msgData : 1 : ec.bytes_Provided - 16));
            endif;
        endif;
        reset ec;
        // On V7R3 set auto-extend='1' using QUSCUSAT API
        QusChgUsrSpaceAttr(rtnLib : usName : autoExtend : ec);
        if (ec.bytes_Returned > 0);
            if (g_logToJoblog);
                snd-msg *INFO %MSG(ec.msgID : 'QCPFMSG' :
                            %SUBST(ec.msgData : 1 : ec.bytes_Provided - 16));
            endif;
        endif;
    endif;

    rtnUserSpace = usName;
    return *on;
end-proc;

dcl-proc getJobInfo;
    dcl-pi getJobInfo ind;
        outJobInfo likeds(Qwc_JOBI0400_t);
    end-pi;

    dcl-ds ec  likeDS(QUSEC_T) inz(*LIKEDS);
    dcl-s jobName char(26) inz(*all' ');
    dcl-s intJob char(16) inz(*allx'00');

    %subst(jobName : 1 : 1) = '*';

    reset ec;
    RTVJOBA(outJobInfo : %size(outJobInfo) : 'JOBI0400' : jobName :
            intJob : ec);
    if (ec.bytes_Returned > 0);
        if (g_logToJoblog);
            snd-msg *INFO %MSG(ec.msgID : 'QCPFMSG' :
                            %SUBST(ec.msgData : 1 : ec.bytes_Provided - 16));
        endif;
        return *off;
    endif;
    return (ec.bytes_Returned = 0);
end-proc;

dcl-proc getOSVer;
    dcl-pi getOSVer int(10);
    end-pi;

    dcl-s platform int(10);
    dcl-s ver int(10);
    CEEGPID(ver : platform : *OMIT);
    return ver;
end-proc;

dcl-proc getShortName;
    dcl-pi getShortName char(20) rtnparm;
        objectname varchar(128) const;
        libName    varchar(128) const OPTIONS(*NOPASS : *OMIT);
    end-pi;

    dcl-pr QDBRTVSN  extPgm('QDBRTVSN');
        rtnQualObj char(20);
        longObjName char(128) Const;
        longObjNameLength int(10) Const;
        longObjLibrary char(10) Const;
        apiError  LikeDS(QUSEC_T) OPTIONS(*VARSIZE);
    end-pr QDBRTVSN;
    dcl-s  library varchar(10);
    dcl-s  fileLib char(20);
    dcl-ds ec likeDS(QUSEC_T) inz(*LIKEDS);

    if (%Parms() >= %ParmNum(LibName) and
        %addr(libName) <> *NULL and
        %len(%TrimR(libName)) > 10);
        library =  getShortName( libName : ' ');
    else;
        library = %TrimR(libName);
    endif;

    reset ec;
    QDBRTVSN( fileLib : objectName : %Len(%TrimR(objectName)) :
              library : ec);

    // %SUBST(objName : 1 : 10) = getShortName(objectName : library);
    %SUBST(fileLib : 11 : 10) = library;
    return fileLib;
end-proc;

dcl-proc getPtrUsrSpace;
    dcl-pi  getPtrUsrSpace  pointer;
        userSpace char(20) const;
    end-pi;
    dcl-ds ec likeDS(qusec_t) inz(*LIKEDS);
    dcl-s p pointer inz(*NULL);

    QUSPTRUS( userSpace : p : ec);
    if (ec.bytes_Returned > 0);
        if (g_logToJoblog);
            snd-msg *INFO %MSG(ec.msgID : 'QCPFMSG' :
                            %SUBST(ec.msgData : 1 : ec.bytes_Provided - 16));
        endif;
        return *NULL;
    endif;
    return p;
end-proc;


dcl-proc getNextFmt;
    dcl-pi getNextFmt int(10);
        return_Format char(10);
        userSpace char(20) const;
        index int(10);
    end-pi;

    dcl-s pBuffer pointer;

    dcl-s pGenHdr pointer;
    dcl-ds genHdr likeds(Qus_Generic_Header_0100_t) based(pGenHdr);

    dcl-s pRcdFmt pointer;
    dcl-ds rcdFmt likeds(Qdb_Lrcd_RCDL0100_t) based(pRcdFmt);

    dcl-s fmtCount int(10);
    dcl-s fmtSize int(10);
    dcl-s listOffset int(10);

    pBuffer = getPtrUsrSpace(userSpace);
    if (pBuffer = *null);
        return -1;
    endif;

    pGenHdr = pBuffer;
    fmtCount = genHdr.Number_List_Entries;
    fmtSize = genHdr.Size_Each_Entry;

    if (index >= fmtCount);
        return -1;
    endif;

    listOffset = genHdr.Offset_List_Data;

    // *LAST support is passed in as -1, same as C++.
    if (index < 0);
        index = fmtCount - 1;
    endif;

    pRcdFmt = pBuffer + listOffset + (fmtSize * index);
    return_Format = rcdFmt.Format_Name;

    return index + 1;
end-proc;

dcl-proc getNextField;
    dcl-pi getNextField pointer;
        userSpace char(20) const;
        index int(10);
    end-pi;

    dcl-s pBuffer pointer;
    dcl-s pGenHdr pointer;
    dcl-ds genHdr likeds(Qus_Generic_Header_0100_t) based(pGenHdr);
    dcl-s fieldCount int(10);
    dcl-s listOffset int(10);
    dcl-s pEntry pointer;
    dcl-s i int(10);

    dcl-s pFldHdr pointer;
    dcl-ds fldHdr likeds(Qdb_Lfld_Header_t) based(pFldHdr);
    dcl-s pFld0300 pointer;
    dcl-ds fld0300 likeds(Qdb_Lfld_FLDL0300_t) based(pFld0300);

    dcl-ds fieldData likeds(fieldData_t) static;

    fieldData.pField = *null;
    fieldData.pListHdr = *null;

    pBuffer = getPtrUsrSpace(userSpace);
    if (pBuffer = *null);
        return *null;
    endif;

    pGenHdr = pBuffer;
    fieldCount = genHdr.Number_List_Entries;
    if (index >= fieldCount);
        return *null;
    endif;

    pFldHdr = pGenHdr + genHdr.Offset_Header_Section;
    fieldData.pListHdr = pFldHdr;

    listOffset = genHdr.Offset_List_Data;
    pEntry = pBuffer + listOffset;

    // FLDL0300 list entries are variable length; walk by each entry length.
    for i = 0 to index - 1;
        pFld0300 = pEntry;
        pEntry += fld0300.Length_of_FLDL0300;
    endfor;

    index += 1;
    fieldData.pField = pEntry;

    return %addr(fieldData);
end-proc;`;
}
