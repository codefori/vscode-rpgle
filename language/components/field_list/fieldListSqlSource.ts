/**
 * FIELD_LIST SQL DDL template synced from open-UDTF.
 * Tokens are substituted per connection before upload:
 * - ${library}
 * - ${version}
 */
const fieldListSqlTemplate = `

-- SPDX-License-Identifier: Apache-2.0
-- Copyright (c) 1996-2026 by R. Cozzi, Jr.
-- @author BobCozzi

CREATE OR REPLACE FUNCTION \${library}.FIELD_LIST(
    LIBRARY_NAME VARCHAR(10) DEFAULT '*LIBL',
    FILE_NAME    VARCHAR(10),
    RCDFMT       VARCHAR(10) DEFAULT '*ALL',
    OVR          varchar(10) DEFAULT 'NO',
    LOG          varchar(10) DEFAULT 'NO'
)
RETURNS TABLE (
    Library_name VARCHAR(10),   -- Library where file was located
    File_name    VARCHAR(10),   -- File object name
    FILE_TYPE    VARCHAR(10),   -- File attribute (PF, LF, DSPF, etc)
    RCDFMT       VARCHAR(10),   -- Record Format name
    COLUMN_COUNT INT,           -- Number of fields in this format

    ORDINAL_POSITION INT,       -- Dumb name for "Field SeqNbr"
    COLUMN_NAME VARCHAR(10),    -- Field name
    SQL_DATA_TYPE VARCHAR(16),  -- SQL Data Type keyword
    RPG_DATA_TYPE VARCHAR(16),  -- RPG IV DCL-S data type keyword

    COLUMN_LENGTH INT,          -- Defined Length of Field
    DATA_TYPE  CHAR(1),         -- 1-Byte Data Type
    DEC_POS INT,                -- Decimal Positions or NULL
    BUFFER_LENGTH INT,          -- Bytes used for field content

    INPUT_BUFFER_POS INT,       -- Starting position of field for Input
    OUTPUT_BUFFER_POS INT,      -- Starting position of field for Output
    USAGE VARCHAR(10),          -- Usage (Input/Output/Both)
    COLUMN_CCSID int,           -- CCSID of the Field's Data

    SQL_DEFN VARCHAR(64),       -- Full SQL datatype and length
    RPG_DEFN VARCHAR(64),       -- Full RPG DCL-S datatype and length

    COLHDG1 VARCHAR(20),
    COLHDG2 VARCHAR(20),
    COLHDG3 VARCHAR(20),

    INTERNAL_NAME VARCHAR(10),  -- Should it be COLUMN_INTERNAL_NAME?
    LONG_NAME     VARCHAR(255),     -- Should it be COLUMN_LONG_NAME?

    COLUMN_TEXT VARCHAR(50),
    RCDFMT_TEXT VARCHAR(50),
    EDIT_CODE VARCHAR(2),
    EDIT_WORD VARCHAR(64),

    DATE_TIME_FMT VARCHAR(4),
    DATE_TIME_SEP CHAR(1),

    UCS2_DSP_LENGTH INT,
    DBCS_CHARS INT,
    LOB_MAX_LENGTH INT,
    LOB_PAD_LENGTH INT,

    UDT_NAME VARCHAR(128),
    UDT_LIBNAME VARCHAR(10),

    DSP_ROW_NBR  INT,  -- Row number for DISPLAY and PRINTER Files else NULL
    DSP_COL_NBR  INT,  -- Column number for DISPLAY and PRINTER Files else NULL

    IS_NULLABLE  CHAR(1),
    IS_UPDATEABLE CHAR(1),
    IS_VARCHAR   CHAR(1),
    IS_HIDDEN    CHAR(1),
    IS_ROWID     CHAR(1),
    IS_IDENTITY  CHAR(1),
    IS_TIMESTAMP CHAR(1),
    HAS_DEFAULT  CHAR(1),

    GENERATED_WHEN CHAR(10),
    GENERATED_FOR  VARCHAR(32),

    PROC_PGMNAME VARCHAR(10),
    PROC_LIBNAME VARCHAR(10),
    TIMESTAMP_PRECISION SMALLINT,
    DEFAULT_VALUE VARCHAR(2000)
)
LANGUAGE C++
NO SQL
NOT DETERMINISTIC
NOT FENCED
CALLED ON NULL INPUT
SCRATCHPAD 2048
SPECIFIC \${library}.FIELD_LIST
EXTERNAL NAME '\${library}/FIELD_LIST'
PARAMETER STYLE DB2SQL;


LABEL on specific routine \${library}.FIELD_LIST IS
'List File Field definitions (similar to DSPFFD)';

comment on specific function \${library}.FIELD_LIST IS
'\${version} - List of definitions for the fileds (columns) in the file(s) specified.
Typically one FILE_NAME entry is specified however any number of files
may be included to returns all the field formats at once.<p>Both the SQL
definition/keyword is returned along with the RPG IV definition.';

comment on parameter Specific Function \${library}.FIELD_LIST
( LIBRARY_NAME is 'The library name where the file is located.
If this parameter is NULL or unspecified, then *LIBL is used.
Any valid library (schema) name may be specified or one of following
special values:<ul><li><u>*LIBL</u> - The library list is used</li>
<li>*CURLIB - The job''s current library is used</li></ul>
Upper/lower case is ignored unless the library name is
enclosed in double-quotes.',

FILE_NAME is '<p>The file name whose field/column properties are returned.
The file name may be any type of *FILE object, including PF, LF, DDMF,
DSPF, PRTF, etc.
Upper/lower case is ignored unless the file name is
enclosed in double-quotes.',

RCDFMT IS
  'The record format name whose field/column properties are returned.
The default is *ALL. Any valid record format name may be specified,
or one of the following special values:
<ul><li><i>*ALL</i> - All record formats are returned.
This is the Default.</li>
<li>*FIRST - The first record format.</li>
<li>*LAST - The last record format.</li>
<li>*ONLY - The only  record format. If multiple recod format exists,
Then this option will behave like <b>*FIRST</b></li>
</ul>Upper/lower case is ignored unless the format name is
enclosed in double-quotes.',

OVR is
'Controls whether to respect any OVERRIDES (OVRDBF, OVRPRTF, etc.) that may
be in effect when the function is run. The valid choices are:
<ul><li><i>*NO</i> - Overrides are not respected. This is the Default.</li>
<li>*YES - Overrides are respected when running the function.</li>
</ul>Note that the leading asterisk and upper/lower case are ignored.',
LOG is
'Controls whether or not to write information to the joblog. This information
is normally unnecssary, but may be helpful for debugging purposes. The
valid choices are:<ul>
<li><i>*NO</i> - DO not write to the joblog. This is the Default.</li>
<li>*YES - Write debug information to the joblog.</li>
</ul>'
);




`;

export function getFieldListSQLSrc(library: string, version: number): string {
    return fieldListSqlTemplate
        .replace(/\$\{library\}/g, library)
        .replace(/\$\{version\}/g, String(version));
}
