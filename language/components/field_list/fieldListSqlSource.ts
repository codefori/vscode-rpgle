/**
 * Generates the SQL DDL used to create (or replace) the FIELD_LIST UDTF in the
 * target library on IBM i.
 *
 * The version number is embedded in the LONG_COMMENT of the specific routine so
 * FieldListChecker.getRemoteState() can detect stale installs and trigger update().
 *
 * Source origin: /Users/cozzi/Downloads/projects/open-UDTF/src/FIELD_LIST/FIELDLIST.SQL
 */
export function getFieldListSQLSrc(library: string, version: number): string {
    return `
CREATE OR REPLACE FUNCTION ${library}.FIELD_LIST(
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
SPECIFIC ${library}.FIELD_LIST
EXTERNAL NAME '${library}/FIELDLIST'
PARAMETER STYLE DB2SQL;

LABEL on specific routine ${library}.FIELD_LIST IS
'List File Field definitions (similar to DSPFFD)';

comment on specific function ${library}.FIELD_LIST IS
'${version} - List of definitions for the fields (columns) in the file(s) specified.
Typically one FILE_NAME entry is specified however any number of files
may be included to return all the field formats at once. Both the SQL
definition/keyword is returned along with the RPG IV definition.';
`;
}
