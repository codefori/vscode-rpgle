/**
 * fieldListQuery.ts — Utilities for querying FIELD_LIST UDTF and mapping results.
 *
 * Handles:
 * - Building SQL queries to FIELD_LIST
 * - Parsing result rows into field definition objects
 * - Converting field data to vscode-rpgle's internal field format
 */


export interface FieldListRow {
    LIBRARY_NAME: string;
    FILE_NAME: string;
    FILE_TYPE: string;
    RCDFMT: string;
    COLUMN_COUNT: number;
    ORDINAL_POSITION: number;
    COLUMN_NAME: string;
    SQL_DATA_TYPE: string;
    RPG_DATA_TYPE: string;
    COLUMN_LENGTH: number;
    DATA_TYPE: string;
    DEC_POS: number | null;
    BUFFER_LENGTH: number;
    INPUT_BUFFER_POS: number;
    OUTPUT_BUFFER_POS: number;
    USAGE: string;
    COLUMN_CCSID: number;
    SQL_DEFN: string;
    RPG_DEFN: string;
    COLHDG1: string;
    COLHDG2: string;
    COLHDG3: string;
    INTERNAL_NAME: string;
    LONG_NAME: string;
    COLUMN_TEXT: string;
    RCDFMT_TEXT: string;
    EDIT_CODE: string;
    EDIT_WORD: string;
    DATE_TIME_FMT: string;
    DATE_TIME_SEP: string;
    UCS2_DSP_LENGTH: number;
    DBCS_CHARS: number | null;
    LOB_MAX_LENGTH: number | null;
    LOB_PAD_LENGTH: number | null;
    UDT_NAME: string;
    UDT_LIBNAME: string;
    DSP_ROW_NBR: number | null;
    DSP_COL_NBR: number | null;
    IS_NULLABLE: string;
    IS_UPDATEABLE: string;
    IS_VARCHAR: string;
    IS_HIDDEN: string;
    IS_ROWID: string;
    IS_IDENTITY: string;
    IS_TIMESTAMP: string;
    HAS_DEFAULT: string;
    GENERATED_WHEN: string;
    GENERATED_FOR: string;
    PROC_PGMNAME: string;
    PROC_LIBNAME: string;
    TIMESTAMP_PRECISION: number;
    DEFAULT_VALUE: string;
}

/**
 * Builds SQL to retrieve field definitions from FIELD_LIST UDTF.
 *
 * @param library Library containing the file (or *LIBL)
 * @param fileName File name to list fields for
 * @param rcdfmt Record format to list (or *ALL for all formats)
 * @param udtfLibrary Library where FIELD_LIST UDTF is installed
 * @returns SQL SELECT statement
 */
export function buildFieldListQuery(
    library: string,
    fileName: string,
    rcdfmt: string = '*ALL',
    udtfLibrary: string = 'SQLTOOLS'
): string {
    const lib = library === '*LIBL' ? "'*LIBL'" : `'${library.toUpperCase()}'`;
    const file = `'${fileName.toUpperCase()}'`;
    const fmt = `'${rcdfmt.toUpperCase()}'`;

    return `
SELECT *
  FROM TABLE(
    ${udtfLibrary}.FIELD_LIST(
      LIBRARY_NAME => ${lib},
      FILE_NAME    => ${file},
      RCDFMT       => ${fmt},
      OVR          => 'NO',
      LOG          => 'NO'
    )
  ) AS FL
  ORDER BY RCDFMT, ORDINAL_POSITION
`;
}

/**
 * Executes a FIELD_LIST query and returns parsed results.
 *
 * @param connection IBM i connection
 * @param library Library containing the file
 * @param fileName File name to query
 * @param rcdfmt Record format (default '*ALL')
 * @param udtfLibrary Library where FIELD_LIST is installed (default 'SQLTOOLS')
 * @returns Array of FieldListRow objects, grouped by record format
 */
export async function queryFieldList(
    connection: any,
    library: string,
    fileName: string,
    rcdfmt: string = '*ALL',
    udtfLibrary: string = 'SQLTOOLS'
): Promise<FieldListRow[]> {
    const sql = buildFieldListQuery(library, fileName, rcdfmt, udtfLibrary);

    try {
        const results = await connection.runSQL(sql);
        return results as FieldListRow[];
    } catch (e) {
        console.error(`[vscode-rpgle] queryFieldList() threw: ${e}`);
        throw e;
    }
}

/**
 * Groups field list results by record format.
 *
 * @param rows Raw query results from queryFieldList
 * @returns Map of RCDFMT -> field rows
 */
export function groupByRecordFormat(rows: FieldListRow[]): Map<string, FieldListRow[]> {
    const groups = new Map<string, FieldListRow[]>();
    for (const row of rows) {
        const rcdfmt = row.RCDFMT?.trim() || '*UNKNOWN';
        if (!groups.has(rcdfmt)) {
            groups.set(rcdfmt, []);
        }
        groups.get(rcdfmt)!.push(row);
    }
    return groups;
}
/**
 * Converts FIELD_LIST UDTF results to DSPFFD outfile-like format.
 * Maps FIELD_LIST columns to match what DSPFFD output file returns.
 *
 * @param rows Field list rows from FIELD_LIST UDTF
 * @returns Array formatted like DSPFFD OUTFILE results
 */
export function convertFieldListToDspffdFormat(rows: FieldListRow[]): any[] {
    return rows.map(row => {
        const isVarchar = String(row.IS_VARCHAR ?? '').trim();
        const varcharFlag = isVarchar === '1' || isVarchar.toUpperCase() === 'Y' ? 'Y' : 'N';

        // Built to mirror the subset that dspffdToRecordFormats() actually reads.
        // The parser expects DSPFFD-style compatibility keys such as WHVARL and
        // alias fields. FIELD_LIST has the equivalent values but under different names.
        return {
            WHFILE: row.FILE_NAME?.trim() || '',
            WHLIB: row.LIBRARY_NAME?.trim() || '',
            WHNAME: row.RCDFMT?.trim() || '',
            WHFLDE: row.COLUMN_NAME?.trim() || '',
            WHFLDT: row.DATA_TYPE?.trim() || '',
            // DSPFFD `WHFLDB` is the physical record byte length, not always the declared field length.
            // For varying-length fields the database stores a length prefix, so BUFFER_LENGTH may be
            // larger than the declared SQL length. Keep the actual field size in WHFLDD.
            WHFLDB: row.BUFFER_LENGTH ?? 0,
            WHFLDD: row.COLUMN_LENGTH !== null && row.COLUMN_LENGTH !== undefined ? row.COLUMN_LENGTH : 0,
            WHFLDP: row.DEC_POS !== null && row.DEC_POS !== undefined ? row.DEC_POS : 0,
            WHTEXT: row.RCDFMT_TEXT?.trim() || '',
            WHFTXT: row.COLUMN_TEXT?.trim() || '',
            WHFTYP: row.FILE_TYPE?.trim() || '',
            WHVARL: varcharFlag,
            // FIELD_LIST consolidates the two legacy alias fields into one effective alias.
            // If the original 30-char alias exists, it is used; otherwise LONG_NAME is used.
            // Emit it in the legacy WHALIS slot so downstream alias handling keeps working.
            WHALIS: (row.LONG_NAME || '').trim() || '',
            WHRPGDEF: row.RPG_DEFN?.trim() || '',
            // Preserve the original UDTF fields for debugging or any compatibility edge cases.
            ...row
        };
    });
}