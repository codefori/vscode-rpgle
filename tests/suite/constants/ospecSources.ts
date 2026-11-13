/**
 * Fixed-format source used by the O-spec navigation tests.
 * Line numbers are 0-based and the tests assert against them, so be careful when editing.
 */

/** A single exception FOO in the O-specs, called once from the C-specs */
export const SINGLE_EXCEPT = [
  `     FQPRINT    O    F  132        PRINTER OFLIND(*INOA)`,
  `     C                   EXCEPT    FOO`,
  `     C                   SETON                                        LR`,
  `     OQPRINT    E            FOO            1`,
  `     O                                           10 'Foo line'`,
].join(`\n`);

/** FOO spread over two O-spec records, BAR over one, and FOO called twice from the C-specs */
export const MULTIPLE_EXCEPT = [
  `     FQPRINT    O    F  132        PRINTER OFLIND(*INOA)`,
  `     C                   EXCEPT    FOO`,
  `     C                   EXCEPT    BAR`,
  `     C                   EXCEPT    FOO`,
  `     C                   SETON                                        LR`,
  `     OQPRINT    E            FOO            1`,
  `     O                                           10 'Foo line'`,
  `     O          E            FOO            1`,
  `     O                                           10 'More foo'`,
  `     O          E            BAR            1`,
  `     O                                           10 'Bar line'`,
].join(`\n`);

/** EXCEPT NOPE has no matching exception in the O-specs (only FOO exists) */
export const UNKNOWN_EXCEPT = [
  `     FQPRINT    O    F  132        PRINTER OFLIND(*INOA)`,
  `     C                   EXCEPT    NOPE`,
  `     C                   SETON                                        LR`,
  `     OQPRINT    E            FOO            1`,
  `     O                                           10 'Foo line'`,
].join(`\n`);

/**
 * Field and constant references in O-spec lines.
 * CUSTNO is a D-spec variable used on two field lines, MAXLEN is a D-spec constant used as the output constant,
 * and NOFIELD is not declared anywhere.
 */
export const FIELD_AND_CONSTANT = [
  `     FQPRINT    O    F  132        PRINTER OFLIND(*INOA)`,
  `     D CUSTNO          S              7  0`,
  `     D MAXLEN          C                   CONST(50)`,
  `     C                   EXCEPT    DETAIL`,
  `     C                   SETON                                        LR`,
  `     OQPRINT    E            DETAIL         1`,
  `     O                       CUSTNO              10`,
  `     O                       CUSTNO              30`,
  `     O                       MAXLEN              40`,
  `     O                       NOFIELD             50`,
].join(`\n`);

/**
 * File and record references in O-spec record lines.
 * QPRINT (F-spec) is named on the first record of each exception, the second HDR record has a blank file name.
 */
export const FILE_AND_RECORD = [
  `     FQPRINT    O    F  132        PRINTER OFLIND(*INOA)`,
  `     C                   EXCEPT    HDR`,
  `     C                   EXCEPT    DETAIL`,
  `     C                   SETON                                        LR`,
  `     OQPRINT    E            HDR            1`,
  `     O                                           10 'Header'`,
  `     O          E            HDR            1`,
  `     O                                           10 'More header'`,
  `     OQPRINT    E            DETAIL         1`,
  `     O                                           10 'Detail'`,
].join(`\n`);

/**
 * Conditioning indicators on O-spec record and field lines (indicator in columns 22-23).
 * Indicator 10 is set on the C-spec, then conditions one record line and one field line.
 * Indicator 20 conditions a record line only.
 */
export const CONDITIONING_INDICATORS = [
  `     FQPRINT    O    F  132        PRINTER OFLIND(*INOA)`,
  `     D CUSTNO          S              7  0`,
  `     C                   SETON                                        10`,
  `     C                   EXCEPT    DETAIL`,
  `     C                   EXCEPT    TOTAL`,
  `     C                   SETON                                        LR`,
  `     OQPRINT    E    10      DETAIL         1`,
  `     O                       CUSTNO              10`,
  `     O               10      CUSTNO              30`,
  `     O          E    20      TOTAL          1`,
  `     O                                           10 'Total'`,
].join(`\n`);

/** 0-based column of the EXCEPT operand (factor 2, column 36 in the source) */
export const FACTOR2 = 35;
/** 0-based column of the EXCEPT name in an O-spec record (column 30 in the source) */
export const OSPEC_EXCEPT_NAME = 29;
/** 0-based column inside the EXCEPT opcode (column 27 in the source) */
export const EXCEPT_OPCODE = 26;
/** 0-based column inside the constant of an O-spec output line */
export const OSPEC_CONSTANT = 55;
/** 0-based column of the file/record name in an O-spec record line (column 7 in the source) */
export const OSPEC_FILE_NAME = 6;
/** 0-based column of the field name in an O-spec field line (column 30 in the source) */
export const OSPEC_FIELD_NAME = 29;
/** 0-based column of the first conditioning indicator in an O-spec line (columns 22-23 in the source) */
export const OSPEC_INDICATOR = 21;
/** 0-based column of the first resulting indicator in a C-spec line (columns 71-72 in the source) */
export const CSPEC_RESULT_INDICATOR = 70;
/** 0-based column of the name in a D-spec line (column 8 in the source) */
export const DSPEC_NAME = 7;
