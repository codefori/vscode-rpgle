import path from "path";
import setupParser from "../parserSetup";
import { test, expect } from "vitest";

const parser = setupParser();
const uri = `source.rpgle`;

test('ospec1 - basic output specification', async () => {
  const lines = [
    `     FQPRINT    O    F  132        PRINTER OFLIND(*INOF)`,
    `     O                                           10 'CUSTOMER'`,
    `     O                       CUSTNO              20`,
    `     O                       CUSTNAME            50`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, { ignoreCache: true, withIncludes: true });

  expect(cache).toBeDefined();
  expect(cache.files.length).toBe(1);
  
  // Check that output specifications are parsed
  expect(cache.outputs.length).toBeGreaterThan(0);
});

test('ospec2 - output with type indicators and edit codes', async () => {
  const lines = [
    `     FQPRINT    O    F  132        PRINTER`,
    `     D TITLE           S             30`,
    `     D AMOUNT          S             10  2`,
    `     O          H                               10 'HEADER'`,
    `     O                       TITLE               40`,
    `     O          D                               10 'DETAIL'`,
    `     O                       AMOUNT        1     30`,
    `     O          T                               10 'TOTAL'`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, { ignoreCache: true, withIncludes: true });

  expect(cache).toBeDefined();
  
  expect(cache.outputs.length).toBeGreaterThan(0);
});

test('ospec3 - complex output specification', async () => {
  const lines = [
    `     FQPRINT    O    F  132        PRINTER OFLIND(*INOF)`,
    `     D CUSTNO          S              5  0`,
    `     D CUSTNAME        S             30`,
    `     D AMOUNT          S             10  2`,
    `     O          H    1P                          10 'CUSTOMER REPORT'`,
    `     O                                           30 'DATE:'`,
    `     O                       *DATE         Y     40`,
    `     O          D       01                       10 'CUSTOMER:'`,
    `     O                       CUSTNO        Z     20`,
    `     O                       CUSTNAME            55`,
    `     O                       AMOUNT        1     70`,
    `     O          T    LR                          10 'TOTAL RECORDS:'`,
    `     O                       *COUNT        Z     30`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, { ignoreCache: true, withIncludes: true });

  expect(cache).toBeDefined();
  expect(cache.files.length).toBe(1);
  expect(cache.variables.length).toBeGreaterThan(0);
  
  // Verify output specifications are parsed
  expect(cache.outputs.length).toBeGreaterThan(0);
});

test('ospec4 - output with fetch overflow', async () => {
  const lines = [
    `     FQPRINT    O    F  132        PRINTER OFLIND(*INOF)`,
    `     D FIELD1          S             20`,
    `     O          H    1P                          10 'HEADER'`,
    `     O          D    OF                          10 'OVERFLOW'`,
    `     O                       FIELD1              30`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, { ignoreCache: true, withIncludes: true });

  expect(cache).toBeDefined();
  
  // Check output with fetch overflow - at least FIELD1 should be parsed
  expect(cache.outputs.length).toBeGreaterThan(0);
});

test('ospec5 - output with data format', async () => {
  const lines = [
    `     FQPRINT    O    F  132        PRINTER`,
    `     D DATE1           S               D`,
    `     D TIME1           S               T`,
    `     D AMOUNT          S             10  2`,
    `     O                       DATE1         Y     20`,
    `     O                       TIME1         Y     35`,
    `     O                       AMOUNT        1     50`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, { ignoreCache: true, withIncludes: true });

  expect(cache).toBeDefined();
  
  // Verify output with data format (Y for date/time)
  expect(cache.outputs.length).toBeGreaterThan(0);
});

test('ospec6 - output with blank after (B)', async () => {
  const lines = [
    `     FQPRINT    O    F  132        PRINTER`,
    `     D FIELD1          S             20`,
    `     D FIELD2          S             15`,
    `     O                       FIELD1        B     25`,
    `     O                       FIELD2              45`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, { ignoreCache: true, withIncludes: true });

  expect(cache).toBeDefined();
  expect(cache.outputs.length).toBeGreaterThan(0);
});

test('ospec7 - output with multiple edit codes', async () => {
  const lines = [
    `     FQPRINT    O    F  132        PRINTER`,
    `     D AMT1            S             10  2`,
    `     D AMT2            S             10  2`,
    `     D AMT3            S             10  2`,
    `     O                       AMT1          1     20`,
    `     O                       AMT2          2     40`,
    `     O                       AMT3          Z     60`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, { ignoreCache: true, withIncludes: true });

  expect(cache).toBeDefined();
  expect(cache.outputs.length).toBeGreaterThan(0);
});

test('ospec8 - output with AND/OR indicators', async () => {
  const lines = [
    `     FQPRINT    O    F  132        PRINTER`,
    `     D FIELD1          S             20`,
    `     O          D       01                       10 'LINE 1'`,
    `     O                       FIELD1              30`,
    `     O          D       02 AND 03                10 'LINE 2'`,
    `     O                       FIELD1              30`,
    `     O          D       04 OR  05                10 'LINE 3'`,
    `     O                       FIELD1              30`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, { ignoreCache: true, withIncludes: true });

  expect(cache).toBeDefined();
  expect(cache.outputs.length).toBeGreaterThan(0);
});

test('ospec9 - output with exception lines (E)', async () => {
  const lines = [
    `     FQPRINT    O    F  132        PRINTER`,
    `     D FIELD1          S             20`,
    `     O          E    EXCEPT1                     10 'EXCEPTION'`,
    `     O                       FIELD1              30`,
    `     O          E    EXCEPT2                     10 'ANOTHER'`,
    `     O                       FIELD1              30`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, { ignoreCache: true, withIncludes: true });

  expect(cache).toBeDefined();
  expect(cache.outputs.length).toBeGreaterThan(0);
});

test('ospec10 - output with space/skip before/after', async () => {
  const lines = [
    `     FQPRINT    O    F  132        PRINTER`,
    `     D FIELD1          S             20`,
    `     O          H    2                           10 'HEADER'`,
    `     O                                    2      30 'SPACED'`,
    `     O          D    1                           10 'DETAIL'`,
    `     O                       FIELD1              30`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, { ignoreCache: true, withIncludes: true });

  expect(cache).toBeDefined();
  expect(cache.outputs.length).toBeGreaterThan(0);
});

test('ospec11 - output with constants and field names mixed', async () => {
  const lines = [
    `     FQPRINT    O    F  132        PRINTER`,
    `     D CUSTNO          S              5  0`,
    `     D CUSTNAME        S             30`,
    `     O                                           10 'CUSTOMER #:'`,
    `     O                       CUSTNO              25`,
    `     O                                           30 'NAME:'`,
    `     O                       CUSTNAME            60`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, { ignoreCache: true, withIncludes: true });

  expect(cache).toBeDefined();
  expect(cache.outputs.length).toBeGreaterThan(0);
});

test('ospec12 - output with reserved words (*DATE, *TIME, *PAGE)', async () => {
  const lines = [
    `     FQPRINT    O    F  132        PRINTER`,
    `     O          H                               10 'REPORT'`,
    `     O                       *DATE         Y     30`,
    `     O                       *TIME               45`,
    `     O                                           60 'PAGE:'`,
    `     O                       *PAGE         Z     70`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, { ignoreCache: true, withIncludes: true });

  expect(cache).toBeDefined();
  expect(cache.outputs.length).toBeGreaterThan(0);
});

test('ospec13 - output with all record types (H, D, T, E)', async () => {
  const lines = [
    `     FQPRINT    O    F  132        PRINTER`,
    `     D FIELD1          S             20`,
    `     D TOTAL           S             10  2`,
    `     O          H                               10 'HEADER LINE'`,
    `     O                       *DATE         Y     30`,
    `     O          D                               10 'DETAIL LINE'`,
    `     O                       FIELD1              30`,
    `     O          T    LR                          10 'TOTAL:'`,
    `     O                       TOTAL         1     30`,
    `     O          E    ERROR1                      10 'ERROR'`,
    `     O                       FIELD1              30`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, { ignoreCache: true, withIncludes: true });

  expect(cache).toBeDefined();
  expect(cache.outputs.length).toBeGreaterThan(0);
});

test('ospec14 - output with numeric edit codes (1-4, A-D, J-Q)', async () => {
  const lines = [
    `     FQPRINT    O    F  132        PRINTER`,
    `     D AMT1            S             10  2`,
    `     D AMT2            S             10  2`,
    `     D AMT3            S             10  2`,
    `     D AMT4            S             10  2`,
    `     O                       AMT1          A     20`,
    `     O                       AMT2          B     40`,
    `     O                       AMT3          J     60`,
    `     O                       AMT4          K     80`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, { ignoreCache: true, withIncludes: true });

  expect(cache).toBeDefined();
  expect(cache.outputs.length).toBeGreaterThan(0);
});

test('ospec15 - output with zero suppression (Z)', async () => {
  const lines = [
    `     FQPRINT    O    F  132        PRINTER`,
    `     D AMOUNT          S             10  2`,
    `     D QUANTITY        S              5  0`,
    `     O                       AMOUNT        Z     20`,
    `     O                       QUANTITY      Z     40`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, { ignoreCache: true, withIncludes: true });

  expect(cache).toBeDefined();
  expect(cache.outputs.length).toBeGreaterThan(0);
});

test('ospec16 - output with multiple files', async () => {
  const lines = [
    `     FQPRINT1   O    F  132        PRINTER`,
    `     FQPRINT2   O    F  132        PRINTER`,
    `     D FIELD1          S             20`,
    `     OQPRINT1   H                               10 'FILE 1'`,
    `     O                       FIELD1              30`,
    `     OQPRINT2   H                               10 'FILE 2'`,
    `     O                       FIELD1              30`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, { ignoreCache: true, withIncludes: true });

  expect(cache).toBeDefined();
  expect(cache.files.length).toBe(2);
  expect(cache.outputs.length).toBeGreaterThan(0);
});

test('ospec17 - output with conditioning indicators (01-99, H1-H9, L1-L9, LR)', async () => {
  const lines = [
    `     FQPRINT    O    F  132        PRINTER`,
    `     D FIELD1          S             20`,
    `     O          H    1P                          10 'FIRST PAGE'`,
    `     O          D       01                       10 'INDICATOR 01'`,
    `     O                       FIELD1              30`,
    `     O          D       L1                       10 'LEVEL BREAK'`,
    `     O                       FIELD1              30`,
    `     O          T    LR                          10 'LAST RECORD'`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, { ignoreCache: true, withIncludes: true });

  expect(cache).toBeDefined();
  expect(cache.outputs.length).toBeGreaterThan(0);
});

test('ospec18 - output with empty lines and comments', async () => {
  const lines = [
    `     FQPRINT    O    F  132        PRINTER`,
    `     D FIELD1          S             20`,
    `      * This is a comment`,
    `     O          H                               10 'HEADER'`,
    `      *`,
    `     O                       FIELD1              30`,
    `      * Another comment`,
    `     O          D                               10 'DETAIL'`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, { ignoreCache: true, withIncludes: true });

  expect(cache).toBeDefined();
  expect(cache.outputs.length).toBeGreaterThan(0);
});

test('ospec19 - output with long constants (up to 28 characters)', async () => {
  const lines = [
    `     FQPRINT    O    F  132        PRINTER`,
    `     O          H                               40 'THIS IS A VERY LONG CONST'`,
    `     O                                           80 'ANOTHER LONG CONSTANT HE'`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, { ignoreCache: true, withIncludes: true });

  expect(cache).toBeDefined();
  expect(cache.outputs.length).toBeGreaterThan(0);
});

test('ospec20 - output with field names at various positions', async () => {
  const lines = [
    `     FQPRINT    O    F  132        PRINTER`,
    `     D FLD1            S             10`,
    `     D FLD2            S             10`,
    `     D FLD3            S             10`,
    `     O                       FLD1                10`,
    `     O                       FLD2                50`,
    `     O                       FLD3               132`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, { ignoreCache: true, withIncludes: true });

  expect(cache).toBeDefined();
  expect(cache.outputs.length).toBeGreaterThan(0);
});

test('ospec21 - output with negative indicators (N01-N99)', async () => {
  const lines = [
    `     FQPRINT    O    F  132        PRINTER`,
    `     D FIELD1          S             20`,
    `     O          D      N01                       10 'NOT 01'`,
    `     O                       FIELD1              30`,
    `     O          D      N99                       10 'NOT 99'`,
    `     O                       FIELD1              30`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, { ignoreCache: true, withIncludes: true });

  expect(cache).toBeDefined();
  expect(cache.outputs.length).toBeGreaterThan(0);
});

test('ospec22 - output with complex AND/OR combinations', async () => {
  const lines = [
    `     FQPRINT    O    F  132        PRINTER`,
    `     D FIELD1          S             20`,
    `     O          D       01 AND 02 AND 03         10 'THREE AND'`,
    `     O                       FIELD1              30`,
    `     O          D       04 OR  05 OR  06         10 'THREE OR'`,
    `     O                       FIELD1              30`,
    `     O          D      N01 ANDN02                10 'NOT AND NOT'`,
    `     O                       FIELD1              30`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, { ignoreCache: true, withIncludes: true });

  expect(cache).toBeDefined();
  expect(cache.outputs.length).toBeGreaterThan(0);
});

test('ospec23 - output with all data formats (Y, blank)', async () => {
  const lines = [
    `     FQPRINT    O    F  132        PRINTER`,
    `     D DATE1           S               D`,
    `     D TIME1           S               T`,
    `     D CHAR1           S             10`,
    `     O                       DATE1         Y     20`,
    `     O                       TIME1         Y     35`,
    `     O                       CHAR1               50`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, { ignoreCache: true, withIncludes: true });

  expect(cache).toBeDefined();
  expect(cache.outputs.length).toBeGreaterThan(0);
});

test('ospec24 - output with minimum specification', async () => {
  const lines = [
    `     FQPRINT    O    F  132        PRINTER`,
    `     O          H                               10 'X'`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, { ignoreCache: true, withIncludes: true });

  expect(cache).toBeDefined();
  expect(cache.outputs.length).toBeGreaterThan(0);
});

test('ospec25 - output with maximum complexity', async () => {
  const lines = [
    `     FQPRINT    O    F  132        PRINTER OFLIND(*INOF)`,
    `     D CUSTNO          S              5  0`,
    `     D CUSTNAME        S             30`,
    `     D AMOUNT          S             10  2`,
    `     D TOTAL           S             12  2`,
    `     OQPRINT    H    1P                          10 'CUSTOMER REPORT'`,
    `     O                                           30 'DATE:'`,
    `     O                       *DATE         Y     40`,
    `     O                                           50 'PAGE:'`,
    `     O                       *PAGE         Z     60`,
    `     O          H    OF                          10 'CONTINUED...'`,
    `     O          D       01 AND 02                10 'CUSTOMER:'`,
    `     O                       CUSTNO        Z     25`,
    `     O                       CUSTNAME      B     60`,
    `     O                       AMOUNT        1     80`,
    `     O          D      N01                       10 'NO CUSTOMER'`,
    `     O          T    L1                          10 'SUBTOTAL:'`,
    `     O                       TOTAL         1     80`,
    `     O          T    LR                          10 'GRAND TOTAL:'`,
    `     O                       TOTAL         1     80`,
    `     O                       *COUNT        Z    100`,
    `     O          E    ERROR1                      10 'ERROR OCCURRED'`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, { ignoreCache: true, withIncludes: true });

  expect(cache).toBeDefined();
  expect(cache.files.length).toBe(1);
  expect(cache.variables.length).toBeGreaterThan(0);
  expect(cache.outputs.length).toBeGreaterThan(0);
});

// Made with Bob
