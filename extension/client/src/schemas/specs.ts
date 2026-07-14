export type SpecFieldValue = {value: string, text: string};
export type SpecFieldDef = {id: string, name: string, start: number, end: number, values?: SpecFieldValue[], padStart?: boolean}

export const SpecRulers: {[spec: string]: string} = {
  C: `.....CL0N01Factor1+++++++Opcode&ExtFactor2+++++++Result++++++++Len++D+HiLoEq....`,
  D: `.....DName+++++++++++ETDsFrom+++To/L+++IDc.Keywords+++++++++++++++++++++++++++++`,
  F: `.....FFilename++IPEASFRlen+LKlen+AIDevice+.Keywords+++++++++++++++++++++++++++++`,
  I: `.....IFilename++SqNORiPos1+NCCPos2+NCCPos3+NCCDcField+++++++++L1M1FrPlMnZr......`,
  O: `.....OFilename++DF..N01N02N03Name++++++B++A++Sb+Sa+.Constant/Editword+++++++++++`,
  P: `.....PName+++++++++++..T...................Keywords+++++++++++++++++++++++++++++`
  // E, H specs are OPM-only - see opmSpecRulers below
}

export const specs: {[spec: string]: SpecFieldDef[]} = {
  C: [
    {
      id: `controlLevel`,
      name: `Control Level`,
      start: 6,
      end: 7,
    },
    {
      id: `indicators`,
      name: `Indicators`,
      start: 8,
      end: 10,
    },
    {
      id: `factor1`,
      name: `Factor 1`,
      start: 11,
      end: 24
    },
    {
      id: `operation`,
      name: `Operation and Extender`,
      start: 25,
      end: 34
    },
    {
      id: `factor2`,
      name: `Factor 2`,
      start: 35,
      end: 48
    },
    {
      id: `result`,
      name: `Result Field`,
      start: 49,
      end: 62
    },
    {
      id: `fieldLength`,
      name: `Field Length`,
      start: 63,
      end: 67
    },
    {
      id: `decimalPositions`,
      name: `Decimal Positions`,
      start: 68,
      end: 69
    },
    {
      id: `resultingIndicatorsA`,
      name: `Resulting Indicator`,
      start: 70,
      end: 71
    },
    {
      id: `resultingIndicatorsB`,
      name: `Resulting Indicator`,
      start: 72,
      end: 73
    },
    {
      id: `resultingIndicatorsC`,
      name: `Resulting Indicator`,
      start: 74,
      end: 75
    }
  ],
  // Note: C-spec above is for RPGLE (ILE). OPM RPG III has different columns - see opmSpecs below

  D: [
    {start: 6, end: 20, name: `Name`, id: `name`},
    {start: 21, end: 21, name: `External Description`, id: `externalDescription`},
    {start: 22, end: 22, name: `Type of Data Structure`, id: `typeOfDs`},
    {start: 23, end: 24, name: `Definition Type`, id: `definitionType`, values: [
      { value: ``,
        text: `The specification defines either a data structure subfield or a parameter within a prototype or procedure interface definition.`},
      { value: `C`,
        text: `The specification defines a constant. Position 25 must be blank.`},
      { value: `DS`,
        text: `The specification defines a data structure.`},
      { value: `PR`,
        text: `The specification defines a prototype and the return value, if any.`},
      { value: `PI`,
        text: `The specification defines a procedure interface, and the return value if any.`},
      { value: `S`,
        text: `The specification defines a standalone field, array or table. Position 25 must be blank.`},
    ]},
    {start: 25, end: 31, name: `From Position`, id: `fromPosition`, padStart: true},
    {start: 32, end: 38, name: `To Position / Length`, id: `toPosition`, padStart: true},
    {start: 39, end: 39, name: `Internal Data Type`, id: `internalDataType`, values: [
      { value: `A`,
        text: `Character (Fixed or Variable-length format)`},
      { value: `B`,
        text: `Numeric (Binary format)`},
      { value: `C`,
        text: `UCS-2 (Fixed or Variable-length format)`},
      { value: `D`,
        text: `Date`},
      { value: `F`,
        text: `Numeric (Float format)`},
      { value: `G`,
        text: `Graphic (Fixed or Variable-length format)`},
      { value: `I`,
        text: `Numeric (Integer format)`},
      { value: `N`,
        text: `Character (Indicator format)`},
      { value: `O`,
        text: `Object`},
      { value: `P`,
        text: `Numeric (Packed decimal format)`},
      { value: `S`,
        text: `Numeric (Zoned format)`},
      { value: `T`,
        text: `Time`},
      { value: `U`,
        text: `Numeric (Unsigned format)`},
      { value: `Z`,
        text: `Timestamp`},
      { value: `*`,
        text: `Basing pointer or procedure pointer`},
      { value: ``, text: `Blank (Character, Packed or Zoned)`}
    ]},
    {start: 40, end: 41, name: `Decimal Positions`, id: `decimalPositions`, padStart: true},
    {start: 43, end: 79, name: `Keywords`, id: `keywords`}
  ],
  F: [
    {start: 6, end: 15, name: `File Name`, id: `fileName`},
    {start: 16, end: 16, name: `File Type`, id: `fileType`},
    {start: 17, end: 17, name: `File Designation`, id: `fileDesignation`},
    {start: 18, end: 18, name: `End of File`, id: `endOfFile`},
    {start: 19, end: 19, name: `File Addition`, id: `fileAddition`},
    {start: 20, end: 20, name: `Sequence`, id: `sequence`},
    {start: 21, end: 21, name: `File Format`, id: `fileFormat`},
    {start: 22, end: 26, name: `Record Length`, id: `recordLength`},
    {start: 27, end: 27, name: `Limits Processing`, id: `limitsProcessing`},
    {start: 28, end: 32, name: `Length of Key or Record Address`, id: `keyLength`},
    {start: 33, end: 33, name: `Record Address Type`, id: `addressType`},
    {start: 34, end: 34, name: `File Organization`, id: `fileOrg`},
    {start: 35, end: 41, name: `Device`, id: `device`},
    {start: 43, end: 79, name: `Keywords`, id: `keywords`}
  ],
  // E, H, I, O specs are OPM-only or have significant differences - see opmSpecs below
  O: [
    // OAnd: AND/OR continuation line
    {start: 0, end: 5, name: `Sequence Number`, id: `seqNum`},
    {start: 6, end: 6, name: `Comment`, id: `comment`},
    {start: 6, end: 15, name: `AND/OR Keyword`, id: `andOrKeyword`},
    {start: 16, end: 18, name: `Indicator 1`, id: `indicator1`},
    {start: 19, end: 20, name: `Indicator 2`, id: `indicator2`},
    {start: 21, end: 23, name: `Indicator 3`, id: `indicator3`},
    {start: 24, end: 26, name: `Fetch Overflow Indicator`, id: `fetchOverflow1`},
    {start: 27, end: 29, name: `EXCEPT Name`, id: `exceptName`},
    // OF: Program-described field
    {start: 30, end: 43, name: `Field Name`, id: `fieldName`},
    {start: 44, end: 44, name: `Edit Code`, id: `editCode`},
    {start: 45, end: 46, name: `Blank After (B)`, id: `blankAfter`},
    {start: 47, end: 51, name: `End Position`, id: `endPosition`, padStart: true},
    {start: 52, end: 52, name: `Data Format`, id: `dataFormat`},
    {start: 53, end: 79, name: `Constant/Edit Word`, id: `constantOrEdit`},
    // OFC: Field constant continuation
    // OXF: Externally-described field
  ],
  P: [
    {start: 6, end: 20, name: `Name`, id: `name`},
    {start: 23, end: 23, name: `Begin/End Procedure`, id: `proc`},
    {start: 43, end: 79, name: `Keywords`, id: `keywords`}
  ]
};

// OPM RPG III specific spec definitions (different column positions than RPGLE)
export const opmSpecs: {[spec: string]: SpecFieldDef[]} = {
  C: [
    {
      id: `controlLevel`,
      name: `Control Level`,
      start: 6,
      end: 7,
    },
    {
      id: `indicators`,
      name: `Indicators`,
      start: 8,
      end: 16,
    },
    {
      id: `factor1`,
      name: `Factor 1`,
      start: 17,
      end: 26
    },
    {
      id: `operation`,
      name: `Operation`,
      start: 27,
      end: 31
    },
    {
      id: `factor2`,
      name: `Factor 2`,
      start: 32,
      end: 41
    },
    {
      id: `result`,
      name: `Result Field`,
      start: 42,
      end: 47
    },
    {
      id: `fieldLength`,
      name: `Field Length`,
      start: 48,
      end: 50
    },
    {
      id: `decimalPositions`,
      name: `Decimal Positions`,
      start: 51,
      end: 51
    },
    {
      id: `extender`,
      name: `Operation Extender`,
      start: 52,
      end: 52
    },
    {
      id: `resultingIndicatorsA`,
      name: `Resulting Indicator`,
      start: 53,
      end: 54
    },
    {
      id: `resultingIndicatorsB`,
      name: `Resulting Indicator`,
      start: 55,
      end: 56
    },
    {
      id: `resultingIndicatorsC`,
      name: `Resulting Indicator`,
      start: 57,
      end: 58
    }
  ],
  F: [
    {start: 6, end: 13, name: `File Name`, id: `fileName`},
    {start: 14, end: 14, name: `File Type`, id: `fileType`},
    {start: 15, end: 15, name: `File Designation`, id: `fileDesignation`},
    {start: 16, end: 16, name: `End of File`, id: `endOfFile`},
    {start: 17, end: 17, name: `Sequence`, id: `sequence`},
    {start: 18, end: 18, name: `File Format`, id: `fileFormat`},
    {start: 19, end: 22, name: `Block Length`, id: `blockLength`},
    {start: 23, end: 26, name: `Record Length`, id: `recordLength`},
    {start: 27, end: 27, name: `Mode of Processing`, id: `modeOfProcessing`},
    {start: 28, end: 30, name: `Length of Key`, id: `keyLength`},
    {start: 31, end: 31, name: `Record Address Type`, id: `addressType`},
    {start: 32, end: 32, name: `File Organization`, id: `fileOrg`},
    {start: 33, end: 37, name: `Overflow Indicator`, id: `overflowInd`},
    {start: 38, end: 42, name: `Key Field Starting Location`, id: `keyFieldStart`},
    {start: 43, end: 46, name: `File Addition`, id: `fileAddition`},
    {start: 47, end: 51, name: `Symbolic Device`, id: `device`},
    {start: 52, end: 57, name: `Reserved`, id: `reserved1`},
    {start: 58, end: 59, name: `Continuation Lines`, id: `continuation`}
  ],
  E: [
    {start: 6, end: 13, name: `From Filename/Array`, id: `fromFile`},
    {start: 14, end: 24, name: `To Filename/Array`, id: `toFile`},
    {start: 25, end: 26, name: `Extension Code`, id: `extCode`},
    {start: 27, end: 29, name: `Entries per Record`, id: `entriesPerRecord`, padStart: true},
    {start: 30, end: 33, name: `Entries per Array`, id: `entriesPerArray`, padStart: true},
    {start: 34, end: 42, name: `Reserved`, id: `reserved`}
  ],
  H: [
    {start: 6, end: 6, name: `Form Type`, id: `formType`},
    {start: 15, end: 15, name: `Debug`, id: `debug`, values: [
      { value: ``, text: `DEBUG and DUMP operations are not used. Compiler-generated symbols are not placed in the symbol table.`},
      { value: `1`, text: `DEBUG and DUMP operations are used. Compiler-generated symbols are placed in the symbol table.`}
    ]},
    {start: 18, end: 18, name: `Currency Symbol`, id: `currencySymbol`},
    {start: 19, end: 19, name: `Date Format (User Dates)`, id: `dateFormat`, values: [
      { value: ``, text: `Month/day/year format (mmddyy) if position 21 is blank. If position 21 contains D, I, or J, day/month/year (ddmmyy) is used.`},
      { value: `M`, text: `Month/day/year (mmddyy). Separator depends on positions 20 or 21.`},
      { value: `D`, text: `Day/month/year (ddmmyy). Separator depends on positions 20 or 21.`},
      { value: `Y`, text: `Year/month/day (yymmdd). Separator depends on positions 20 or 21.`}
    ]},
    {start: 20, end: 20, name: `Date Edit (Y Edit Code)`, id: `dateEdit`},
    {start: 21, end: 21, name: `Decimal Notation`, id: `decimalNotation`, values: [
      { value: ``, text: `Uses period as decimal notation and comma for separators. If position 19 is blank, uses mmddyy. If position 20 is blank, uses slash (/) separator for date.`},
      { value: `I`, text: `Uses comma as decimal notation and period as separator. If position 19 is blank, uses ddmmyy. If position 20 is blank, uses period (.) separator for date.`},
      { value: `J`, text: `Uses comma as decimal notation and period as separator. If position 19 is blank, uses ddmmyy. If position 20 is blank, uses period (.) separator for date.`},
      { value: `D`, text: `Uses period as decimal notation and comma as separator. If position 19 is blank, uses ddmmyy. If position 20 is blank, uses slash (/) separator for date.`}
    ]},
    {start: 26, end: 26, name: `Alternate Collating Sequence`, id: `altCollatingSequence`, values: [
      { value: ``, text: `Normal collating sequence is used.`},
      { value: `S`, text: `Alternate collating sequence is used.`}
    ]},
    {start: 40, end: 40, name: `Sign Handling`, id: `signHandling`, values: [
      { value: ``, text: `Sign is always forced on input and output of zoned numeric fields.`}
    ]},
    {start: 41, end: 41, name: `Forms Alignment`, id: `formsAlignment`, values: [
      { value: ``, text: `First line is printed only once.`},
      { value: `1`, text: `First line can be printed repeatedly.`}
    ]},
    {start: 43, end: 43, name: `File Translation`, id: `fileTranslation`, values: [
      { value: ``, text: `No file translation is requested.`},
      { value: `F`, text: `Files are to be translated.`}
    ]},
    {start: 57, end: 57, name: `Transparency Check`, id: `transparencyCheck`, values: [
      { value: ``, text: `No check for DBCS in literals.`},
      { value: `1`, text: `Check for DBCS in literals.`}
    ]},
    {start: 75, end: 80, name: `Program Identification`, id: `programId`}
  ],
  I: [
    {start: 6, end: 13, name: `Filename/Structure`, id: `fileName`},
    {start: 14, end: 15, name: `Sequence`, id: `sequence`},
    {start: 16, end: 16, name: `Number`, id: `number`},
    {start: 17, end: 17, name: `Option`, id: `option`},
    {start: 18, end: 19, name: `Record ID Indicator`, id: `recId`},
    {start: 20, end: 29, name: `External Field/Name`, id: `externalField`},
    {start: 30, end: 41, name: `Position/From-To`, id: `position`},
    {start: 42, end: 42, name: `Data Format`, id: `dataFormat`},
    {start: 43, end: 46, name: `From Position`, id: `fromPos`, padStart: true},
    {start: 47, end: 50, name: `To Position`, id: `toPos`, padStart: true},
    {start: 51, end: 51, name: `Decimal Positions`, id: `decimals`},
    {start: 52, end: 57, name: `Field Name`, id: `fieldName`}
  ],
  O: [
    {start: 6, end: 6, name: `Comment`, id: `comment`},
    {start: 6, end: 13, name: `Filename`, id: `fileName`},
    {start: 14, end: 15, name: `Type/Logical Relation`, id: `type`},
    {start: 16, end: 17, name: `Record Addition/Deletion`, id: `addDel`},
    {start: 18, end: 29, name: `Output Indicators`, id: `outputInds`},
    {start: 31, end: 36, name: `Field Name/EXCPT`, id: `fieldName`},
    {start: 37, end: 37, name: `Edit Code`, id: `editCode`},
    {start: 38, end: 38, name: `Blank After`, id: `blankAfter`},
    {start: 39, end: 42, name: `End Position`, id: `endPos`, padStart: true},
    {start: 43, end: 43, name: `Data Format`, id: `dataFormat`},
    {start: 44, end: 69, name: `Constant/Edit Word`, id: `constant`}
  ]
};

// OPM RPG III rulers (different from ILE RPG IV)
// RC: Added all RPG III (RPGII) rulers (added H, E, L and corrected F and I)
export const opmSpecRulers: {[spec: string]: string} = {
  H: `.....H........1..CDYI....S..............1.F...............................PgmID+`,
  F: `.....FFilenameIPEAF....RlenLK1AIOvKlocEDevice+......KExit++Entry+A....UC..`,
  E: `.....E....FromfileTofile++Name++N/rN/tbLenPDSArrnamLenPDSComments+++++++++`,
  L: `.....LFilename066Fl060Ol..................................................`,
  I: `.....IFilenameSqNORiPos1NCCPos2NCCPos3NCC.PFromTo++DField+L1M1FrPlMnZr....`,
  C: `.....CL0N01N02N03Factor1+++OpcdeFactor2+++Result+++LenDXHiLoEq............`,
  O: `.....OFilename+DTAAIndIndIndField++++EBPAAADCONSTANT/EDITWORD+++++++......`
};