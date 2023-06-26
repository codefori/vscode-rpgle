module.exports = {
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
      id: `resultingIndicators`,
      name: `Resulting Indicators`,
      start: 70,
      end: 75
    }
  ],

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
  P: [
    {start: 6, end: 20, name: `Name`, id: `name`},
    {start: 23, end: 23, name: `Begin/End Procedure`, id: `proc`},
    {start: 43, end: 79, name: `Keywords`, id: `keywords`}
  ]
};