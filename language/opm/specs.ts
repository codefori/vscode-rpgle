// Shared base type for all specifications

type TokenValue = string | number | undefined;

export interface Token {
  range: [number, number];
  value: TokenValue;
}

export interface SpecificationBase {
  type: string;
  rawLine: string;
}

export interface ControlSpecification extends SpecificationBase {
  type: "control";
  controlOptions: string;
}

export interface Directive extends SpecificationBase {
  type: "directive";
  directiveName: Token;
  value?: Token;
}

export interface FileDescriptionSpecification extends SpecificationBase {
  type: "file";
  fileName: Token;
  fileType: Token;
  usage: Token;
}

export interface ExtensionSpecification extends SpecificationBase {
  type: "extension";
  fieldName: Token;
  externalFormat: Token;
}

export interface LineCounterSpecification extends SpecificationBase {
  type: "lineCounter";
  lineCountID: Token;
  associatedField: Token;
}

type DescribeType = "program" | "external" | "structure" | "constant";
type ISpecSubtype = "record" | "field";

export interface InputSpecification extends SpecificationBase {
  type: "input";
  subtype: ISpecSubtype;
  described?: DescribeType;
}

// Works for both program described files and externally described files
export interface RecordIdentifierEntry extends InputSpecification {
  type: "input";
  described: "program" | "external";
  subtype: "record";
  fileName: Token; //7-14
  logicalRelationship: Token; //14-16
  sequenceNumber?: Token; //15-16 -- if this is provided ? program : external
  number?: Token; //17 -- only applies to program described files
  option?: Token; //18 -- only applies to program described files and structs
  // recordIdentifyingIndicator: Token; //20-21
  // TODO: position, not, code part, character
}

export interface InputDataStructureEntry extends InputSpecification {
  type: "input";
  described: "structure";
  subtype: "record";
  name: Token; // 7-12 -- Name of the data structure
  externalDescription?: Token; // 16-17 -- External description name, if applicable
  option?: Token; // 18 -- Option, if applicable
  externalFileName?: Token; // 21-30 -- External file name, if applicable
  occurrences?: Token; // 44-47 -- Occurrence, if applicable
  dsLength?: Token; // 48-51 -- Data structure length, if applicable
}

export interface InputConstantEntry extends InputSpecification {
  type: "input";
  described: "constant";
  subtype: "record";
  constantValue: Token; // 21-42 -- Value of the constant
  constantName: Token; // 53-58 -- Name of the constant
}

export interface InputField extends InputSpecification {
  type: "input";
  subtype: "field";
  described?: never;
  externalField?: Token; // 21-30 -- External field name, if applicable
  initialValue?: Token; // 21-42 -- Initial value, if applicable. Not used if external
  internalDataFormat?: Token; // 43 -- Internal data format, if applicable
  from?: Token; // 44-47 -- From position, if applicable
  to?: Token; // 48-51 -- To position, if applicable
  decimalPositions?: Token; // 52 -- Decimal positions, if applicable
  keywords?: Token; // 44-51 Keywords, if applicable
  name?: Token; // 53-58 -- Field name, data structure name, subfield name, array name, array element, PAGE, PAGE1-PAGE7, IN, or INxx.
}

export type InputSpecifications =
  | RecordIdentifierEntry
  | InputDataStructureEntry
  | InputConstantEntry
  | InputField;

export interface OutputSpecification extends SpecificationBase {
  type: "output";
  subtype: "record" | "field";
}

export interface OutputRecord extends OutputSpecification {
  subtype: "record";
  fileName: Token; // 7-14 -- Name of the file
  logicalRelationship?: Token; // 14-16 -- Logical relationship, if applicable
  recordType: Token; // 15
  recordAdditionDeletionField?: Token; // 16-18
  fetchOverflowSpecifier?: Token; // 16
  excptName?: Token; // 32-37
}

export interface OutputField extends OutputSpecification {
  subtype: "field";
  fieldName: Token; // 32-37 -- Name of the field
  editCode?: Token; // 38
  blankAfter?: Token; // 39
  endPosition?: Token; // 40-43 -- End position of the field
  dataFormat?: Token; // 44 -- Data format of the field
  constOrEditWord?: Token; // 45-70
}

export type OutputSpecifications = OutputRecord | OutputField;

export interface CalculationSpecification extends SpecificationBase {
  type: "calculation";
  operation: Token;
  factor1?: Token;
  factor2?: Token;
  resultField: Token;
  fieldLength?: Token; // If this is specified, it means the field is being defined
  decimalPositions?: Token; // If this is specified, it means the field is numeric
}

export interface EmbeddedSqlSpecification extends SpecificationBase {
  type: "sql";
  end?: boolean;
  contents?: string;
  specs?: EmbeddedSqlSpecification[];
}

// Union type for all specifications
export type Specification =
  | Directive
  | ControlSpecification
  | FileDescriptionSpecification
  | ExtensionSpecification
  | LineCounterSpecification
  | InputSpecifications
  | OutputSpecifications
  | CalculationSpecification
  | EmbeddedSqlSpecification;

const START_SQL = `EXEC SQL`;
const END_SQL = `END-EXEC`;
const LINE_LENGTH = 74;

export function parseSpecification(line: string, startIndex: number = 0): Specification | null {
  const rawLine = line;

  if (line.charAt(6) === `*`) {
    // It's a comment
    return null;
  }

  line = line.padEnd(74, ' ').substring(0, 74); // Ensure line is at least 75 characters long
  const isDirective = line.charAt(6) === '/'; // Check if the line is a directive
  const isContinuation = line.charAt(6) === `+`; // Check if the line is a continuation
  const code = line.charAt(5).toUpperCase();

  const toToken = (start: number, end: number, opts: { default?: TokenValue, isNumber?: boolean } = {}): Token | undefined => {
    const strValue = line.substring(start, end).trim();
    let value: TokenValue = strValue;

    if (opts.isNumber && value) {
      value = Number(value);
      if (isNaN(value)) {
        value = undefined; // If conversion fails, set to undefined
      }
    }

    if (value === undefined || value === '') {
      if (opts.default !== undefined) {
        value = opts.default; // Use default value if provided
      } else {
        return undefined;
      }
    }

    return {
      range: [startIndex + start, startIndex + start + strValue.length],
      value,
    };
  };

  if (isContinuation) {
    return {
      type: `sql`,
      rawLine,
      contents: line.substring(7).trim()
    }

  } else if (isDirective) {
    const nextSpace = line.indexOf(' ', 6);
    const sqlCharacters = toToken(7, 8+7);

    if ([START_SQL, END_SQL].includes(String(sqlCharacters?.value).toUpperCase())) {
      return {
        type: "sql",
        rawLine,
        end: sqlCharacters.value === END_SQL,
        contents: line.substring(15).trim()
      } satisfies EmbeddedSqlSpecification;
    }

    return {
      type: "directive",
      rawLine,
      directiveName: toToken(7, nextSpace),
      value: toToken(nextSpace + 1, LINE_LENGTH)
    };
  }

  switch (code) {
    case 'H': // Control Specification
      return {
        type: "control",
        rawLine,
        controlOptions: line.substring(6, LINE_LENGTH).trim(),
      };

    case 'F': // File Description Specification
      return {
        type: "file",
        rawLine,
        fileName: toToken(6, 14),
        fileType: toToken(14, 15),
        usage: toToken(15, 16),
        // Additional fields can be added here if needed
      };

    case 'E': // Extension Specification
      return {
        type: "extension",
        rawLine,
        fieldName: toToken(10, 18),
        externalFormat: toToken(18, 26),
        // More precise parsing of array/table names etc.
      };

    case 'L': // Line Counter Specification
      return {
        type: "lineCounter",
        rawLine,
        lineCountID: toToken(6, 14),
        associatedField: toToken(14, 17),
        // Form length, overflow line, etc., are next
      };

    case 'I': // Input Specification
      let described: DescribeType;
      let subtype: ISpecSubtype = "field";

      const recordIdentifyingIndicator = toToken(18, 20);
      const dataFormat = toToken(42, 43);

      if (recordIdentifyingIndicator && recordIdentifyingIndicator.value === `DS`) {
        described = "structure";

        const fieldName = toToken(53, 58);

        if (!fieldName) {
          subtype = "record";
        }

      } else if (dataFormat && dataFormat.value === `C`) {
        // If data format is C, it is a const
        described = "constant";
        subtype = "record";

      } else {
        const sequenceNumber = toToken(14, 16);
        if (sequenceNumber) {
          described = "program";
        } else {
          described = "external";
        }

        const fieldName = toToken(6, 14);
        const subFieldName = toToken(52, 58);
        if (!fieldName && !subFieldName) {
          subtype = "record";
        }
      }

      if (subtype === `field`) {
        const initOption = toToken(7, 8);
        const inputField = {
          type: "input",
          rawLine,
          subtype,
          internalDataFormat: toToken(42, 43),
          from: toToken(43, 47, { isNumber: true }),
          to: toToken(47, 51, { isNumber: true }),
          decimalPositions: toToken(51, 52, { isNumber: true }),
          keywords: toToken(43, 51),
          name: toToken(52, 58),
          initialValue: undefined,
          externalField: undefined,
        } satisfies InputField;

        if (initOption && initOption.value === `I`) {
          inputField.initialValue = toToken(20, 42);
        } else {
          inputField.externalField = toToken(20, 30);
        }

        return inputField;

      } else {
        switch (described) {
          case `constant`:
            return {
              type: "input",
              rawLine,
              described,
              subtype: `record`,
              constantValue: toToken(20, 42),
              constantName: toToken(52, 58),
            } satisfies InputConstantEntry;

          case `program`:
          case `external`:
            return {
              type: "input",
              rawLine,
              described,
              subtype,
              fileName: toToken(6, 14),
              logicalRelationship: toToken(13, 16),
              option: toToken(17, 18),
            } satisfies RecordIdentifierEntry;

          case `structure`:
            return {
              type: "input",
              rawLine,
              described,
              subtype,
              name: toToken(6, 12),
              externalDescription: toToken(15, 17),
              option: toToken(17, 18),
              externalFileName: toToken(20, 30),
              occurrences: toToken(43, 47, { isNumber: true }),
              dsLength: toToken(47, 51, { isNumber: true }),
            } satisfies InputDataStructureEntry;
        }
      }
      break;

    case 'O': // Output Specification
      const recordName = toToken(6, 14);

      if (recordName) {
        return {
          type: "output",
          subtype: "record",
          rawLine,
          fileName: toToken(6, 14),
          logicalRelationship: toToken(14, 16),
          recordType: toToken(14, 15),
          recordAdditionDeletionField: toToken(15, 18),
          fetchOverflowSpecifier: toToken(15, 16),
          excptName: toToken(31, 37),
        } satisfies OutputRecord;
      } else {
        return {
          type: "output",
          subtype: "field",
          rawLine,
          fieldName: toToken(31, 37),
          editCode: toToken(37, 38),
          blankAfter: toToken(38, 49),
          endPosition: toToken(39, 43, { isNumber: true }),
          dataFormat: toToken(43, 44),
          constOrEditWord: toToken(44, 70),
        } satisfies OutputField;
      }

    case 'C': // Calculation Specification
      return {
        type: "calculation",
        rawLine,
        operation: toToken(27, 32),
        factor1: toToken(17, 27),
        factor2: toToken(32, 42),
        resultField: toToken(42, 48),
        fieldLength: toToken(48, 51, { isNumber: true }),
        decimalPositions: toToken(51, 52, { isNumber: true }),
      };

    default:
      return null;
  }
}
