import Parser from "../ile/parser";
import { Keywords } from "../parserTypes";
import { Token } from "../types";

function calculateToken(lineNumber: number, startingPos: number, value: string, type?: string): Token | undefined {
  let resultValue = value.trim();

  if (resultValue === ``) {
    return;
  }

  switch (type) {
  case `special-ind`:
    type = `special`;
    resultValue = `*IN` + resultValue;
    break;

  case `opcode`:
    // Remove extender from opcode
    if (resultValue.includes(`(`) && resultValue.includes(`)`)) {
      resultValue = resultValue.substring(0, resultValue.indexOf(`(`));
    }
    break;
  }

  const frontSpaces = value.length - value.trimStart().length;
  const start = startingPos + frontSpaces;
  return {
    type: type || `word`,
    value: resultValue,
    range: {
      start,
      end: start + resultValue.length,
      line: lineNumber
    }
  }
}

export function parseFLine(lineNumber: number, lineIndex: number, content: string) {
  const name = content.substr(6, 10); //File name
  // const type = content.substr(16, 1).toUpperCase(); // I, U, O, C
  // const field = content.substr(33, 1).toUpperCase(); //KEYED
  // const device = content.substr(35, 7).toUpperCase().trim(); //device: DISK, WORKSTN
  const keywords = content.substr(43);

  return {
    name: calculateToken(lineNumber, lineIndex+6, name),
    keywords: Parser.expandKeywords(Parser.getTokens(keywords))
  };
}

export function parseCLine(lineNumber: number, lineIndex: number, content: string) {
  content = content.padEnd(80);
  const clIndicator = content.substr(7, 8).toUpperCase();
  const indicator = content.substr(9, 11);
  const factor1 = content.substr(11, 14);
  const opcode = content.substr(25, 10).toUpperCase();
  const factor2 = content.substr(35, 14);
  const extended = content.substr(35);
  const result = content.substr(49, 14);

  const fieldLength = content.substr(63, 5);
  const fieldDecimals = content.substr(68, 2);

  const ind1 = content.substr(70, 2);
  const ind2 = content.substr(72, 2);
  const ind3 = content.substr(74, 2);

  return {
    clIndicator: calculateToken(lineNumber, lineIndex+7, clIndicator, `special-ind`),
    indicator: calculateToken(lineNumber, lineIndex+9, indicator, `special-ind`),
    opcode: calculateToken(lineNumber, lineIndex+25, opcode, `opcode`),
    factor1: calculateToken(lineNumber, lineIndex+11, factor1),
    factor2: calculateToken(lineNumber, lineIndex+35, factor2),
    result: calculateToken(lineNumber, lineIndex+49, result),
    extended: calculateToken(lineNumber, lineIndex+35, extended),

    fieldLength: calculateToken(lineNumber, lineIndex+63, fieldLength),
    fieldDecimals: calculateToken(lineNumber, lineIndex+68, fieldDecimals),

    ind1: calculateToken(lineNumber, lineIndex+70, ind1, `special-ind`),
    ind2: calculateToken(lineNumber, lineIndex+72, ind2, `special-ind`),
    ind3: calculateToken(lineNumber, lineIndex+74, ind3, `special-ind`)
  };
}

export function parseDLine(lineNumber: number, lineIndex: number, content: string) {
  content = content.padEnd(80);
  const longForm = content.substring(6).trimEnd();
  const potentialName = longForm.endsWith(`...`) ? calculateToken(lineNumber, lineIndex+6, longForm.substring(0, longForm.length - 3)) : undefined;
  const name = content.substr(6, 15);
  const pos = content.substr(25, 7);
  const len = content.substr(32, 7);
  const type = content.substr(39, 1);
  const decimals = content.substr(40, 2);
  const field = content.substr(23, 2).toUpperCase();
  const keywords = content.substr(43);
  const keywordTokens = Parser.getTokens(keywords, lineNumber, lineIndex+43);

  return {
    potentialName: potentialName,
    name: calculateToken(lineNumber, lineIndex+6, name),
    pos: calculateToken(lineNumber, lineIndex+25, pos),
    len: calculateToken(lineNumber, lineIndex+32, len),
    type: calculateToken(lineNumber, lineIndex+39, type),
    decimals: calculateToken(lineNumber, lineIndex+40, decimals),
    field: calculateToken(lineNumber, lineIndex+23, field),
    keywordsRaw: keywordTokens,
    keywords: Parser.expandKeywords(keywordTokens, field.trim() === `C`)
  };
}

export function parsePLine(content: string, lineNumber: number, lineIndex: number) {
  content = content.padEnd(80);
  let name = content.substr(6, 16).trimEnd();
  if (name.endsWith(`...`)) {
    name = name.substring(0, name.length - 3);
  }

  const longForm = content.substring(6).trimEnd();
  const potentialName = longForm.endsWith(`...`) ? calculateToken(lineNumber, lineIndex+6, longForm.substring(0, longForm.length - 3)) : undefined;
  const start = content[23].toUpperCase() === `B`;
  const keywords = content.substr(43)
  const keywordTokens = Parser.getTokens(keywords, lineNumber, lineIndex+43);

  return {
    name: calculateToken(lineNumber, lineIndex+6, name),
    potentialName,
    keywordsRaw: keywordTokens,
    keywords: Parser.expandKeywords(keywordTokens),
    start
  };
}

/**
 * Detect O-Spec line type
 */
function detectOSpecType(filename: string, type: string, andOrKeyword:string, fieldName: string, endPos: string, constant: string): 'O' | 'OAnd' | 'OF' | 'OFC' | 'OXF' {
  const conditioalOperators = ['AND', 'OR'];
  if (filename || type) 
    return 'O'; 
  else if (conditioalOperators.includes(andOrKeyword)) 
    return 'OAnd';
  else if (!fieldName && constant)//if no field name and constant literal is present its OFC or Output constant
    return 'OFC';                 //This is need not be part of outline
  else if (fieldName && endPos)
    return 'OF';
  else
    return 'OXF';
}

//TODO: return type annotations are required to be 
//updated once feedback wrt fields required in outline is received
/**
 * Parse O-Spec (Output Specification) line
 * @param {number} lineNumber
 * @param {number} lineIndex
 * @param {string} content
 */
export function parseOLine(lineNumber: number, lineIndex: number, content: string): any {
  content = content.padEnd(80);

  const fileName = content.substring(6, 16).trim();//col[7,..,16] which o/p file or record this spec belongs to
  const type = content.substring(16, 17).trim();//col[17]D:Detailed o/p, T:Total o/p, H:Heading, E:Exception o/p
  const andOrKeyword = content.substring(18, 20).trim().toUpperCase();//col[16,..,20]conditioal operators
  const fieldName = content.substring(29, 43).trim();//col[30,43] var name (14 chars; col 44 is edit code)
  const endPos = content.substring(46, 52).trim();//col[47,52]
  const constant = content.substring(52).trim();//col[53,..] constant name

  const lineType = detectOSpecType(fileName, type, andOrKeyword, fieldName, endPos, constant);
  
  //OFC is not handled as its not supposed to be part of outline
  //currently 3 diff cases are maintained , once feedback for desc is received
  //OF and OXF case should look diff , if no changes then they can be combined
  //as 1 case as it is repetitative
  switch(lineType) {
    case 'O': {
       return {
        fileName: calculateToken(lineNumber, lineIndex+6, fileName),
        fieldName: calculateToken(lineNumber, lineIndex+29, fieldName),
        type,
        endPos,
       };
    }
    case 'OF': {
      return {
        fieldName: calculateToken(lineNumber, lineIndex+29, fieldName),
      };
    }
    case 'OXF': {
      return {
        fieldName: calculateToken(lineNumber, lineIndex+29, fieldName),
      };
    }
    default: return null;
  }
}

export function prettyTypeFromDSpecTokens(dSpec) {
  return getPrettyType({
    type: dSpec.type ? dSpec.type.value : ``,
    keywords: dSpec.keywords,
    len: dSpec.len ? dSpec.len.value : ``,
    pos: dSpec.pos ? dSpec.pos.value : ``,
    decimals: dSpec.decimals ? dSpec.decimals.value : ``,
    field: dSpec.field ? dSpec.field.value : ``
  });
}

export function prettyTypeFromISpecTokens(iSpec) {
  return getPrettyType({
    type: iSpec.dataFormat ? iSpec.dataFormat.value : ``,
    keywords: {},
    len: iSpec.length ? iSpec.length.value : ``,
    pos: iSpec.fieldLocation ? iSpec.fieldLocation.value : ``,
    decimals: iSpec.decimalPositions ? iSpec.decimalPositions.value : ``,
    field: ``
  })
}

export function getPrettyType(lineData: {type: string, keywords: Keywords, len: string, pos: string, decimals: string, field: string}): Keywords {
  let outType = ``;
  let length = Number(lineData.len);

  if (lineData.pos) {
    length = length - Number(lineData.pos) + 1;
  }

  const hasDecimals = lineData.decimals !== undefined && lineData.decimals !== null && lineData.decimals !== ``;
  if (!lineData.decimals) {
    lineData.decimals = `0`;
  }

  switch (lineData.type.toUpperCase()) {
  case `A`:
    if (Number(lineData.keywords[`VARYING`]) >= 0) {
      outType = `Varchar`;
      length -= 2;
    } else {
      outType = `Char`;
    }
    outType += `(` + length + `)`;
    break;
  case `B`:
    if (lineData.pos != ``) {
      if (Number(lineData.len) == 4) {
        outType = `Bindec(9` + (lineData.decimals ? `:` + lineData.decimals : ``) + `)`;
      } else {
        outType = `Bindec(4` + (lineData.decimals ? `:` + lineData.decimals : ``) + `)`;
      }
    } else {
      outType = `Bindec` + `(` + lineData.len + (lineData.decimals ? `:` + lineData.decimals : ``) + `)`;
    }
    break;
  case `C`:
    outType = `Ucs2` + `(` + lineData.len + `)`;
    break;
  case `D`:
    outType = `Date`;
    break;
  case `L`:
    outType = `Date`;
    break;
  case `F`:
    outType = `Float` + `(` + lineData.len + `)`;
    break;
  case `G`:
    if (Number(lineData.keywords[`VARYING`]) >= 0) {
      outType = `Vargraph`;
    } else {
      outType = `Graph`;
    }
    outType += `(` + lineData.len + `)`;
    break;
  case `I`:
    switch (length) {
    case 1:
      outType = `Int(3)`;
      break;
    case 2:
      outType = `Int(5)`;
      break;
    case 4:
      outType = `Int(10)`;
      break;
    case 8:
      outType = `Int(20)`;
      break;
    default:
      outType = `Int(` + length + `)`;
    }
    break;
  case `N`:
    outType = `Ind`;
    break;
  case `P`:
    outType = `Packed` + `(` + length + `:` + (lineData.decimals || `0`) + `)`;
    break;
  case `S`:
    outType = `Zoned` + `(` + length + `:` + (lineData.decimals || `0`) + `)`;
    break;
  case `T`:
    outType = `Time`;
    break;
  case `U`:
    switch (length) {
    case 1:
      outType = `Uns(3)`;
      break;
    case 2:
      outType = `Uns(5)`;
      break;
    case 4:
      outType = `Uns(10)`;
      break;
    case 8:
      outType = `Uns(20)`;
      break;
    default:
      outType = `Uns(` + length + `)`;
    }
    break;
  case `Z`:
    outType = `Timestamp`;
    outType += `(` + length + `)`;
    break;
  case `*`:
    outType = `Pointer`;
    break;
  case ``:
    if (lineData.field == `DS`) {
      outType = `lineData.Len(` + lineData.len + `)`;
    } else if (lineData.len != ``) {
      if (!hasDecimals) {
        if (Number(lineData.keywords[`VARYING`]) >= 0) {
          outType = `Varchar`;
        } else {
          outType = `Char`;
        }
        outType += `(` + length + `)`;
      } else {
        if (lineData.field === ``) {
          outType = `Zoned` + `(` + length + `:` + (lineData.decimals || `0`) + `)`;
        } else {
          outType = `Packed` + `(` + length + `:` + (lineData.decimals || `0`) + `)`;
        }
      }
    }
    break;
  }

  return Parser.expandKeywords(Parser.getTokens(outType));
}

// https://www.ibm.com/docs/fr/i/7.4.0?topic=specifications-input
export function parseISpec(lineNumber: number, lineIndex: number, content: string) {
  content = content.padEnd(80);
  let iType: "programRecord"|"programField"|"externalRecord"|"externalField"|undefined;

  const name = content.substring(6, 16).trimEnd();

  if (name) {
    const externalReserved = content.substring(15, 20).trim();
    if (externalReserved) {
      iType = `programRecord`;
    } else {
      iType = `externalRecord`;
    }
  } else {
    const sequencingIndicator = content.substring(16, 18).trim();
    if (sequencingIndicator) {
      return { iType: `continuationRecord` as const };
    }

    const externalName = content.substring(20, 30).trim();
    if (externalName) {
      iType = `externalField`;
    } else {
      iType = `programField`;
    }
  }

  const getPart = (start: number, end: number, type?: string) => {
    return calculateToken(lineNumber, lineIndex + (start-1), content.substring(start-1, end).trimEnd(), type);
  }

  switch (iType) {
    case `programRecord`:
      return {
        iType,
        name: getPart(7, 16),
        logicalRelationship: getPart(16, 18),
        sequence: getPart(18, 19),
        number: getPart(19, 20),
        option: getPart(20, 21),
        recordIdentifyingIndicator: getPart(21, 23, `special-ind`)
      }
    case `programField`:
      return {
        iType,
        dataAttributes: getPart(31, 34),
        dateTimeSeparator: getPart(35, 36),
        dataFormat: getPart(36, 37),
        fieldLocation: getPart(37, 41),
        length: getPart(42, 46),
        decimalPositions: getPart(47, 48),
        fieldName: getPart(49, 62),
        controlLevel: getPart(63, 64, `special-ind`),
        matchingFields: getPart(65, 66, `special-ind`),
        fieldRecordRelation: getPart(67, 68, `special-ind`),
        fieldIndicators: [
          getPart(69, 70, `special-ind`),
          getPart(71, 72, `special-ind`),
          getPart(73, 74, `special-ind`)
        ]
      }
    case `externalRecord`:
      return {
        iType,
        name: getPart(7, 16),
        recordIdentifyingIndicator: getPart(21, 22, `special-ind`),
      };
    case `externalField`:
      return {
        iType,
        externalName: getPart(21, 30),
        fieldName: getPart(49, 62),
        controlLevel: getPart(63, 64, `special-ind`),
        matchingFields: getPart(65, 66, `special-ind`),
        fieldIndicators: [
          getPart(69, 70, `special-ind`),
          getPart(71, 72, `special-ind`),
          getPart(73, 74, `special-ind`)
        ]
      };
  }
}
