import Parser from "../parser";

/**
 * @param {number} lineNumber 
 * @param {number} startingPos 
 * @param {string} value 
 * @param {string} [type]
 * @returns {import("../types").Token|undefined}
 */
function calculateToken(lineNumber, startingPos, value, type) {
  let resultValue = value.trim();

  if (resultValue === ``) {
    return;
  }

  if (type === `special-ind`) {
    type = `special`;
    resultValue = `*IN` + resultValue;
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

/**
 * @param {number} lineNumber
 * @param {number} lineIndex
 * @param {string} content
 */
export function parseFLine(lineNumber, lineIndex, content) {
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

/**
 * @param {string} content 
 */
export function parseCLine(lineNumber, lineIndex, content) {
  content = content.padEnd(80);
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

/**
 * @param {string} content
 */
export function parseDLine(lineNumber, lineIndex, content) {
  content = content.padEnd(80);
  const longForm = content.substring(6).trimEnd();
  const potentialName = longForm.endsWith(`...`) ? calculateToken(lineNumber, lineIndex+6, longForm.substring(0, longForm.length - 3)) : undefined;
  const name = content.substr(6, 15);
  const pos = content.substr(25, 7);
  const len = content.substr(32, 7);
  const type = content.substr(39, 1);
  const decimals = content.substr(40, 3);
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

/**
 * @param {string} content
 */
export function parsePLine(content, lineNumber, lineIndex) {
  content = content.padEnd(80);
  const name = content.substr(6, 16)
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

export function prettyTypeFromToken(dSpec) {
  return getPrettyType({
    type: dSpec.type ? dSpec.type.value : ``,
    keywords: dSpec.keywords,
    len: dSpec.len ? dSpec.len.value : ``,
    pos: dSpec.pos ? dSpec.pos.value : ``,
    decimals: dSpec.decimals ? dSpec.decimals.value : ``,
    field: dSpec.field ? dSpec.field.value : ``
  })
}

/**
 * 
 * @param {{type: string, keywords: import("../parserTypes").Keywords, len: string, pos: string, decimals: string, field: string}} lineData 
 * @returns {import("../parserTypes").Keywords}
 */
export function getPrettyType(lineData) {
  let outType = ``;
  let length = Number(lineData.len);

  if (lineData.pos) {
    length = length - Number(lineData.pos) + 1;
  }

  if (!lineData.decimals) {
    lineData.decimals = ``;
  }

  switch (lineData.type.toUpperCase()) {
  case `A`:
    if (Number(lineData.keywords[`VARYING`]) >= 0) {
      outType = `Varchar`;
	  // For VARCHAR, the field defined will have a 2-byte integer field appended to the beginning of it that will contain the length of the data that is valid in the data portion of the field
      length -= 2;			  
    } else {
      outType = `Char`;
    }
    outType += `(` + length + `)`;
    break;
  case `B`:
    if (lineData.pos != ``) {
      // When using positions binary decimal is only 2 or 4 
      // This equates to 4 or 9 in free
      if (Number(lineData.len) == 4) {
        outType = `Bindec(9)`;
      } else {
        outType = `Bindec(4)`;
      }    
    } else {
      // Not using positions, then the length is correct
      outType = `Bindec` + `(` + lineData.len + `)`;
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
    outType = `Packed` + `(` + length + `:` + lineData.decimals + `)`;
    break;
  case `S`:
    outType = `Zoned` + `(` + length + `:` + lineData.decimals + `)`;
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
      if (lineData.decimals == ``) {
        if (Number(lineData.keywords[`VARYING`]) >= 0) {
          outType = `Varchar`;
        } else {
          outType = `Char`;
        }
        outType += `(` + length + `)`;
      } else {
        if (lineData.field === ``) {
          // Means it's a subfield.
          outType = `Zoned` + `(` + length + `:` + lineData.decimals + `)`;
        } else {
          // Means it's a standalone.
          outType = `Packed` + `(` + length + `:` + lineData.decimals + `)`;
        }
      }
    }
    break;
  }

  return Parser.expandKeywords(Parser.getTokens(outType));
}