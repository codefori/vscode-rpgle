
/**
 * @param {string} line 
 */
export function parseFLine(line) {
  const name = line.substr(6, 10).trim(); //File name
  // const type = line.substr(16, 1).toUpperCase(); // I, U, O, C
  // const field = line.substr(33, 1).toUpperCase(); //KEYED
  // const device = line.substr(35, 7).toUpperCase().trim(); //device: DISK, WORKSTN
  const keywords = line.substr(43).trim();
  const splitKeywords = keywords.split(` `).filter(word => word !== ``);

  return {
    name, 
    keywords: splitKeywords
  };
}

/**
 * @param {string} line 
 */
export function parseCLine(line) {
  line = line.padEnd(80);
  const factor1 = line.substr(11, 14).trim();
  const opcode = line.substr(25, 10).trim().toUpperCase();
  const factor2 = line.substr(35, 14).trim();
  const extended = line.substr(35).trim();
  const result = line.substr(49, 14).trim();

  const ind1 = line.substr(70, 2).trim();
  const ind2 = line.substr(72, 2).trim();
  const ind3 = line.substr(74, 2).trim();

  return {
    opcode,
    factor1,
    factor2,
    result,
    extended,
    ind1,
    ind2,
    ind3
  };
}

/**
 * @param {string} line
 */
export function parseDLine(line) {
  line = line.padEnd(80);
  const potentialName = line.substring(6).trim();
  const name = line.substr(6, 15).trim();
  const pos = line.substr(25, 7).trim();
  const len = line.substr(32, 7).trim();
  const type = line.substr(39, 1).trim();
  const decimals = line.substr(40, 3).trim();
  const field = line.substr(23, 2).trim().toUpperCase();
  const keywords = line.substr(43).trim().toUpperCase();
  const splitKeywords = keywords.split(` `).filter(word => word !== ``);

  return {
    potentialName,
    name,
    pos,
    len,
    type,
    decimals,
    field,
    keywords: splitKeywords
  };
}

/**
 * @param {string} line
 */
export function parsePLine(line) {
  line = line.padEnd(80);
  const name = line.substr(6, 16).trim();
  const potentialName = line.substring(6).trim();
  const start = line[23].toUpperCase() === `B`;
  const keywords = line.substr(43).trim().toUpperCase();
  const splitKeywords = keywords.split(` `).filter(word => word !== ``);

  return {
    name,
    potentialName,
    keywords: splitKeywords,
    start
  };
}

/**
 * 
 * @param {{type: string, keywords: string[], len: string, pos?: string, decimals?: string, field?: string}} lineData 
 * @returns {string}
 */
export function getPrettyType(lineData) {
  let outType = ``;
  let length = Number(lineData.len);

  if (lineData.pos) {
    length = length - Number(lineData.pos) + 1;
  }

  switch (lineData.type.toUpperCase()) {
  case `A`:
    if (lineData.keywords.indexOf(`VARYING`) >= 0) {
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
    if (lineData.keywords.indexOf(`VARYING`) >= 0) {
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
        if (lineData.keywords.indexOf(`VARYING`) >= 0) {
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

  return outType.toUpperCase();
}