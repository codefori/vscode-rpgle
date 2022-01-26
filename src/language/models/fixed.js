exports.parseLine = (line) => {
  /** @type {string} */
  const potentialName = line.substring(6).trim();
  /** @type {string} */
  const name = line.substr(6, 15).trim();
  /** @type {string} */
  const pos = line.substr(19, 3).trim();
  /** @type {string} */
  const len = line.substr(32, 7).trim();
  /** @type {string} */
  const type = line.substr(39, 1).trim();
  /** @type {string} */
  const decimals = line.substr(40, 3).trim();
  /** @type {string} */
  const field = line.substr(23, 2).trim().toUpperCase();
  /** @type {string} */
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
  }
}

exports.getPrettyType = (lineData) => {
  let outType = ``;

  switch (lineData.type.toUpperCase()) {
  case `A`:
    if (lineData.keywords.indexOf(`VARYING`) >= 0) {
      lineData.keywords = lineData.keywords.replace(/varying/ig, ``);
      outType = `Varchar`;
    } else {
      outType = `Char`;
    }
    outType += `(` + lineData.len + `)`;
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
  case `F`:
    outType = `Float` + `(` + lineData.len + `)`;
    break;
  case `G`:
    if (lineData.keywords.toUpperCase().indexOf(`VARYING`) >= 0) {
      lineData.keywords = lineData.keywords.replace(/varying/ig, ``);
      outType = `Vargraph`;
    } else {
      outType = `Graph`;
    }
    outType += `(` + lineData.len + `)`;
    break;
  case `I`:
    switch (lineData.len) {
    case `1`:
      outType = `Int(3)`;
      break;
    case `2`:
      outType = `Int(5)`;
      break;
    case `4`:
      outType = `Int(10)`;
      break;
    case `8`:
      outType = `Int(20)`;
      break;
    default:
      outType = `Int(` + lineData.len + `)`;
    }
    break;
  case `N`:
    outType = `Ind`;
    break;
  case `P`:
    if (lineData.pos != ``) {
      // When using positions packed lineData.length is one less than double the bytes
      outType = `Packed` + `(` + String(Number(lineData.len)*2-1)  + `:` + lineData.decimals + `)`;
    } else {
      // Not using positions, then the lineData.length is correct
      outType = `Packed` + `(` + lineData.len + `:` + lineData.decimals + `)`;
    }  
    break;
  case `S`:
    outType = `Zoned` + `(` + lineData.len + `:` + lineData.decimals + `)`;
    break;
  case `T`:
    outType = `Time`;
    break;
  case `U`:
    outType = `Uns` + `(` + lineData.len + `)`;
    break;
  case `Z`:
    outType = `Timestamp`;
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
        outType += `(` + lineData.len + `)`;
      } else {
        if (lineData.field === `DS`) {
          outType = `Zoned` + `(` + lineData.len + `:` + lineData.decimals + `)`;
        } else {
          outType = `Packed` + `(` + lineData.len + `:` + lineData.decimals + `)`;
        }
      }
    }
    break;
  }

  return outType.toUpperCase();
}