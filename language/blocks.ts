
export interface Block {
  indent: number;
  start: number;
  end?: number;
}

const mixed = [`ELSE`, `ELSEIF`, `WHEN`, `OTHER`];
const openers = [`IF`, `IFEQ`, `IFNE`, `IFLE`, `IFGE`, `IFGT`, `IFLT`, `SELECT`, `DOW`, `DOU`, `MONITOR`, ...mixed];
const closers = [`ENDIF`, `ENDDO`, `ENDSL`, `ENDMON`, ...mixed];

/**
 * @param content RPGLE non-**free code
 * @returns Blocks
 */
export function getBlockRanges(content: string): Block[] {
  if (content.length < 6 || content.substring(0, 6).toLowerCase() === '**free') return [];

  const eol = content.includes('\r\n') ? '\r\n' : '\n';
  const lines = content.split(eol);

  let blocks = [];
  let currentBlocks = [];

  let indent = -1;
  let lineNumber = -1;
  let lineIsFree = false;

  let currentOp: string;

  for (let line of lines) {
    lineNumber++;
    lineIsFree = false;
    currentOp = ``;

    if (line.length > 6) {
      const comment = line[6];
      const spec = line[5].toUpperCase();

      if (comment === `*`) {
        // Fixed format comments
        continue;
      }

      if (comment === `/`) {
        // We don't care about directives
        // line = line.substring(6);
        // lineIsFree = true;
        continue;

      } else {
        if (spec === ` `) {
          //Clear out stupid comments
          line = line.substring(7);
          lineIsFree = true;
          
          currentOp = line.substring(0, line.indexOf(` `)).trim();

          if (currentOp.endsWith(`;`)) {
            currentOp = currentOp.substring(0, currentOp.length - 1);
          }

        } else if ([`D`, `P`, `F`, `H`, `O`, `I`].includes(spec)) {
          // TODO: maybe we do care about P later?
          // We don't care about most specifications
          continue;
        } else if (line.length >= 25) {
          currentOp = line.padEnd(40).substring(25, 34).trim();
        }
      }
    }

    if (currentOp) {
      currentOp = currentOp.toUpperCase();
      if (closers.includes(currentOp)) {
        const lastBlock = currentBlocks.pop();
        indent--;
        if (lastBlock) {
          lastBlock.end = lineNumber;
          blocks.push(lastBlock);
        }
      }

      if (openers.includes(currentOp)) {
        indent++;
        currentBlocks.push({ indent, start: lineNumber });
      }
    }
  }

  return blocks;
}