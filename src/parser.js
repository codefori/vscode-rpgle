
const vscode = require(`vscode`);

const Output = require(`./output`);
const getInstance = require(`./base`);
const Parser = require(`./language/parser`);
const Declaration = require(`./language/models/declaration`);
const Fixed = require(`./language/models/fixed`);

/**
 * 
 * @param {string} table 
 * @returns {Promise<Declaration[]>}
 */
const fetchTables = async (table) => {
  const instance = getInstance();

  if (instance) {
    const connection = instance.getConnection();
    if (connection) {
      const content = instance.getContent();
      const config = instance.getConfig();

      const dateStr = Date.now().toString().substr(-6);
      const randomFile = `R${table.substring(0, 3)}${dateStr}`.substring(0, 10);
      const fullPath = `${config.tempLibrary}/${randomFile}`;

      Output.write(`Temp OUTFILE: ${fullPath}`);

      await vscode.commands.executeCommand(`code-for-ibmi.runCommand`, {
        environment: `ile`,
        command: `DSPFFD FILE(*LIBL/${table}) OUTPUT(*OUTFILE) OUTFILE(${fullPath})`
      });

      Output.write(`Temp OUTFILE created.`);

      /** @type {object[]} */
      const data = await content.getTable(config.tempLibrary, randomFile);

      Output.write(`Temp OUTFILE read. ${data.length} rows.`);

      // Delete this file when we're done.. slower but nicer to the OS memory :)
      vscode.commands.executeCommand(`code-for-ibmi.runCommand`, {
        environment: `ile`,
        command: `DLTOBJ OBJ(${fullPath}) OBJTYPE(*FILE)`
      }).then(out => {
        Output.write(`Temp OUTFILE deleted. ` + JSON.stringify(out));
      });

      /** @type {{[name: string]: Declaration}} */
      let recordFormats = {};

      data.forEach(row => {
        const {
          WHNAME: formatName,
          WHFLDE: name,
          WHFLDT: type,
          WHFLDB: strLength, 
          WHFLDD: digits,
          WHFLDP: decimals,
          WHFTXT: text,
        } = row;

        if (name.startsWith(`*`)) return;

        let recordFormat;
        if (recordFormats[formatName]) {
          recordFormat = recordFormats[formatName];
        } else {
          recordFormat = new Declaration(`struct`);
          recordFormat.name = formatName;
          recordFormats[formatName] = recordFormat;
        }

        const currentSubfield = new Declaration(`subitem`);
        currentSubfield.name = name;
        currentSubfield.keywords = [Fixed.getPrettyType({
          type,
          len: digits === `0` ? strLength : digits,
          decimals: decimals,
          keywords: [],
        })];
        currentSubfield.description = text.trim();

        recordFormat.subItems.push(currentSubfield);
      });

      return Object.values(recordFormats);
    }
  }

  return [];
}

const rpgleParser = new Parser();
rpgleParser.setTableFetch(fetchTables);


exports.Parser = rpgleParser;
