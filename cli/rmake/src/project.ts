import { readFileSync } from 'fs';
import path from 'path';
import { Targets } from './targets';

interface CompileData {
	becomes: string;
	/** `dir` is used to indicate where the source lives for this object */
	dir?: string;
	/** `member` will copy the source to a temp member first */
	member?: boolean,
	/** `commands` do not respect the library list */
	commands?: string[]
	/** `command` does respect the library list */
	command?: string;
};

interface iProject {
	includePaths: string[];
	compiles: {[ext: string]: CompileData}
}

export class Project {
	private settings: iProject;
	constructor(private cwd: string, private targets: Targets) {
		this.settings = {
			includePaths: [],
			compiles: {
				".pgm.rpgle": {
					becomes: `PGM`,
					dir: `qrpglesrc`,
					command: `CRTBNDRPG ...`
				},
				".pgm.sqlrpgle": {
					becomes: "PGM",
					dir: "qrpglesrc",
					command: "CRTSQLRPGI OBJTYPE(*PGM)"
				},
				dspf: {
					becomes: "FILE",
					dir: "qddssrc",
					member: true,
					command: "CRTDSPF FILE($(BIN_LIB)/$*) SRCFILE($(BIN_LIB)/qddssrc) SRCMBR($*)"
				},
				sql: {
					becomes: `FILE`,
					dir: `qsqlsrc`,
					command: `system "RUNSQLSTM SRCSTMF('$<') COMMIT(*NONE)"`
				},
				table: {
					becomes: `FILE`,
					dir: `qsqlsrc`,
					command: `system "RUNSQLSTM SRCSTMF('$<') COMMIT(*NONE)"`
				},
				srvpgm: {
					becomes: `SRVPGM`,
					commands: [
						`-system -q "RMVBNDDIRE BNDDIR($(BIN_LIB)/$*) OBJ($(BIN_LIB)/$* *SRVPGM)"`,
						`-system "DLTOBJ OBJ($(BIN_LIB)/$*) OBJTYPE(*SRVPGM)"`
					],
					command: `system "CRTSRVPGM SRVPGM($(BIN_LIB)/$*) MODULE(*SRVPGM) EXPORT(*ALL) BNDDIR($(BIN_LIB)/$(BNDDIR))"`
				},
				bnddir: {
					becomes: `BNDDIR`,
					commands: [
						`-system -q "CRTBNDDIR BNDDIR($(BIN_LIB)/$*)"`,
						`-system -q "ADDBNDDIRE BNDDIR($(BIN_LIB)/$*) OBJ($(patsubst %.srvpgm,(*LIBL/% *SRVPGM *IMMED),$^))`
					]
				}
			}
		}

		this.setupSettings();
	}

	private setupSettings() {
		try {
			const content = readFileSync(path.join(this.cwd, `iproj.json`), {encoding: `utf-8`});
			const asJson: iProject = JSON.parse(content);

			if (asJson.includePaths) {
				this.settings.includePaths = asJson.includePaths;
			}

			if (asJson.compiles) {
				for (const [ext, data] of Object.entries(asJson.compiles)) {
					// We don't want to fully overwrite the default settings,
					// perhaps the user is only changing the `dir`?
					this.settings.compiles[ext] = {
						...(this.settings.compiles[ext] || {}),
						...data
					};
				}
			}
		} catch (e) {
			console.log(`Failed to read 'iproj.json'.`);
		}
	}

	private generateTargets(): string[] {
		let lines = [];

		// BIN_LIB=DEV
		// PREPATH=/QSYS.LIB/$(BIN_LIB).LIB

		for (const target of this.targets.getDeps()) {
			lines.push(
				`$(PREPATH)/${target.name}.${target.type}: ${target.deps.map(dep => `$(PREPATH)/${dep.name}.${dep.type}`).join(` `)}`
			)
		};

		return lines;
	}

	private generateRules(): string[] {
		let lines = [];

		for (const entry of Object.entries(this.settings.compiles)) {
			const [type, data] = entry;

			// Only used for member copies
			const qsysTempName = (data.dir.length > 10 ? data.dir.substring(0, 10) : data.dir);

			lines.push(
				`$(PREPATH)/%.${data.becomes}: ${data.dir ? path.posix.join(data.dir, `%.${type}`) : ``}`,
				...(data.member ?
					[
						`-system -qi "CRTSRCPF FILE($(BIN_LIB)/${qsysTempName}) RCDLEN(112)"`,
						`system "CPYFRMSTMF FROMSTMF('./qddssrc/$*.dspf') TOMBR('$(PREPATH)/${qsysTempName}.FILE/$*.MBR') MBROPT(*REPLACE)"`
					] : []),
				...(data.commands ? data.commands.map(cmd => `\t${cmd}`) : [] ),
				...(data.command ?
					[
						`\tliblist -c $(BIN_LIB);\\`,
						`\tsystem "${data.command}"` // TODO: write the spool file somewhere?
					]
					: []
					)
			)

		}

		return lines;
	}
}