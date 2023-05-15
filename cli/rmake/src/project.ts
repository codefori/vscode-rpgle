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
	compiles: {[ext: string]: CompileData},
	binders: string[];
}

export class Project {
	private settings: iProject;

	constructor(private cwd: string, private targets: Targets) {
		this.settings = Project.getDefaultSettings();

		this.setupSettings();
	}

	public static getDefaultSettings(): iProject {
		return {
			binders: [],
			includePaths: [],
			compiles: {
				"pgm.rpgle": {
					becomes: `PGM`,
					dir: `qrpglesrc`,
					command: `CRTBNDRPG PGM($(BIN_LIB)/$*) SRCSTMF('$<') OPTION(*EVENTF) DBGVIEW(*SOURCE) TGTRLS(*CURRENT) TGTCCSID(*JOB) BNDDIR($(BNDDIR)) DFTACTGRP(*no)`
				},
				"pgm.sqlrpgle": {
					becomes: "PGM",
					dir: "qrpglesrc",
					command: `CRTSQLRPGI OBJ($(BIN_LIB)/$*) SRCSTMF('$<') COMMIT(*NONE) DBGVIEW(*SOURCE) OPTION(*EVENTF) COMPILEOPT('BNDDIR($(BNDDIR)) DFTACTGRP(*no)')`
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
					command: `RUNSQLSTM SRCSTMF('$<') COMMIT(*NONE)`
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
					command: `CRTSRVPGM SRVPGM($(BIN_LIB)/$*) MODULE(*SRVPGM) EXPORT(*ALL) BNDDIR($(BNDDIR))`
				},
				bnddir: {
					becomes: `BNDDIR`,
					commands: [
						`-system -q "CRTBNDDIR BNDDIR($(BIN_LIB)/$*)"`,
						`-system -q "ADDBNDDIRE BNDDIR($(BIN_LIB)/$*) OBJ($(patsubst %.srvpgm,(*LIBL/% *SRVPGM *IMMED),$^))`
					]
				}
			}
		};
	}

	private setupSettings() {
		try {
			const content = readFileSync(path.join(this.cwd, `iproj.json`), {encoding: `utf-8`});
			const asJson: iProject = JSON.parse(content);

			if (asJson.includePaths) {
				this.settings.includePaths = asJson.includePaths;
			}

			if (asJson.binders) {
				this.settings.binders = asJson.binders;
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

	public getMakefile() {
		return [
			...this.generateHeader(),
			``,
			...this.generateTargets(),
			``,
			...this.generateGenericRules()
		];
	}

	public generateHeader(): string[] {
		return [
			`BIN_LIB=DEV`,
			`APP_BNDDIR=$(BIN_LIB)/APP`,
			``,
			`INCDIR="${this.settings.includePaths.join(`:`)}"`,
			`BNDDIR=${this.targets.binderRequired() ? [`($(APP_BNDDIR))`, ...this.settings.binders.map(b => `(${b})`)].join(` `) : `*NONE`}`,
			`PREPATH=/QSYS.LIB/$(BIN_LIB).LIB`,
			`SHELL=/QOpenSys/usr/bin/qsh`,
		];
	}

	public generateTargets(): string[] {
		let lines = [];

		const allPrograms = this.targets.getObjects(`PGM`);

		if (allPrograms.length > 0) {
			lines.push(
				`all: ${allPrograms.map(dep => `$(PREPATH)/${dep.name}.${dep.type}`).join(` `)}`,
				``
			)
		}

		for (const target of this.targets.getDeps()) {
			if (target.deps.length > 0) {
				lines.push(
					`$(PREPATH)/${target.name}.${target.type}: ${target.deps.map(dep => `$(PREPATH)/${dep.name}.${dep.type}`).join(` `)}`
				)
			}
		};

		return lines;
	}

	public generateGenericRules(): string[] {
		let lines = [];

		for (const entry of Object.entries(this.settings.compiles)) {
			const [type, data] = entry;

			// Only used for member copies
			const qsysTempName: string|undefined = (data.dir && data.dir.length > 10 ? data.dir.substring(0, 10) : data.dir);

			lines.push(
				`$(PREPATH)/%.${data.becomes}: ${data.dir ? path.posix.join(data.dir, `%.${type}`) : ``}`,
				...(qsysTempName && data.member ?
					[
						`\t-system -qi "CRTSRCPF FILE($(BIN_LIB)/${qsysTempName}) RCDLEN(112)"`,
						`\tsystem "CPYFRMSTMF FROMSTMF('./qddssrc/$*.dspf') TOMBR('$(PREPATH)/${qsysTempName}.FILE/$*.MBR') MBROPT(*REPLACE)"`
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