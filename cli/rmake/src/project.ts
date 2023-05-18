import { existsSync, readFileSync } from 'fs';
import path from 'path';
import { ObjectType, Targets } from './targets';

interface CompileData {
	/** if the non-source object now requires source */
	targetSource?: string;
	becomes: ObjectType;
	/** `member` will copy the source to a temp member first */
	member?: boolean,
	/** `commands` do not respect the library list and run before 'command' */
	commands?: string[]
	/** `command` does respect the library list */
	command?: string;
	/** used if the commands are built up from source */
	commandSource?: boolean;
};

interface iProject {
	includePaths?: string[];
	compiles?: { [ext: string]: CompileData },
	binders?: string[];
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
					command: `CRTBNDRPG PGM($(BIN_LIB)/$*) SRCSTMF('$<') OPTION(*EVENTF) DBGVIEW(*SOURCE) TGTRLS(*CURRENT) TGTCCSID(*JOB) BNDDIR($(BNDDIR)) DFTACTGRP(*no)`
				},
				"pgm.sqlrpgle": {
					becomes: "PGM",
					commands: [
						`system -s "CHGATR OBJ('$<') ATR(*CCSID) VALUE(1252)"`
					],
					command: `CRTSQLRPGI OBJ($(BIN_LIB)/$*) SRCSTMF('$<') COMMIT(*NONE) DBGVIEW(*SOURCE) OPTION(*EVENTF) COMPILEOPT('BNDDIR($(BNDDIR)) DFTACTGRP(*no)')`
				},
				"sqlrpgle": {
					becomes: "MODULE",
					commands: [
						`system -s "CHGATR OBJ('$<') ATR(*CCSID) VALUE(1252)"`
					],
					command: `CRTSQLRPGI OBJ($(BIN_LIB)/$*) SRCSTMF('$<') COMMIT(*NONE) DBGVIEW(*SOURCE) OPTION(*EVENTF) OBJTYPE(*MODULE)`
				},
				dspf: {
					becomes: "FILE",
					member: true,
					command: "CRTDSPF FILE($(BIN_LIB)/$*) SRCFILE($(BIN_LIB)/$(SRCPF)) SRCMBR($*)"
				},
				cmd: {
					becomes: "CMD",
					member: true,
					command: "CRTCMD CMD($(BIN_LIB)/$*) PGM($(BIN_LIB)/$*) SRCFILE($(BIN_LIB)/$(SRCPF))"
				},
				sql: {
					becomes: `FILE`,
					command: `RUNSQLSTM SRCSTMF('$<') COMMIT(*NONE)`
				},
				table: {
					becomes: `FILE`,
					command: `RUNSQLSTM SRCSTMF('$<') COMMIT(*NONE)`
				},
				srvpgm: {
					becomes: `SRVPGM`,
					commands: [
						`-system -q "CRTBNDDIR BNDDIR($(BNDDIR))"`,
						`-system -q "RMVBNDDIRE BNDDIR($(BIN_LIB)/$*) OBJ($(BIN_LIB)/$* *SRVPGM)"`,
						`-system "DLTOBJ OBJ($(BIN_LIB)/$*) OBJTYPE(*SRVPGM)"`
					],
					command: `CRTSRVPGM SRVPGM($(BIN_LIB)/$*) MODULE(*SRVPGM) EXPORT(*ALL) BNDDIR($(BNDDIR))`
				},
				bnddir: {
					becomes: `BNDDIR`,
					commands: [
						`-system -q "CRTBNDDIR BNDDIR($(BIN_LIB)/$*)"`,
						`-system -q "ADDBNDDIRE BNDDIR($(BIN_LIB)/$*) OBJ($(patsubst %.SRVPGM,(*LIBL/% *SRVPGM *IMMED),$(notdir $^)))"`
					]
				},
				dtaara: {
					becomes: `DTAARA`,
					commandSource: true
				}
			}
		};
	}

	private setupSettings() {
		try {
			const content = readFileSync(path.join(this.cwd, `iproj.json`), { encoding: `utf-8` });
			const asJson: iProject = JSON.parse(content);

			this.applySettings(asJson);
		} catch (e) {
			console.log(`Failed to read 'iproj.json'.`);
		}
	}

	public applySettings(input: iProject) {
		if (input.includePaths && input.includePaths.length > 0) {
			this.settings.includePaths = input.includePaths;
		}

		if (input.binders && input.binders.length > 0) {
			this.settings.binders = input.binders;
		}

		if (input.compiles) {
			for (const [ext, data] of Object.entries(input.compiles)) {
				// We don't want to fully overwrite the default settings,
				// perhaps the user is only changing the `dir`?
				this.settings.compiles[ext] = {
					...(this.settings.compiles[ext] || {}),
					...data
				};
			}
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
		let baseBinders = [
			...(this.targets.binderRequired() ? [`($(BIN_LIB)/$(APP_BNDDIR))`] : []),
			...this.settings.binders.map(b => `(${b})`)
		];

		if (baseBinders.length === 0) baseBinders.push(`*NONE`);

		return [
			`BIN_LIB=DEV`,
			`APP_BNDDIR=APP`,
			``,
			`INCDIR="${this.settings.includePaths ? this.settings.includePaths.join(`:`) : `.`}"`,
			`BNDDIR=${baseBinders.join(` `)}`,
			`PREPATH=/QSYS.LIB/$(BIN_LIB).LIB`,
			`SHELL=/QOpenSys/usr/bin/qsh`,
		];
	}

	public generateTargets(): string[] {
		let lines = [];

		const all = [
			...this.targets.getParentObjects(`PGM`),
			...this.targets.getParentObjects(`CMD`)
		];

		if (all.length > 0) {
			lines.push(
				`all: ${all.map(dep => `$(PREPATH)/${dep.name}.${dep.type}`).join(` `)}`,
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

			// commandSource means 'is this object built from CL commands in a file'
			if (data.commandSource) {
				const objects = this.targets.getResolvedObjects(data.becomes);

				for (const ileObject of objects) {
					if (ileObject.relativePath) {
						const sourcePath = path.join(this.cwd, ileObject.relativePath);
						const exists = existsSync(sourcePath);

						if (exists) {
							try {
								const content = readFileSync(sourcePath, { encoding: `utf-8` });
								const eol = content.indexOf(`\r\n`) >= 0 ? `\r\n` : `\n`;
								const commands = content.split(eol).filter(l => !l.startsWith(`/*`)); // Remove comments

								lines.push(
									`$(PREPATH)/${ileObject.name}.${data.becomes}: ${ileObject.relativePath}`,
									...(commands.map(l => `\t-system -q "${l}"`)),
								);

							} catch (e) {
								console.log(`Failed to parse '${ileObject.relativePath}'`);
								process.exit();
							}
						}
					}
				}

			} else {
				// Only used for member copies
				const objects = this.targets.getObjectsByExtension(type);

				if (objects.length > 0) {
					for (const ileObject of objects) {
						const parentName = ileObject.relativePath ? path.dirname(ileObject.relativePath) : undefined;
						const qsysTempName: string | undefined = (parentName && parentName.length > 10 ? parentName.substring(0, 10) : parentName);

						const resolve = (command: string) => {
							command = command.replace(new RegExp(`\\$\\*`, `g`), ileObject.name);
							command = command.replace(new RegExp(`\\$<`, `g`), ileObject.relativePath);
							command = command.replace(new RegExp(`\\$\\(SRCPF\\)`, `g`), qsysTempName);
							return command;
						}

						lines.push(
							`$(PREPATH)/${ileObject.name}.${ileObject.type}: ${ileObject.relativePath || ``}`,
							...(qsysTempName && data.member ?
								[
									`\t-system -qi "CRTSRCPF FILE($(BIN_LIB)/${qsysTempName}) RCDLEN(112)"`,
									`\tsystem "CPYFRMSTMF FROMSTMF('${ileObject.relativePath}') TOMBR('$(PREPATH)/${qsysTempName}.FILE/${ileObject.name}.MBR') MBROPT(*REPLACE)"`
								] : []),
							...(data.commands ? data.commands.map(cmd => `\t${resolve(cmd)}`) : []),
							...(data.command ?
								[
									`\tliblist -c $(BIN_LIB);\\`,
									`\tsystem "${resolve(data.command)}"` // TODO: write the spool file somewhere?
								]
								: []
							)
						);
					}
				} else {
					lines.push(
						`$(PREPATH)/%.${data.becomes}: ${data.targetSource || ``}`,
						...(data.commands ? data.commands.map(cmd => `\t${cmd}`) : []),
						...(data.command ?
							[
								`\tliblist -c $(BIN_LIB);\\`,
								`\tsystem "${data.command}"` // TODO: write the spool file somewhere?
							]
							: []
						)
					);
				}
			}

			lines.push(``);

		}

		return lines;
	}
}