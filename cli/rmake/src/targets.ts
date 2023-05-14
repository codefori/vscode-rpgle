import glob from 'glob';
import path from 'path';
import Cache from '../../../server/src/language/models/cache';

type ObjectType = "PGM" | "SRVPGM" | "MODULE" | "FILE" | "BNDDIR";

interface ILEObject {
	name: string;
	type: ObjectType;
	relativePath?: string;
	extension?: string;
}

interface ILEObjectTarget extends ILEObject {
	deps: ILEObject[];
}

export class Targets {
	private pathCache: { [path: string]: true } | undefined;
	private resolvedPaths: { [query: string]: string } = {};
	private resolvedObjects: { [localPath: string]: ILEObject } = {};

	private deps: ILEObjectTarget[];

	constructor(private cwd: string) { }

	public resolveObject(localPath: string) {
		if (this.resolvedObjects[localPath]) return this.resolvedObjects[localPath];

		const detail = path.parse(localPath);

		const isProgram = detail.name.toUpperCase().endsWith(`.PGM`);
		const name = isProgram ? detail.name.substring(0, detail.name.length - 4) : detail.name;
		const type: ObjectType = (isProgram ? "PGM" : Targets.getObjectType(detail.ext));
		const relativePath = path.relative(this.cwd, localPath);

		const theObject: ILEObject = {
			name: name.toUpperCase(),
			type: type,
			relativePath,
			extension: detail.ext.length > 1 ? detail.ext.substring(1) : detail.ext
		};

		this.resolvedObjects[localPath] = theObject;

		return theObject;
	}

	/**
	 * Resolves a search to a filename. Basically a special blob
	 */
	public resolveLocalObjectQuery(name: string, ignoreName?: string): string {
		name = name.toUpperCase();

		if (this.resolvedPaths[name]) return this.resolvedPaths[name];

		if (!this.pathCache) {
			// We don't really want to spam the FS
			// So we can a list of files which can then
			// use in glob again later.
			this.pathCache = {};

			glob.sync(`**/*`, {
				cwd: this.cwd,
				absolute: true,
				nocase: true,
			}).forEach(localPath => {
				this.pathCache[localPath] = true;
			});
		}

		let globString = `**/${name}.*`;

		const results = glob.sync(globString, {
			cwd: this.cwd,
			absolute: true,
			nocase: true,
			ignore: ignoreName ? `**/${ignoreName}` : undefined,
			cache: this.pathCache
		});

		this.resolvedPaths[name] = results[0];

		return results[0];
	}

	private static getObjectType(ext: string): ObjectType {
		switch (ext.toLowerCase()) {
			case `sql`:
			case `table`:
			case `view`:
				// TODO: add more types
				return "FILE";

			case `rpgle`:
			case `sqlrpgle`:
			case `clle`:
			case `cl`:
				return "MODULE";
		}
	}

	public createTarget(localPath: string, cache: Cache) {
		const sourceName = path.basename(localPath);
		const ileObject = this.resolveObject(localPath);
		const target: ILEObjectTarget = {
			...ileObject,
			deps: []
		};

		if (ileObject.type === `PGM` && cache.keyword[`NOMAIN`]) {
			throw new Error(`${localPath}: type detected as ${ileObject.type} but NOMAIN keyword found.`);
		}

		// Find external programs
		cache.procedures
			.filter((proc: any) => proc.keyword[`EXTPGM`])
			.map(ref => {
				const keyword = ref.keyword;
				let fileName = ref.name;
				const extpgm = keyword[`EXTPGM`];
				if (extpgm) {
					if (extpgm === true) fileName = ref.name;
					else fileName = trimQuotes(extpgm);
				}

				return fileName + `.pgm`;
			})
			.forEach(ref => {
				const resolvedPath = this.resolveLocalObjectQuery(ref, sourceName);
				if (resolvedPath) target.deps.push(this.resolveObject(resolvedPath));
			});

		// Find external data structure sources
		cache.structs
			.filter((struct: any) => struct.keyword[`EXTNAME`])
			.map(struct => {
				const keyword = struct.keyword;
				return trimQuotes(keyword[`EXTNAME`]).toLowerCase();
			})
			.forEach(ref => {
				const resolvedPath = this.resolveLocalObjectQuery(ref, sourceName);
				if (resolvedPath) target.deps.push(this.resolveObject(resolvedPath));
			});

		cache.files
			.map(file => file.name)
			.forEach(ref => {
				const resolvedPath = this.resolveLocalObjectQuery(ref, sourceName);
				if (resolvedPath) target.deps.push(this.resolveObject(resolvedPath));
			})

		// We ignore anything with hardcoded schemas
		cache.sqlReferences
			.filter(ref => !ref.description)
			.map(ref => ref.name)
			.forEach(ref => {
				const resolvedPath = this.resolveLocalObjectQuery(ref, sourceName);
				if (resolvedPath) target.deps.push(this.resolveObject(resolvedPath));
			});

		this.deps.push(target);
	}

	getDeps() {
		return this.deps;
	}

	// Generates targets for service programs and binding directories
	public determineLibraries() {
		// Right now, we really only support single module programs and service programs

		const bindingDirectoryTarget: ILEObject = {name: `$(BNDDIR)`, type: `BNDDIR`};

		// Create all the service program targets
		for (const target of this.deps) {
			switch (target.type) {
				case `MODULE`:
					const serviceProgramTarget: ILEObject = {
						name: target.name,
						type: `SRVPGM`
					};
	
					// This creates the service program target
					this.createOrAppend(serviceProgramTarget, target);

					// Before the binding directory can be built, we need the service program
					this.createOrAppend(bindingDirectoryTarget, serviceProgramTarget);
					break;

				case `PGM`:
					// Before the program can be built, we need the binding directory
					target.deps.push(bindingDirectoryTarget);
					break;
			}
		}
	}

	private createOrAppend(parentObject: ILEObject, newDep: ILEObject) {
		let existingTarget = this.deps.find(dep => dep.name === parentObject.name && dep.type === parentObject.type);

		if (!existingTarget) {
			existingTarget = {
				...parentObject,
				deps: []
			};

			this.deps.push(existingTarget);
		}

		existingTarget.deps.push(newDep);
	}
}

function trimQuotes(input: string) {
	if (input[0] === `'`) input = input.substring(1);
	if (input[input.length - 1] === `'`) input = input.substring(0, input.length - 1);
	return input;
}