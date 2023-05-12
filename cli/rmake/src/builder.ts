import path from 'path';

const cwd = process.cwd();

type ObjectType = "PGM"|"SRVPGM"|"MODULE"|"FILE";

interface ILEObject {
	name: string;
	type: ObjectType;
	relativePath: string;
}

interface ILEObjectTarget extends ILEObject {
	deps: ILEObject[];
}

export class Builder {
	private resolvedPaths: {[query: string]: string} = {};
	private resolvedObjects: {[localPath: string]: ILEObject} = {};

	public resolveObject(localPath: string) {
		if (this.resolvedObjects[localPath]) return this.resolvedObjects[localPath];

		const detail = path.parse(localPath);

		const isProgram = detail.name.toUpperCase().endsWith(`.PGM`);
		const name = isProgram ? detail.name.substring(0, detail.name.length - 4) : detail.name;
		const type: ObjectType = (isProgram ? "PGM" : Builder.getObjectType(detail.ext));
		const relativePath = path.relative(cwd, localPath);

		const theObject: ILEObject = {
			name,
			type,
			relativePath
		};

		this.resolvedObjects[localPath] = theObject;
		return theObject;
	}

	/**
	 * Resolves a search to a filename. Basically a special blob
	 */
	public resolveLocalObjectQuery(query: string): string {
		query = query.toUpperCase();

		if (this.resolvedPaths[query]) return this.resolvedPaths[query];
		
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
}