import { BindingDirectory, SymbolList, symbolLookup } from './connection';
import { trimQuotes } from './providers';

export let bindingDirectories: {[key: string]: SymbolList} = {};

export async function getSymbolFiles(bnddir: string, lookup: string): Promise<string[] | undefined> {
	const validBnddir = bnddir.toUpperCase();

	if (!bindingDirectories[validBnddir]) {
		const symbolList = await symbolLookup({
			binders: parseBnddir(bnddir)
		})
	
		if (symbolList) {
			bindingDirectories[validBnddir] = symbolList;
		}
	}

	if (bindingDirectories[validBnddir]) {
		const symbols = bindingDirectories[validBnddir];
		const realSymbol = Object.keys(symbols).find(symbol => symbol.toUpperCase() === lookup.toUpperCase());

		if (realSymbol) {
			return symbols[realSymbol];
		}
	}

	return undefined;
}

/**
 * Gets all possible symbols for a BNDDIR string.
 */
export async function getSymbols(bnddir: string): Promise<string[] | undefined> {
	const validBnddir = bnddir.toUpperCase();

	if (bindingDirectories[validBnddir]) {
		return Object.keys(bindingDirectories[validBnddir]);
	}

	const symbolList = await symbolLookup({
		binders: parseBnddir(bnddir)
	})

	if (symbolList) {
		bindingDirectories[validBnddir] = symbolList;
		return Object.keys(symbolList);
	}

	return;
}

export function parseBnddir(input: string): BindingDirectory[] {
	const objectStrings = input.split(`:`).map(obj => trimQuotes(obj));
	return objectStrings.map(qualifiedPath => {
		const parts = qualifiedPath.split(`/`);
		return {
			name: parts[parts.length - 1],
			lib: parts[parts.length - 2]
		}
	});
}