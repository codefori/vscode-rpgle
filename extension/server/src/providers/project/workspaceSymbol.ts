import { Range, SymbolKind, WorkspaceSymbol, WorkspaceSymbolParams } from 'vscode-languageserver';
import { parser } from '..';
import * as Project from '.';
import path = require('path');

export default function workspaceSymbolProvider(params: WorkspaceSymbolParams): WorkspaceSymbol[] | undefined {
	console.log(params.query);
	if (Project.isEnabled) {
		const parsedFiles = Object.keys(parser.parsedCache);
		let symbols: WorkspaceSymbol[] = [];

		parsedFiles.forEach(uri => {
			const basename = path.basename(uri);
			const baseNameLower = basename.toLowerCase();

			if (baseNameLower.endsWith(`.rpgleinc`)) {
				symbols.push(
					WorkspaceSymbol.create(
						basename,
						SymbolKind.File,
						uri,
						Range.create(
							0,
							0,
							0,
							0
						)
					)
				)
			} else {
				const cache = parser.getParsedCache(uri);

				if (cache) {
					if (cache.keyword[`MAIN`]) {
						symbols.push(
							WorkspaceSymbol.create(
								basename,
								SymbolKind.Method,
								uri,
								Range.create(
									0,
									0,
									0,
									0
								)
							)
						);
					} else
					if (cache.keyword[`NOMAIN`]) {
						cache.procedures.forEach(proc => {
							if (proc.keyword[`EXPORT`]) {
								symbols.push(
									WorkspaceSymbol.create(
										proc.name,
										SymbolKind.Function,
										uri,
										Range.create(
											proc.position.range.line,
											0,
											proc.position.range.line,
											0
										)
									)
								);
							}
						});
					}
				}
			}
		});

		return symbols;
	}

	return;
}