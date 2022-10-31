import path = require('path');
import { workspace } from 'vscode';
import { LanguageClient } from 'vscode-languageclient/node';
import { updateMakefile } from './makefile';

export function initBuilder(client: LanguageClient) {
	let substriptions = [];

	if (workspace.workspaceFolders) {
		substriptions.push(
			workspace.onDidSaveTextDocument(async document => {
				if (document.uri.scheme === `file` && document.languageId === `rpgle`) {
					const uri = document.uri.toString();

					const cache: any = await client.sendRequest(`getCache`, uri);

					if (cache) {
						const sourceName = path.basename(uri);
						const detail = path.parse(uri);

						// We don't do anything with include files.
						if (detail.ext.toLowerCase() === `.rpgleinc`) return;

						let deps: string[] = [];

						// Build a possible object name
						let objectName;
						if (detail.name.toLowerCase().endsWith(`.pgm`)) {
							objectName = path.parse(detail.name).name.toUpperCase() + `.PGM`;
						} else {
							objectName = detail.name + `.MODULE`;
						}

						const includes: any[] = cache.includes;
						includes.forEach(include => {
							const sourceDep = path.basename(include.toPath);
							deps.push(sourceDep);
						});

						// Look for file definitions
						const files: any[] = cache.files;
						const fileObjectSource = await Promise.all(files.map(file => {
							const fileName = file.name.toLowerCase();
							return workspace.findFiles(`**/{${fileName},${fileName.toUpperCase()}}.*`, `**/${sourceName}`, 1);
						}));
						fileObjectSource.forEach(files => {
							const foundFile = files[0];
							if (foundFile) {
								const sourceDep = path.basename(foundFile.toString());
								deps.push(sourceDep);
							}
						});

						// Find external data structure sources
						const extStructs: any[] = cache.structs.filter((struct: any) => struct.keyword[`EXTNAME`]);
						const otherFileObjectSource = await Promise.all(extStructs.map(struct => {
							const keyword = struct.keyword;
							const fileName = trimQuotes(keyword[`EXTNAME`]).toLowerCase();
							return workspace.findFiles(`**/{${fileName},${fileName.toUpperCase()}}.*`, `**/${sourceName}`, 1);
						}));
						otherFileObjectSource.forEach(files => {
							const foundFile = files[0];
							if (foundFile) {
								const sourceDep = path.basename(foundFile.toString());
								deps.push(sourceDep);
							}
						});

						// Find external programs
						const extPgms: any[] = cache.procedures.filter((proc: any) => proc.keyword[`EXTPGM`]);
						const pgmSources = await Promise.all(extPgms.map(struct => {
							const keyword = struct.keyword;
							let fileName = struct.name;
							const extpgm = keyword[`EXTPGM`];
							if (extpgm) {
								if (extpgm === true) fileName = struct.name;
								else fileName = trimQuotes(extpgm);
							}
							return workspace.findFiles(`**/{${fileName.toLowerCase()},${fileName.toUpperCase()}}.*`, `**/${sourceName}`, 1);
						}));
						pgmSources.forEach(files => {
							const foundFile = files[0];
							if (foundFile && foundFile.toString() !== uri) {
								const sourceDep = path.basename(foundFile.toString());
								deps.push(sourceDep);
							}
						});

						deps = deps.filter(d => d !== sourceName);
						console.log(`${sourceName}: ${deps.join(` `)}`);

						if (deps.length > 0) {
							updateMakefile(document.uri, deps);
						}
					}
				}
			})
		);
	}

	return substriptions;
}

function trimQuotes(input: string) {
	if (input[0] === `'`) input = input.substring(1);
	if (input[input.length - 1] === `'`) input = input.substring(0, input.length - 1);
	return input;
}