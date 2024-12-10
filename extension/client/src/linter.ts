import path = require('path');
import { commands, ExtensionContext, Uri, ViewColumn, window, workspace } from 'vscode';
import {getInstance} from './base';

import {DEFAULT_SCHEMA} from "./schemas/linter"

export function initialise(context: ExtensionContext) {
	context.subscriptions.push(
		commands.registerCommand(`vscode-rpgle.openLintConfig`, async (filter) => {
			const instance = getInstance();
			const editor = window.activeTextEditor;

			let exists = false;

			if (editor && ![`member`, `streamfile`].includes(editor.document.uri.scheme)) {
				const workspaces = workspace.workspaceFolders;
				if (workspaces && workspaces.length > 0) {
					const linter = await workspace.findFiles(`**/.vscode/rpglint.json`, `**/.git`, 1);
					let uri;
					if (linter && linter.length > 0) {
						uri = linter[0];

						console.log(`Uri path: ${JSON.stringify(uri)}`);

					} else {
						console.log(`String path: ${path.join(workspaces[0].uri.fsPath, `.vscode`, `rpglint.json`)}`);

						uri = Uri.from({
							scheme: `file`,
							path: path.join(workspaces[0].uri.fsPath, `.vscode`, `rpglint.json`)
						});

						console.log(`Creating Uri path: ${JSON.stringify(uri)}`);

						await workspace.fs.writeFile(
							uri,
							Buffer.from(JSON.stringify(DEFAULT_SCHEMA, null, 2), `utf8`)
						);
					}

					workspace.openTextDocument(uri).then(doc => {
						window.showTextDocument(doc, {
							viewColumn: ViewColumn.One
						});
					});
				}

			} else if (instance && instance.getConnection()) {
				const connection = instance.getConnection();
				const content = instance.getContent();

				/** @type {"member"|"streamfile"} */
				let type = `member`;
				let configPath: string | undefined;

				if (filter && filter.description) {
					// Bad way to get the library for the filter ..
					const library: string = (filter.description.split(`/`)[0]).toLocaleUpperCase();

					if (library.includes(`*`)) {
						window.showErrorMessage(`Cannot show lint config for a library filter.`);
						return;
					}

					configPath = `${library}/VSCODE/RPGLINT.JSON`;

					exists = (await connection.runCommand({
						command: `CHKOBJ OBJ(${library}/VSCODE) OBJTYPE(*FILE) MBR(RPGLINT)`,
						noLibList: true
					})).code === 0;

				} else if (editor) {
					//@ts-ignore
					type = editor.document.uri.scheme;

					console.log(`Uri remote path: ${JSON.stringify(editor.document.uri)}`);

					switch (type) {
						case `member`:
							const memberPath = parseMemberUri(editor.document.uri.path);
							const cleanString = [
								memberPath.library,
								`VSCODE`,
								`RPGLINT.JSON`
							].join(`/`);

							const memberUri = Uri.from({
								scheme: `member`,
								path: cleanString
							});

							configPath = memberUri.path;

							exists = (await connection.runCommand({
								command: `CHKOBJ OBJ(${memberPath.library!.toLocaleUpperCase()}/VSCODE) OBJTYPE(*FILE) MBR(RPGLINT)`,
								noLibList: true
							})).code === 0;
							break;

						case `streamfile`:
							const config = instance.getConfig();
							if (config.homeDirectory) {
								configPath = path.posix.join(config.homeDirectory, `.vscode`, `rpglint.json`)
								exists = await content.testStreamFile(configPath, `r`);
							}
							break;
					}
				} else {
					window.showErrorMessage(`No active editor found.`);
				}

				if (configPath) {
					console.log(`Current path: ${configPath}`);

					if (exists) {
						await commands.executeCommand(`code-for-ibmi.openEditable`, configPath);
					} else {
						window.showErrorMessage(`RPGLE linter config doesn't exist for this file. Would you like to create a default at ${configPath}?`, `Yes`, `No`).then
							(async (value) => {
								if (value === `Yes`) {
									const jsonString = JSON.stringify(DEFAULT_SCHEMA, null, 2);

									switch (type) {
										case `member`:
											if (configPath) {
												const memberPath = configPath.split(`/`);

												// Will not crash, even if it fails
												await connection.runCommand(
													{
														'command': `CRTSRCPF FILE(${memberPath[0]}/VSCODE) RCDLEN(112)`
													}
												);

												// Will not crash, even if it fails
												await connection.runCommand(
													{
														command: `ADDPFM FILE(${memberPath[0]}/VSCODE) MBR(RPGLINT) SRCTYPE(JSON)`
													}
												);

												try {
													console.log(`Member path: ${[memberPath[0], `VSCODE`, `RPGLINT`].join(`/`)}`);

													await content.uploadMemberContent(undefined, memberPath[0], `VSCODE`, `RPGLINT`, jsonString);
													await commands.executeCommand(`code-for-ibmi.openEditable`, configPath);
												} catch (e) {
													console.log(e);
													window.showErrorMessage(`Failed to create and open new lint configuration file: ${configPath}`);
												}
											}
											break;

										case `streamfile`:
											console.log(`IFS path: ${configPath}`);

											try {
												await content.writeStreamfile(configPath, jsonString);
												await commands.executeCommand(`code-for-ibmi.openEditable`, configPath);
											} catch (e) {
												console.log(e);
												window.showErrorMessage(`Failed to create and open new lint configuration file: ${configPath}`);
											}
											break;
									}
								}
							});
					}
				} else {
					window.showErrorMessage(`No lint config path for this file. File must either be a member or a streamfile on the host IBM i.`);
				}
			} else {
				window.showErrorMessage(`Not connected to a system.`);
			}
		}),
	)
}

function parseMemberUri(fullPath: string): {asp?: string, library?: string, file?: string, name: string} {
	const parts = fullPath.split(`/`).map(s => s.split(`,`)).flat().filter(s => s.length >= 1);
	return {
		name: path.parse(parts[parts.length - 1]).name,
		file: parts[parts.length - 2],
		library: parts[parts.length - 3],
		asp: parts[parts.length - 4]
	}
};