import path = require('path');
import { commands, ExtensionContext, Uri, ViewColumn, window, workspace } from 'vscode';
import { getInstance } from './base';

import { DEFAULT_SCHEMA } from "./schemas/linter"

const GLOBAL_LINT_CONFIG_PATH = `/etc/vscode/rpglint.json`;

export function initialise(context: ExtensionContext) {
	context.subscriptions.push(
		commands.registerCommand(`vscode-rpgle.openLintConfig`, async (filter) => {
			const instance = getInstance();
			const editor = window.activeTextEditor;

			let exists = false;

			if (editor && ![`member`, `streamfile`].includes(editor.document.uri.scheme)) {
				// Local workspace file — existing behaviour unchanged
				const workspaces = workspace.workspaceFolders;
				if (workspaces && workspaces.length > 0) {
					const linter = await workspace.findFiles(`**/.vscode/rpglint.json`, `**/.git`, 1);
					let uri;
					if (linter && linter.length > 0) {
						uri = linter[0];
					} else {
						uri = Uri.from({
							scheme: `file`,
							path: path.join(workspaces[0].uri.fsPath, `.vscode`, `rpglint.json`)
						});

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

				// Check if global lint config is enabled
				const useGlobal = workspace.getConfiguration(`vscode-rpgle`).get<boolean>(`useGlobalLintConfig`, false);

				if (useGlobal) {
					// Global IFS mode — use /etc/vscode/rpglint.json
					const globalExists = await content.testStreamFile(GLOBAL_LINT_CONFIG_PATH, `r`);

					if (globalExists) {
						await commands.executeCommand(`code-for-ibmi.openEditable`, GLOBAL_LINT_CONFIG_PATH);
					} else {
						const answer = await window.showInformationMessage(
							`Global lint config does not exist at ${GLOBAL_LINT_CONFIG_PATH}. Would you like to create it?`,
							`Yes`, `No`
						);

						if (answer === `Yes`) {
							const jsonString = JSON.stringify(DEFAULT_SCHEMA, null, 2);
							try {
								await content.writeStreamfile(GLOBAL_LINT_CONFIG_PATH, jsonString);
								await commands.executeCommand(`code-for-ibmi.openEditable`, GLOBAL_LINT_CONFIG_PATH);
							} catch (e) {
								console.log(e);
								window.showErrorMessage(`Failed to create global lint config at ${GLOBAL_LINT_CONFIG_PATH}. Ensure /etc/vscode/ directory exists.`);
							}
						}
					}
				} else {
					// Per-library mode — existing behaviour
					let type: `member` | `streamfile` = `member`;
					let configPath: string | undefined;

					if (filter && filter.description) {
						const library: string = (filter.description.split(`/`)[0]).toLocaleUpperCase();

						if (library.includes(`*`)) {
							window.showErrorMessage(`Cannot show lint config for a library filter.`);
							return;
						}

						configPath = `/${library}/VSCODE/RPGLINT.JSON`;

						exists = (await connection.runCommand({
							command: `QSYS/CHKOBJ OBJ(${library}/VSCODE) OBJTYPE(*FILE) MBR(RPGLINT)`,
							noLibList: true
						})).code === 0;

					} else if (editor) {
						//@ts-ignore
						type = editor.document.uri.scheme;

						switch (type) {
							case `member`:
								const memberPath = parseMemberUri(editor.document.uri.path);
								const library = memberPath.library!.toLocaleUpperCase();

								configPath = `/${library}/VSCODE/RPGLINT.JSON`;

								exists = (await connection.runCommand({
									command: `QSYS/CHKOBJ OBJ(${library}/VSCODE) OBJTYPE(*FILE) MBR(RPGLINT)`,
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
						if (exists) {
							await commands.executeCommand(`code-for-ibmi.openEditable`, configPath);
						} else {
							window.showErrorMessage(
								`RPGLE linter config doesn't exist. Would you like to create a default at ${configPath}?`,
								`Yes`, `No`
							).then(async (value) => {
								if (value === `Yes`) {
									const jsonString = JSON.stringify(DEFAULT_SCHEMA, null, 2);

									switch (type) {
										case `member`:
											if (configPath) {
												const pathParts = configPath.split(`/`);
												// pathParts = ['', 'LIBRARY', 'VSCODE', 'RPGLINT.JSON']
												const lib = pathParts[1];

												await connection.runCommand(
													{ 'command': `QSYS/CRTSRCPF FILE(${lib}/VSCODE) RCDLEN(112)` }
												);

												await connection.runCommand(
													{ command: `QSYS/ADDPFM FILE(${lib}/VSCODE) MBR(RPGLINT) SRCTYPE(JSON)` }
												);

												try {
													await content.uploadMemberContent(undefined, lib, `VSCODE`, `RPGLINT`, jsonString);
													await commands.executeCommand(`code-for-ibmi.openEditable`, configPath);
												} catch (e) {
													console.log(e);
													window.showErrorMessage(`Failed to create and open new lint configuration file: ${configPath}`);
												}
											}
											break;

										case `streamfile`:
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
				}
			} else {
				window.showErrorMessage(`Not connected to a system.`);
			}
		}),
	)
}

function parseMemberUri(fullPath: string): { asp?: string, library?: string, file?: string, name: string } {
	const parts = fullPath.split(`/`).map(s => s.split(`,`)).flat().filter(s => s.length >= 1);
	return {
		name: path.parse(parts[parts.length - 1]).name,
		file: parts[parts.length - 2],
		library: parts[parts.length - 3],
		asp: parts[parts.length - 4]
	}
};