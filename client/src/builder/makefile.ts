import path = require('path');
import { EndOfLine, RelativePattern, Uri, workspace } from 'vscode';

export async function updateMakefile(baseUri: Uri, deps: string[]) {
	const workspaceFolder = workspace.getWorkspaceFolder(baseUri);
	const sourceName = path.basename(baseUri.path);
	let doWrite = false;

	if (workspaceFolder) {
		const relativeFind = new RelativePattern(workspaceFolder, `**/makefile`);
		const [makefile] = await workspace.findFiles(relativeFind, null, 1);

		if (makefile) {
			const document = await workspace.openTextDocument(makefile);
			if (document) {
				const eol = document.eol === EndOfLine.CRLF ? `\r\n` : `\n`;
				let lines = document.getText().split(eol);

				// This is the trigger for this functionality.
				const startingIndex = lines.findIndex(line => line === `#DEPS`);

				if (startingIndex >= 0) {
					const existingDef = lines.findIndex(line => line.startsWith(`${sourceName}:`));

					if (existingDef >= 0) {
						const line = lines[existingDef];
						const split = line.indexOf(`:`);
						if (split >= 0) {
							const existingDeps = lines[existingDef].substring(split + 1).trim().split(` `).filter(dep => dep.length);

							// Remove any existing RPGLE deps. This allows users to add their custom ones still
							const filteredDeps = existingDeps.filter(dep => !dep.toLowerCase().includes(`rpgle`) && !deps.includes(dep));
							deps.push(...filteredDeps);
						}

						lines[existingDef] = `${sourceName}: ${deps.join(` `)}`;
						doWrite = true;
					} else {
						// New file possibly? Write it as a new line
						lines.splice(startingIndex + 1, 0, `${sourceName}: ${deps.join(` `)}`);
						doWrite = true;
					}

					if (doWrite) {
						await workspace.fs.writeFile(makefile, Buffer.from(lines.join(eol)));
					}
				}
			}
		}
	}
}