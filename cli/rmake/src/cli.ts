export let cliSettings = {
	infoMessages: false,
	createMakefile: true,
	lookup: ``
};

export function info(message: string) {
	if (cliSettings.infoMessages) console.log(message);
}

export function warning(message: string) {
	console.log(`[WARNING] ${message}`);
}