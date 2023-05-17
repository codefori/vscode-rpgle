export let cliSettings = {
	infoMessages: false,
	createMakefile: true,
	lookup: ``
};

export function info(message: string) {
	if (cliSettings.infoMessages) console.log(message);
}