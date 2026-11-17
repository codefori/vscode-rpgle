import * as vscode from 'vscode';

let outputChannel: vscode.OutputChannel | undefined;

export function initRpgleOutput(context: vscode.ExtensionContext) {
    if (!outputChannel) {
        outputChannel = vscode.window.createOutputChannel('RPGLE');
        context.subscriptions.push(outputChannel);
    }
}

function formatLogLine(level: string, message: string) {
    const now = new Date();
    const timestamp = `${now.toTimeString().split(' ')[0]}.${now.getMilliseconds().toString().padStart(3, '0')}`;
    return `[${timestamp}] [${level}] ${message}`;
}

function errorText(error: unknown): string {
    if (error instanceof Error) {
        return `${error.name}: ${error.message}`;
    }

    if (typeof error === 'string') {
        return error;
    }

    try {
        return JSON.stringify(error);
    } catch {
        return String(error);
    }
}

export function logInfo(message: string) {
    const line = formatLogLine('INFO', message);
    console.log(line);
    outputChannel?.appendLine(line);
}

export function logWarn(message: string) {
    const line = formatLogLine('WARN', message);
    console.warn(line);
    outputChannel?.appendLine(line);
}

export function logError(message: string, error?: unknown) {
    const line = formatLogLine('ERROR', message);
    if (error !== undefined) {
        console.error(line, error);
        outputChannel?.appendLine(`${line} ${errorText(error)}`);
        return;
    }

    console.error(line);
    outputChannel?.appendLine(line);
}
