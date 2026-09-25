import * as vscode from 'vscode';

let outputChannel: vscode.OutputChannel | undefined;

export function initRpgleOutput(context: vscode.ExtensionContext) {
    if (!outputChannel) {
        outputChannel = vscode.window.createOutputChannel('RPGLE');
        context.subscriptions.push(outputChannel);
    }
}

function append(level: string, message: string) {
    outputChannel?.appendLine(`[${level}] ${message}`);
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
    console.log(message);
    append('INFO', message);
}

export function logWarn(message: string) {
    console.warn(message);
    append('WARN', message);
}

export function logError(message: string, error?: unknown) {
    if (error !== undefined) {
        console.error(message, error);
        append('ERROR', `${message} ${errorText(error)}`);
        return;
    }

    console.error(message);
    append('ERROR', message);
}
