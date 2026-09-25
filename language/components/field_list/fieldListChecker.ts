/**
 * FieldListChecker — manages the FIELD_LIST UDTF lifecycle on IBM i.
 *
 * Implements the standard 6-step UDF installation pipeline:
 *   1. Upload C++ source to IFS
 *   2. Ensure target library exists
 *   3. Compile C++ module (CRTCPPMOD)
 *   4. Link program (CRTPGM)
 *   5. Upload SQL DDL
 *   6. Create/replace UDTF (RUNSQLSTM)
 *
 * Version is stored in the LONG_COMMENT of QSYS2.SYSROUTINES for detection
 * of stale installs and conditional updates.
 */

import * as vscode from 'vscode';

import { getFieldListCPPSrc } from './fieldListCppSource';
import { getFieldListSQLSrc } from './fieldListSqlSource';
import { logError, logInfo } from '../logger';

type ComponentState = 'NotChecked' | 'NotInstalled' | 'Installed' | 'NeedsUpdate' | 'Error';
type ComponentIdentification = {
    name: string;
    version: number;
};

/**
 * Resolves the target library for FIELD_LIST UDTF.
 * *TEMPLIB (default) → Code for IBM i's configured temp library.
 * Any other value is used as-is (e.g. SQLTOOLS or a custom library name).
 */
function getUDTFLibrary(connection: any): string {
    const configured = vscode.workspace
        .getConfiguration('vscode-rpgle')
        .get<string>('codeforiFunctionLibrary', '*TEMPLIB')
        ?.trim()
        .toUpperCase();

    const config = connection?.config ?? connection?.getConfig?.();
    const tempLib = (config?.tempLibrary as string | undefined)?.trim().toUpperCase();
    const currentLib = (config?.currentLibrary as string | undefined)?.trim().toUpperCase();

    if (!configured || configured === '*TEMPLIB') {
        return tempLib || currentLib || 'ILEDITOR';
    }

    return configured;
}

/**
 * Retrieves the installed version of FIELD_LIST from QSYS2.SYSROUTINES.
 * Returns -1 if not found (will trigger install on first use).
 */
async function getFieldListVersion(connection: any, schema: string): Promise<number> {
    const sql = `SELECT CAST(LONG_COMMENT AS VARCHAR(200)) AS LONG_COMMENT \
FROM qsys2.sysroutines \
WHERE ROUTINE_SCHEMA = '${schema.toUpperCase()}' \
  AND SPECIFIC_NAME  = 'FIELD_LIST'`;
    try {
        const [result] = await connection.runSQL(sql);
        if (result?.LONG_COMMENT) {
            const comment = String(result.LONG_COMMENT);
            const dash = comment.indexOf('-');
            if (dash > -1) {
                const v = Number(comment.substring(0, dash).trim());
                if (!isNaN(v)) {
                    return v;
                }
            }
        }
    } catch (e) {
        logInfo(`[vscode-rpgle] getFieldListVersion() threw: ${e}`);
    }
    return -1;
}

/**
 * FieldListChecker — UDTF component manager for FIELD_LIST.
 */
export class FieldListChecker {
    readonly id = 'field-list-udtf';
    readonly PGM_NAME = 'FIELDLIST';
    readonly UDTF_SPECIFIC = 'FIELD_LIST';
    readonly currentVersion = 1;

    getIdentification(): ComponentIdentification {
        return { name: this.id, version: this.currentVersion };
    }

    getCPPSrc(): string {
        return getFieldListCPPSrc();
    }

    getSQLSrc(library: string, version: number): string {
        return getFieldListSQLSrc(library, version);
    }

    async getRemoteState(connection: any): Promise<ComponentState> {
        const library = getUDTFLibrary(connection);
        logInfo(`[vscode-rpgle] FIELD_LIST.getRemoteState() — library=${library}`);

        try {
            const version = await getFieldListVersion(connection, library);
            const status: ComponentState = version >= this.currentVersion ? 'Installed' : 'NeedsUpdate';
            logInfo(`[vscode-rpgle] FIELD_LIST.getRemoteState() — version=${version}, status=${status}`);
            return status;
        } catch (e) {
            logInfo(`[vscode-rpgle] FIELD_LIST.getRemoteState() — query threw: ${e}, returning NeedsUpdate`);
            return 'NeedsUpdate';
        }
    }

    async update(connection: any): Promise<ComponentState> {
        logInfo(`[vscode-rpgle] FIELD_LIST.update() — starting install`);
        return connection.withTempDirectory(async (tempDir: string) => {
            const content = connection.content ?? connection.getContent?.();
            const encoder = new TextEncoder();
            const library = getUDTFLibrary(connection);
            logInfo(`[vscode-rpgle] FIELD_LIST.update() — tempDir=${tempDir}, library=${library}`);

            const cppPath = `${tempDir}_${this.PGM_NAME}.cpp`;
            const cppBytes = encoder.encode(this.getCPPSrc());
            logInfo(`[vscode-rpgle] FIELD_LIST.update() — uploading C++ to ${cppPath} (${cppBytes.length} bytes)`);

            try {
                const cppUploadErr = await content.writeStreamfileRaw(cppPath, cppBytes);
                if (cppUploadErr) {
                    logError(`[vscode-rpgle] writeStreamfileRaw(cpp) failed: ${cppUploadErr}`);
                    return 'Error';
                }
            } catch (e) {
                logError(`[vscode-rpgle] writeStreamfileRaw(cpp) threw: ${e}`);
                return 'Error';
            }

            const crtlibResult = await connection.runCommand({ command: `CRTLIB LIB(${library})`, noLibList: true });
            logInfo(`[vscode-rpgle] FIELD_LIST.update() — CRTLIB(${library}) code=${crtlibResult.code}: ${crtlibResult.stderr}`);

            const crtcppmodCmd = `CRTCPPMOD MODULE(${library}/${this.PGM_NAME}) SRCSTMF('${cppPath}') LANGLVL(*EXTENDED0X) SYSIFCOPT(*IFS64IO) OUTPUT(*PRINT)`;
            logInfo(`[vscode-rpgle] FIELD_LIST.update() — running: ${crtcppmodCmd}`);
            const moduleResult = await connection.runCommand({ command: crtcppmodCmd, noLibList: true });
            if (moduleResult.code !== 0) {
                logError(`[vscode-rpgle] CRTCPPMOD failed (code=${moduleResult.code})`);
                logError(`[vscode-rpgle] CRTCPPMOD stdout: ${moduleResult.stdout}`);
                logError(`[vscode-rpgle] CRTCPPMOD stderr: ${moduleResult.stderr}`);
                return 'Error';
            }

            const pgmResult = await connection.runCommand({
                command: `CRTPGM PGM(${library}/${this.PGM_NAME}) MODULE(${library}/${this.PGM_NAME}) ACTGRP(*CALLER)`,
                noLibList: true
            });
            if (pgmResult.code !== 0) {
                logError(`[vscode-rpgle] CRTPGM failed for ${this.PGM_NAME}: ${pgmResult.stderr}`);
                return 'Error';
            }

            const sqlPath = `${tempDir}_${this.UDTF_SPECIFIC}.sql`;
            try {
                const sqlUploadErr = await content.writeStreamfileRaw(
                    sqlPath,
                    new TextEncoder().encode(this.getSQLSrc(library, this.currentVersion))
                );
                if (sqlUploadErr) {
                    logError(`[vscode-rpgle] writeStreamfileRaw(sql) failed: ${sqlUploadErr}`);
                    return 'Error';
                }
            } catch (e) {
                logError(`[vscode-rpgle] writeStreamfileRaw(sql) threw: ${e}`);
                return 'Error';
            }

            try {
                await connection.runSQL(`DROP SPECIFIC FUNCTION ${library}.${this.UDTF_SPECIFIC}`);
            } catch (e) {
                logInfo(`[vscode-rpgle] DROP SPECIFIC FUNCTION (expected to fail on first install): ${e}`);
            }

            const runsqlResult = await connection.runCommand({
                command: `RUNSQLSTM SRCSTMF('${sqlPath}')`,
                noLibList: true
            });
            if (runsqlResult.code !== 0) {
                logError(`[vscode-rpgle] RUNSQLSTM failed (code=${runsqlResult.code})`);
                logError(`[vscode-rpgle] RUNSQLSTM stderr: ${runsqlResult.stderr}`);
                return 'Error';
            }

            logInfo(`[vscode-rpgle] FIELD_LIST.update() — successfully installed`);
            return 'Installed';
        });
    }
}

export const fieldListChecker = new FieldListChecker();
