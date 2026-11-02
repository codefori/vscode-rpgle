/**
 * FieldListChecker — manages the FIELD_LIST UDTF lifecycle on IBM i.
 *
 * FIELD_LIST was rewritten from C++ to RPG IV so the component can be
 * installed on hosts without the C/C++ compiler option installed.
 *
 * Standard Code for IBM i component install pipeline:
 *   1. Upload RPGLE source to the IFS
 *   2. Ensure the target library exists
 *   3. Compile the program (CRTBNDRPG, with CCSID conversion fallbacks)
 *   4. Upload the SQL DDL
 *   5. Drop the previous specific function (ignored if absent)
 *   6. Create/replace the UDTF (RUNSQLSTM)
 *
 * The installed version is stored as the leading integer of LONG_COMMENT in
 * QSYS2.SYSROUTINES. getIdentification() also exposes a sha256 content
 * signature over the RPGLE source and the library-normalized SQL so the
 * component manager can detect content drift independently of the version.
 */

import * as vscode from 'vscode';
import { createHash } from 'crypto';

import { getFieldListRPGLESrc } from './fieldListRpgleSource';
import { getFieldListSQLSrc } from './fieldListSqlSource';
import { COMPONENT_SIGNATURE_LIBRARY_TOKEN, normalizeComponentSqlForSignature } from '../signatureUtils';
import { logError, logInfo } from '../logger';

export type ComponentState = 'NotChecked' | 'NotInstalled' | 'Installed' | 'NeedsUpdate' | 'Error';
export type SecureComponentState = {
    status: ComponentState;
    remoteSignature?: string;
};
export type ComponentIdentification = {
    name: string;
    version: number;
    signature: string;
};

type CompileResult = { ok: boolean; detail?: string };

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
 * Parses the leading integer from LONG_COMMENT, e.g. '2 - description' → 2.
 */
function parseLeadingVersion(longComment: unknown): number {
    if (longComment === null || longComment === undefined) {
        return -1;
    }
    const leadingVersion = /^\s*(\d+)/.exec(String(longComment));
    if (!leadingVersion?.[1]) {
        return -1;
    }
    const parsed = Number(leadingVersion[1]);
    return Number.isNaN(parsed) ? -1 : parsed;
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
    const [result] = await connection.runSQL(sql);
    return parseLeadingVersion(result?.LONG_COMMENT);
}

/**
 * FieldListChecker — UDTF component manager for FIELD_LIST.
 */
export class FieldListChecker {
    readonly id = 'field-list-udtf';
    /** Program object name — must match EXTERNAL NAME in the SQL DDL. */
    readonly PGM_NAME = 'FIELD_LIST';
    readonly UDTF_SPECIFIC = 'FIELD_LIST';
    /** Version 2 = RPG IV rewrite (version 1 was the C++ implementation). */
    readonly currentVersion = 2;

    getSourceSrc(): string {
        return getFieldListRPGLESrc();
    }

    getSQLSrc(library: string, version: number): string {
        return getFieldListSQLSrc(library, version);
    }

    private getSignature(): string {
        const normalizedSql = normalizeComponentSqlForSignature(
            this.getSQLSrc(COMPONENT_SIGNATURE_LIBRARY_TOKEN, this.currentVersion),
            COMPONENT_SIGNATURE_LIBRARY_TOKEN
        );
        return createHash('sha256')
            .update(this.getSourceSrc())
            .update(normalizedSql)
            .digest('hex');
    }

    getIdentification(): ComponentIdentification {
        return {
            name: this.id,
            version: this.currentVersion,
            signature: this.getSignature(),
        };
    }

    async getRemoteState(connection: any, _installDirectory?: string): Promise<SecureComponentState> {
        const library = getUDTFLibrary(connection);
        const signature = this.getSignature();
        logInfo(`[vscode-rpgle] FIELD_LIST.getRemoteState() — library=${library}`);

        try {
            const version = await getFieldListVersion(connection, library);
            const status: ComponentState = version >= this.currentVersion ? 'Installed' : 'NeedsUpdate';
            logInfo(`[vscode-rpgle] FIELD_LIST.getRemoteState() — version=${version}, status=${status}`);
            return { status, remoteSignature: signature };
        } catch (e) {
            logError(`[vscode-rpgle] FIELD_LIST.getRemoteState() — version query threw:`, e);
            return { status: 'NeedsUpdate', remoteSignature: signature };
        }
    }

    async update(connection: any, _installDirectory?: string): Promise<SecureComponentState> {
        logInfo(`[vscode-rpgle] FIELD_LIST.update() — starting install`);
        return connection.withTempDirectory(async (tempDir: string) => {
            const content = connection.content ?? connection.getContent?.();
            if (!content || typeof content.writeStreamfileRaw !== 'function') {
                throw new Error('FIELD_LIST component requires a connection content adapter with writeStreamfileRaw().');
            }

            const encoder = new TextEncoder();
            const library = getUDTFLibrary(connection);
            const signature = this.getSignature();
            logInfo(`[vscode-rpgle] FIELD_LIST.update() — tempDir=${tempDir}, library=${library}`);

            // ── Step 1: upload RPGLE source ─────────────────────────────────
            // tempDir is a unique path prefix, not a subdirectory — append with '_'
            const rpglePath = `${tempDir}_${this.PGM_NAME}.rpgle`;
            const rpgleBytes = encoder.encode(this.getSourceSrc());
            logInfo(`[vscode-rpgle] FIELD_LIST.update() — uploading RPGLE to ${rpglePath} (${rpgleBytes.length} bytes)`);

            try {
                const rpgleUploadErr = await content.writeStreamfileRaw(rpglePath, rpgleBytes);
                if (rpgleUploadErr) {
                    logError(`[vscode-rpgle] writeStreamfileRaw(rpgle) failed: ${rpgleUploadErr}`);
                    throw new Error(`Failed to write FIELD_LIST RPGLE source to ${rpglePath}: ${rpgleUploadErr}`);
                }
            } catch (e) {
                logError(`[vscode-rpgle] writeStreamfileRaw(rpgle) threw:`, e);
                throw e;
            }

            // ── Step 2: ensure target library exists ─────────────────────────
            const crtlibResult = await connection.runCommand({ command: `CRTLIB LIB(${library})`, noLibList: true });
            logInfo(`[vscode-rpgle] FIELD_LIST.update() — CRTLIB(${library}) code=${crtlibResult.code}: ${crtlibResult.stderr}`);

            // ── Step 3: compile the program ──────────────────────────────────
            const compileResult = await this.compileRpgle(connection, library, rpglePath);
            if (!compileResult.ok) {
                throw new Error(`Failed to compile ${this.PGM_NAME}: ${compileResult.detail ?? '<no detail>'}`);
            }

            // ── Step 4: upload SQL DDL ───────────────────────────────────────
            const sqlPath = `${tempDir}_${this.UDTF_SPECIFIC}.sql`;
            try {
                const sqlUploadErr = await content.writeStreamfileRaw(
                    sqlPath,
                    encoder.encode(this.getSQLSrc(library, this.currentVersion))
                );
                if (sqlUploadErr) {
                    logError(`[vscode-rpgle] writeStreamfileRaw(sql) failed: ${sqlUploadErr}`);
                    throw new Error(`Failed to write FIELD_LIST SQL source to ${sqlPath}: ${sqlUploadErr}`);
                }
            } catch (e) {
                logError(`[vscode-rpgle] writeStreamfileRaw(sql) threw:`, e);
                throw e;
            }

            // ── Step 5: drop existing specific function (ignore error) ───────
            try {
                await connection.runSQL(`DROP SPECIFIC FUNCTION ${library}.${this.UDTF_SPECIFIC}`);
            } catch (e) {
                logInfo(`[vscode-rpgle] DROP SPECIFIC FUNCTION (expected to fail on first install): ${e}`);
            }

            // ── Step 6: RUNSQLSTM to create/replace the UDTF ─────────────────
            const runsqlResult = await connection.runCommand({
                command: `RUNSQLSTM SRCSTMF('${sqlPath}') COMMIT(*NONE) NAMING(*SYS) TGTRLS(*CURRENT) OPTION(*LIST)`,
                noLibList: true
            });
            if (runsqlResult.code !== 0) {
                logError(`[vscode-rpgle] RUNSQLSTM failed (code=${runsqlResult.code})`);
                logError(`[vscode-rpgle] RUNSQLSTM stderr: ${runsqlResult.stderr}`);
                throw new Error(`Failed to create ${this.UDTF_SPECIFIC}: ${runsqlResult.stderr || runsqlResult.stdout}`);
            }

            logInfo(`[vscode-rpgle] FIELD_LIST.update() — successfully installed (version ${this.currentVersion})`);
            return { status: 'Installed', remoteSignature: signature };
        });
    }

    /**
     * Compiles the uploaded RPGLE source with CRTBNDRPG. Stream files written
     * from VS Code are tagged CCSID 1208/819, which the ILE RPG compiler may
     * reject depending on PTF level, so on failure the source is converted to
     * the job CCSID (or CCSID 37 as a last resort) and recompiled.
     */
    private async compileRpgle(connection: any, library: string, sourcePath: string): Promise<CompileResult> {
        const compileFromStream = async (path: string, useTargetCcsid: boolean): Promise<{ code: number; stderr: string; stdout: string }> => {
            const tgtCcsidParm = useTargetCcsid ? ' TGTCCSID(*JOB)' : '';
            const cmd = `CRTBNDRPG PGM(${library}/${this.PGM_NAME}) SRCSTMF('${path}')${tgtCcsidParm} OPTION(*SRCSTMT) DBGVIEW(*NONE) TGTRLS(*CURRENT)`;
            logInfo(`[vscode-rpgle] FIELD_LIST.update() — compile command: ${cmd}`);
            const result = await connection.runCommand({ command: cmd });
            return {
                code: result.code,
                stderr: result.stderr,
                stdout: result.stdout
            };
        };

        let compileResult = await compileFromStream(sourcePath, true);

        if (compileResult.code !== 0) {
            logInfo(`[vscode-rpgle] CRTBNDRPG with TGTCCSID(*JOB) failed; retrying with TOCCSID(*JOBCCSID) conversion.`);

            const jobCcsidPath = sourcePath.replace(/\.rpgle$/i, '_ccsid_job.rpgle');
            const cpyToJobCcsid = await connection.runCommand({
                command: `CPY OBJ('${sourcePath}') TOOBJ('${jobCcsidPath}') TOCCSID(*JOBCCSID) REPLACE(*YES)`
            });

            if (cpyToJobCcsid.code === 0) {
                compileResult = await compileFromStream(jobCcsidPath, false);
            } else {
                logInfo(`[vscode-rpgle] TOCCSID(*JOBCCSID) conversion failed; retrying with TOCCSID(37).`);

                const ccsid37Path = sourcePath.replace(/\.rpgle$/i, '_ccsid37.rpgle');
                const cpyTo37 = await connection.runCommand({
                    command: `CPY OBJ('${sourcePath}') TOOBJ('${ccsid37Path}') TOCCSID(37) REPLACE(*YES)`
                });

                if (cpyTo37.code === 0) {
                    compileResult = await compileFromStream(ccsid37Path, false);
                } else {
                    logError(`[vscode-rpgle] CPY conversion failed for ${this.PGM_NAME}. *JOBCCSID: ${cpyToJobCcsid.stderr} | CCSID(37): ${cpyTo37.stderr}`);
                    return { ok: false, detail: `${cpyToJobCcsid.stderr} ${cpyTo37.stderr}` };
                }
            }
        }

        if (compileResult.code !== 0) {
            logError(`[vscode-rpgle] CRTBNDRPG failed for ${this.PGM_NAME} (code=${compileResult.code})`);
            logError(`[vscode-rpgle] CRTBNDRPG stdout: ${compileResult.stdout}`);
            logError(`[vscode-rpgle] CRTBNDRPG stderr: ${compileResult.stderr}`);
            return { ok: false, detail: compileResult.stderr || compileResult.stdout };
        }

        return { ok: true };
    }

    reset(): void {
        // No per-connection state to clear — library is re-read from settings each time
    }
}

export const fieldListChecker = new FieldListChecker();
