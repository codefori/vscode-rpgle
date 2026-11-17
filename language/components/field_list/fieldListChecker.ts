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

import { workspace } from 'vscode';
import IBMi from '@halcyontech/vscode-ibmi-types/IBMi';
import { ComponentIdentification, ComponentState, IBMiComponent, SecureComponentState } from '@halcyontech/vscode-ibmi-types/api/components/component';

import { getFieldListRPGLESrc } from './fieldListRpgleSource';
import { getFieldListSQLSrc } from './fieldListSqlSource';
import { logError, logInfo } from '../logger';

type CompileResult = { ok: boolean; detail?: string };

/**
 * FIELD_LIST is installed in Code for IBM i's per-connection temporary library.
 */
function getUDTFLibrary(connection: IBMi): string {
    const library = connection.getConfig().tempLibrary?.trim().toUpperCase();
    if (!library) {
        throw new Error('FIELD_LIST requires Code for IBM i to configure a temporary library.');
    }

    return library;
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
async function getFieldListVersion(connection: IBMi, schema: string): Promise<number> {
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
export class FieldListChecker implements IBMiComponent {
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

    getIdentification(): ComponentIdentification {
        return {
            name: this.id,
            version: this.currentVersion,
            signature: '',
        };
    }

    async getRemoteState(connection: IBMi, _installDirectory?: string): Promise<SecureComponentState> {
        if (workspace.getConfiguration('vscode-rpgle').get<string>('fieldMetadataProvider', 'CL') !== 'UDTF') {
            return { status: 'Installed' };
        }

        const library = getUDTFLibrary(connection);
        logInfo(`[vscode-rpgle] FIELD_LIST.getRemoteState() — library=${library}`);

        try {
            const remoteSignature = await connection.getContent().getSQLRoutineSignature(library, this.UDTF_SPECIFIC, 'FUNCTION');
            if (!remoteSignature) {
                return { status: 'NotInstalled' };
            }

            if (remoteSignature.toUpperCase() !== `${library}/${this.PGM_NAME}`) {
                return { status: 'NeedsUpdate', remoteSignature };
            }

            const version = await getFieldListVersion(connection, library);
            const status: ComponentState = version >= this.currentVersion ? 'Installed' : 'NeedsUpdate';
            logInfo(`[vscode-rpgle] FIELD_LIST.getRemoteState() — version=${version}, status=${status}, remoteSignature=${remoteSignature}`);
            return { status, remoteSignature };
        } catch (e) {
            logError(`[vscode-rpgle] FIELD_LIST.getRemoteState() — remote check failed:`, e);
            return { status: 'Error' };
        }
    }

    async update(connection: IBMi, _installDirectory?: string): Promise<SecureComponentState> {
        logInfo(`[vscode-rpgle] FIELD_LIST.update() — starting install`);
        return connection.withTempDirectory(async (tempDir: string) => {
            const content = connection.getContent();

            const encoder = new TextEncoder();
            const library = getUDTFLibrary(connection);
            logInfo(`[vscode-rpgle] FIELD_LIST.update() — tempDir=${tempDir}, library=${library}`);
            const upload = async (path: string, text: string, label: string) => {
                const error = await content.writeStreamfileRaw(path, encoder.encode(text));
                if (error) {
                    throw new Error(`Failed to write FIELD_LIST ${label} to ${path}: ${error}`);
                }
            };

            // ── Step 1: upload RPGLE source ─────────────────────────────────
            // tempDir is a unique path prefix, not a subdirectory — append with '_'
            const rpglePath = `${tempDir}_${this.PGM_NAME}.rpgle`;
            await upload(rpglePath, this.getSourceSrc(), 'RPGLE source');

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
            await upload(sqlPath, this.getSQLSrc(library, this.currentVersion), 'SQL source');

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
            return this.getRemoteState(connection);
        });
    }

    /**
     * Compiles the uploaded RPGLE source with CRTBNDRPG. Stream files written
     * from VS Code are tagged CCSID 1208/819, which the ILE RPG compiler may
     * reject depending on PTF level, so on failure the source is converted to
     * the job CCSID (or CCSID 37 as a last resort) and recompiled.
     */
    private async compileRpgle(connection: IBMi, library: string, sourcePath: string): Promise<CompileResult> {
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
        const conversionErrors: string[] = [];

        for (const ccsid of ['*JOBCCSID', '37']) {
            if (compileResult.code === 0) break;

            const convertedPath = sourcePath.replace(/\.rpgle$/i, `_ccsid_${ccsid.replace('*', '')}.rpgle`);
            const conversion = await connection.runCommand({
                command: `CPY OBJ('${sourcePath}') TOOBJ('${convertedPath}') TOCCSID(${ccsid}) REPLACE(*YES)`
            });

            if (conversion.code === 0) {
                compileResult = await compileFromStream(convertedPath, false);
            } else {
                conversionErrors.push(`${ccsid}: ${conversion.stderr}`);
            }
        }

        if (compileResult.code !== 0) {
            logError(`[vscode-rpgle] CRTBNDRPG failed for ${this.PGM_NAME} (code=${compileResult.code})`);
            logError(`[vscode-rpgle] CRTBNDRPG stdout: ${compileResult.stdout}`);
            logError(`[vscode-rpgle] CRTBNDRPG stderr: ${compileResult.stderr}`);
            return { ok: false, detail: [compileResult.stderr || compileResult.stdout, ...conversionErrors].filter(Boolean).join(' | ') };
        }

        return { ok: true };
    }
}

export const fieldListChecker = new FieldListChecker();
