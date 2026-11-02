/**
 * Shared helpers for Code for IBM i component content signatures.
 *
 * The install library is an installation detail, not a functional change to a
 * component, so it is normalized to a single token before hashing. This keeps
 * the component signature stable even when *TEMPLIB resolves to a different
 * library on different connections.
 */

export const COMPONENT_SIGNATURE_LIBRARY_TOKEN = '__VSCODE_RPGLE_COMPONENT_LIB__';

function escapeRegExp(value: string): string {
    return value.replace(/[.*+?^${}()|[\]\\]/g, '\\$&');
}

/**
 * Normalizes generated SQL so a component signature is stable regardless of
 * which IBM i library the UDTF ultimately gets installed into.
 */
export function normalizeComponentSqlForSignature(sql: string, library?: string): string {
    let normalized = sql;

    const targetLibrary = (library ?? '').trim();
    if (targetLibrary) {
        normalized = normalized.replace(new RegExp(escapeRegExp(targetLibrary), 'gi'), COMPONENT_SIGNATURE_LIBRARY_TOKEN);
    }

    return normalized
        .replace(/\bSQLTOOLS\b/gi, COMPONENT_SIGNATURE_LIBRARY_TOKEN)
        .replace(/\$\{(library|functionLibrary)\}/gi, COMPONENT_SIGNATURE_LIBRARY_TOKEN);
}
