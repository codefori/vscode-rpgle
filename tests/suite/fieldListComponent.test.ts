import { describe, expect, test, vi } from 'vitest';

vi.mock('vscode', () => ({
    workspace: {
        getConfiguration: () => ({
            get: () => '*TEMPLIB'
        })
    }
}));

import { fieldListChecker } from '../../language/components/field_list/fieldListChecker';

describe('FIELD_LIST component contract', () => {
    test('exposes a component identity with a version and content signature', () => {
        const id = fieldListChecker.getIdentification();

        expect(id).toMatchObject({
            name: 'field-list-udtf',
            version: 2,
            signature: expect.any(String)
        });
        expect(id.signature).toMatch(/^[0-9a-f]{64}$/);
    });

    test('signature is stable across calls', () => {
        expect(fieldListChecker.getIdentification().signature)
            .toBe(fieldListChecker.getIdentification().signature);
    });

    test('embeds RPGLE source so no C/C++ compiler is required', () => {
        const src = fieldListChecker.getSourceSrc();

        expect(src).toContain('**free');
        expect(src).toContain(`dcl-pi main extpgm('FIELD_LIST')`);
    });

    test('SQL DDL declares LANGUAGE RPGLE and a matching external name', () => {
        const sql = fieldListChecker.getSQLSrc('ILEDITOR', 2);

        expect(sql).toContain('LANGUAGE RPGLE');
        expect(sql).toContain(`EXTERNAL NAME 'ILEDITOR/FIELD_LIST'`);
        expect(sql).not.toContain('C++');
    });

    test('returns a SecureComponentState object', async () => {
        const state = await fieldListChecker.getRemoteState({
            config: { tempLibrary: 'QTEMP' },
            runSQL: async () => [{ LONG_COMMENT: '2 - installed' }]
        } as any);

        expect(state.status).toBe('Installed');
        expect(state.remoteSignature).toBe(fieldListChecker.getIdentification().signature);
    });

    test('reports NeedsUpdate when the remote version is older or missing', async () => {
        const older = await fieldListChecker.getRemoteState({
            config: { tempLibrary: 'QTEMP' },
            runSQL: async () => [{ LONG_COMMENT: '1 - c++ build' }]
        } as any);
        expect(older.status).toBe('NeedsUpdate');

        const missing = await fieldListChecker.getRemoteState({
            config: { tempLibrary: 'QTEMP' },
            runSQL: async () => []
        } as any);
        expect(missing.status).toBe('NeedsUpdate');
    });
});
