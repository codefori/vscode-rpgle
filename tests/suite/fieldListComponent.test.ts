import { describe, expect, test, vi } from 'vitest';

const providerSetting = vi.hoisted(() => ({ value: 'UDTF' }));

vi.mock('vscode', () => ({
    workspace: {
        getConfiguration: () => ({
            get: (setting: string, defaultValue: string) => setting === 'fieldMetadataProvider' ? providerSetting.value : defaultValue
        })
    }
}));

import { fieldListChecker } from '../../language/components/field_list/fieldListChecker';

describe('FIELD_LIST component contract', () => {
    test('exposes a component identity without claiming a local signature is remote', () => {
        const id = fieldListChecker.getIdentification();

        expect(id).toMatchObject({
            name: 'field-list-udtf',
            version: 2,
            signature: ''
        });
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
            getConfig: () => ({ tempLibrary: 'QTEMP' }),
            getContent: () => ({ getSQLRoutineSignature: async () => 'QTEMP/FIELD_LIST' }),
            runSQL: async () => [{ LONG_COMMENT: '2 - installed' }]
        } as any);

        expect(state.status).toBe('Installed');
        expect(state.remoteSignature).toBe('QTEMP/FIELD_LIST');
    });

    test('reports NeedsUpdate when the remote version is older', async () => {
        const older = await fieldListChecker.getRemoteState({
            getConfig: () => ({ tempLibrary: 'QTEMP' }),
            getContent: () => ({ getSQLRoutineSignature: async () => 'QTEMP/FIELD_LIST' }),
            runSQL: async () => [{ LONG_COMMENT: '1 - c++ build' }]
        } as any);
        expect(older.status).toBe('NeedsUpdate');
    });

    test('reports NotInstalled when the routine is missing', async () => {
        const missing = await fieldListChecker.getRemoteState({
            getConfig: () => ({ tempLibrary: 'QTEMP' }),
            getContent: () => ({ getSQLRoutineSignature: async () => '' })
        } as any);
        expect(missing.status).toBe('NotInstalled');
    });

    test('reports NeedsUpdate when FIELD_LIST points to a different program', async () => {
        const mismatched = await fieldListChecker.getRemoteState({
            getConfig: () => ({ tempLibrary: 'QTEMP' }),
            getContent: () => ({ getSQLRoutineSignature: async () => 'QTEMP/OTHER' })
        } as any);

        expect(mismatched.status).toBe('NeedsUpdate');
    });

    test('does not inspect FIELD_LIST when the CL provider is selected', async () => {
        providerSetting.value = 'CL';
        const getContent = vi.fn();

        const state = await fieldListChecker.getRemoteState({ getContent } as any);

        expect(state.status).toBe('Installed');
        expect(getContent).not.toHaveBeenCalled();
        providerSetting.value = 'UDTF';
    });
});
