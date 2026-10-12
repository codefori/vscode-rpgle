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
    test('exposes a component identity with a version', () => {
        const id = fieldListChecker.getIdentification();

        expect(id).toMatchObject({
            name: 'field-list-udtf',
            version: 1,
            signature: expect.any(String)
        });
    });

    test('returns a component state instead of a raw object wrapper', async () => {
        const state = await fieldListChecker.getRemoteState({
            config: { tempLibrary: 'QTEMP' },
            runSQL: async () => [{ LONG_COMMENT: '1 - installed' }]
        } as any);

        expect(state).toBe('Installed');
    });
});
