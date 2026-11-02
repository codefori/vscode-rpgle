import { describe, expect, it } from 'vitest';
import { shouldUseCachedTableMetadata } from '../../language/utils/tableCache';

describe('table metadata refresh behavior', () => {
    it('skips the warm cache when a manual refresh is forced', () => {
        const now = Date.now();
        const cached = {
            fetchedAt: now - 1000,
            source: 'UDTF' as const,
            data: [{ name: 'FIELD1' }]
        };

        expect(shouldUseCachedTableMetadata(cached, now, true)).toBe(false);
    });

    it('uses the warm cache during normal operation', () => {
        const now = Date.now();
        const cached = {
            fetchedAt: now - 1000,
            source: 'UDTF' as const,
            data: [{ name: 'FIELD1' }]
        };

        expect(shouldUseCachedTableMetadata(cached, now, false)).toBe(true);
    });
});
