import { describe, expect, it } from 'vitest';
import { shouldUseCachedTableMetadata } from '../../language/utils/tableCache';
import { pruneOldest } from '../../language/utils/pruneOldest';

describe('table metadata refresh behavior', () => {
    it('skips the warm cache when a manual refresh is forced', () => {
        const now = Date.now();
        const cached = {
            fetchedAt: now - 1000,
            source: 'UDTF' as const,
            data: [{ name: 'FIELD1' }]
        };

        expect(shouldUseCachedTableMetadata(cached, now, 0)).toBe(false);
    });

    it('uses the warm cache during normal operation', () => {
        const now = Date.now();
        const cached = {
            fetchedAt: now - 1000,
            source: 'UDTF' as const,
            data: [{ name: 'FIELD1' }]
        };

        expect(shouldUseCachedTableMetadata(cached, now)).toBe(true);
    });

    it('evicts the oldest entry when a bounded map exceeds its limit', () => {
        const cache = new Map([
            ['first', 1],
            ['second', 2],
        ]);

        pruneOldest(cache, 1);

        expect([...cache.entries()]).toEqual([['second', 2]]);
    });
});
