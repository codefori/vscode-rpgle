export type TableCacheEntry = {
    fetchedAt: number,
    source: 'UDTF' | 'DSPFFD',
    data: any[]
};

export function shouldUseCachedTableMetadata(
    cached: TableCacheEntry | undefined,
    now: number,
    forceRefresh = false,
    cacheTtlMs = 30 * 60 * 1000
): boolean {
    if (forceRefresh) {
        return false;
    }

    return !!cached && (now - cached.fetchedAt) <= cacheTtlMs;
}
