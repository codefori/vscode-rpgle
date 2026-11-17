export type TableCacheEntry = {
    fetchedAt: number,
    source: 'UDTF' | 'DSPFFD',
    data: any[]
};

export function shouldUseCachedTableMetadata(
    cached: TableCacheEntry | undefined,
    now: number,
    cacheTtlMs = 30 * 60 * 1000
): boolean {
    return !!cached && (now - cached.fetchedAt) <= cacheTtlMs;
}
