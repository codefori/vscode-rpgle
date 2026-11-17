export function pruneOldest<K, V>(map: Map<K, V>, limit: number, onEvict?: (value: V) => void): void {
    while (map.size > limit) {
        const oldest = map.entries().next();
        if (oldest.done) return;

        const [key, value] = oldest.value;
        map.delete(key);
        onEvict?.(value);
    }
}