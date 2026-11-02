import { describe, expect, it } from 'vitest';
import Parser from '../../language/ile/parser';
import Declaration from '../../language/models/declaration';

describe('parser table fetch concurrency', () => {
    it('shares in-flight table fetch results across concurrent callers', async () => {
        const parser = new Parser();
        let fetchCalls = 0;

        parser.setTableFetch(async () => {
            fetchCalls += 1;
            await new Promise(resolve => setTimeout(resolve, 25));

            const recordFormat = new Declaration('struct');
            recordFormat.name = 'R_FORMAT';

            const field = new Declaration('subitem');
            field.name = 'FIELD1';
            recordFormat.subItems.push(field);

            return [recordFormat];
        });

        const firstPromise = parser.fetchTable('MYLIB/MYFILE');
        const secondPromise = parser.fetchTable('MYLIB/MYFILE');

        const [firstResult, secondResult] = await Promise.all([firstPromise, secondPromise]);

        expect(fetchCalls).toBe(1);
        expect(firstResult.length).toBe(1);
        expect(secondResult.length).toBe(1);
        expect(firstResult[0].subItems[0].name).toBe('FIELD1');
        expect(secondResult[0].subItems[0].name).toBe('FIELD1');
    });
});
