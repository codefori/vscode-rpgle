import Cache from "./models/cache";
import Declaration from "./models/declaration";

export type Keywords = { [key: string]: string | boolean };
export type RpgleVariableType = `char` | `varchar` | `int` | `uns` | `packed` | `zoned` | `ind` | `date` | `time` | `timestamp` | `pointer` | `float` | `graph`;
export interface RpgleTypeDetail {
    type?: { name: RpgleVariableType, value?: string };
    reference?: Declaration;
}

export namespace Utils {
    export function prettyKeywords(keywords: Keywords, filter: boolean = false): string {
        const filteredKeywords = ['QUALIFIED', 'EXPORT'];

        return Object.keys(keywords).map(key => {
            if ((!filter || !filteredKeywords.includes(key)) && keywords[key]) {
                if (typeof keywords[key] === `boolean`) {
                    return key.toLowerCase();
                }

                return `${key.toLowerCase()}(${keywords[key]})`;
            } else {
                return undefined;
            }
        }).filter(k => k).join(` `);
    }

    /**
     * Typically used to resolve the type of a procedure correctly.
     */
    export function resolveType(docs: Cache, def: Declaration): RpgleTypeDetail {
        const keywords = def.keyword;
        let refName: string;
        let reference: Declaration | undefined;

        if (typeof keywords[`LIKEDS`] === `string`) {
            refName = (keywords[`LIKEDS`] as string).toUpperCase();
            reference = docs.structs.find(s => s.name.toUpperCase() === refName);

            return { reference };
        } else if (typeof keywords[`LIKE`] === `string`) {
            refName = (keywords[`LIKE`] as string).toUpperCase();
            reference = docs.variables.find(s => s.name.toUpperCase() === refName);

            if (!reference) {
                // Like does technically work on structs too, so let's check those
                // Though it's recommend to use LIKEDS in modern code
                reference = docs.structs.find(s => s.name.toUpperCase() === refName);
            }

            if (!reference) {
                // LIKE can also be used on procedures, and it will return the return type of the procedure
                reference = docs.procedures.find(s => s.name.toUpperCase() === refName);
                if (reference) {
                    return this.resolveType(reference);
                }
            }

            return { reference }
        } else {
            const validTypes: RpgleVariableType[] = [`char`, `varchar`, `int`, `uns`, `packed`, `zoned`, `ind`, `date`, `time`, `timestamp`, `pointer`, `float`, `graph`];
            const type = Object.keys(keywords).find(key => validTypes.includes(key.toLowerCase() as RpgleVariableType));
            if (type) {
                return { type: { name: (type.toLowerCase() as RpgleVariableType), value: keywords[type] as string } };
            }
        }

        return {};
    }
}