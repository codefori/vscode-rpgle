import { expect } from "vitest";

export const assertCache = <T>(value: T | undefined): T => {
    expect(value).toBeDefined();
    if (value === undefined) {
        throw new Error(`Expected parser cache to be defined`);
    }
    return value;
};

export const assertFound = <T>(value: T | undefined, name: string): T => {
    expect(value, `${name} should exist`).toBeDefined();
    if (value === undefined) {
        throw new Error(`Expected ${name} to exist`);
    }
    return value;
};

export const assertScope = <T>(value: T | undefined, name: string): T => {
    expect(value, `${name} scope should exist`).toBeDefined();
    if (value === undefined) {
        throw new Error(`Expected ${name} scope to exist`);
    }
    return value;
};