# Change Log

All notable changes to the "vscode-rpgle" extension can be found in the
[Releases](https://github.com/codefori/vscode-rpgle/releases) section of the GitHub repository.

### Added

- **Input specification (`I` spec) parsing** (fixed-format RPG IV): the parser now recognises and processes all four I spec sub-types — `programRecord`, `programField`, `externalRecord`, and `externalField`.
- New `parseISpec()` and `prettyTypeFromISpecTokens()` functions in `language/models/fixed.ts` for decoding fixed-format I spec column layout.
- New `trimQuotes()` utility exported from `language/tokens.ts`.
- `cache.inputs` getter — returns all `Declaration` objects whose type is `"input"`, mirroring the existing `cache.structs`, `cache.files`, etc. accessors.
- Document Symbols provider now surfaces Input spec records as `Interface` symbols with their fields listed as `Property` children, making them visible in the Outline view.
- Constants with sub-items are now shown as `Enum` / `EnumMember` symbols in the Outline view.
- Test fixtures for `QRPGLESRC` and `QCBBLESRC` physical files (`tests/tables/qrpglesrc.ts`, `tests/tables/qcbblesrc.ts`).
- Test suite `tests/suite/ispec.test.ts` covering I spec rename/prefix handling, column-position range maths, and program-described file field definitions.
- The popup Hover, Go-to-Definition, and Find-All-References now work for symbols defined on Input specs, subject to the same fixed-format limitations that already existed for F, D, and C specs: because `getWordRangeAtPosition` is not spec/column aware, `hover` does not trigger when the cursor is positioned on a name that is immediately adjacent to a non-space character (e.g. the spec-type letter `D` in col 6 with the name starting in column 7, or a decimal-position digit in col 48 of an I spec and the field name immediately after it.) It is a future objective to enhance this feature.

### Fixed

- **Linter diagnostics not clearing when `**FREE` is removed**: previously, if a source file was opened as `**FREE` (activating the linter) and `**FREE` was later removed or the file was re-opened without it, stale linter diagnostics persisted indefinitely. `refreshLinterDiagnostics` now explicitly sends an empty diagnostics notification when the file is not `**FREE`, clearing any previously published warnings.

### Changed

- `cache.find()` and `cache.findAll()` now accept an optional `ignorePrefix` parameter and internally delegate sub-item lookup to a new private `findSubfields()` method, improving correctness when files declare a `PREFIX` keyword.
- `prettyTypeFromToken` renamed to `prettyTypeFromDSpecTokens` for clarity.
- `language/models/fixed.js` converted to `language/models/fixed.ts` (TypeScript) to support the new typed I spec APIs; the original `.js` file has been removed.
