# Change Log

All notable changes to the "vscode-rpgle" extension can be found in the
[Releases](https://github.com/codefori/vscode-rpgle/releases) section of the GitHub repository.

## [Unreleased]

### Added - OPM RPG Language Support

- **OPM (Original Program Model) RPG Parser**: Full support for legacy OPM RPG language
  - New [`OpmParser`](language/opm/parser.ts) class for fixed-format specification parsing
  - Complete specification type support: Control (H), File (F), Extension (E), Input (I), Calculation (C), and Output (O) specs
  - [`parseSpecification()`](language/opm/specs.ts) function with typed specification objects for all OPM spec types
  - Symbol extraction for: files, data structures, variables, constants, subroutines, PLISTs, KLISTs, and CALL statements
  - External file format resolution via table fetch (EXTNAME support)
  - Include file processing (`/COPY` directive support)
  - Embedded SQL recognition and aggregation
  - Local Data Area (LDA) marker detection (`**`) to stop parsing at compile-time data

- **Dual Parser Architecture**
  - [`ParserFactory`](language/parserFactory.ts) class for intelligent parser routing based on RPG language variant
  - Reorganized ILE parser to [`language/ile/`](language/ile/) subdirectory for clean separation
  - Common [`IParser`](language/parserFactory.ts:12-18) interface implemented by both parsers
  - Shared table fetch and include file resolution between OPM and ILE parsers
  - Dynamic parser selection in language server based on RPG language variant

- **Language Server Integration**
  - VS Code language activation for OPM RPG via `onLanguage:rpg` event
  - All providers updated to use appropriate parser:
    - Completions, hover, definitions, references, rename, signature help
    - Document symbols (outline view)
    - Code actions and linting
  - Unified cache model shared between both parsers

- **Comprehensive Test Suite**
  - [`tests/suite/opm/scope.test.ts`](tests/suite/opm/scope.test.ts) - 8 parser integration tests covering real-world OPM scenarios
  - [`tests/suite/opm/specs.test.ts`](tests/suite/opm/specs.test.ts) - Specification parsing validation tests
  - 7 OPM test fixtures covering various patterns: data structures, file operations, subroutines, PLISTs, KLISTs, edge cases
  - Test coverage for: symbol resolution, external formats, multi-line C-specs, constants, LDA boundaries

### Changed - OPM RPG Language Support

- **Column Assistant and Fixed-Format Tools now support both RPG language variants**:
  - All commands (`Shift+F4`, `Ctrl+Shift+F4`, `Ctrl+[`, `Ctrl+]`) now work with both ILE RPG and OPM RPG
  - Added **OPM-specific spec definitions** (`opmSpecs` and `opmSpecRulers`) in [`specs.ts`](extension/client/src/schemas/specs.ts) with correct RPG III column positions
  - Column Assistant automatically uses correct spec definitions based on RPG language variant
  - **OPM specs supported**: H-spec (Control), E-spec (Extension), F-spec (File), I-spec (Input), C-spec (Calculation), O-spec (Output)
  - **Critical fix**: OPM and ILE have **different column positions** for specs (e.g., C-spec Factor1 is 18-27 in OPM vs 12-25 in ILE)
  - Updated `documentIsFree()` to recognize OPM as always fixed-format
  - Language ID checks updated throughout [`columnAssist.ts`](extension/client/src/language/columnAssist.ts) and [`package.json`](package.json)
- Folder structure reorganized for dual-parser architecture:
  - ILE parser moved from `language/*.ts` to `language/ile/*.ts`
  - OPM parser added in `language/opm/` directory
  - Shared models remain in `language/models/`
- All language server providers now use `getParser(uri)` for dynamic parser selection
- Extension now supports both ILE RPG (`.rpgle`/`.sqlrpgle`) and OPM RPG (`.rpg`/`.sqlrpg`) language variants



### Added - Previous ILE RPG Enhancements

- **Input specification (`I` spec) parsing** (fixed-format ILE RPG): the parser now recognises and processes all four I spec sub-types — `programRecord`, `programField`, `externalRecord`, and `externalField`.
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
