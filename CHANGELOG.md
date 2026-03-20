# Change Log

All notable changes to the "vscode-rpgle" extension can be found in the
[Releases](https://github.com/codefori/vscode-rpgle/releases) section of the GitHub repository.
## Unreleased

### Fixed

- Fixed type inference for fixed-format D-specifications when the data type position (40) is blank
  - Correctly distinguishes between "no decimals specified" and "decimals = 0"
  - Applies proper RPGLE type inference rules:
    - Standalone fields (S) with decimals → Packed
    - Standalone fields (S) without decimals → Char
    - Subfields with decimals → Zoned
    - Subfields without decimals → Char
  - Resolves 5 failing test cases in fixed-format parser tests

- Fixed hover tooltips not displaying for data structure subfields
  - Added support for qualified data structures (accessed as `DS.FIELD`)
  - Added support for unqualified data structures (accessed as standalone `FIELD`)
  - Hover text now displays parent structure context: `parent.field Type QUALIFIED` or `*UNQUALIFIED` or `*UNNAMED`
  - Shows description tags from external file metadata when available
  - Displays reference count in hover information