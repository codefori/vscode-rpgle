# Change Log

All notable changes to the "vscode-rpgle" extension can be found in the
[Releases](https://github.com/codefori/vscode-rpgle/releases) section of the GitHub repository.
## Unreleased

### Fixed

- **Shift+F4 ruler incorrectly shown on legacy embedded SQL lines**: the fixed-format spec ruler is no longer painted on C spec SQL continuation lines (column 7 = `+`, e.g. `.....C+ FROM qiws.qcustcdt;`), matching the existing behaviour for directive lines (column 7 = `/`).
- **Shift+F4 ruler persists when Cmd+Shift+F4 Column Assistant opens**: the ruler is now cleared immediately when the active editor changes (e.g. focus moves to the SEU-style Column Assistant webview panel), preventing it from staying frozen over the source until the next cursor move.
- **Shift+F4 ruler overlaps Source Change Date columns**: the opaque ruler background is now capped at 80 columns wide, so it no longer paints over the Source Change Date text that IBM i source physical files carry beyond column 80.
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