# vscode-rpgle README

<img src="https://github.com/codefori/vscode-rpgle/blob/main/media/logo.png?raw=true" height="180px" align="right">

Adds functionality to assist in writing accurate, readable and consistent RPG language code, including:

- Content assist
- Outline view
- Linter, including indentation checking and reformatting (`**FREE` only)
- Column assist for fixed-format RPGLE and OPM RPG

Depends on the Code for IBM i extension due to source code living on the remote system when developing with source members.

## Documentation

Check out the [official documentation](https://codefori.github.io/docs/#/pages/extensions/rpgle/linter) for a usage guide.

For indentation warnings in `**FREE` source, place the cursor on the affected line
or select several lines, then choose **Fix indentation** from Quick Fix (`Ctrl+.`
on Windows/Linux, `Cmd+.` on macOS). This corrects indentation on those lines using
your linter configuration, without applying other formatting changes. Lines marked
with `@rpglint-skip` or `@rpglint-skip-indent` are respected.
Tabs in leading whitespace are reported and replaced with the configured number
of spaces. Skip directives suppress the indicated line's warning while block depth
continues to be tracked. Quick Fix and Format Document avoid indentation edits in
incomplete or ambiguous blocks. Indentation inside conditional sections is left unchanged; fixes resume
after `/ENDIF` when all branches leave compatible block structure and indentation.
On a block selection it offers the indentation fix without listing individual lint
fixes for every selected line. Other lint fixes remain available on their diagnostics.
The shared checks also supply the `rpglint` CLI, including explicit diagnostics for tabs.

# Developing

1. Fork & clone
2. `npm i`
3. Run

## Debugging

To run the tests, you have two options:

1. `npm run test` from the command line
2. 'Debug Tests' from the VS Code debugger

To run debug the extension and server, from the VS Code debugger:

1. Debug 'Launch Client'
2. Debug 'Attach to Server'

## Testing

The test suite covers both RPG language variants:

- **ILE RPG tests**: `tests/suite/*.test.ts`
- **Language server action, formatting and diagnostic tests**: `extension/server/tests/*.test.ts`
- **CLI entry-point tests**: `tests/suite/rpglint.test.ts`
- **OPM RPG tests**: `tests/suite/opm/*.test.ts`
  - Specification parsing (`specs.test.ts`)
  - Symbol resolution and scoping (`scope.test.ts`)
  - Real-world OPM fixtures (`tests/fixtures/opm/*.rpg`)

Language server tests are included in `tests/tsconfig.json` for `npm run compile:tests`.
Run `npm run lint` to check the extension, shared language code, CLI entry point,
and the server and CLI tests. CI runs this same command.

# Previous contributors

Thanks so much to everyone [who has contributed](https://github.com/codefori/vscode-rpgle/graphs/contributors).

- [@worksofliam](https://github.com/worksofliam)
- [@SJLennon](https://github.com/SJLennon)
- [@sebCIL](https://github.com/sebCIL)
- [@p-behr](https://github.com/p-behr)
- [@chrjorgensen](https://github.com/chrjorgensen)
- [@sebjulliand](https://github.com/sebjulliand)
- [@richardm90](https://github.com/richardm90)
- [@wright4i](https://github.com/wright4i)
- [@SanjulaGanepola](https://github.com/SanjulaGanepola)
- [@bobcozzi](https://github.com/bobcozzi)
- [@buzzia2001](https://github.com/buzzia2001)
- [@Mohammed-Yaseen-Ali-2081](https://github.com/Mohammed-Yaseen-Ali-2081)
- [@eric-simpson](https://github.com/eric-simpson)
- [@bpmcpherson](https://github.com/bpmcpherson)
- [@arora23saurabh](https://github.com/arora23saurabh)
