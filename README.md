# vscode-rpgle README

<img src="https://github.com/halcyon-tech/vscode-rpgle/blob/main/media/logo.png?raw=true" height="180px" align="right">

Adds functionality to improve writing RPGLE free-format (only `**FREE`), including:

* Content assist
* Outline view
* Linter
* Column assist for fixed-format RPGLE.

Depends on Code for IBM i due to source code living on the remote system.

## FAQ

1. **Does this only work with free-format?** The content assist and outline view only works with completely free-format (`**FREE`). The column assist is for fixed-format.
2. **My copybook is not opening or prototypes are not coming from my copybook.** Right now, it is an explicit path to your member or streamfile. For example, if you're editing `YOURLIB/QRPGLESRC/HELLOWORLD.rpgle` and your copybook path is `QRPGLEREF,PRTYPE`, then it will assume the library of `YOURLIB`. For streamfiles, it will be relative to your working directory. For local files, it will be your VS Code workspace.
3. **Does this work with local projects?** Yes! But, your local RPGLE must be the IFS style path on your `copy` and `include` directives.
4. **Why am I getting indentation errors?** When the linter is enabled in the settings, you will start to get errors based on the lint configuration. Indentation errors are based on the indentation setting from VS Code (or the assumed indentation from the current file)

## How to enable

Enable these options in VS Code settings.

* `vscode-rpgle.rpgleLanguageToolsEnabled` - enabled by default
   * Gives outline view, go to defintion and find references
* `vscode-rpgle.rpgleLinterSupportEnabled` - disabled by default
   * `vscode-rpgle.openLintConfig` to open or create linter file. Creates / opens relative to source that is currently open. Read more below on linting.
* `vscode-rpgle.showFixedFormatOutline` - column assist for RPGLE fixed-format.
   * `vscode-rpgle.rpgleColumnAssistant` / Shift+F4 to launch it when on fixed-format line

## Developing

1. Fork & clone
2. `npm i`
3. Run
   * `npm run test`
   * Debug 'Run Extension'

# Linter

The linter configuration is held in a JSON document relative to the RPGLE source code that is being worked on.

If the user is developing in `LIB/QRPGLESRC/MYSOURCE.RPGLE`, then the linter settings exist in `LIB/VSCODE/RPGLINT.JSON`. Each library has its own configuration file, binding it to all sources in that library. When developing in the IFS, it will create `.vscode/rpglint.json` in the current working directory.

See `./src/schemas/rpglint.json` for the available linter options.

Use `vscode-rpgle.openLintConfig` to open the configuration for the source you're working in. If it does not exist, it will ask the user to create one and it will provide some defaults.
