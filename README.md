# vscode-rpgle README

<img src="https://github.com/halcyon-tech/vscode-rpgle/blob/main/media/logo.png?raw=true" height="180px" align="right">

Adds functionality to improve writing RPGLE free-format (only `**FREE`), including:

* Content assist
* Outline view
* Linter
* Column assist for fixed-format RPGLE.

Depends on Code for IBM i due to source code living on the remote system.

## How to enable

Enable these options in VS Code settings.

* `vscode-rpgle.rpgleColumnAssistEnabled` - column assist for RPGLE fixed-format.
   * `vscode-rpgle.rpgleColumnAssistant` / Shift+F4 to launch it when on fixed-format line
* `vscode-rpgle.rpgleContentAssistEnabled` - shows content assist and enables the following commands
   * `vscode-rpgle.rpgleOpenInclude` / Shift+F12 when on copy or include statement
* `vscode-rpgle.rpgleLinterSupportEnabled` - enables linter
   * `vscode-rpgle.openLintConfig` to open or create linter file. Creates / opens relative to source that is currently open. Read more below on linting.

# Linter

The linter configuration is held in a JSON document relative to the RPGLE source code that is being worked on.

If the user is developing in `LIB/QRPGLESRC/MYSOURCE.RPGLE`, then the linter settings exist in `LIB/VSCODE/RPGLINT.JSON`. Each library has its own configuration file, binding it to all sources in that library. When developing in the IFS, it will create `.vscode/rpglint.json` in the current working directory.

See `./src/schemas/rpglint.json` for the available linter options.

Use `vscode-rpgle.openLintConfig` to open the configuration for the source you're working in. If it does not exist, it will ask the user to create one and it will provide some defaults.
