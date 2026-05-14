# RPG Linter CLI

This is a command-line interface (CLI) for the RPG Linter, derived from the vscode-rpgle extension. It allows you to lint your RPG code from the command line, using the same rules and configuration as the vscode-rpgle extension.

## Installation

`rpglint` can be installed through npm. You can see the package on npmjs.com! `rpglint` is intended to be installed globally and not at a project level. To do that, you can simply run:

```bash
npm i @halcyontech/rpglint -g
```

## Usage

You can use the following command to see the available parameters:

```bash
rpglint -h
```

The CLI accepts several command-line arguments to customize its behavior:

- `-f` or `--files`: A glob pattern used to search for source files in the working directory. Defaults to `**/*.{SQLRPGLE,sqlrpgle,RPGLE,rpgle}`.
- `-d` or `--cwd`: The directory where the source code lives. Defaults to the current working directory.
- `-m` or `--max`: The maximum number of errored files before the process ends itself.
- `-o` or `--output`: The format of the lint errors in standard out. Defaults to `standard`. Available options are `standard` and `flc`.
- `-h` or `--help`: Displays help information.

The CLI uses a configuration file named `rpglint.json` located in the working directory (usually in `.vscode/rpglint.json`) to determine the linting rules.

## Example

Let's test `rpglint` with the IBM sample repository, Company System! `ibmi-company_system` is available on GitHub and anybody can clone it. For this example, we will clone it to our local device and then run `rpglint` against it.

```bash
# cd to where the repo will be created
cd ~/Downloads/

# clone the repository. HTTPS clone should work for everyone
git clone git@github.com:IBM/ibmi-company_system.git

# cd into the repository
cd ibmi-company_system
```

By default, there are a bunch of errors in this repository. We do this to show off that `rpglint` works as expected! You can just use `rpglint` inside of this directory for the linter to run!

```bash
barry$ pwd
~/Downloads/ibmi-company_system

barry$ rpglint
Linting 3 files.

/Users/barry/Downloads/ibmi-company_system/qrpglesrc/employees.pgm.sqlrpgle: 22 errors.
        Line 8: expected indent of 0, got 6
        Line 12: expected indent of 0, got 6
        Line 38: expected indent of 0, got 6
        Line 39: expected indent of 0, got 6
        Line 45: expected indent of 0, got 8
        Line 64: expected indent of 0, got 8
        Line 4, column 0: Variable name casing does not match definition.
        Line 8, column 6: Comments must be correctly formatted.
        Line 10, column 0: Directives must be in uppercase.
        Line 12, column 6: Comments must be correctly formatted.
        Line 14, column 0: Variable name casing does not match definition.
        Line 38, column 6: Comments must be correctly formatted.
        Line 39, column 6: Comments must be correctly formatted.
        Line 45, column 8: Comments must be correctly formatted.
        Line 64, column 8: Comments must be correctly formatted.
        Line 74, column 2: Variable name casing does not match definition.
        Line 107, column 8: Variable name casing does not match definition.
        Line 116, column 2: Variable name casing does not match definition.
        Line 141, column 6: Variable name casing does not match definition.
        Line 91, column 2: Same string literal used more than once. Consider using a constant instead.
        Line 93, column 4: Same string literal used more than once. Consider using a constant instead.
        Line 101, column 6: Same string literal used more than once. Consider using a constant instead.
rpglint simply sp
```

`rpglint` simply spits out all the issues it finds with the RPGLE code in the repository folder. The intention of a linter is to enforce code standards in a code base. When the linter finds errors, it will have an exit code of `1` and `0` if there are none.

## rpglint in your CI process

Because we are able to use `rpglint` from the command line, this means we've opened up a totally new world to ourselves. We can now run `rpglint` as part of a CI (Continuous Integration) process. For example, here's a GitHub Action .yml which configures `rpglint` to run when a PR request is created or updated against the main branch:

```yaml
name: rpglint CI

on:
  pull_request:
    # Set your workflow to run on pull_request events that target the main branch
    branches: [ "main" ]

jobs:
  build:
    runs-on: ubuntu-latest

    strategy:
      matrix:
        node-version: [18.x]

    steps:
    - uses: actions/checkout@v3
    - name: Use Node.js ${{ matrix.node-version }}
      uses: actions/setup-node@v3
      with:
        node-version: ${{ matrix.node-version }}
        cache: 'npm'
    - run: npm i -g rpglint
    - run: rpglint
```

The best part about the `rpglint` command is that it returns exit code `1` if there are errors. CI tools (like GitHub Actions, Jenkins, GitLab Runner, etc) can use this status code to determine if the CI was successful. It makes sense that the CI fails if there are lint errors, because it doesn't match the code standard defined in the `rpglint.json`.
