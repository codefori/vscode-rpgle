import path = require("path");
import { CodeAction, CodeActionKind, commands, Disposable, languages, Position, Range, TextDocument, TextEdit, ThemeIcon, Uri, workspace, WorkspaceEdit, WorkspaceFolder } from "vscode";
import Cache from "../../../language/models/cache";
import Declaration from "../../../language/models/declaration";
import { getInstance } from "./base";
import { RpgleTypeDetail, RpgleVariableType, Utils } from "../../../language/utilts";

export namespace TestCodeActions {
    interface TestCaseSpec {
        prototype: string[];
        testCase: string[];
        includes: string[];
    }

    export function getTestCodeActions(): Disposable[] {
        const disposables: Disposable[] = [];

        // Setup code actions
        disposables.push(
            languages.registerCodeActionsProvider({ language: 'rpgle' },
                {
                    async provideCodeActions(document, range, context, token) {
                        const codeActions: CodeAction[] = [];

                        if (document) {
                            const docs = await getDocs(document.uri);
                            if (docs) {
                                const testStubCodeActions = await getTestStubCodeActions(document, docs, range);
                                if (testStubCodeActions) {
                                    codeActions.push(...testStubCodeActions);
                                }
                            }
                        }

                        return codeActions;
                    }
                }
            )
        );

        // Set commands
        disposables.push(
            commands.registerCommand('vscode-rpgle.generateTestStub', async (document: TextDocument, docs: Cache, testFileName: string, exportProcedures: Declaration[]) => {
                // Build test file URI
                let testFileUri: Uri;
                const workspaceFolder = workspace.getWorkspaceFolder(document.uri);
                if (workspaceFolder && workspaceFolder.uri.scheme === 'file') {
                    const testFilePath = path.posix.join(workspaceFolder.uri.fsPath, 'qtestsrc', testFileName);
                    testFileUri = Uri.file(testFilePath);
                } else if (document.uri.scheme === 'member') {
                    const ibmi = getInstance();
                    const connection = ibmi!.getConnection();
                    const parsedPath = connection.parserMemberPath(document.uri.fsPath);
                    const testFilePath = parsedPath.asp ?
                        path.posix.join(parsedPath.asp, parsedPath.library, 'QTESTSRC', testFileName) :
                        path.posix.join(parsedPath.library, 'QTESTSRC', testFileName);
                    testFileUri = Uri.from({ scheme: 'member', path: `/${testFilePath}` });
                } else {
                    return;
                }

                // Build test suite
                // TODO: Workspace folder is optional
                const newTestCases = await Promise.all(exportProcedures.map(async proc => await getTestCaseSpec(docs, proc, workspaceFolder!)));
                const newTestSuite = generateTestSuite(newTestCases);

                // Build workspace edit
                const workspaceEdit = new WorkspaceEdit();
                workspaceEdit.createFile(
                    testFileUri,
                    {
                        ignoreIfExists: true
                    },
                    {
                        label: `Create test file ${testFileName}`,
                        iconPath: new ThemeIcon('file'),
                        needsConfirmation: true
                    }
                );
                // TODO: Fix label
                workspaceEdit.insert(
                    testFileUri,
                    new Position(0, 0),
                    newTestSuite.join(`\n`),
                    {
                        label: `Generate test suite`,
                        iconPath: new ThemeIcon('symbol-method'),
                        needsConfirmation: true
                    }
                );

                await workspace.applyEdit(workspaceEdit);
            })
        )

        return disposables;
    }

    async function getDocs(uri: Uri): Promise<Cache | undefined> {
        return await commands.executeCommand('vscode-rpgle.server.getCache', uri);
    }

    export async function getTestStubCodeActions(document: TextDocument, docs: Cache, range: Range): Promise<CodeAction[] | undefined> {
        const codeActions: CodeAction[] = [];

        const exportProcedures = docs.procedures.filter(proc => proc.keyword[`EXPORT`]);
        if (exportProcedures.length > 0) {
            // Build test file name
            const parsedPath = path.parse(document.uri.fsPath);
            const fileName = parsedPath.base;
            const testFileName = `${parsedPath.name}.test${parsedPath.ext}`;

            // Test case generation
            const currentProcedure = exportProcedures.find(sub => sub.range.start && sub.range.end && range.start.line >= sub.range.start && range.end.line <= sub.range.end);
            if (currentProcedure) {
                const title = `Generate test case for '${currentProcedure.name}'`;
                const testCaseAction = new CodeAction(title, CodeActionKind.RefactorExtract);
                testCaseAction.command = {
                    title: title,
                    command: `vscode-rpgle.generateTestStub`,
                    arguments: [document, docs, testFileName, [currentProcedure]]
                }
                codeActions.push(testCaseAction);
            }

            // Test suite generation
            const title = `Generate test suite for '${fileName}'`;
            const testSuiteAction = new CodeAction(title, CodeActionKind.RefactorExtract);
            testSuiteAction.command = {
                title: title,
                command: `vscode-rpgle.generateTestStub`,
                arguments: [document, docs, testFileName, exportProcedures]
            }
            codeActions.push(testSuiteAction);
        }

        return codeActions;
    }

    function generateTestSuite(testCaseSpecs: TestCaseSpec[]) {
        const prototypes = testCaseSpecs.map(tc => tc.prototype.length > 0 ? [``, ...tc.prototype] : tc.prototype).flat();
        const testCases = testCaseSpecs.map(tc => tc.testCase.length > 0 ? [``, ...tc.testCase] : tc.testCase).flat();
        const allIncludes = testCaseSpecs.map(tc => tc.includes).flat();
        const uniqueIncludes = [...new Set(allIncludes)];

        return [
            `**free`,
            ``,
            `ctl-opt nomain;`,
            ...prototypes,
            ``,
            `/include qinclude,TESTCASE`,
            ...uniqueIncludes,
            ...testCases
        ]
    }

    async function getTestCaseSpec(docs: Cache, procedure: Declaration, workspaceFolder: WorkspaceFolder): Promise<TestCaseSpec> {
        // Get procedure prototype
        const prototype = await getPrototype(procedure);

        // Get inputs
        const inputDecs: string[] = [];
        const inputInits: string[] = [];
        const inputIncludes: string[] = [];
        for (const subItem of procedure.subItems) {
            const subItemType = Utils.resolveType(docs, subItem);

            const subItemDec = getDeclaration(subItemType, `${subItem.name}`);
            inputDecs.push(...subItemDec);

            const subItemInits = getInitializations(docs, subItemType, `${subItem.name}`);
            inputInits.push(...subItemInits);

            const subItemIncludes = getIncludes(subItemType, workspaceFolder);
            inputIncludes.push(...subItemIncludes);
        }

        // Get return
        const resolvedType = Utils.resolveType(docs, procedure);
        const actualDec = getDeclaration(resolvedType, 'actual');
        const expectedDec = getDeclaration(resolvedType, 'expected');
        const expectedInits = getInitializations(docs, resolvedType, 'expected');
        const returnIncludes = getIncludes(resolvedType, workspaceFolder);

        // Get unique includes
        const includes = [...new Set([...inputIncludes, ...returnIncludes])];

        // Get assertions
        const assertions = getAssertions(docs, resolvedType, 'expected', 'actual');

        const testCase = [
            `dcl-proc test_${procedure.name} export;`,
            `  dcl-pi *n extproc(*dclcase) end-pi;`,
            ``,
            ...inputDecs.map(dec => `  ${dec}`),
            ...actualDec.map(dec => `  ${dec}`),
            ...expectedDec.map(dec => `  ${dec}`),
            ``,
            `  // Input`,
            ...inputInits.map(init => `  ${init}`),
            ``,
            `  // Actual results`,
            `  actual = ${procedure.name}(${procedure.subItems.map(s => s.name).join(` : `)});`,
            ``,
            `  // Expected results`,
            ...expectedInits.map(init => `  ${init}`),
            ``,
            `  // Assertions`,
            ...assertions.map(assert => `  ${assert}`),
            `end-proc;`
        ];

        return {
            prototype,
            testCase,
            includes
        };
    }

    function getDeclaration(detail: RpgleTypeDetail, name: string): string[] {
        const declarations: string[] = [];

        if (detail) {
            if (detail.type) {
                declarations.push(`dcl-s ${name} ${detail.type.name}${detail.type.value ? `(${detail.type.value})` : ``};`);
            } else if (detail.reference) {
                declarations.push(`dcl-ds ${name} likeDs(${detail.reference.name});`);
            }
        }

        return declarations;
    }

    function getInitializations(docs: Cache, detail: RpgleTypeDetail, name: string): string[] {
        const inits: string[] = [];

        if (detail) {
            if (detail.type) {
                const defaultValue = getDefaultValue(detail.type.name);
                inits.push(`${name} = ${defaultValue};`);
            } else if (detail.reference) {
                for (const subItem of detail.reference.subItems) {
                    const subItemType = Utils.resolveType(docs, subItem);
                    const subItemInits = subItemType ?
                        getInitializations(docs, subItemType, `${name}.${subItem.name}`) : [];
                    inits.push(...subItemInits);
                }
            }
        }

        return inits;
    }

    async function getPrototype(procedure: Declaration): Promise<string[]> {
        for (const reference of procedure.references) {
            const docs = await getDocs(Uri.parse(reference.uri));
            if (docs) {
                const prototype = docs.procedures.some(proc => proc.name === procedure.name && proc.keyword['EXTPROC'])
                if (prototype) {
                    return [];
                }
            }
        }

        return [
            `dcl-pr ${procedure.name} ${Utils.prettyKeywords(procedure.keyword, true)} extproc('${procedure.name.toLocaleUpperCase()}');`,
            ...procedure.subItems.map(s => `  ${s.name} ${Utils.prettyKeywords(s.keyword, true)};`),
            `end-pr;`
        ];
    }

    function getIncludes(detail: RpgleTypeDetail, workspaceFolder: WorkspaceFolder): string[] {
        const includes: string[] = [];

        if (detail.reference) {
            const structPath = detail.reference.position.path;
            if (workspaceFolder) {
                const relativePath = asPosix(path.relative(workspaceFolder.uri.toString(), structPath));
                if (!includes.includes(relativePath)) {
                    includes.push(`/include '${relativePath}'`); // TODO: Support members style includes
                }
            }
        }

        return includes;
    }

    function getAssertions(docs: Cache, detail: RpgleTypeDetail, expected: string, actual: string): string[] {
        const assertions: string[] = [];

        if (detail) {
            if (detail.type) {
                const assertion = getAssertion(detail.type.name);
                const fieldName = actual.split(`.`).pop();
                if (assertion === `assert`) {
                    assertions.push(`${assertion}(${expected} = ${actual}${fieldName ? ` : '${fieldName}'` : ``});`);
                } else {
                    assertions.push(`${assertion}(${expected} : ${actual}${fieldName ? ` : '${fieldName}'` : ``});`);
                }
            } else if (detail.reference) {
                for (const subItem of detail.reference.subItems) {
                    const subItemType = Utils.resolveType(docs, subItem);
                    const subItemAssertions = subItemType ?
                        getAssertions(docs, subItemType, `${expected}.${subItem.name}`, `${actual}.${subItem.name}`) : [];
                    assertions.push(...subItemAssertions);
                }
            }
        }

        return assertions;
    }

    function getDefaultValue(type: RpgleVariableType): string {
        switch (type) {
            case `char`:
            case `varchar`:
                return `''`;
            case `int`:
            case `uns`:
                return `0`;
            case `packed`:
            case `zoned`:
                return `0.0`;
            case `ind`:
                return `*off`;
            case `date`:
                return `%date('0001-01-01' : *iso)`;
            case `time`:
                return `%time('00.00.00' : *iso)`;
            case `timestamp`:
                return `%timestamp('0001-01-01-00.00.00.000000' : *iso)`;
            case `pointer`:
                return `*null`;
            default:
                return 'unknown';
        }
    }

    function getAssertion(type: RpgleVariableType): string {
        switch (type) {
            case `char`:
            case `varchar`:
                return `aEqual`;
            case `int`:
            case `uns`:
                return `iEqual`;
            case `packed`:
            case `zoned`:
                return `assert`;
            case `ind`:
                return `nEqual`;
            case `date`:
                return `assert`;
            case `time`:
                return `assert`;
            case `timestamp`:
                return `assert`;
            case `pointer`:
                return `assert`;
            default:
                return 'unknown';
        }
    }

    function asPosix(inPath?: string) {
        return inPath ? inPath.split(path.sep).join(path.posix.sep) : ``;
    }
}