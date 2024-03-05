
import setupParser from "../parserSetup";
import { test, expect } from "vitest";

const parser = setupParser();
const uri = `source.rpgle`;

test("edit1", async () => {
  const lines = [
    `     H ALTSEQ(*EXT) CURSYM('$') DATEDIT(*MDY) DATFMT(*MDY/) DEBUG(*YES)`,
    `     H DECEDIT('.') FORMSALIGN(*YES) FTRANS(*SRC) DFTNAME(name)`,
    `     H TIMFMT(*ISO)`,
    `     H COPYRIGHT('(C) Copyright ABC Programming - 1995')`,
    ``,
    `     d InType          s             10`,
    ``,
    `      *`,
    `      * Date structure for retriving userspace info`,
    `      *`,
    `     d InputDs         DS`,
    `     d  UserSpace              1     20`,
    `     d  SpaceName              1     10`,
    `     d  SpaceLib              11     20`,
    `     d  InpFileLib            29     48`,
    `     d  InpFFilNam            29     38`,
    `     d  InpFFilLib            39     48`,
    `     d  InpRcdFmt             49     58`,
    `     d Worktype        s             10    inz('*OUTQ')`,
    ``,
  ].join(`\n`);

  let currentDoc = ``;

  for (const char of lines) {
    currentDoc += char;

    await parser.getDocs(uri, currentDoc, {
      ignoreCache: true
    });
  }
});

test("edit2", async () => {
  const lines = [
    `**free`,
    `Ctl-opt datfmt(*iso) timfmt(*iso) alwnull(*usrctl) debug;`,
    ``,
    `Dcl-F TESTFILE3 Keyed Usage(*Update :*Delete);`,
    ``,
    `Dcl-Pr TESTCHAIN1 ExtPgm('TESTCHAIN1');`,
    `Parm1 Char(1);`,
    `End-Pr TESTCHAIN1;`,
    ``,
    `Dcl-Pi TESTCHAIN1;`,
    `Parm1 Char(1);`,
    `End-Pi TESTCHAIN1;`,
    ``,
    `Dcl-DS AAA;`,
    `a Char(10);`,
    `Dcl-ds a;`,
    `End-ds a;`,
    `End-Ds AAA;`,
    ``,
    `If (Parm1 = 'N');`,
    `Chain ('CHIAVE' :1) TESTFILE3;`,
    `Else;`,
    `Chain ('CHIAVE' :1) TESTFILE3;`,
    `EndIf;`,
    ``,
    `job_name = 'TESTFILE1';`,
    ``,
    `Update TESTREC;`,
    ``,
    `Return;`,
    ``,
    `// ____________________________________________________________________________`,
    `Dcl-Proc aaa;`,
    ``,
    `Dcl-Pi aaa;`,
    `end-proc;`,
    `End-Pi aaa;`,
    `// ____________________________________________________________________________`,
    ``,
    `End-Proc aaa;`,
  ].join(`\n`);

  let currentDoc = ``;

  for (const char of lines) {
    currentDoc += char;

    await parser.getDocs(uri, currentDoc, {
      ignoreCache: true
    });
  }
});

test("edit3", async () => {
  const lines = [
    `      *  Field Definitions.`,
    `      * ~~~~~~~~~~~~~~~~~~~~~~~~`,
    `     D ObjNam          s             10a`,
    `     D ObjLib          s             10a`,
    `     D ObjTyp          s             10a`,
    ``,
    `     P Obj_List        B                   Export`,
    `     D Obj_List        PI`,
    `     D    pLibrary                   10A   Const`,
    `     D    pObject                    10A   Const`,
    `     D    pType                      10A   Const`,
    `     D Result          s              5i 0`,
    ``,
    `      /Free`,
    ``,
    `          exsr $QUSCRTUS;`,
    `          ObjectLib =  pObject + pLibrary;`,
    `          WorkType = pType;`,
    ``,
    `          Format = 'OBJL0200';`,
    `          $ListObjects( Userspace : Format : ObjectLib : WorkType);`,
    `          //`,
    `          // Retrive header entry and process the user space`,
    `          //`,
    `          StartPosit = 125;`,
    `          StartLen   = 16;`,
    `          $UserSpace( Userspace : StartPosit : StartLen : GENDS);`,
    ``,
    `          StartPosit = OffsetHdr + 1;`,
    `          StartLen = %size(ObjectDS);`,
    ``,
    `          Return;`,
    ``,
    `          //--------------------------------------------------------`,
    `          // $QUSCRTUS - create userspace`,
    `          //--------------------------------------------------------`,
    `          begsr $QUSCRTUS;`,
    ``,
    `             system('DLTOBJ OBJ(QTEMP/LISTOUTQS) OBJTYPE(*USRSPC)');`,
    ``,
    `             BytesPrv = 116;`,
    `             Spacename = 'LISTOUTQS';`,
    `             SpaceLib = 'QTEMP';`,
    ``,
    `             // Create the user space`,
    `             $CreateSpace( Userspace : SpaceAttr : 4096 :`,
    `                           SpaceVal : SpaceAuth : SpaceText : SpaceRepl:`,
    `                           ErrorDs);`,
    `          endsr;`,
    `      /End-Free`,
    `     P                 E`,
    ``,
  ].join(`\n`);

  let currentDoc = ``;

  const parser = setupParser();

  for (const char of lines) {
    currentDoc += char;

    await parser.getDocs(uri, currentDoc, {
      ignoreCache: true
    });
  }
});