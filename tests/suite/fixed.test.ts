
import path from "path";
import setupParser from "../parserSetup";
import { test, expect } from "vitest";

const parser = setupParser();
const uri = `source.rpgle`;

test('fixed1', async () => {
  const lines = [
    ``,
    `     FINVMST    IF   E           K DISK`,
    `   `,
    `     D wkCorp          S             10    inz('100')`,
    `     D wkInvoice       S             15`,
    `   `,
    `     C                   eval      wkInvoice = 'I035552120'`,
    `   `,
    `     C                   eval      *inlr = *on`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, { ignoreCache: true, withIncludes: true });

  expect(cache.files.length).to.equal(1);
  expect(cache.variables.length).to.equal(2);

  const wkCorp = cache.variables[0];
  expect(wkCorp.name).to.equal('wkCorp');
  expect(wkCorp.position.range.line).to.equal(3);
  expect(wkCorp.keyword[`CHAR`]).to.equal('10');
  expect(wkCorp.keyword[`INZ`]).to.equal(`'100'`);

  const wkInvoice = cache.variables[1];
  expect(wkInvoice.name).to.equal('wkInvoice');
  expect(wkInvoice.position.range.line).to.equal(4);
  expect(wkInvoice.keyword[`CHAR`]).to.equal('15');
});

test('fixed2', async () => {
  const lines = [
    ``,
    `      *`,
    `      *  Field Definitions.`,
    `      *`,
    `     d Count           s              4  0`,
    `     d Format          s              8`,
    `     d GenLen          s              8`,
    `     d InLibrary       s             10`,
    `     d InType          s             10`,
    `     d ObjectLib       s             20`,
    `     d SpaceVal        s              1    inz(*BLANKS)`,
    `     d SpaceAuth       s             10    inz('*CHANGE')`,
    `     d SpaceText       s             50    inz(*BLANKS)`,
    `     d SpaceRepl       s             10    inz('*YES')`,
    `     d SpaceAttr       s             10    inz(*BLANKS)`,
    `     d UserSpaceOut    s             20`,
    `     d Worktype        s             10    inz('*OUTQ')`,
    ``,
    `     `,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, { ignoreCache: true, withIncludes: true });

  expect(cache.variables.length).to.equal(13);

  const CHARFields = cache.variables.filter(v => v.keyword[`CHAR`] !== undefined);
  expect(CHARFields.length).to.equal(12);

  const countVar = cache.variables.find(v => v.name === `Count`);
  expect(countVar.keyword[`PACKED`]).to.equal(`4:0`);
});

test('fixed3', async () => {
  const lines = [
    `     d Worktype        s             10    INZ('*OUTQ')`,
    ``,
    `      *`,
    `     d                 DS`,
    `     d  StartPosit             1      4B 0`,
    `     d  StartLen               5      8B 0`,
    `     d  SpaceLen               9     12B 0`,
    `     d  ReceiveLen            13     16B 0`,
    `     d  MessageKey            17     20B 0`,
    `     d  MsgDtaLen             21     24B 0`,
    `     d  MsgQueNbr             25     28B 0`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, { ignoreCache: true, withIncludes: true });

  expect(cache.variables.length).to.equal(1);
  expect(cache.structs.length).to.equal(1);

  const Worktype = cache.variables[0];
  expect(Worktype.name).to.equal('Worktype');
  expect(Worktype.position.range.line).to.equal(0);
  expect(Worktype.keyword[`CHAR`]).to.equal('10');
  expect(Worktype.keyword[`INZ`]).to.equal(`'*OUTQ'`);

  const DS = cache.structs[0];
  expect(DS.name).to.equal('*N');
  expect(DS.position.range.line).to.equal(3);
  expect(DS.subItems.length).to.equal(7);
});

test('fixed4', async () => {
  const lines = [
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

  const cache = await parser.getDocs(uri, lines, { ignoreCache: true, withIncludes: true });

  expect(cache.variables.length).to.equal(2);
  expect(cache.structs.length).to.equal(1);

  const InType = cache.find(`InType`);
  expect(InType.name).to.equal(`InType`);
  expect(InType.position.range.line).to.equal(1);

  const Worktype = cache.variables[1];
  expect(Worktype.name).to.equal(`Worktype`);
  expect(Worktype.position.range.line).to.equal(14);

  const InputDs = cache.structs[0];
  expect(InputDs.name).to.equal(`InputDs`);
  expect(InputDs.position.range.line).to.equal(6);
  expect(InputDs.subItems.length).to.equal(7);
});

test('fixed5', async () => {
  const lines = [
    ``,
    `      *`,
    `      *  Field Definitions.`,
    `      *`,
    `     d UserSpaceOut    s             20`,
    `     d Worktype        s             10    inz('*OUTQ')`,
    ``,
    `      *`,
    `     d                 DS`,
    `     d  StartPosit             1      4B 0`,
    `     d  StartLen               5      8B 0`,
    `     d  SpaceLen               9     12B 0`,
    `     d  ReceiveLen            13     16B 0`,
    `     d  MessageKey            17     20B 0`,
    `     d  MsgDtaLen             21     24B 0`,
    `     d  MsgQueNbr             25     28B 0`,
    ``,
    `      *-- Retrieve object description:  -------------------------------`,
    `     d RtvObjD         Pr                  ExtPgm( 'QUSROBJD' )`,
    `     d  RoRcvVar                  32767a         Options( *VarSize )`,
    `     d  RoRcvVarLen                  10i 0 Const`,
    `     d  RoFmtNam                      8a   Const`,
    `     d  RoObjNamQ                    20a   Const`,
    `     d  RoObjTyp                     10a   Const`,
    `     d  RoError                   32767a         Options( *VarSize )`,
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
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, { ignoreCache: true, withIncludes: true });

  expect(cache.variables.length).to.equal(2);
  expect(cache.structs.length).to.equal(2);
  expect(cache.procedures.length).to.equal(1);

  const RtvObjD = cache.procedures[0];
  expect(RtvObjD.name).to.equal(`RtvObjD`);
  expect(RtvObjD.position.range.line).to.equal(18);
  expect(RtvObjD.keyword[`EXTPGM`]).to.equal(`'QUSROBJD'`);
  expect(RtvObjD.subItems.length).to.equal(6);
});

test('fixed6', async () => {
  const lines = [
    ``,
    `0.00 DDATE0            S               D                                             130124`,
    `2.00 DDATE1            S               D                                             130129`,
    `0.00 DDATE2            S               D   DATFMT(*JIS)                              130129`,
    `4.00 DDATE3            S               D   INZ(D'2001-01-12')                        130129`,
    `5.00 DDATE3_CHAR       S             10                                              130129`,
    `0.00 D len             S              5I 0                                           130130`,
    `6.00 DTIME0            S               T   INZ(T'10.12.15')                          130129`,
    `0.00 DTIME0_CHAR       S              8                                              130129`,
    ``,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, { ignoreCache: true, withIncludes: true });

  expect(cache.variables.length).to.equal(8);

  const lenVar = cache.find(`len`);
  expect(lenVar.name).to.equal(`len`);
  expect(lenVar.position.range.line).to.equal(6);
  expect(lenVar.keyword[`INT`]).to.equal(`5`);

  const date2Var = cache.find(`DATE2`);
  expect(date2Var.name).to.equal(`DATE2`);
  expect(date2Var.position.range.line).to.equal(3);
  expect(date2Var.keyword[`DATE`]).to.equal(true);
  expect(date2Var.keyword[`DATFMT`]).to.equal(`*JIS`);

  const time0Var = cache.find(`TIME0`);
  expect(time0Var.name).to.equal(`TIME0`);
  expect(time0Var.position.range.line).to.equal(7);
  expect(time0Var.keyword[`TIME`]).to.equal(true);
  expect(time0Var.keyword[`INZ`]).to.equal(`T'10.12.15'`);
});

test('fixed7', async () => {
  const lines = [
    ``,
    `       // -----------------------`,
    ``,
    `     P Obj_Next        B                   Export`,
    `     D Obj_Next        PI                  LikeDS(ObjectDs)`,
    ``,
    `      /Free`,
    `          $UserSpace( Userspace : StartPosit : StartLen : ObjectDs);`,
    `          StartPosit += SizeEntry;`,
    ``,
    `          Return ObjectDs;`,
    `      /End-Free`,
    ``,
    `     P                 E`,
    ``,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, { ignoreCache: true, withIncludes: true });

  expect(cache.procedures.length).to.equal(1);

  const Obj_Next = cache.find(`Obj_Next`);
  expect(Obj_Next.name).to.equal(`Obj_Next`);
  expect(Obj_Next.position.range.line).to.equal(3);
  expect(Obj_Next.keyword[`EXPORT`]).to.equal(true);
  expect(Obj_Next.keyword[`LIKEDS`]).to.equal(`ObjectDs`);
  expect(Obj_Next.subItems.length).to.equal(0);
});

test('fixed8', async () => {
  const lines = [
    ``,
    `      **========================================================================`,
    `      ** $QUSCRTUS - API to create user space`,
    `      **========================================================================`,
    `     c     $QUSCRTUS     begsr`,
    `      **`,
    `      ** Delete old space`,
    `      **`,
    `             system('DLTOBJ OBJ(QTEMP/MEMBERS) OBJTYPE(*USRSPC)');`,
    `      **`,
    `      ** Create a user space named ListMember in QTEMP.`,
    `      **`,
    `     c                   Eval      BytesPrv = 116`,
    `     c                   Eval      SpaceName = 'MEMBERS'`,
    `     c                   Eval      SpaceLib = 'QTEMP'`,
    `      **`,
    `      ** Create the user space`,
    `      **`,
    `     c                   call(e)   'QUSCRTUS'`,
    `     c                   parm      UserSpace     UserSpaceOut`,
    `     c                   parm                    SpaceAttr`,
    `     c                   parm      4096          SpaceLen`,
    `     c                   parm                    SpaceVal`,
    `     c                   parm                    SpaceAuth`,
    `     c                   parm                    SpaceText`,
    `     c                   parm                    SpaceRepl`,
    `     c                   parm                    ErrorDs`,
    `      **`,
    `     c                   endsr`,
    ``,
    `      **========================================================================`,
    `      ** $QUSLMBR  - API List all members in a file`,
    `      **========================================================================`,
    `     c     $QUSLMBR      begsr`,
    `      **`,
    `     c                   eval      nBufLen = %size(MbrD0100)`,
    `      **`,
    `     c                   call(e)   'QUSLMBR'`,
    `     c                   parm                    UserSpaceOut`,
    `     c                   parm                    Format`,
    `     c                   parm                    FileLib`,
    `     c                   parm                    AllMembers`,
    `     c                   parm                    bOvr`,
    `     c                   parm                    ErrorDs`,
    `      *`,
    `     c                   endsr`,
    ``,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, { ignoreCache: true, withIncludes: true });

  expect(cache.subroutines.length).to.equal(2);
  expect(cache.subroutines[0].name).to.equal(`$QUSCRTUS`);
  expect(cache.subroutines[1].name).to.equal(`$QUSLMBR`);
});

test('fixed9', async () => {
  const lines = [
    ``,
    `       // -----------------------`,
    `      /copy './rpgle/copy1.rpgle'`,
    `       // -----------------------`,
    ``,
    `     P Obj_Next        B                   Export`,
    `     D Obj_Next        PI                  LikeDS(ObjectDs)`,
    ``,
    `      /Free`,
    `          $UserSpace( Userspace : StartPosit : StartLen : ObjectDs);`,
    `          StartPosit += SizeEntry;`,
    ``,
    `          Return ObjectDs;`,
    `      /End-Free`,
    ``,
    `     P                 E`,
    ``,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, { ignoreCache: true, withIncludes: true });

  expect(cache.includes.length).to.equal(1);
  expect(cache.procedures.length).to.equal(2);

  const Obj_Next = cache.find(`Obj_Next`);
  expect(Obj_Next.name).to.equal(`Obj_Next`);
  expect(Obj_Next.position.range.line).to.equal(5);
  expect(Obj_Next.keyword[`EXPORT`]).to.equal(true);
  expect(Obj_Next.keyword[`LIKEDS`]).to.equal(`ObjectDs`);
  expect(Obj_Next.subItems.length).to.equal(0);

  const theExtProcedure = cache.find(`theExtProcedure`);
  expect(theExtProcedure.name).to.equal(`theExtProcedure`);
  expect(theExtProcedure.position.range.line).to.equal(2);
  expect(theExtProcedure.keyword[`EXTPROC`]).to.equal(true);
  expect(theExtProcedure.subItems.length).to.equal(1);
});

test('fixed9_2', async () => {
  const lines = [
    ``,
    `       // -----------------------`,
    `     d/copy './rpgle/copy1.rpgle'`,
    `     */copy './rpgle/copy2.rpgle'`,
    `       // -----------------------`,
    `     P Obj_Next        B                   Export`,
    `     D Obj_Next        PI                  LikeDS(ObjectDs)`,
    ``,
    `      /Free`,
    `          $UserSpace( Userspace : StartPosit : StartLen : ObjectDs);`,
    `          StartPosit += SizeEntry;`,
    ``,
    `          Return ObjectDs;`,
    `      /End-Free`,
    ``,
    `     P                 E`,
    ``,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, { ignoreCache: true, withIncludes: true });

  expect(cache.includes.length).to.equal(1);
  expect(cache.procedures.length).to.equal(2);

  const Obj_Next = cache.find(`Obj_Next`);
  expect(Obj_Next.name).to.equal(`Obj_Next`);
  expect(Obj_Next.position.range.line).to.equal(5);
  expect(Obj_Next.keyword[`EXPORT`]).to.equal(true);
  expect(Obj_Next.keyword[`LIKEDS`]).to.equal(`ObjectDs`);
  expect(Obj_Next.subItems.length).to.equal(0);

  const theExtProcedure = cache.find(`theExtProcedure`);
  expect(theExtProcedure.name).to.equal(`theExtProcedure`);
  expect(theExtProcedure.position.range.line).to.equal(2);
  expect(theExtProcedure.keyword[`EXTPROC`]).to.equal(true);
  expect(theExtProcedure.subItems.length).to.equal(1);
});

test('fixed9_3', async () => {
  const lines = [
    ``,
    `         Ctl-Opt DftActGrp(*No);`,
    `      /copy eof4                                  Call plist update program ESF`,
    `      *COPY EQCPYLESRC,PLUPT_SB                   Call plist update program ESF`,
    ``,
    `         Dcl-s MyVariable2 Char(20);`,
    ``,
    `         Dcl-C theConstant 'Hello world';`,
    ``,
    `         dsply theConstant;`,
    ``,
    `         Return;`
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, { withIncludes: true, ignoreCache: true });

  expect(cache.includes.length).to.equal(1);
  expect(cache.variables.length).to.equal(1, `Expect length of 1`);
  expect(cache.constants.length).to.equal(1, `Expect length of 1`);
  expect(cache.procedures.length).to.equal(1, `Expect length of 1`);

  const uppercase = cache.find(`UPPERCASE`);

  const baseNameInclude = path.basename(uppercase.position.path);
  expect(baseNameInclude).to.equal(`eof4.rpgle`);
});

test('fixed10', async () => {
  const lines = [
    `     d  data           ds                  inz`,
    `     d   arid                         6`,
    `     d   ardesc                      50`,
    `     d   artifa                       3`,
    `     d   arsalePr                     7  2`,
    `     d act             c                   'act'`,
    `     D rrn02           s              7  2`,
    ``,
    `         return;`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, { ignoreCache: true, withIncludes: true });

  const dataDs = cache.find(`data`);
  expect(dataDs.name).to.equal(`data`);
  expect(dataDs.subItems.length).to.equal(4);

  expect(dataDs.range.start).to.equal(0);
  expect(dataDs.range.end).to.equal(4);

  const rrn02 = cache.find(`rrn02`);
  expect(rrn02.name).to.equal(`rrn02`);
  expect(rrn02.keyword[`PACKED`]).to.equal(`7:2`);

  const arsalePr = dataDs.subItems[3];
  expect(arsalePr.name).to.equal(`arsalePr`);
  expect(arsalePr.keyword[`ZONED`]).to.equal(`7:2`);
});

test('fixedfree1', async () => {
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

  const cache = await parser.getDocs(uri, lines, { ignoreCache: true, withIncludes: true });

  expect(cache.variables.length).to.equal(3);
  expect(cache.variables.find(i => !i.keyword[`CHAR`])).to.be.undefined;

  expect(cache.subroutines.length).to.equal(0);

  expect(cache.procedures.length).to.equal(1);

  const Obj_List = cache.find(`Obj_List`);
  expect(Obj_List.name).to.equal(`Obj_List`);
  expect(Obj_List.range.start).to.equal(6);
  expect(Obj_List.range.end).to.equal(50);
  expect(Obj_List.position.range.line).to.equal(6);
  expect(Obj_List.keyword[`EXPORT`]).to.equal(true);
  expect(Obj_List.subItems.length).to.equal(3);

  expect(Obj_List.subItems.find(i => !i.keyword[`CHAR`])).to.be.undefined;
  expect(Obj_List.subItems.find(i => !i.keyword[`CONST`])).to.be.undefined;

  const scope = Obj_List.scope;
  expect(scope.subroutines.length).to.equal(1);
  expect(scope.variables.length).to.equal(1);
});

test('fixed11', async () => {
  const lines = [
    `     D F4DATE          PR`,
    `     D                                1`,
    `     D                               15`,
    `     D                                6    OPTIONS(*NOPASS)`,
    `     D                                1    OPTIONS(*NOPASS)`,
    `      `,
    `     D F4DATEDS        DS                  QUALIFIED`,
    `     D  IOF                           1A`,
    `     D  DATE15A                      15A`,
    `     D  FORMAT                        6A`,
    `     D  VIEW                          1A`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, { ignoreCache: true, withIncludes: true });

  const F4DATE = cache.find(`F4DATE`);
  expect(F4DATE.subItems.length).to.equal(4);
  expect(F4DATE.range.start).to.equal(0);
  expect(F4DATE.range.end).to.equal(4);

  const parm1 = F4DATE.subItems[0];
  expect(parm1.keyword[`CHAR`]).to.equal(`1`);

  const parm2 = F4DATE.subItems[1];
  expect(parm2.keyword[`CHAR`]).to.equal(`15`);

  const parm3 = F4DATE.subItems[2];
  expect(parm3.keyword[`CHAR`]).to.equal(`6`);
  expect(parm3.keyword[`OPTIONS`]).to.equal(`*NOPASS`);

  const parm4 = F4DATE.subItems[3];
  expect(parm4.keyword[`CHAR`]).to.equal(`1`);
  expect(parm4.keyword[`OPTIONS`]).to.equal(`*NOPASS`);

  const F4DATEDS = cache.find(`F4DATEDS`);
  expect(F4DATEDS.subItems.length).to.equal(4);
  expect(F4DATEDS.range.start).to.equal(6);
  expect(F4DATEDS.range.end).to.equal(10);
});

test('columnFix', async () => {
  const lines = [
    `       Dcl-pr abcd1         Extpgm('ABC049');`,
    `         ParentProductSearch           zoned(11);`,
    `         AllowSelect                   char(1)   Options(*nopass);`,
    `         ReturnItemNumber              zoned(7)  Options(*nopass);`,
    `       end-pr;`,
    `       dcl-pr abcd2    extpgm('ABC039');`,
    `         SelectFlag                  char(1);`,
    `         ReturnProduct               zoned(7);`,
    `         SupplierFilter              zoned(3) options(*nopass);`,
    `         DescriptionFilter           char(20) Options(*nopass);`,
    `       end-pr;`,
    `       dcl-pr abcd3      extpgm('ABC001');`,
    `         ProductZoned                  Zoned(7);`,
    `       end-pr;`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, { ignoreCache: true, withIncludes: true });

  expect(cache.procedures.length).to.equal(3);

  const abcd1 = cache.find(`abcd1`);
  expect(abcd1.range).to.deep.equal({
    start: 0,
    end: 4
  });

  const abcd2 = cache.find(`abcd2`);
  expect(abcd2.range).to.deep.equal({
    start: 5,
    end: 10
  });

  const abcd3 = cache.find(`abcd3`);
  expect(abcd3.range).to.deep.equal({
    start: 11,
    end: 13
  });
});

test('comments1', async () => {
  const lines = [
    `       //=== Prototypes for SRV_MSG routines ========================`,
    `       //============================================================`,
    `     D SndMsgPgmQ      pr                                                       Send Msg to PGM Q`,
    `     D  pMsgQ                        10`,
    `     D  pMsgid                        7`,
    `     D  pMsgFile                     10`,
    `     D  pMsgDta                     512    options(*NOPASS)`,
    `     D                                     Varying`,
    `       //============================================================`,
    `     D ClrMsgPgmQ      pr              N                                        Clear PGM Msg Q`,
    `     D pPgmMsgQ                      10`,
    ``,
    `       //============================================================`,
    `     D SndEscMsg       pr                                                       Send ESC Msg`,
    `     D piMsg                        512a   Const Varying`,
    ``,
    `       //============================================================`,
    `     D SndInfMsg       pr                                                       Send INF Msg`,
    `     D piMsg                        512a   Const Varying`,
    ``,
    `       //============================================================`,
    `     D JobLogMsg       Pr`,
    `     D piMsg                        512a   Value Varying                        Msg to job log`,
    ``,
    `       //============================================================`,
    `     D Show            pr                  extpgm('SHOW')                       Show popup msg`,
    `     D piPext                      8192a   Const Varying`,
    `     D piMsgId                        7a   Const options(*NOPASS)`,
    `     d piMsgFile                     21a   Const options(*NOPASS)`,
    ``,
    ``,
    `       //=== End of Prototypes forSRV_MSG Routines ==================`,
    ``,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, { ignoreCache: true, withIncludes: true });

  expect(cache.procedures.length).to.equal(6);

  const SndMsgPgmQ = cache.find(`SndMsgPgmQ`);
  expect(SndMsgPgmQ.subItems.length).to.equal(4);
  expect(SndMsgPgmQ.range).to.deep.equal({
    start: 2,
    end: 7
  });

  const ClrMsgPgmQ = cache.find(`ClrMsgPgmQ`);
  expect(ClrMsgPgmQ.subItems.length).to.equal(1);
  expect(ClrMsgPgmQ.range).to.deep.equal({
    start: 9,
    end: 10
  });

  const SndEscMsg = cache.find(`SndEscMsg`);
  expect(SndEscMsg.subItems.length).to.equal(1);
  expect(SndEscMsg.range).to.deep.equal({
    start: 13,
    end: 14
  });

  const SndInfMsg = cache.find(`SndInfMsg`);
  expect(SndInfMsg.subItems.length).to.equal(1);
  expect(SndInfMsg.range).to.deep.equal({
    start: 17,
    end: 18
  });

  const JobLogMsg = cache.find(`JobLogMsg`);
  expect(JobLogMsg.subItems.length).to.equal(1);
  expect(JobLogMsg.range).to.deep.equal({
    start: 21,
    end: 22
  });

  const Show = cache.find(`Show`);
  expect(Show.subItems.length).to.equal(3);
  expect(Show.range).to.deep.equal({
    start: 25,
    end: 28
  });
});

test('ranges', async () => {
  const lines = [
    `     D******************************************************************`,
    `     D*Record structure for QUSRJOBI JOBI1000 format`,
    `     D******************************************************************`,
    `     DQUSI1000         DS`,
    `     D*                                             Qwc JOBI1000`,
    `     D QUSBR12                 1      4I 0`,
    `     D*                                             Bytes Return`,
    `     D QUSBA12                 5      8I 0`,
    `     D*                                             Bytes Avail`,
    `     D QUSJN16                 9     18`,
    `     D*                                             Job Name`,
    `     D QUSUN19                19     28`,
    `     D*                                             User Name`,
    `     D QUSJNBR15              29     34`,
    `     D*                                             Job Number`,
    `     D QUSIJID13              35     50`,
    `     D*                                             Int Job ID`,
    `     D QUSJS25                51     60`,
    `     D*                                             Job Status`,
    `     D QUSJT13                61     61`,
    `     D*                                             Job Type`,
    `     D QUSJS26                62     62`,
    `     D*                                             Job Subtype`,
    `     D QUSERVED50             63     64`,
    `     D*                                             Reserved`,
    `     D QUSET01                65     72U 0`,
    `     D*                                             Elapsed Time`,
    `     D QUSTDIOC               73     80U 0`,
    `     D*                                             Total DiskIO Count`,
    `     D QUSADIOC               81     88U 0`,
    `     D*                                             Async DiskIO Count`,
    `     D QUSSDIOC               89     96U 0`,
    `     D*                                             Sync DiskIO Count`,
    `     D QUSIRT                 97    100I 0`,
    `     D*                                             Int Response Time`,
    `     D QUSITC                101    104I 0`,
    `     D*                                             Int Trans Count`,
    `     D QUSCPUUP              105    108I 0`,
    `     D*                                             CPU Used Percent`,
    `     D QUSUUDBP              109    112I 0`,
    `     D*                                             CPU Used DB Percent`,
    `     D QUSCPUUT              113    120U 0`,
    `     D*                                             CPU Used Time`,
    `     D QUSUUDBT              121    128U 0`,
    `     D*                                             CPU Used DB Time`,
    `     D QUSLWT                129    136U 0`,
    `     D*                                             Lock Wait Time`,
    `     D QUSPFC                137    144U 0`,
    `     D*                                             Page Fault Count`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, { ignoreCache: true, withIncludes: true });

  const QUSLWT = cache.find(`QUSLWT`);
  expect(QUSLWT.keyword[`UNS`]).to.equal(`20`);

  const QUSUUDBP = cache.find(`QUSUUDBP`);
  expect(QUSUUDBP.keyword[`INT`]).to.equal(`10`);

  const QUSUN19 = cache.find(`QUSUN19`);
  expect(QUSUN19.keyword[`CHAR`]).to.equal(`10`);
});

test('def_ranges', async () => {
  const lines = [
    `      * ********************************************************************/`,
    `      *                                                                    *`,
    `      *  Last Amend No. MIDAS2122          Date 26Jul18    Author ABDN198  *`,
    `      *  Prev Amend No. MIDAS1939          Date 25May18    Author ABDN198  *`,
    `      *  Prev Amend No. MIDAS841           Date 23Jun17    Author ABDN198  *`,
    `      *                                                                    *`,
    `      * ********************************************************************/`,
    `      *                                                                    *`,
    `      *  MIDAS2122  - Added $F4_TYPEMST procedure                          *`,
    `      *  MIDAS1939  - Added $Close_ procedure                              *`,
    `      *  MIDAS841   - Add additional columns                               *`,
    `      *                                                                    *`,
    `      * ********************************************************************/`,
    `     D* -------------------------------------------------------------------`,
    `     D* TYPEMSTPF`,
    `     D* -------------------------------------------------------------------`,
    `     D TYPEMST_T     E Ds                  ExtName(TYPEMSTPF) Qualified Template`,
    `     D`,
    `     D TYPEMST_P       s               *`,
    `     D/IF DEFINED(TYPEMSTPF)`,
    `     D TYPEMST_K     E Ds                  ExtName(TYPEMSTPF: *KEY)`,
    `     D                                     Qualified`,
    `     D TYPEMST_R     E Ds                  ExtName(TYPEMSTPF)`,
    `     D                                     Based(TYPEMST_P)`,
    `     D/ELSEIF DEFINED(TYPEMSTPF_PREFIX)`,
    `     D TYPEMST_K     E Ds                  ExtName(TYPEMSTPF: *KEY)`,
    `     D                                     Prefix('KTM1')`,
    `     D TYPEMST_R     E Ds                  ExtName(TYPEMSTPF)`,
    `     D                                     Based(TYPEMST_P)`,
    `     D                                     Prefix('TM1')`,
    `     D/ELSE`,
    `     D TYPEMST_K     E Ds                  ExtName(TYPEMSTPF: *KEY)`,
    `     D                                     Qualified`,
    `     D TYPEMST_R     E Ds                  ExtName(TYPEMSTPF)`,
    `     D                                     Based(TYPEMST_P)`,
    `     D                                     Qualified`,
    `     D/ENDIF`,
    `     D TYPEMST_Ds    E Ds                  ExtName(TYPEMSTPF)`,
    `     D                                     Qualified`,
    `     D                                     Dim(TYPEMST_Dim)`,
    `     D`,
    `     D TYPEMST_F       Ds                  LikeDs(TYPEMST_T)`,
    `     D* -------------------------------------------------------------------`,
    `     D* Service Program Procedures`,
    `     D* -------------------------------------------------------------------`,
    `     D $Validate_TYPEMST...`,
    `     D                 Pr              n`,
    `     D  $i_Action                     4    Const`,
    `     D  $i_Pointer                     *   Const`,
    `     D`,
    `     D $GetError_TYPEMST...`,
    `     D                 Pr            80a   Varying`,
    `     D  $o_ErrNo                     10i 0 Options(*NoPass: *Omit)`,
    `     D`,
    `     D $GetErrors_TYPEMST...`,
    `     D                 Pr            10i 0`,
    `     D  $o_ErrDs                           likeds($ErrorDs_TYPEMST)`,
    `     D                                     Dim(TYPEMST_Dim)`,
    `     D`,
    `     D* Input Handler`,
    `     D`,
    `     D $SetLL_TYPEMST  Pr              n`,
    `     D  $i_Pointer                     *   Const`,
    `     D  $i_Key                             Const`,
    `     D                                     likeds(TYPEMST_K)`,
    `     D`,
    `     D $Read_TYPEMST   Pr              n`,
    `     D  $i_Pointer                     *   Const`,
    `     D`,
    `     D $ReadE_TYPEMST  Pr              n`,
    `     D  $i_Pointer                     *   Const`,
    `     D  $i_Key                             LikeDs(TYPEMST_K)`,
    `     D                                     Const Options(*NoPass)`,
    `     D`,
    `     D $Chain_TYPEMST  Pr              n`,
    `     D  $i_Pointer                     *   Const`,
    `     D  $i_Key                             LikeDs(TYPEMST_K)`,
    `     D                                     Const Options(*NoPass)`,
    `     D`,
    `     D $CloseI_TYPEMST...`,
    `     D                 Pr`,
    `     D`,
    `     D $Close_TYPEMST...`,
    `     D                 Pr`,
    `     D`,
    `     D $SetGT_TYPEMST  Pr              n`,
    `     D  $i_Pointer                     *   Const`,
    `     D  $i_Key                             LikeDs(TYPEMST_K)`,
    `     D                                     Const Options(*NoPass)`,
    `     D`,
    `     D $ReadPE_TYPEMST...`,
    `     D                 Pr              n`,
    `     D  $i_Pointer                     *   Const`,
    `     D  $i_Key                             LikeDs(TYPEMST_K)`,
    `     D                                     Const Options(*NoPass)`,
    `     D`,
    `     D $ReadP_TYPEMST  Pr              n`,
    `     D  $i_Pointer                     *   Const`,
    `     D`,
    `     D $SaveKeys_TYPEMST...`,
    `     D                 Pr              n`,
    `     D  $i_Pointer                     *   Const`,
    `     D  $i_Key                             LikeDs(TYPEMST_K)`,
    `     D                                     Const Options(*NoPass)`,
    `     D`,
    `     D $Restore_TYPEMST...`,
    `     D                 Pr              n`,
    `     D`,
    `     D* Update Handler`,
    `     D`,
    `     D $CloseU_TYPEMST...`,
    `     D                 Pr`,
    `     D`,
    `     D $Write_TYPEMST  Pr              n`,
    `     D  $i_Pointer                     *   Const`,
    `     D  $i_Key                             LikeDs(TYPEMST_K)`,
    `     D                                     Const Options(*NoPass)`,
    `     D`,
    `     D $Update_TYPEMST...`,
    `     D                 Pr              n`,
    `     D  $i_Pointer                     *   Const`,
    `     D  $i_Key                             LikeDs(TYPEMST_K)`,
    `     D                                     Const Options(*NoPass)`,
    `     D`,
    `     D $Delete_TYPEMST...`,
    `     D                 Pr              n`,
    `     D  $i_Pointer                     *   Const`,
    `     D  $i_Key                             LikeDs(TYPEMST_K)`,
    `     D                                     Const Options(*NoPass)`,
    `     D`,
    `     D* SQL Handler`,
    `     D`,
    `     D $SQLRead_TYPEMST...`,
    `     D                 Pr              n`,
    `     D $i_Pointer                      *   Const`,
    `     D $i_Statement                 500a   Const Options(*NoPass: *VarSize)`,
    `     D`,
    `     D $Select_TYPEMST...`,
    `     D                 Pr              n`,
    `     D $o_TYPEMST_Ds                       LikeDs(TYPEMST_R) Dim(TYPEMST_Dim)`,
    `     D $o_TYPEMST_Elem...`,
    `     D                               10i 0`,
    `     D $i_SQLWhere                  200a   Const Options(*NoPass)`,
    `     D $i_SQLOrder                  200a   Const Options(*NoPass)`,
    `     D`,
    `     D $SQLFetch_TYPEMST...`,
    `     D                 Pr              n`,
    `     D $i_Pointer                      *   Const`,
    `     D $i_Procedure                  10a   Const`,
    `     D`,
    `     D $F4_TYPEMST...`,
    `     D                 Pr                  LikeDs(TYPEMST_K)`,
    `     D  $i_Filter                          LikeDs(TYPEMST_F)`,
    `     D                                     Const`,
    `     D  $i_Row                        3s 0 Const Options(*NoPass)`,
    `     D  $i_Col                        3s 0 Const Options(*NoPass)`,
    `     D`,
    `     D $GetFilter_TYPEMST...`,
    `     D                 Pr          5000a   Varying`,
    `     D  $i_Filter                          LikeDs(TYPEMST_F)`,
    `     D                                     Const`,
    `     D* -------------------------------------------------------------------`,
    `     D* Data Structure`,
    `     D* -------------------------------------------------------------------`,
    `     D $ErrorDS_TYPEMST...`,
    `     D                 Ds                  Qualified Dim(TYPEMST_Dim)`,
    `     D  Column                       10a`,
    `     D  Message                      70a`,
    `     D`,
    `     D* -------------------------------------------------------------------`,
    `     D* Constants`,
    `     D* -------------------------------------------------------------------`,
    `     D TYPEMST_FILENAME...`,
    `     D                 c                   'TYPEMSTPF'                          FILENAME`,
    `     D TYPEMST_Dim     c                   100`,
    `     D TYPEMST_IN51    c                   51                                   FILENAME`,
    `     D TYPEMST_IN52    c                   52                                   TYPE`,
    `     D TYPEMST_IN53    c                   53                                   TYPNAME`,
    `     D TYPEMST_IN54    c                   54                                   TYPSNAME`,
    `     D TYPEMST_IN55    c                   55                                   ACTION`,
    `     D TYPEMST_IN56    c                   56                                   PROC1`,
    `     D TYPEMST_IN57    c                   57                                   PROC2`,
    `     D TYPEMST_IN58    c                   58                                   PROC3`,
    `     D`,
    `     /*MIDAS560   ABDN198   */`,
    `     /*MIDAS1939  ABDN198   */`,
    ``,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, { ignoreCache: true, withIncludes: true });

  const TYPEMST_T = cache.find(`TYPEMST_T`);
  expect(TYPEMST_T.range).to.deep.equal({
    start: 16,
    end: 16
  });

  const TYPEMST_Ds = cache.find(`TYPEMST_Ds`);
  expect(TYPEMST_Ds.range).to.deep.equal({
    start: 37,
    end: 39
  });

  const TYPEMST_F = cache.find(`TYPEMST_F`);
  expect(TYPEMST_F.range).to.deep.equal({
    start: 41,
    end: 41
  });

  const $ErrorDS_TYPEMST = cache.find(`$ErrorDS_TYPEMST`);
  expect($ErrorDS_TYPEMST.range).to.deep.equal({
    start: 164,
    end: 167
  });

  const $Validate_TYPEMST = cache.find(`$Validate_TYPEMST`);
  expect($Validate_TYPEMST.range).to.deep.equal({
    start: 45,
    end: 48
  });
});

test('ctl_opt_fixed', async () => {
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

  const cache = await parser.getDocs(uri, lines, { ignoreCache: true, withIncludes: true });

  expect(cache.variables.length).to.equal(2);
  expect(cache.structs.length).to.equal(1);

  expect(Object.keys(cache.keyword).length).to.equal(11);
  expect(cache.keyword[`FTRANS`]).to.equal(`*SRC`);
  expect(cache.keyword[`DATFMT`]).to.equal(`*MDY/`);
  expect(cache.keyword[`COPYRIGHT`]).to.equal(`'(C) Copyright ABC Programming - 1995'`);
});

test('call_opcode', async () => {
  const lines = [
    `     C     CreateNewBoardBEGSR`,
    `     C                   EVAL      wWinMode = 'T'`,
    `     C                   EVAL      wWinText = *BLANKS`,
    `     C                   EVAL      wWinNumber = 0`,
    `     C                   EVAL      wWinF3 = *OFF`,
    `     C                   CALL      'BBSWINASKR'`,
    `     C                   PARM                    wWinMode`,
    `     C                   PARM                    wWinText`,
    `     C                   PARM                    wWinNumber`,
    `     C                   PARM                    wWinF3`,
    `     C                   IF        wWinF3 = *OFF`,
    `     C                   EVAL      BRDSHT = wWinText`,
    `      * Convert it to Uppercase`,
    `     C     cLo:cUp       XLATE     BRDSHT        BRDSHT`,
    `     C                   EVAL      BRDLNG = BRDSHT`,
    `     C                   EVAL      BRDALV = 99`,
    `     C                   WRITE     RBOARD`,
    `     C                   EXSR      ReLoadSFL`,
    `     C                   ENDIF`,
    `     C                   ENDSR`,
    ``
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, { ignoreCache: true, withIncludes: true });

  expect(cache.subroutines.length).to.equal(1);
  expect(cache.procedures.length).to.equal(1);

  const fixedCall = cache.find(`BBSWINASKR`)
  expect(fixedCall.name).to.equal(`BBSWINASKR`);
  expect(fixedCall.keyword[`EXTPGM`]).to.equal(true);
});

test('file_keywords', async () => {
  const lines = [
    ``,
    `     forder     o    e             disk`,
    `     fdetord    o    e           k disk`,
    `     fTmpdetord uf a e           k disk    EXTDESC('DETORD')`,
    `     f                                     EXTFILE(*EXTDESC)`,
    `     f                                     rename(fdeto:tmprec)`,
    `     ford100d   cf   e             workstn`,
    `     F                                     indds(indds)`,
    `     F                                     sfile(sfl01:rrn01)`,
    `     F                                     Infds(Info)`,
    ``,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, { ignoreCache: true, withIncludes: true });

  expect(cache.files.length).to.equal(4);

  const tmpdetord = cache.find(`Tmpdetord`);
  expect(tmpdetord.keyword[`EXTDESC`]).to.equal(`'DETORD'`);
  expect(tmpdetord.keyword[`EXTFILE`]).to.equal(`*EXTDESC`);
  expect(tmpdetord.keyword[`RENAME`]).to.equal(`fdeto:tmprec`);

  const ord100d = cache.find(`ord100d`);
  expect(ord100d.keyword[`INDDS`]).to.equal(`indds`);
  expect(ord100d.keyword[`SFILE`]).to.equal(`sfl01:rrn01`);
  expect(ord100d.keyword[`INFDS`]).to.equal(`Info`);
});

test('plist_test', async () => {
  const lines = [
    ``,
    `     ?*                                                                                       PLPVD`,
    `     ?*  PLPVD  - Calling Plist for prompt/validate module driver                             PLPVD`,
    `     ?*                                                                                       PLPVD`,
    `     ?*  Kaprog - E3A                                                                         PLPVD`,
    `     ?*                                                                                       PLPVD`,
    `     ?*  @PGMID - Program name                                                                PLPVD`,
    `     ?*  @FLN   - Field to prompt/validate on                                                 PLPVD`,
    `     ?*  @SQN   - Sequence number of type of validation/prompt                                PLPVD`,
    `     ?*  @PRMPT - Prompt mode ('Y' or 'N')                                                    PLPVD`,
    `     ?*  CCN    - Communication array                                                         PLPVD`,
    `     ?*  @ERMSG - Error message return field & parms                                          PLPVD`,
    `     ?*  @NUM   - Numeric return field                                                        PLPVD`,
    `     ?*  @CKEY  - Command key used from prompt screen return field                            PLPVD`,
    `     ?*  @PPF   - Prompt performed flag ('Y' or 'N') returned                                 PLPVD`,
    `     ?*  @DSCNTRL      - API Control Fields                                                   PLPVD`,
    `     ?*  @DSSUPER      - API Supervisor Data                                                  PLPVD`,
    `     ?*  @DSINCRM      - API Incremental Mode Control Fields                                  PLPVD`,
    `     ?*  @DSPV         - PV Control Fields                                                    PLPVD`,
    `     ?*  @DLFILTER     - DL Filter Data                                                       PLDLD`,
    `     ?*  @DLLIST       - Array of DL row data                                                 PLDLD`,
    `     ?*  @DLSELECTION  - DL Selected Item                                                     PLDLD`,
    `     ?*                                                                                       PLDLD`,
    `     C     PLPVD         PLIST`,
    `     C                   PARM                    @PGMID           10`,
    `     C                   PARM                    @FLN              6`,
    `     C                   PARM                    @SQN              2 0`,
    `     C                   PARM                    @PRMPT            1`,
    `     C                   PARM                    CCN`,
    `     C     DSEPMS        PARM      DSEPMS        @ERMSG           37`,
    `     C                   PARM                    @NUM             15 0`,
    `     C                   PARM                    @CKEY             2`,
    `     C                   PARM                    @PPF              1`,
    `     C                   PARM                    @DSCNTRL`,
    `     C                   PARM                    @DSSUPER`,
    `     C                   PARM                    @DSINCRM`,
    `     C                   PARM                    @DSPV`,
    `     C                   PARM                    @PVFILTER       256`,
    `     C                   PARM                    @PVLIST        9999`,
    `     C                   PARM                    @PVSELECTION    256`,
    ``
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, { ignoreCache: true, withIncludes: true });

  expect(cache.variables.length).toBe(11);

  const atPGMID = cache.find(`@PGMID`);
  expect(atPGMID.keyword[`CHAR`]).toBe(`10`);

  const atSON = cache.find(`@SQN`);
  expect(atSON.keyword[`PACKED`]).toBe(`2:0`);

  const atPVSELECTION = cache.find(`@PVSELECTION`);
  expect(atPVSELECTION.keyword[`CHAR`]).toBe(`256`);
});

test(`range test 2`, async () => {
  const lines = [
    `     D TYPEMST_F       Ds                  LikeDs(TYPEMST_T)`,
    `     D* -------------------------------------------------------------------`,
    `     D* Service Program Procedures`,
    `     D* -------------------------------------------------------------------`,
    `     D $Validate_TYPEMST...`,
    `     D                 Pr              n`,
    `     D  $i_Action                     4    Const`,
    `     D  $i_Pointer                     *   Const`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, { ignoreCache: true, withIncludes: true });

  const TYPEMST_F = cache.find(`TYPEMST_F`);
  expect(TYPEMST_F.range).to.deep.equal({
    start: 0,
    end: 0
  });
});

test(`test document build up`, async () => {
  const lines = [
    `    H*****************************************************************`,
    `    D*`,
    `    DRESSTR           DS                  INZ`,
    `    D VARNAM                  1     10    INZ('VAR018    ')`,
    `    D PF                     11     11    INZ('F')`,
    `    D ERRMSG                 12     90`,
    `    D EXP                    12     16    INZ('EXP: ')`,
    `    D EXPCOD                 17     21    INZ`,
    `    D RCV                    22     27    INZ(' RCV:')`,
    `    D RECODE                 28     32`,
    `    D TNAME                  92    101    INZ('SQRPGNRUN ')`,
    `    D LIB                   102    111    INZ('SQTEST  ')`,
    `    D FILE                  112    121    INZ('RPGNRSLTS ')`,
    `    D LIBLEN                122    125B 0 INZ(8)`,
    `    D FILLEN                126    129B 0 INZ(10)`,
    `    D*`,
    `    D ACTSQL          S              4  0`,
    `    D CMPCOD          S              4  0`,
    `    D*`,
    `    D DATHV           S             10D   DATFMT(*ISO-) INZ(D'2025-12-10')`,
    `    D CHKDAT          S             10D   DATFMT(*ISO-) INZ(D'2025-12-02')`,
    `    D*`,
    `    C/EXEC SQL`,
    `    C+ WHENEVER SQLERROR CONTINUE`,
    `    C/END-EXEC`,
    `    C*`,
    `    C*****************************************************************`,
    `    C*`,
    `    C*****************************************************************`,
    `    C*`,
    `    C                   MOVEL     'VAR018'      VARNAM`,
    `    C                   Z-ADD     -180          CMPCOD`,
    `    C*`,
  ].join(`\n`);

  let document = ``;

  for (let c of lines.split(``)) {
    document += c;
    await parser.getDocs(uri, document, { ignoreCache: true, withIncludes: true, collectReferences: true });
  }
})