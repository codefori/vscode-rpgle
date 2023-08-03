
const assert = require(`assert`);

const {default: parserSetup} = require(`../parserSetup`);

const uri = `source.rpgle`;
  
exports.fixed1 = async () => {
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

  const parser = parserSetup();
  const cache = await parser.getDocs(uri, lines);

  assert.strictEqual(cache.files.length, 1);
  assert.strictEqual(cache.variables.length, 2, `Expect length of 2`);

  const wkCorp = cache.variables[0];
  assert.strictEqual(wkCorp.name, `wkCorp`);
  assert.strictEqual(wkCorp.position.line, 3);
  assert.strictEqual(wkCorp.keywords[0], `CHAR(10)`);
  assert.strictEqual(wkCorp.keywords[1], `INZ('100')`);

  const wkInvoice = cache.variables[1];
  assert.strictEqual(wkInvoice.name, `wkInvoice`);
  assert.strictEqual(wkInvoice.position.line, 4);
  assert.strictEqual(wkInvoice.keywords[0], `CHAR(15)`);
};

exports.fixed2 = async () => {
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

  const parser = parserSetup();
  const cache = await parser.getDocs(uri, lines);

  assert.strictEqual(cache.variables.length, 13, `Expect length of 13`);

  const CHARFields = cache.variables.filter(v => v.keywords[0].startsWith(`CHAR`));
  assert.strictEqual(CHARFields.length, 12, `Expect length of 12`);

  const countVar = cache.variables.find(v => v.name === `Count`);
  assert.strictEqual(countVar.keywords[0], `PACKED(4:0)`);
  assert.strictEqual(countVar.keyword[`PACKED`], `4:0`)
};

exports.fixed3 = async () => {
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

  const parser = parserSetup();
  const cache = await parser.getDocs(uri, lines);

  assert.strictEqual(cache.variables.length, 1, `Expect length of 1`);
  assert.strictEqual(cache.structs.length, 1, `Expect length of 1`);
    
  const Worktype = cache.variables[0];
  assert.strictEqual(Worktype.name, `Worktype`);
  assert.strictEqual(Worktype.position.line, 0);
  assert.strictEqual(Worktype.keywords[0], `CHAR(10)`);
  assert.strictEqual(Worktype.keywords[1], `INZ('*OUTQ')`);
  assert.strictEqual(Worktype.keyword[`CHAR`], `10`);
  assert.strictEqual(Worktype.keyword[`INZ`], `'*OUTQ'`);

  const DS = cache.structs[0];
  assert.strictEqual(DS.name, `*N`);
  assert.strictEqual(DS.position.line, 3);
  assert.strictEqual(DS.subItems.length, 7);
  assert.strictEqual(DS.subItems.find(i => !i.keyword[`BINDEC`]), undefined);
};

exports.fixed4 = async () => {
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

  const parser = parserSetup();
  const cache = await parser.getDocs(uri, lines);

  assert.strictEqual(cache.variables.length, 2, `Expect length of 2`);
  assert.strictEqual(cache.structs.length, 1, `Expect length of 1`);

  const InType = cache.find(`InType`);
  assert.strictEqual(InType.name, `InType`);
  assert.strictEqual(InType.position.line, 1);

  const Worktype = cache.variables[1];
  assert.strictEqual(Worktype.name, `Worktype`);
  assert.strictEqual(Worktype.position.line, 14);

  const InputDs = cache.structs[0];
  assert.strictEqual(InputDs.name, `InputDs`);
  assert.strictEqual(InputDs.position.line, 6);
  assert.strictEqual(InputDs.subItems.length, 7);
};

exports.fixed5 = async () => {
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

  const parser = parserSetup();
  const cache = await parser.getDocs(uri, lines);

  assert.strictEqual(cache.variables.length, 2, `Expect length of 2`);
  assert.strictEqual(cache.structs.length, 2, `Expect length of 2`);
  assert.strictEqual(cache.procedures.length, 1, `Expect length of 1`);

  const RtvObjD = cache.procedures[0];
  assert.strictEqual(RtvObjD.name, `RtvObjD`);
  assert.strictEqual(RtvObjD.position.line, 18);
  assert.strictEqual(RtvObjD.keywords.join(` `).trim(), `EXTPGM( 'QUSROBJD' )`);
  assert.strictEqual(RtvObjD.keyword[`EXTPGM`], `'QUSROBJD'`);
  assert.strictEqual(RtvObjD.subItems.length, 6);
};

exports.fixed6 = async () => {
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

  const parser = parserSetup();
  const cache = await parser.getDocs(uri, lines);

  assert.strictEqual(cache.variables.length, 8, `Expect length of 8`);

  const lenVar = cache.find(`len`);
  assert.strictEqual(lenVar.name, `len`);
  assert.strictEqual(lenVar.position.line, 6);
  assert.strictEqual(lenVar.keywords[0], `INT(5)`);
  assert.strictEqual(lenVar.keyword[`INT`], `5`);

  const date2Var = cache.find(`DATE2`);
  assert.strictEqual(date2Var.name, `DATE2`);
  assert.strictEqual(date2Var.position.line, 3);
  assert.strictEqual(date2Var.keywords[0], `DATE`);
  assert.strictEqual(date2Var.keyword[`DATE`], true);
  assert.strictEqual(date2Var.keywords[1], `DATFMT(*JIS)`);
  assert.strictEqual(date2Var.keyword[`DATFMT`], `*JIS`);

  const time0Var = cache.find(`TIME0`);
  assert.strictEqual(time0Var.name, `TIME0`);
  assert.strictEqual(time0Var.position.line, 7);
  assert.strictEqual(time0Var.keywords[0], `TIME`);
  assert.strictEqual(time0Var.keyword[`TIME`], true);
  assert.strictEqual(time0Var.keywords[1], `INZ(T'10.12.15')`);
  assert.strictEqual(time0Var.keyword[`INZ`], `T'10.12.15'`);
};

exports.fixed7 = async () => {
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

  const parser = parserSetup();
  const cache = await parser.getDocs(uri, lines);

  assert.strictEqual(cache.procedures.length, 1, `Expect length of 1`);

  const Obj_Next = cache.find(`Obj_Next`);
  assert.strictEqual(Obj_Next.name, `Obj_Next`);
  assert.strictEqual(Obj_Next.position.line, 3);
  assert.strictEqual(Obj_Next.keywords.includes(`EXPORT`), true);
  assert.strictEqual(Obj_Next.keyword[`EXPORT`], true);
  assert.strictEqual(Obj_Next.keywords.includes(`LIKEDS(OBJECTDS)`), true);
  assert.strictEqual(Obj_Next.keyword[`LIKEDS`], `OBJECTDS`);
  assert.strictEqual(Obj_Next.subItems.length, 0);
};

exports.fixed8 = async () => {
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

  const parser = parserSetup();
  const cache = await parser.getDocs(uri, lines);

  assert.strictEqual(cache.subroutines.length, 2);
  assert.strictEqual(cache.subroutines[0].name, `$QUSCRTUS`);
  assert.strictEqual(cache.subroutines[1].name, `$QUSLMBR`);
};

exports.fixed9 = async () => {
  const lines = [
    ``,
    `       // -----------------------`,
    `      /copy './tests/rpgle/copy1.rpgle'`,
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

  const parser = parserSetup();
  const cache = await parser.getDocs(uri, lines);

  assert.strictEqual(cache.includes.length, 1);
  assert.strictEqual(cache.procedures.length, 2);

  const Obj_Next = cache.find(`Obj_Next`);
  assert.strictEqual(Obj_Next.name, `Obj_Next`);
  assert.strictEqual(Obj_Next.position.line, 5);
  assert.strictEqual(Obj_Next.keywords.includes(`EXPORT`), true);
  assert.strictEqual(Obj_Next.keywords.includes(`LIKEDS(OBJECTDS)`), true);
  assert.strictEqual(Obj_Next.keyword[`EXPORT`], true);
  assert.strictEqual(Obj_Next.keyword[`LIKEDS`], `OBJECTDS`);
  assert.strictEqual(Obj_Next.subItems.length, 0);

  const theExtProcedure = cache.find(`theExtProcedure`);
  assert.strictEqual(theExtProcedure.name, `theExtProcedure`);
  assert.strictEqual(theExtProcedure.position.line, 2);
  assert.strictEqual(theExtProcedure.keywords.includes(`EXTPROC`), true);
  assert.strictEqual(theExtProcedure.subItems.length, 1);
};

exports.fixed9_2 = async () => {
  const lines = [
    ``,
    `       // -----------------------`,
    `     d/copy './tests/rpgle/copy1.rpgle'`,
    `     */copy './tests/rpgle/copy2.rpgle'`,
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

  const parser = parserSetup();
  const cache = await parser.getDocs(uri, lines);

  assert.strictEqual(cache.includes.length, 1);
  assert.strictEqual(cache.procedures.length, 2);

  const Obj_Next = cache.find(`Obj_Next`);
  assert.strictEqual(Obj_Next.name, `Obj_Next`);
  assert.strictEqual(Obj_Next.position.line, 5);
  assert.strictEqual(Obj_Next.keywords.includes(`EXPORT`), true);
  assert.strictEqual(Obj_Next.keywords.includes(`LIKEDS(OBJECTDS)`), true);
  assert.strictEqual(Obj_Next.keyword[`EXPORT`], true);
  assert.strictEqual(Obj_Next.keyword[`LIKEDS`], `OBJECTDS`);
  assert.strictEqual(Obj_Next.subItems.length, 0);

  const theExtProcedure = cache.find(`theExtProcedure`);
  assert.strictEqual(theExtProcedure.name, `theExtProcedure`);
  assert.strictEqual(theExtProcedure.position.line, 2);
  assert.strictEqual(theExtProcedure.keywords.includes(`EXTPROC`), true);
  assert.strictEqual(theExtProcedure.subItems.length, 1);
};

/**
   * Issue with detecting correct type on subfield.
   */
exports.fixed10 = async () => {
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

  const parser = parserSetup();
  const cache = await parser.getDocs(uri, lines);

  const dataDs = cache.find(`data`);
  assert.strictEqual(dataDs.name, `data`);
  assert.strictEqual(dataDs.subItems.length, 4);

  assert.strictEqual(dataDs.range.start, 0);
  assert.strictEqual(dataDs.range.end, 4);

  const rrn02 = cache.find(`rrn02`);
  assert.strictEqual(rrn02.name, `rrn02`);
  assert.strictEqual(rrn02.keywords.includes(`PACKED(7:2)`), true);
  assert.strictEqual(rrn02.keyword[`PACKED`], `7:2`);

  const arsalePr = dataDs.subItems[3];
  assert.strictEqual(arsalePr.name, `arsalePr`);
  assert.strictEqual(arsalePr.keywords.includes(`ZONED(7:2)`), true);
  assert.strictEqual(arsalePr.keyword[`ZONED`], `7:2`);
};

exports.fixedfree1 = async () => {
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

  const parser = parserSetup();
  const cache = await parser.getDocs(uri, lines);

  assert.strictEqual(cache.variables.length, 3);
  assert.strictEqual(cache.variables.find(i => !i.keywords.includes(`CHAR(10)`)), undefined);

  assert.strictEqual(cache.subroutines.length, 0);

  assert.strictEqual(cache.procedures.length, 1);
    
  const Obj_List = cache.find(`Obj_List`);
  assert.strictEqual(Obj_List.name, `Obj_List`);
  assert.strictEqual(Obj_List.range.start, 6);
  assert.strictEqual(Obj_List.range.end, 50);
  assert.strictEqual(Obj_List.position.line, 6);
  assert.strictEqual(Obj_List.keywords.includes(`EXPORT`), true);
  assert.strictEqual(Obj_List.keyword[`EXPORT`], true);
  assert.strictEqual(Obj_List.subItems.length, 3);

  assert.strictEqual(Obj_List.subItems.find(i => !i.keywords.includes(`CHAR(10)`)), undefined);
  assert.strictEqual(Obj_List.subItems.find(i => !i.keywords.includes(`CONST`)), undefined);

  const scope = Obj_List.scope;
  assert.strictEqual(scope.subroutines.length, 1);
  assert.strictEqual(scope.variables.length, 1);
};

exports.fixed11 = async () => {
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

  const parser = parserSetup();
  const cache = await parser.getDocs(uri, lines);

  const F4DATE = cache.find(`F4DATE`);
  assert.strictEqual(F4DATE.subItems.length, 4);
  assert.strictEqual(F4DATE.range.start, 0);
  assert.strictEqual(F4DATE.range.end, 4);

  const parm1 = F4DATE.subItems[0];
  assert.strictEqual(parm1.keywords[0], `CHAR(1)`);

  const parm2 = F4DATE.subItems[1];
  assert.strictEqual(parm2.keywords[0], `CHAR(15)`);

  const parm3 = F4DATE.subItems[2];
  assert.strictEqual(parm3.keywords[0], `CHAR(6)`);
  assert.strictEqual(parm3.keywords[1], `OPTIONS(*NOPASS)`);

  const parm4 = F4DATE.subItems[3];
  assert.strictEqual(parm4.keywords[0], `CHAR(1)`);
  assert.strictEqual(parm4.keywords[1], `OPTIONS(*NOPASS)`);

  const F4DATEDS = cache.find(`F4DATEDS`);
  assert.strictEqual(F4DATEDS.subItems.length, 4);
  assert.strictEqual(F4DATEDS.range.start, 6);
  assert.strictEqual(F4DATEDS.range.end, 10);
};

exports.columnFix = async () => {
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

  const parser = parserSetup();
  const cache = await parser.getDocs(uri, lines);

  assert.strictEqual(cache.procedures.length, 3);

  const abcd1 = cache.find(`abcd1`);
  assert.deepStrictEqual(abcd1.range, {
    start: 0,
    end: 4
  });

  const abcd2 = cache.find(`abcd2`);
  assert.deepStrictEqual(abcd2.range, {
    start: 5,
    end: 10
  });

  const abcd3 = cache.find(`abcd3`);
  assert.deepStrictEqual(abcd3.range, {
    start: 11,
    end: 13
  });
};

exports.comments1 = async () => {
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

  const parser = parserSetup();
  const cache = await parser.getDocs(uri, lines);

  assert.strictEqual(cache.procedures.length, 6);

  const SndMsgPgmQ = cache.find(`SndMsgPgmQ`);
  assert.strictEqual(SndMsgPgmQ.subItems.length, 4);
  assert.deepStrictEqual(SndMsgPgmQ.range,  {
    start: 2,
    end: 7
  });

  const ClrMsgPgmQ = cache.find(`ClrMsgPgmQ`);
  assert.strictEqual(ClrMsgPgmQ.subItems.length, 1);
  assert.deepStrictEqual(ClrMsgPgmQ.range,  {
    start: 9,
    end: 10
  });

  const SndEscMsg = cache.find(`SndEscMsg`);
  assert.strictEqual(SndEscMsg.subItems.length, 1);
  assert.deepStrictEqual(SndEscMsg.range,  {
    start: 13,
    end: 14
  });

  const SndInfMsg = cache.find(`SndInfMsg`);
  assert.strictEqual(SndInfMsg.subItems.length, 1);
  assert.deepStrictEqual(SndInfMsg.range,  {
    start: 17,
    end: 18
  });

  const JobLogMsg = cache.find(`JobLogMsg`);
  assert.strictEqual(JobLogMsg.subItems.length, 1);
  assert.deepStrictEqual(JobLogMsg.range,  {
    start: 21,
    end: 22
  });

  const Show = cache.find(`Show`);
  assert.strictEqual(Show.subItems.length, 3);
  assert.deepStrictEqual(Show.range,  {
    start: 25,
    end: 28
  });
};

exports.ranges = async () => {
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

  const parser = parserSetup();
  const cache = await parser.getDocs(uri, lines);

  const QUSLWT = cache.find(`QUSLWT`);
  assert.strictEqual(QUSLWT.keyword[`UNS`], `20`);

  const QUSUUDBP = cache.find(`QUSUUDBP`);
  assert.strictEqual(QUSUUDBP.keyword[`INT`], `10`);

  const QUSUN19 = cache.find(`QUSUN19`);
  assert.strictEqual(QUSUN19.keyword[`CHAR`], `10`);
}

exports.def_ranges = async () => {
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

  const parser = parserSetup();
  const cache = await parser.getDocs(uri, lines);

  const TYPEMST_T = cache.find(`TYPEMST_T`);
  assert.deepStrictEqual(TYPEMST_T.range, {
    start: 16,
    end: 16
  });

  const TYPEMST_Ds = cache.find(`TYPEMST_Ds`);
  assert.deepStrictEqual(TYPEMST_Ds.range, {
    start: 37,
    end: 39
  });

  const TYPEMST_F = cache.find(`TYPEMST_F`);
  assert.deepStrictEqual(TYPEMST_F.range, {
    start: 41,
    end: 41
  });

  const $ErrorDS_TYPEMST = cache.find(`$ErrorDS_TYPEMST`);
  assert.deepStrictEqual($ErrorDS_TYPEMST.range, {
    start: 164,
    end: 167
  });

  const $Validate_TYPEMST = cache.find(`$Validate_TYPEMST`);
  assert.deepStrictEqual($Validate_TYPEMST.range, {
    start: 45,
    end: 48
  });
};

exports.ctl_opt_fixed = async () => {
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

  const parser = parserSetup();
  const cache = await parser.getDocs(uri, lines);

  assert.strictEqual(cache.variables.length, 2, `Expect length of 2`);
  assert.strictEqual(cache.structs.length, 1, `Expect length of 1`);

  assert.strictEqual(Object.keys(cache.keyword).length, 11);
  assert.strictEqual(cache.keyword[`FTRANS`], `*SRC`);
  assert.strictEqual(cache.keyword[`DATFMT`], `*MDY/`);
  assert.strictEqual(cache.keyword[`COPYRIGHT`], `'(C) Copyright ABC Programming - 1995'`);
};

exports.call_opcode = async () => {
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

  const parser = parserSetup();
  const cache = await parser.getDocs(uri, lines);

  assert.strictEqual(cache.subroutines.length, 1);
  assert.strictEqual(cache.procedures.length, 1);

  const fixedCall = cache.find(`BBSWINASKR`)
  assert.strictEqual(fixedCall.name, `BBSWINASKR`);
  assert.strictEqual(fixedCall.keyword[`EXTPGM`], true);
}

exports.file_keywords = async () => {
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

  const parser = parserSetup();
  const cache = await parser.getDocs(uri, lines);

  assert.strictEqual(cache.files.length, 4);

  const tmpdetord = cache.find(`Tmpdetord`);
  assert.strictEqual(tmpdetord.keyword[`EXTDESC`], `'DETORD'`);
  assert.strictEqual(tmpdetord.keyword[`EXTFILE`], `*EXTDESC`);
  assert.strictEqual(tmpdetord.keyword[`RENAME`], `fdeto:tmprec`);

  const ord100d = cache.find(`ord100d`);
  assert.strictEqual(ord100d.keyword[`INDDS`], `indds`);
  assert.strictEqual(ord100d.keyword[`SFILE`], `sfl01:rrn01`);
  assert.strictEqual(ord100d.keyword[`INFDS`], `Info`);
}