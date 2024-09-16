
import setupParser from "../parserSetup";
import Linter from "../../language/linter";
import { test, expect } from "vitest";

const parser = setupParser();
const uri = `source.rpgle`;

test("qualified1", async () => {
  const lines = [
    `**FREE`,
    `Dcl-Ds Kx Likerec(TitXe :*Key);`,
    `Dcl-s MyVariable2 Char(20);`,
    ``,
    `Dsply MyVariable2;`,
    ``,
    `Return`,
  ].join(`\n`);
  
  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
  const { errors } = Linter.getErrors({uri, content: lines}, {
    QualifiedCheck: true,
  }, cache);
  
  expect(errors.length).toBe(0);
});

test("ctdata1", async () => {
  const lines = [
    `**free`,
    `dcl-s myarray char(100) dim(10) ctdata;`,
    `dcl-s xxField1 char(1);`,
    `dcl-ds master qualified inz;`,
    ``,
    `  dcl-ds a inz;`,
    `    fielda1 Like(xxFiel1);`,
    `    fielda2 packed(2);`,
    `  End-ds;`,
    ``,
    `  dcl-ds b inz;`,
    `    fieldb1 like(xxField1);`,
    `    fieldb2 packed(9);`,
    `  End-ds;`,
    `End-ds;`,
    ``,
    ``,
    `eval master.a.fielda1 = 'a1';`,
    `eval master.a.f;`,
    `eval master.b.fieldb1 = 'b1';`,
    `//eval myds2.p.field1 = 'p';`,
    `//eval myds2.o.`,
    ``,
    `*INLR = *ON;`,
    `**ctdata myarray`,
    `select RMSDESC ,RMACRONYM ,RMLPID ,RMCBAPLAN ,LTTYPE ,LTID ,LTATTREA`,
    `,digits( RHHRTYPE ) as RHHRTYPE ,varchar( PWDES ,30 )`,
    ` ,EOEMP as EMP ,min( RHEFFDT ) as EFFDATE`,
    ` ,dec( 0.0 ,7,2 ) as Hours`,
    ` ,dec( 0.0 ,10,5 ) as Earned`,
    ` ,dec( 0.0 ,7,2 ) as Taken`,
    ` ,dec( ifnull( PTHRS ,0 ) ,7,2 ) as Due`,
    ` ,dec( 0.0 ,7,2 ) as Prior`,
    ` ,'N' as SysGen`,
    `from PRPEMPV0 V0`,
    `cross join PRPLPMTB RM`,
    `inner join PRPLPTTB LO on LTLPID = RMLPID`,
    `inner join PRPLPHTB HT on RHLTID = LTID`,
    `inner join PRPPHRTP on PWHTP = RHHRTYPE`,
    `left  join PRPHWLTB PT on EOEMP = PTEMP and PTLPID = LTLPID and PTTID = LTID`,
    `       and ( PTDTEOW between date( xEARNED_LEAVE_TO_x ) -7 days`,
    `        and date( xEARNED_LEAVE_TO_x ) -1 days )`,
    `where EOEFFDT = ( select EOEFFDT from PRPEOCPF where EOEMP = V0.EOEMP`,
    `            anD EOEFFDT <=xEARNED_LEAVE_TO_8x order by EOEFFDT desc fetch first row only )`,
    `and   EHHDT = ( select EHHDT from PRPEHTPF where EHEMP = V0.EOEMP`,
    `            and EHHDT <=xEARNED_LEAVE_TO_8x order by EHHDT desc fetch first row only )`,
    `and   ETEFFDT= ( select ETEFFDT from PRPETXPF where ETEMP = V0.EOEMP`,
    `            and ETEFFDT <=xEARNED_LEAVE_TO_8x order by ETEFFDT desc fetch first row only )`,
    `and RMACRONYM = 'CBA'`,
    `and EOEMP = xEMP_USEx`,
    `and LTEFFDT = ( select LTEFFDT from PRPLPTTB LI where LO.LTLPID = LI.LTLPID`,
    `                and LO.LTTYPE = LI.LTTYPE`,
    `                and LI.LTEFFDT <= xEARNED_LEAVE_TO_x`,
    `                order by LTEFFDT desc fetch first row only ) and LTSTS = 'A'`,
    `and RHEFFDT = ( select RHEFFDT from PRPLPHTB I where I.RHLTID = HT.RHLTID`,
    `                and I.RHEFFDT <= xEARNED_LEAVE_TO_x`,
    `                order by RHEFFDT desc fetch first row only ) and RHHTSTS = 'A'`,
    `group by RMSDESC ,RMACRONYM ,RMLPID ,RMCBAPLAN ,LTTYPE ,LTID ,LTATTREA`,
    ` ,RHHRTYPE ,PWDES ,EOEMP ,PTHRS`,
    `order by RMLPID ,LTID ,EFFDATE`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});
  const { indentErrors } = Linter.getErrors({uri, content: lines}, {
    indent: 2
  }, cache);

  expect(indentErrors.length).toBe(0);
});

test("ctdata2", async () => {
  const lines = [
    `**FREE`,
    `ctl-opt debug option(*nodebugio: *srcstmt);`,
    `dcl-ds mything DIM(8) PERRCD(3) CTDATA;`,
    `end-ds;`,
    ``,
    `Dcl-s MyVariable2 Char(20);`,
    ``,
    `myVariable2 = *blank;`,
    ``,
    `If myVariable2 = *blank;`,
    `MyVariable2 = 'Hello world';`,
    `Endif;`,
    `Return;`,
    ``,
    `** ARC`,
    `Toronto        12:15:00Winnipeg       13:23:00Calgary        15:44:00`,
    `Sydney         17:24:30Edmonton       21:33:00Saskatoon      08:40:00`,
    `Regina         12:33:00Vancouver      13:20:00`
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});

  expect(Object.keys(cache.keyword).length).toBe(2);
  expect(cache.keyword[`DEBUG`]).toBe(true);
  expect(cache.keyword[`OPTION`]).toBe(`*NODEBUGIO:*SRCSTMT`);

  expect(cache.variables.length).toBe(1);
  expect(cache.structs.length).toBe(1);
});

test("ctdata3", async () => {
  const lines = [
    `       DCL-F QSYSPRT PRINTER(132) USAGE(*OUTPUT) OFLIND(*INOF);`,
    ` `,
    `       DCL-S OVR_FILE CHAR(21);`,
    ``,
    `       DCL-S TP CHAR(1) DIM(6) CTDATA PERRCD(1);                                // Deduction types`,
    `       DCL-S TD CHAR(20) DIM(6) ALT(TP);`,
    ``,
    `       *INLR = *ON;`,
    `       Return;`,
    ``,
    `**    TP and TD - Deduction types and descriptions`,
    `BBenefit      Benefit`,
    `DDeferred CompDef Cmp`,
    `CChild supportCh Sup`,
    `GGarnishment  Garnish`,
    `SStatutory    Statut.`,
    `VVoluntary    Volntry`,
  ].join(`\n`);
  
  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});

  expect(cache.files.length).toBe(1);
  expect(cache.variables.length).toBe(3);
});

test("likeds1", async () => {
  const lines = [
    `**FREE`,
    `Dcl-s MyVariable2 CHAR(20);`,
    `Dcl-Ds astructure qualified;`,
    `  Subitem1 CHAR(20);`,
    `  Subitem2 CHAR(20);`,
    `End-ds;`,
    `Dcl-s MyVariable CHAR(20);`,
    `Dcl-Ds MyOtherStruct LikeDS(Astructure);`,
    `//Yes`
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});

  expect(cache.variables.length).toBe(2);
  expect(cache.structs.length).toBe(2);

  const MyOtherStruct = cache.find(`MyOtherStruct`);
  expect(MyOtherStruct.name).toBe(`MyOtherStruct`);
  expect(MyOtherStruct.position.line).toBe(7);
  expect(MyOtherStruct.subItems.length).toBe(2);
});

test("likeds2", async () => {
  const lines = [
    `**FREE`,
    `Dcl-s MyVariable2 CHAR(20);`,
    `Dcl-Ds astructure qualified;`,
    `  Subitem1 CHAR(20);`,
    `  Subitem2 CHAR(20);`,
    `End-ds;`,
    `Dcl-s MyVariable CHAR(20);`,
    `Dsply MyVariable;`,
    `Return;`,
    `Dcl-Proc myprocedure;`,
    `  Dcl-Pi *N;`,
    `    inputDS Likeds(astructure);`,
    `  End-Pi;`,
    `  Dsply 'Inside';`,
    `  Return;`,
    `End-Proc;`
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});

  expect(cache.variables.length).toBe(2);
  expect(cache.structs.length).toBe(1);
  expect(cache.procedures.length).toBe(1);

  const myProc = cache.find(`myprocedure`);
  expect(myProc.name).toBe(`myprocedure`);
  expect(myProc.position.line).toBe(9);
  expect(myProc.subItems.length).toBe(1);

  const parmInputDs = myProc.subItems[0];
  expect(parmInputDs.name).toBe(`inputDS`);
  expect(parmInputDs.position.line).toBe(11);
  expect(parmInputDs.subItems.length).toBe(2);
});

test("overload1", async () => {
  const lines = [
    `**FREE`,
    ``,
    `Dcl-PR json_setBool pointer extproc(*CWIDEN : 'jx_SetBoolByName');`,
    `  node pointer value;`,
    `  nodePath pointer value options(*string);`,
    `  value ind value;`,
    `End-PR;`,
    ``,
    `Dcl-PR json_setNum pointer extproc(*CWIDEN : 'jx_SetDecByName');`,
    `  node pointer value;`,
    `  nodePath pointer value options(*string);`,
    `  value packed(30:15) value;`,
    `End-PR;`,
    ``,
    `Dcl-PR json_set pointer overload ( `,
    `    json_setBool: `,
    `    json_setNum `,
    `);`,
    ``,
    `Dcl-PR json_nodeType int(5) extproc(*CWIDEN : 'jx_GetNodeType');`,
    `  node pointer value;`,
    `End-PR;`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});

  const { indentErrors } = Linter.getErrors({uri, content: lines}, {
    indent: 2,
    PrettyComments: true
  }, cache);

  expect(cache.procedures.length).toBe(4);
  expect(indentErrors.length).toBe(0);
});

test(`extproc1`, async () => {
  const lines = [
    `**free`,
    `dcl-pr APIVAL01S_iws_validate int(10) extproc(*dclcase);`,
    `  i_validationsDs                 likeds(APIVAL01S_validationsDs) dim(APIVAL01S_MAX_DIM);`,
    `  i_data                          varchar(APIVAL01S_DATA_LEN)     dim(APIVAL01S_MAX_DIM);`,
    `  o_errorJson                     varchar(APIVAL01S_JSON_LEN);`,
    `end-pr;`,
  ].join(`\n`);

  const cache = await parser.getDocs(uri, lines, {ignoreCache: true, withIncludes: true});

  expect(cache.procedures.length).toBe(1);
  expect(cache.procedures[0].name).toBe(`APIVAL01S_iws_validate`);
})