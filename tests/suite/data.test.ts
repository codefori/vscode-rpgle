import { test, expect } from "vitest";
import { dspffdToRecordFormats } from "../../extension/server/src/data";

test("dspffd alias mode emits all aliases and system name when they differ", () => {
  const data = [
    {
      WHNAME: "LONGTABLE",
      WHFLDE: "SYSNAME",
      WHFLDT: "A",
      WHFLDB: 50,
      WHFLDD: 0,
      WHFLDP: 0,
      WHFTXT: "",
      WHVARL: "N",
      WHALIS: "THIS_IS_A_VERY_LONG_ALIAS_NAME",
      WHALI2: "THIS_IS_A_VERY_LONG_ALIAS_NAME_12345"
    }
  ];

  const recordFormats = dspffdToRecordFormats(data, true);

  expect(recordFormats.length).toBe(1);
  expect(recordFormats[0].subItems.length).toBe(3);
  expect(recordFormats[0].subItems[0].name).toBe("THIS_IS_A_VERY_LONG_ALIAS_NAME");
  expect(recordFormats[0].subItems[1].name).toBe("THIS_IS_A_VERY_LONG_ALIAS_NAME_12345");
  expect(recordFormats[0].subItems[2].name).toBe("SYSNAME");
});

test("dspffd non-alias mode emits only system name", () => {
  const data = [
    {
      WHNAME: "LONGTABLE",
      WHFLDE: "SYSNAME",
      WHFLDT: "A",
      WHFLDB: 50,
      WHFLDD: 0,
      WHFLDP: 0,
      WHFTXT: "",
      WHVARL: "N",
      WHALIS: "THIS_IS_A_VERY_LONG_ALIAS_NAME",
      WHALI2: "THIS_IS_A_VERY_LONG_ALIAS_NAME_12345"
    }
  ];

  const recordFormats = dspffdToRecordFormats(data, false);

  expect(recordFormats.length).toBe(1);
  expect(recordFormats[0].subItems.length).toBe(1);
  expect(recordFormats[0].subItems[0].name).toBe("SYSNAME");
});

test("dspffd alias falls back to system field name", () => {
  const data = [
    {
      WHNAME: "LONGTABLE",
      WHFLDE: "SYSTEM_FIELD",
      WHFLDT: "A",
      WHFLDB: 10,
      WHFLDD: 0,
      WHFLDP: 0,
      WHFTXT: "",
      WHVARL: "N",
      WHALIS: "",
      WHALI2: ""
    }
  ];

  const recordFormats = dspffdToRecordFormats(data, true);

  expect(recordFormats.length).toBe(1);
  expect(recordFormats[0].subItems.length).toBe(1);
  expect(recordFormats[0].subItems[0].name).toBe("SYSTEM_FIELD");
});

test("dspffd alias mode emits only system name when alias equals system name", () => {
  const data = [
    {
      WHNAME: "LONGTABLE",
      WHFLDE: "SYSNAME",
      WHFLDT: "A",
      WHFLDB: 10,
      WHFLDD: 0,
      WHFLDP: 0,
      WHFTXT: "",
      WHVARL: "N",
      WHALIS: "SYSNAME",
      WHALI2: ""
    }
  ];

  const recordFormats = dspffdToRecordFormats(data, true);

  expect(recordFormats.length).toBe(1);
  expect(recordFormats[0].subItems.length).toBe(1);
  expect(recordFormats[0].subItems[0].name).toBe("SYSNAME");
});


test("dspffd alias mode emits only system name when alias equals system name", () => {
  const data = [
    {
      WHNAME: "LONGTABLE",
      WHFLDE: "SYSNAME",
      WHFLDT: "A",
      WHFLDB: 10,
      WHFLDD: 0,
      WHFLDP: 0,
      WHFTXT: "",
      WHVARL: "N",
      WHALIS: "SYSNAME",
      WHALI2: "SYSNAME"
    }
  ];

  const recordFormats = dspffdToRecordFormats(data, true);

  expect(recordFormats.length).toBe(1);
  expect(recordFormats[0].subItems.length).toBe(1);
  expect(recordFormats[0].subItems[0].name).toBe("SYSNAME");
});
