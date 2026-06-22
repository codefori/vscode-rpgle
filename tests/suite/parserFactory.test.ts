import { describe, expect, it } from "vitest";
import { ParserFactory } from "../../language/parserFactory";
import Parser from "../../language/ile/parser";
import { OpmParser } from "../../language/opm/parser";

describe("ParserFactory", () => {
  it("uses OPM parser for .rpg and .sqlrpg", () => {
    expect(ParserFactory.getParser("example.rpg")).toBeInstanceOf(OpmParser);
    expect(ParserFactory.getParser("example.sqlrpg")).toBeInstanceOf(OpmParser);
    expect(ParserFactory.getParser("EXAMPLE.SQLRPG")).toBeInstanceOf(OpmParser);
  });

  it("uses OPM parser for deprecated .rpg36, .rpg38, and .sqlrpg38", () => {
    expect(ParserFactory.getParser("example.rpg36")).toBeInstanceOf(OpmParser);
    expect(ParserFactory.getParser("example.rpg38")).toBeInstanceOf(OpmParser);
    expect(ParserFactory.getParser("example.sqlrpg38")).toBeInstanceOf(OpmParser);
    expect(ParserFactory.getParser("EXAMPLE.RPG38")).toBeInstanceOf(OpmParser);
  });

  it("uses ILE parser for .rpgle and .sqlrpgle", () => {
    expect(ParserFactory.getParser("example.rpgle")).toBeInstanceOf(Parser);
    expect(ParserFactory.getParser("example.sqlrpgle")).toBeInstanceOf(Parser);
    expect(ParserFactory.getParser("EXAMPLE.SQLRPGLE")).toBeInstanceOf(Parser);
  });

  it("identifies OPM extensions correctly", () => {
    expect(ParserFactory.isOpmFile("example.rpg")).toBe(true);
    expect(ParserFactory.isOpmFile("example.sqlrpg")).toBe(true);
    expect(ParserFactory.isOpmFile("example.rpg36")).toBe(true);
    expect(ParserFactory.isOpmFile("example.rpg38")).toBe(true);
    expect(ParserFactory.isOpmFile("example.sqlrpg38")).toBe(true);
    expect(ParserFactory.isOpmFile("EXAMPLE.SQLRPG")).toBe(true);
    expect(ParserFactory.isOpmFile("example.rpgle")).toBe(false);
  });

  it("identifies ILE extensions correctly", () => {
    expect(ParserFactory.isIleFile("example.rpgle")).toBe(true);
    expect(ParserFactory.isIleFile("example.sqlrpgle")).toBe(true);
    expect(ParserFactory.isIleFile("EXAMPLE.SQLRPGLE")).toBe(true);
    expect(ParserFactory.isIleFile("example.sqlrpg")).toBe(false);
  });
});