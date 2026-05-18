import { describe, expect, it } from "vitest";
import { InputConstantEntry, InputDataStructureEntry, InputField, InputSpecification, parseSpecification } from "../../../language/opm/specs";

describe("Specs Parser", () => {
  it('I base test', () => {
    // This is a placeholder for the actual test implementation.
    // You can replace this with your actual test logic.
    const line = `     I$APIER      DS`;
    const iSpec = parseSpecification(line) as InputDataStructureEntry;

    expect(iSpec).toBeDefined();

    expect(iSpec).toBeDefined();
    expect(iSpec.type).toBe("input");
    expect(iSpec.subtype).toBe("record");
    expect(iSpec.described).toBe("structure");

    expect(iSpec.name.value).toBe("$APIER");
    expect(line.substring(
      iSpec.name.range[0],
      iSpec.name.range[1]
    )).toBe("$APIER");
  });

  it('I field test', () => {
    const line = `     I I            80                    B   1   40$ERSIZ`;

    const iSpec = parseSpecification(line) as InputField;
    expect(iSpec).toBeDefined();
    expect(iSpec.type).toBe("input");
    expect(iSpec.subtype).toBe("field");
    expect(iSpec.name.value).toBe("$ERSIZ");
    expect(iSpec.internalDataFormat.value).toBe("B");
    expect(iSpec.from.value).toBe(1);
    expect(iSpec.to.value).toBe(4);
    expect(iSpec.decimalPositions.value).toBe(0);
    expect(iSpec.externalField).toBeFalsy();
    expect(iSpec.initialValue.value).toBe("80")

    expect(line.substring(
      iSpec.name.range[0],
      iSpec.name.range[1]
    )).toBe("$ERSIZ");
  });

  it('Simple I field test', () => {
    const line = `     I                                        9  15 $ERMIC`;
    const iSpec = parseSpecification(line) as InputField;

    expect(iSpec).toBeDefined();
    expect(iSpec.type).toBe("input");
    expect(iSpec.subtype).toBe("field");
    expect(iSpec.name.value).toBe("$ERMIC");
    expect(iSpec.internalDataFormat).toBeFalsy();
    expect(iSpec.from.value).toBe(9);
    expect(iSpec.to.value).toBe(15);
    expect(iSpec.decimalPositions).toBeFalsy();
  });

  it('I comment test', () => {
    const iSpec = parseSpecification(`     I* $ERSIZ = bytes provided for error data; controls error handling:`) as InputSpecification;
    expect(iSpec).toBeNull();
  });

  it('I constant test', () => {
    const iSpec = `@1A  I              'CRTLF FILE('         C         CRTLF`;
    const constantSpec = parseSpecification(iSpec) as InputConstantEntry;
    expect(constantSpec).toBeDefined();
    expect(constantSpec.type).toBe("input");
    expect(constantSpec.subtype).toBe("record");
    expect(constantSpec.described).toBe("constant");
    expect(constantSpec.constantName.value).toBe("CRTLF");
  });
});