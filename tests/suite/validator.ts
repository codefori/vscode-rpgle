import * as assert from "assert";
import Linter from "../../language/linter"
import parserSetup from "../parserSetup"

const uri = `source.rpgle`;

export async function format_test() {
  const lines = [
    `**free`,
    `dcl-s **free char(5);`
  ].join(`\n`);

  const parser = parserSetup();
  const cache = await parser.getDocs(uri, lines);
  const { errors } = Linter.getErrors({uri, content: lines}, {
    Validator: true
  }, cache);

  assert.strictEqual(errors.length, 1);
  assert.deepStrictEqual(errors[0], {
    type: `Validator`,
    message: 'Token is expected at the start of statements',
    offset: {position: 13, end: 19}
  });
}