import { OpmParser } from "../../../language/opm/parser";
import Declaration from "../../../language/models/declaration";
import { PREMMAST } from "./files/premmast";

const files: Record<string, Declaration[]> = {
  PREMMAST
}

export function setupParser() {
  const opmparser = new OpmParser();

  opmparser.setTableFetch(async (name: string): Promise<Declaration[]> => {
    return files[name] || [];
  });

  return opmparser;
}
