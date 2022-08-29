
interface APIInterface {
  type: "function"|"struct";
  insertText: string;
  /** Usually indicates what it will return */
  detail: string;
  description: string; 
  prototype: string[];
}