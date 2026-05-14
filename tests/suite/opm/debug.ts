import { OpmParser } from '../../../language/opm/parser';
import path from 'path';
import { readFile } from 'fs/promises';

async function readOpmFixture(fixturePath: string): Promise<string> {
  const fullPath = path.join(__dirname, '../../fixtures/opm', fixturePath);
  return readFile(fullPath, 'utf-8');
}

async function debugTest() {
  const parser = new OpmParser();
  const fileUri = `ldaMarker.rpg`;
  const content = await readOpmFixture(fileUri);
  
  console.log('Content:', content);
  
  const cache = await parser.getDocs(fileUri, content);
  
  console.log('Symbols:', cache.symbols.map(s => ({ name: s.name, type: s.type })));
  console.log('Variables:', cache.variables.map(s => ({ name: s.name, type: s.type })));
  console.log('Structs:', cache.structs.map(s => ({ name: s.name, type: s.type, subItems: s.subItems.length })));
  console.log('Constants:', cache.constants.map(s => ({ name: s.name, type: s.type })));
}

debugTest();
