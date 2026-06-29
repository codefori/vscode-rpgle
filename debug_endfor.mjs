import { findAllBlockMatches } from './language/utils/blockParser.js';
import { RPGLE_BLOCK_PAIRS } from './language/utils/blockParser.js';

// Mock functions
const isInCommentOrString = () => false;
const isInSqlBlock = () => false;

const testCode = `       ctl-opt dftactgrp(*NO) actgrp(*NEW);
         DCL-S f2f_tempDO INT(10);
         dcl-s pIndy pointer inz(%ADDR(*IN));
         dcl-ds indy len(1) DIM(99) Qualified based(pIndy) end-ds;
         %SUBARR(*in : 41) = %LIST('1':'0':'1');
         *IN(42) = '1';
         %subarr(*IN:41) = %List('1':'1':'1':'1':'0':'0':'1');
         %SUBARR(indy : 41 : 2) = '10';
         *IN(41) = '0';
         *IN42 = '1';
           // MOVEA '10' *IN(41)  --> to FOR loop
         for f2f_tempDO = 1 to  %len('10');
         *in(41 + f2f_tempDO-1) = %subst('10' : f2f_tempDO : 1);
         endFor;

          *INLR = *ON;
        return;`;

const matches = findAllBlockMatches(testCode, isInCommentOrString, isInSqlBlock);

console.log('Found matches:');
matches.forEach((m, i) => {
    const line = testCode.substring(0, m.offset).split('\n').length;
    console.log(`${i}: "${m.word}" at offset ${m.offset} (line ${line})`);
});

console.log('\nLooking for FOR-related matches:');
matches.filter(m => m.word === 'for' || m.word === 'endfor').forEach((m, i) => {
    const line = testCode.substring(0, m.offset).split('\n').length;
    console.log(`"${m.word}" at line ${line}`);
});

console.log('\nFor pair definition:');
const forPair = RPGLE_BLOCK_PAIRS.find(p => p.open.includes('for'));
console.log('Open:', forPair.open);
console.log('Close:', forPair.close);
