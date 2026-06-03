# PR #491 Final Review - Comprehensive Analysis
## Date: June 3, 2026
## Reviewer: Bob Cozzi

## Executive Summary

✅ **Author's latest regex fix was excellent** - solved the hyphenated keyword issue
❌ **Stack corruption bug still present** - requires one critical fix (provided below)
✅ **Additional SQL keyword filtering needed** - implemented and tested
📝 **Future enhancement needed** - Single-line DCL-* detection requires proper statement parsing

---

## Summary of All Changes

The contributor made **3 commits** to PR #491:
1. Initial implementation (Enhanced blocks logic)
2. "Feat: add validity checker for closer"
3. "Fixed end- opcodes" (commit bdc1850)

---

## What the Author Fixed ✅

### 1. **Regex Pattern Fix** (Lines 243-249)
**Problem:** The regex `\b(keywords)\b` matched `end` in `end-proc` before matching the full keyword.

**Solution:**
```typescript
// Sort keywords by length (longest first) to match longer keywords before shorter ones
const sortedKeywords = allKeywords.sort((a, b) => b.length - a.length);
const regex = new RegExp(`\\b(${sortedKeywords.map(k => k.replace(/-/g, '\\-')).join('|')})\\b`, 'gi');
```

**Result:** ✅ `END-PROC`, `END-DS`, `END-PI`, `END-PR`, `END-ENUM` now parse correctly

---

### 2. **Improved Closer Identification** (Multiple locations)
**Better Logic:**
```typescript
const closerPair = RPGLE_BLOCK_PAIRS.find(p => {
  if (!p.close.includes(word)) return false;
  if (p.close.length === 1) return true;  // Single-closer pairs
  const specificClosers = p.close.filter(c => c !== 'end' && c !== 'enddo');
  return specificClosers.includes(word);  // Multi-closer pairs
});
```

**Result:** ✅ Cleaner distinction between specific closers (`endif`) and generic closers (`end`)

---

## Critical Bug Still Present ❌

### **Stack Corruption in `findMatchingOpenForAnyClosing()`** (Lines 568-576)

**The Problem:**
When an invalid closer is encountered (e.g., `endfor` trying to close an `if` block), the stack is not modified, leaving it corrupted for subsequent validation.

**Example of the Bug:**
```rpgle
for x = 1 to 10;     // Stack: [FOR]
  if (x > 4);        // Stack: [FOR, IF]
  endfor;            // INVALID! Should close FOR, but IF is on top
                     // Author's code: lastBlock.pair !== closerPair, so doesn't pop
                     // Stack STAYS: [FOR, IF] ← CORRUPTED!
endif;               // Now validates against corrupted stack
                     // Finds IF on top → no error reported!
```

**Current Code (Buggy):**
```typescript
if (closerPair && stack.length > 0) {
  const lastBlock = stack[stack.length - 1];
  if (lastBlock.pair === closerPair) {
    stack.pop();
  }
  // If it's not the correct type, don't remove anything (it's an error)
}
```

**Required Fix:**
```typescript
if (closerPair && stack.length > 0) {
  // For specific closers, search the stack for a matching block type
  // This maintains stack integrity by removing the block even when nesting is incorrect
  // The validation of correctness happens in validateClosingKeyword
  for (let j = stack.length - 1; j >= 0; j--) {
    if (stack[j].pair === closerPair) {
      stack.splice(j, 1);
      break;
    }
  }
}
```

**Why This Works:**
The stack represents which blocks are still open. Even if a closer is used incorrectly (wrong nesting order), it still closes *some* block, so we must remove it from the stack. The `validateClosingKeyword()` function then determines if the nesting was correct and flags the error.

---

## Additional Fixes Applied ✅

### 3. **SQL Keyword Filtering** (Lines 254-259)

**Problem:** Keywords like `END`, `WHEN`, `CASE` inside `EXEC SQL` blocks were being treated as RPG keywords.

**Solution:**
```typescript
// Skip SQL keywords when inside EXEC SQL blocks
const sqlKeywords = ['select', 'for', 'when', 'case', 'end', 'then', 'else'];
if (sqlKeywords.includes(matchWord) && isInSqlBlock(text, match.index)) {
  continue;
}
```

**Result:** ✅ SQL `CASE/WHEN/END` statements no longer flagged as errors

---

## Future Enhancements 📝

### 4. **Single-Line DCL-* Detection** (Not Implemented)

**Challenge:**
Distinguishing between:
```rpgle
dcl-ds ec likeds(QUSEC_T) inz(*LIKEDS);  // Single-line, no END-DS needed
```
vs.
```rpgle
dcl-pi getObjD;      // Starts a block, needs END-PI
  param1 ...;
END-PI;
```

Both have semicolons on the same line, but mean different things.

**Recommendation:**
Implement proper statement parsing (similar to RPGIV2FREE's `getStatement()` function) that follows statements across multiple lines. This would require:
- Tracking statement continuation beyond line breaks
- Understanding when a semicolon ends a single-line declaration vs. just ending the DCL statement itself

**Current Trade-off:**
Single-line `dcl-ds` declarations will show a false positive expecting `end-ds`. This is acceptable until proper statement parsing is implemented.

---

## Files Modified

### `extension/client/src/language/bracketMatcher.ts`
**Changes Applied:**
1. ✅ Stack corruption fix (lines 568-576) - search and remove matching block
2. ✅ SQL keyword filtering (lines 254-259) - expanded keyword list
3. ✅ Function signature updates - added `text` parameter to `validateClosingKeyword` and `findMatchingOpenForAnyClosing`

---

## Testing Results

### Test Case 1: `test_mismatched_blocks.rpgle`
- ✅ Lines 14-15 (INCORRECT section): `endfor` and `endif` **highlighted in RED** ← CORRECT
- ✅ Lines 22-23 (CORRECT section): `endif` and `endfor` **no errors** ← CORRECT
- ✅ Line 30: `enddo` **no errors** ← CORRECT
- ✅ Lines 39-40: `end` (generic closer) **no errors** ← CORRECT

### Test Case 2: Real-world code with DCL blocks
- ✅ `END-PROC`, `END-DS`, `END-PI`, `END-PR` **no errors** ← CORRECT
- ✅ SQL `CASE/WHEN/END` statements **no errors** ← CORRECT

### Known Limitation:
- ⚠️ Single-line declarations like `dcl-ds ec likeds(...);` will show false positive expecting `end-ds`

---

## Recommendation for Contributor

**Accept the Author's Work:** The regex fix and improved closer identification are excellent contributions.

**Request One Additional Change:** Apply the stack corruption fix (lines 568-576) as shown above. This is **critical** for correct validation.

**Acknowledge the SQL Fix:** The additional SQL keyword filtering is needed and tested.

**Future Work:** Document the single-line DCL-* limitation and consider implementing proper statement parsing in a future PR.

---

## Summary of Changes to Apply

Replace lines 568-576 in `findMatchingOpenForAnyClosing()`:

**FROM:**
```typescript
if (closerPair && stack.length > 0) {
  const lastBlock = stack[stack.length - 1];
  if (lastBlock.pair === closerPair) {
    stack.pop();
  }
  // If it's not the correct type, don't remove anything (it's an error)
}
```

**TO:**
```typescript
if (closerPair && stack.length > 0) {
  // For specific closers, search the stack for a matching block type
  // This maintains stack integrity by removing the block even when nesting is incorrect
  // The validation of correctness happens in validateClosingKeyword
  for (let j = stack.length - 1; j >= 0; j--) {
    if (stack[j].pair === closerPair) {
      stack.splice(j, 1);
      break;
    }
  }
}
```

Add SQL keyword filtering (lines 254-259):

**FROM:**
```typescript
if (isInCommentOrString(text, match.index)) continue;
if (matchWord === 'select' && isInSqlBlock(text, match.index)) continue;
if (matchWord === 'for' && isInSqlBlock(text, match.index)) continue;
```

**TO:**
```typescript
if (isInCommentOrString(text, match.index)) continue;

// Skip SQL keywords when inside EXEC SQL blocks
const sqlKeywords = ['select', 'for', 'when', 'case', 'end', 'then', 'else'];
if (sqlKeywords.includes(matchWord) && isInSqlBlock(text, match.index)) {
  continue;
}
```

---

## Conclusion

The PR is **very close to ready**. With the stack corruption fix applied, it will correctly validate all RPG block structures while avoiding false positives from SQL statements. The single-line DCL-* limitation is acceptable for now and can be addressed in future work.

**Recommendation: Approve with requested changes**
