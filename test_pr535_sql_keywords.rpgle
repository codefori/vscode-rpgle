**free

// Test file for PR 535: SQL keyword detection in bracket matcher
// This PR should prevent SQL keywords like IF, FOR, WHEN, etc. from being
// highlighted as mismatched RPG block keywords when inside EXEC SQL blocks

dcl-s customer varchar(50);
dcl-s status varchar(20);
dcl-s recordCount int(10);
dcl-s endif int(10);
dcl-s endDo int(10);

  endif += 1;


// Test 1: IF inside SQL (should NOT be highlighted as RPG IF)
exec sql
  SELECT name INTO :customer
  FROM customers
  WHERE IF(status = 'ACTIVE', 1, 0) = 1;

// Test 2: FOR inside SQL (should NOT be highlighted as RPG FOR)
exec sql
  DECLARE c1 CURSOR FOR
  SELECT * FROM orders
  WHERE order_date > CURRENT_DATE - 30 DAYS;

// Test 3: CASE/WHEN/THEN/END inside SQL (should NOT match RPG blocks)
exec sql
  SELECT
    CASE
      WHEN amount > 1000 THEN 'High'
      WHEN amount > 100 THEN 'Medium'
      ELSE 'Low'
    END as priority
  INTO :status
  FROM transactions;

// Test 4: Multiple SQL keywords in one block
exec sql
  SELECT COUNT(*) INTO :recordCount
  FROM products
  WHERE category IN (
    SELECT category
    FROM featured_categories
    WHERE IF(is_active = 1, 1, 0) = 1
  )
  FOR READ ONLY;

// Test 5: Normal RPG IF block (SHOULD still be highlighted normally)
if customer = 'TEST';
  dsply 'Test customer found';
endif;

// Test 6: Normal RPG SELECT block (SHOULD still work)
select;
  when status = 'ACTIVE';
    dsply 'Active';
  when status = 'PENDING';
    dsply 'Pending';
  other;
    dsply 'Unknown';
endsl;

// Test 7: FOR loop (SHOULD still work)
for recordCount = 1 to 10;
  dsply recordCount;
endfor;

*inlr = *on;
return;
