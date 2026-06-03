**free

dcl-pr  extpgm('');
  if ();
    
  endif;
end-pr;


dcl-ds name qualified dim;
  
end-ds;




// Test per verificare il folding con END generico
dcl-s pluto int(10);
dcl-s pippo int(10);


pluto = 10;
pippo = 5;

// ===== TEST 1: Nesting misto IF + DOW =====
// Questo è il test critico
if (pluto > 0);
  dsply 'dentro if esterno';
  
  dow (pippo < 10);
    dsply 'dentro dow';
    pippo = pippo + 1;
    endif;
  end; // Questo END deve chiudere DOW
  
  dsply 'dopo dow, ancora dentro if';
end; // Questo END deve chiudere IF

// ===== TEST 2: DOW semplice con END =====
dow (pluto > 0);
  dsply 'dentro dow';
  pluto = pluto - 1;
end;

if ();
  
else;
 
endif;

// ===== TEST 3: FOR con END =====
for pippo = 1 to 10;
  dsply 'dentro for';
end;

// ===== TEST 4: SELECT con END =====
select;
  when pluto = 0;
    dsply 'pluto è zero';
  when pluto > 0;
    dsply 'pluto è positivo';
  other;
    dsply 'pluto è negativo';
end;

// ===== TEST 5: Nesting triplo =====
if (pluto > 0);
  dow (pippo < 10);
    for pluto = 1 to 5;
      dou ();
        
      enddo;    
      dsply 'triplo nesting';
    end; // Chiude FOR
  end; // Chiude DOW
end; // Chiude IF

// ===== TEST 6: Chiusure specifiche (devono continuare a funzionare) =====
if (pluto > 0);
  dsply 'test endif';
endif;

dow (pippo > 0);
  pippo = pippo - 1;
enddo;

for pluto = 1 to 5;
  dsply 'test endfor';
endfor;

// ===== TEST 7: Mix di chiusure specifiche e generiche =====
if (pluto > 0);
  dow (pippo < 10);
    dou ();
      
    enddo;
  enddo; // Chiusura specifica per DOW
end; // Chiusura generica per IF

*inlr = *on;