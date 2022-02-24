     D UPPERCASE       PR          4096    Varying                            
     D   String                    4096    Const Varying                      
     D   Escaped                       n   Const Options(*NoPass)             
      /EoF                                                                    
            Converts all of the letters in String to their                    
            UPPER CASE equivalents.  Non-alphabetic characters                
            remain unchanged.                                                 
                                                                              
            Escaped = *ON = converts characters that would crash iPDF and     
                            HTML to approximately equivalent characters.      
                            For example, translate " and ' to ` .             
                            (Default)                                         
                      *OFF= Do not convert any characters other than A-Z.     