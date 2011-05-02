
%{ open BDD %}
        %token <int> VAR 
	%token <string> AND OR IMP BIMP 
	%token NEG OPEN CLOSE EOL

	%left AND NEG OR IMP BIMP

        %start main             
        %type <BDD.expression> main

        %%
        
        expression:
          | VAR                        { BDD.Var $1 }
          | OPEN expression CLOSE      { $2 } 
          | expression AND expression  { BDD.And($1,$3) }
          | expression OR expression   { BDD.Or($1,$3) }
          | expression IMP expression  { BDD.Imp($1,$3) }
          | expression BIMP expression { BDD.BImp($1,$3) }
	  | NEG expression             { BDD.Neg($2) }
	;
        main:
          | expression EOL             { $1 }
        ;
