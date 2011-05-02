{
  open Lexing
  open Parser
  exception Eof
}

        rule token = parse
          | [' ' '\t'] { token lexbuf }
          | ['\n' ] { EOL }
          | ['0'-'9']+ as s { VAR(int_of_string s) }
          | "and"             { AND (Lexing.lexeme lexbuf) }
          | "or"              { OR (Lexing.lexeme lexbuf)  }
          | "->"              { IMP (Lexing.lexeme lexbuf) }
          | "<->"             { BIMP (Lexing.lexeme lexbuf)}
          | '!'               { NEG }
	  | '('               { OPEN }
	  | ')'               { CLOSE }
          | eof               { raise Eof }
