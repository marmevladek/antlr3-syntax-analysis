// Пример грамматики для арифметических выражений
grammar CLanguage;

options {
  language = C;
  output=AST;
}

// Начальная точка входа для грамматики
expr
  : term ((PLUS | MINUS) term)* ;

term
  : factor ((TIMES | DIVIDE) factor)* ;

factor
  : NUMBER
  | LPAREN expr RPAREN ;

NUMBER
  : ('0'..'9')+ ('.' ('0'..'9')+)? ;

PLUS
  : '+' ;

MINUS
  : '-' ;

TIMES
  : '*' ;

DIVIDE
  : '/' ;

LPAREN
  : '(' ;

RPAREN
  : ')' ;

WS
  : (' ' | '\t' | '\n' | '\r') ;