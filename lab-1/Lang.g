grammar Lang;


options {
  language = C;
  output=AST;
}


// Лексические правила
IDENTIFIER
    : ('a'..'z' | 'A'..'Z' | '_') ('a'..'z' | 'A'..'Z' | '0'..'9' | '_')*
    ; // Идентификатор

STRING
    : '"' ( '\\' . | ~('\\' | '"') )* '"'
    ; // Строка в двойных кавычках
CHAR
    : '\'' ( '\\' . | ~('\'' | '\\') ) '\''
    ; // Символ в одинарных кавычках

INT
    : ('0'..'9')+
    ; // Целочисленный литерал

WS
    : (' ' | '\t' | '\r' | '\n')+ { $channel = HIDDEN; }
    ; // Пропуск пробелов и переносов строк

// Синтаксические правила

source: (funcDef | varDef | statement)*;

funcDef
    : typeRef IDENTIFIER '(' argList? ')' block
    ;

argList
    : (typeRef IDENTIFIER (',' typeRef IDENTIFIER)*)
    ;

varDef
    : typeRef IDENTIFIER ('=' expr)? ';'
    ;

statement
    : block
    | ifStatement
    | whileStatement
    | breakStatement
    | expr ';'
    ;

ifStatement
    : 'if' '(' expr ')' statement ifElseSuffix?
    ;

ifElseSuffix
    : 'else' statement
    ;

whileStatement
    : 'while' '(' expr ')' statement
    ;

breakStatement
    : 'break' ';'
    ;

block
    : '{' statement* '}'
    ;

expr
    : logicalOrExpr
    ;

logicalOrExpr
    : logicalAndExpr ('||' logicalAndExpr)*
    ;

logicalAndExpr
    : equalityExpr ('&&' equalityExpr)*
    ;

equalityExpr
    : relationalExpr (('==' | '!=') relationalExpr)*
    ;

relationalExpr
    : additiveExpr (('<' | '<=' | '>' | '>=') additiveExpr)*
    ;

additiveExpr
    : multiplicativeExpr (('+' | '-') multiplicativeExpr)*
    ;

multiplicativeExpr
    : unaryExpr (('*' | '/' | '%') unaryExpr)*
    ;

unaryExpr
    : ('-' | '!') unaryExpr
    | primary
    ;

primary
    : '(' expr ')'
    | IDENTIFIER '(' exprList? ')'
    | IDENTIFIER '[' expr ']'
    | IDENTIFIER
    | literal
    ;

exprList
    : expr (',' expr)*
    ;

literal
    : INT
    | STRING
    | CHAR
    ;

typeRef
    : 'int' | 'char' | 'string'
    ;