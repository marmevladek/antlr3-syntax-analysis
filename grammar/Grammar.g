grammar Grammar;

options {
  language=C;
  output=AST;
  backtrack=true;
}

tokens {
  SourceToken;
  FuncDefToken;
  FuncSignatureToken;
  ArgListToken;
  TypeRefToken;
  ExpressionToken;
  LoopToken;
  DoWhileToken;
  ArgToken;
  Body;
  BreakToken;
  VarDeclToken;
  CallToken;
  ArrayToken;
  ArrayList;
  IfToken;
  BlockToken;
  WhileToken;
  ReturnToken;
}

source
  : sourceItem* -> ^(SourceToken sourceItem*)
  ;

sourceItem
  : funcDef
  ;

funcDef
  : funcSignature (block | ';') -> ^(FuncDefToken funcSignature block?)
  ;

funcSignature
  : typeRef? identifier '(' argList ')' -> ^(FuncSignatureToken typeRef? identifier argList)
  ;

argList
  : (argDef (',' argDef)*)? -> ^(ArgListToken argDef*)
  ;

argDef
  : typeRef? identifier -> ^(ArgToken typeRef? identifier)
  ;

statement
  : varDeclaration
  | ifStatement
  | whileStatement
  | doWhileStatement
  | breakStatement
  | returnStatement
  | expressionStatement
  | block
  ;

block
  : '{' statement* '}' -> ^(BlockToken statement*)
  ;

varDeclaration
  : typeRef (identifier ('=' expr)?) (',' identifier ('=' expr)?)* ';'
  -> ^(VarDeclToken typeRef (identifier expr?)*)
  ;

ifStatement
  : 'if' '(' expr ')' statement ('else' statement)?
  -> ^(IfToken expr statement statement?)
  ;

whileStatement
  : 'while' '(' expr ')' statement
  -> ^(WhileToken expr statement)
  ;

doWhileStatement
  : 'do' block 'while' '(' expr ')' ';'
  -> ^(DoWhileToken block expr)
  ;

breakStatement
  : 'break' ';' -> ^(BreakToken)
  ;

returnStatement
  : 'return' expr? ';' -> ^(ReturnToken expr?)
  ;

expressionStatement
  : expr ';' -> ^(ExpressionToken expr)
  ;

expr
  : assignExpr
  ;

assignExpr
  : logicalOrExpr ('=' assignExpr)?
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
  : UnOp unaryExpr
  | primaryExpr
  ;

primaryExpr
  : '(' expr ')'
  | identifier '(' expressionList? ')'  -> ^(CallToken identifier expressionList?)
  | identifier '[' expressionList ']' -> ^(ArrayToken identifier expressionList)
  | identifier
  | Literal
  ;

expressionList
  : expr (',' expr)*
  ;

typeRef
  : builtin
  | identifier
  | arrayType
  ;

arrayType
  : (builtin | identifier) '[' (',')* ']' -> ^(ArrayList builtin? identifier?)
  ;

builtin
  : BuiltIn
  ;

BinOp
  : '+' | '-' | '*' | '/' | '%' 
  | '&&' | '||' 
  | '&' | '|' | '^' 
  | '==' | '!=' | '<' | '<=' | '>' | '>='
  ;

UnOp
  : '-' | '!'
  ;

Literal
  : Bool
  | Bits
  | Hex
  | Dec
  | Char
  | Str
  ;

identifier
  : Identifier
  ;

fragment
Bool:  ('true'|'false');

fragment
Bits:  '0' ('b'|'B') ('0'..'1')+;

fragment
Hex :  '0' ('x'|'X') ('0'..'9'|'a'..'f'|'A'..'F')+;

fragment
Dec  :  ('0'..'9')+;

fragment
Char:  '\'' ~('\'') '\'';

fragment
Str :  '"' ( ~('"'|'\\') | ('\\'.) )* '"';

BuiltIn
  :  'bool'
  |  'byte'
  |  'int'
  |  'uint'
  |  'long'
  |  'ulong'
  |  'char'
  |  'string'
  ;

Identifier
  :  ('a'..'z'|'A'..'Z'|'_') ('a'..'z'|'A'..'Z'|'_'|'0'..'9')*
  ;

WS  :  (' '|'\r'|'\t'|'\u000C'|'\n') { $channel=HIDDEN; };