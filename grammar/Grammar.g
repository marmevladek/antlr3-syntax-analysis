grammar Grammar;

options {
    language = C;
    output = AST;
    backtrack = true;
    ASTLabelType = pANTLR3_BASE_TREE;
}

tokens {
  Source;
  SourceItem;
  FuncDef;
  Statement;
  Expression;
  BreakStatement;
  DoStatement;
  WhileStatement;
  Block;
  IfStatement;
  Var;
  FuncSignature;
  List;
  IdItem;
  ArgDef;
  TypeRef;
  Array;
  Custom;
  Builtin;
  Expr;
  Literal;
  Place;
  Indexer;
  Call;
  Braces;
  Unary;
  UnOp;
  Binary;
  BinOpBinAssAnd;
  BinOpBinAssXOR;
  BinOpBinAssOr;
  BinOpBinAssLS;
  BinOpBinAssRS;
  BinOpBinAssMul;
  BinOpBinAssDiv;
  BinOpBinAssMod;
  BinOpBinAssSum;
  BinOpBinAssNeg;
  BinOpAss;
  BinOpLogOr;
  BinOpLogAnd;
  BinOpBinOr;
  BinOpBinXOR;
  BinOpBinAnd;
  BinOpEq;
  BinOpNonEq;
  BinOpLt;
  BinOpLte;
  BinOpGt;
  BinOpGte;
  BinOpBinLS;
  BinOpBinRS;
  BinOpBinPlus;
  BinOpBinMinus;
  BinOpMul;
  BinOpDiv;
  BinOpMod;
  BoolStatement;
  Dec;
  Bits;
  Hex;
  CharStatement;
  Str;
  Identifier;
}

@header
{
   #define _empty NULL
}

ID: ('a'..'z' | 'A'..'Z' | '_')('a'..'z' | 'A'..'Z' | '_' | '0'..'9')*;
identifier: ID -> ^(Identifier ID);

STRING: '"' ~('\\' | '"')* ('\\'~('\r' | '\n') ~('\\' | '"')*)* '"';
str: STRING -> ^(Str STRING); 

CHARACTER: '\'' ~('\'') '\'';
charStatement: CHARACTER -> ^(CharStatement CHARACTER);

HEXADEMIC: '0'('x' | 'X')('0'..'9' | 'A'..'F' | 'a'..'f')+;
hex: HEXADEMIC -> ^(Hex HEXADEMIC);

BIT: '0'('b' | 'B')('0' | '1')+;
bits: BIT -> ^(Bits BIT);

DECIMAL: ('0'..'9')+;
dec: DECIMAL -> ^(Dec DECIMAL);

boolStatement: tr='true' -> ^(BoolStatement $tr)
| fl='false' -> ^(BoolStatement $fl)
;


binOpBinAssLog: assbina='&=' -> ^(BinOpBinAssAnd)
| assbinx='^=' -> ^(BinOpBinAssXOR)
| assbino='|=' -> ^(BinOpBinAssOr)
;

binOpBinAssShift: assbinls='<<=' -> ^(BinOpBinAssLS)
| assbinrs='>>=' -> ^(BinOpBinAssRS)
;

binOpMultAss: assmul='*=' -> ^(BinOpBinAssMul)
| assdiv='/=' -> ^(BinOpBinAssDiv)
| assmod='%=' -> ^(BinOpBinAssMod)
;

binOpSumAss: asssum='+=' -> ^(BinOpBinAssSum) 
| assneg='-=' -> ^(BinOpBinAssNeg)
;

binOpAss: '=' -> ^(BinOpAss);

binOpLogOr: '||' -> ^(BinOpLogOr);

binOpLogAnd: '&&' -> ^(BinOpLogAnd);

binOpBinOr: '|' -> ^(BinOpBinOr);

binOpXOR: '^' -> ^(BinOpBinXOR);

binOpBinAnd: '&' -> ^(BinOpBinAnd);

binOpEq: eq='==' -> ^(BinOpEq) 
| noneq='!=' -> ^(BinOpNonEq)
;

binOpComp: lt='<' -> ^(BinOpLt) 
| lte='<=' -> ^(BinOpLte) 
| gt='>' -> ^(BinOpGt) 
| gte='>=' -> ^(BinOpGte)
;

binOpBinShift: binls='<<' -> ^(BinOpBinLS) 
| binrs='>>' -> ^(BinOpBinRS)
;

binOpPlus: binp='+' -> ^(BinOpBinPlus) 
| binm='-' -> ^(BinOpBinMinus)
;

binOpMult: binmul='*' -> ^(BinOpMul) 
| bindiv='/' -> ^(BinOpDiv) 
| binmod='%' -> ^(BinOpMod)
;


binary: assShiftBinary (binOpBinAssLog assShiftBinary)* -> ^(Binary assShiftBinary (binOpBinAssLog assShiftBinary)*);
assShiftBinary: multAssBinary (binOpBinAssShift^ multAssBinary)*;// -> ^(Binary multAssBinary (binOpBinAssShift multAssBinary)*);
multAssBinary: sumAssBinary (binOpMultAss^ sumAssBinary)*;// -> ^(Binary sumAssBinary (binOpMultAss sumAssBinary)*);
sumAssBinary: assBinary (binOpSumAss^ assBinary)*;// -> ^(Binary assBinary (binOpSumAss assBinary)*);
assBinary: logOrBinary (binOpAss^ logOrBinary)*;// -> ^(Binary logOrBinary (binOpAss logOrBinary)*);
logOrBinary: logAndBinary (binOpLogOr^ logAndBinary)*;// -> ^(Binary logAndBinary (binOpLogOr logAndBinary)*);
logAndBinary: binOrBinary (binOpLogAnd^ binOrBinary)*;// -> ^(Binary binOrBinary (binOpLogAnd binOrBinary)*);
binOrBinary: xorBinary (binOpBinOr^ xorBinary)*;// -> ^(Binary xorBinary (binOpBinOr xorBinary)*);
xorBinary: binAndBinary (binOpXOR^ binAndBinary)*;// -> ^(Binary binAndBinary (binOpXOR binAndBinary)*);
binAndBinary: eqBinary (binOpBinAnd^ eqBinary)*;// -> ^(Binary eqBinary (binOpBinAnd eqBinary)*);
eqBinary: compBinary (binOpEq^ compBinary)*;// -> ^(Binary compBinary (binOpEq compBinary)*);
compBinary: binShiftBinary (binOpComp^ binShiftBinary)*;// -> ^(Binary binShiftBinary (binOpComp binShiftBinary)*);
binShiftBinary: plusBinary (binOpBinShift^ plusBinary)*;// -> ^(Binary plusBinary (binOpBinShift plusBinary)*);
plusBinary: multBinary (binOpPlus^ multBinary)*;// -> ^(Binary multBinary (binOpPlus multBinary)*);
multBinary: exprTerm (binOpMult^ expr)*;// -> ^(Binary exprTerm (binOpMult expr)+);

//binary: assShiftBinary (binOpBinAssLog assShiftBinary)* -> ^(Binary assShiftBinary (binOpBinAssLog assShiftBinary)*);
//assShiftBinary: multAssBinary (binOpBinAssShift multAssBinary)*;
//multAssBinary: sumAssBinary (binOpMultAss sumAssBinary)*;
//sumAssBinary: assBinary (binOpSumAss assBinary)*;
//assBinary: logOrBinary (binOpAss logOrBinary)*;
//logOrBinary: logAndBinary (binOpLogOr logAndBinary)*;
//logAndBinary: binOrBinary (binOpLogAnd binOrBinary)*;
//binOrBinary: xorBinary (binOpBinOr xorBinary)*;
//xorBinary: binAndBinary (binOpXOR binAndBinary)*;
//binAndBinary: eqBinary (binOpBinAnd eqBinary)*;
//eqBinary: compBinary (binOpEq compBinary)*;
//compBinary: binShiftBinary (binOpComp binShiftBinary)*;
//binShiftBinary: plusBinary (binOpBinShift plusBinary)*;
//plusBinary: multBinary (binOpPlus multBinary)*;
//multBinary: exprTerm (binOpMult expr)*;
//multBinary: (braces | call | indexer | place | literal) (binOpMult expr)*;

unOpNeg: logneg='!' -> ^(UnOp $logneg) 
| binneg='~' -> ^(UnOp $binneg) 
;

unOpPlus: unp='+' -> ^(UnOp $unp)  
| unm='-' -> ^(UnOp $unm) 
;

unOpFix: uninc='++' -> ^(UnOp $uninc)  
| undec='--' -> ^(UnOp $undec) 
;

unary: (unOpFix | unOpPlus | unOpNeg) expr -> ^(Unary unOpFix? unOpPlus? unOpNeg? expr);

braces: '(' expr ')' -> ^(Braces '(' expr ')');

listExpr: (expr (',' expr)*)? -> ^(List (expr (',' expr)*)?);

call: '(' listExpr ')' -> ^(Call '(' listExpr ')');

indexer: '[' listExpr ']' -> ^(Indexer '[' listExpr ']');

place: identifier -> ^(Place identifier);

literal: boolStatement -> ^(Literal boolStatement)
| str -> ^(Literal str)
| charStatement -> ^(Literal charStatement)
| hex -> ^(Literal hex)
| bits -> ^(Literal bits)
| dec -> ^(Literal dec)
;

expr: binary -> ^(Expr binary);

exprTerm: unary exprTermTerm -> ^(Expr unary exprTermTerm)
| braces exprTermTerm -> ^(Expr braces exprTermTerm)
| place exprTermTerm -> ^(Expr place exprTermTerm)
| literal exprTermTerm -> ^(Expr literal exprTermTerm)
;

exprTermTerm: ((call | indexer)+)?;
	

builtin: bl='bool' -> ^(Builtin $bl)
| bt='byte' -> ^(Builtin $bt)
| it='int' -> ^(Builtin $it)
| uit='uint' -> ^(Builtin $uit)
| lng='long' -> ^(Builtin $lng)
| ulng='ulong' -> ^(Builtin $ulng)
| cha='char' -> ^(Builtin $cha)
| strng='string' -> ^(Builtin $strng)
;

custom: identifier -> ^(Custom identifier);

array: custom ('[' (',')* ']')+ -> ^(Array custom ('[' (',')* ']')+)
| builtin ('[' (',')* ']')+ -> ^(Array builtin ('[' (',')* ']')+)
;

typeRef: custom -> ^(TypeRef custom)
| builtin -> ^(TypeRef builtin)
| array -> ^(TypeRef array)
;

argDef: typeRef? identifier -> ^(ArgDef typeRef? identifier);

listArgDef: (argDef (',' argDef)*)? -> ^(List (argDef (',' argDef)*)?);

identifierItem: identifier (binOpAss expr)? -> ^(IdItem identifier (binOpAss expr)?);
listIdentifier: (identifierItem (',' identifierItem)*)? -> ^(List (identifierItem (',' identifierItem)*)?);

funcSignature: typeRef? identifier '(' listArgDef ')' -> ^(FuncSignature typeRef? identifier '(' listArgDef ')');

var: typeRef listIdentifier ';' -> ^(Var typeRef listIdentifier ';'); // for static typing

ifStatement: 'if' '(' expr ')' statement ('else' statement)? -> ^(IfStatement 'if' '(' expr ')' statement ('else' statement)?);

block: '{' statement* '}' -> ^(Block '{' statement* '}');

whileStatement: 'while' '(' expr ')' statement -> ^(WhileStatement 'while' '(' expr ')' statement);

doStatement: 'do' block 'while' '(' expr ')' ';' -> ^(DoStatement 'do' block 'while' '(' expr ')' ';');

breakStatement: 'break' ';' -> ^(BreakStatement 'break' ';');

expression: expr ';' -> ^(Expression expr ';');

statement: var -> ^(Statement var)
| ifStatement -> ^(Statement ifStatement)
| block -> ^(Statement block)
| whileStatement -> ^(Statement whileStatement)
| doStatement -> ^(Statement doStatement)
| breakStatement -> ^(Statement breakStatement)
| expression -> ^(Statement expression)
;


//funcDef: funcSignature (block | ';');
funcDef: funcSignature block -> ^(FuncDef funcSignature block)
		| funcSignature ';' -> ^(FuncDef funcSignature ';')
		;

sourceItem: funcDef -> ^(SourceItem funcDef);

source: sourceItem* -> ^(Source sourceItem*);

WHITESPACE  : ( '\t' | ' ' | '\r' | '\n'| '\u000C' )+
              {
                 $channel = HIDDEN;
              }
            ;