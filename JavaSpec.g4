grammar JavaSpec;

rules : rule+ EOF;
rule : lhs ':' rhs ';' ;
lhs : ID ;
rhs : rhs_sym* ;
rhs_sym :
  ID
  | SL
  | PIPE
  | zero_or_more
  | zero_or_one;
zero_or_more : '{' rhs '}' ;
zero_or_one : '[' rhs ']' ;
DOT : '.';
ID : [a-zA-Z_]+;
SL : '"' ~'"'*? '"' 
  | '\'' ~'\''*? '\'' ;
AMP : '&amp;' ;
LT : '&lt;' ;
GT : '&gt;' ;
COMMA : ',';
QM : '?';
COLON : ':';
SEMI : ';';
STAR : '*';
EQ : '=';
RP : ')';
LP : '(';
RC : '}';
LC : '{';
RB : ']';
LB : '[';
PIPE : '|';
WS  :  [ \t\r\n\u000C]+ -> skip ;
