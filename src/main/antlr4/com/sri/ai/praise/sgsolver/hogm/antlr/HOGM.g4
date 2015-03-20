grammar HOGM;

model
    : statements+=statement* EOF
    ;
    
statement
    : declaration
    | rule_definition
    ;

declaration
    : sort_decl
    | random_variable_decl
    ;
    
sort_decl // sort symbol [ ":" ( number | "Unknown" ) [ ", " constant+ ] ]
    : SORT name=symbol (':' size=(INTEGER | UNKNOWN) (',' constants+=CONSTANT)*)?
    ;

// "random" symbol : symbol
// "random" symbol ":" [ ( symbol | symbol "x" )+ ] "->" symbol
random_variable_decl 
    : RANDOM name=symbol ':' range=symbol
    | RANDOM name=symbol ':' parameters+=symbol (X parameters+=symbol)* '->' range=symbol
    ;
    
rule_definition
    : IF formula THEN rule_definition ELSE rule_definition #conditionalRule
    | f=formula (p=potential)? #formulaWithOptionalPotentialRule
    ;
    
formula
    : '(' formula ')' #termWithParentheses
    | functor=symbol '(' ( args+=formula (',' args+=formula)* )? ')' #formulaFunctionApplication
    | NOT formula #notFunctionApplication
    | leftconj=formula AND rightconj=formula #andFunctionApplication
    | leftconj=formula OR rightconj=formula #orFunctionApplication
    | IF condition=formula THEN thenbranch=formula ELSE elsebranch=formula #ifThenElseFunctionApplication
    | quantified_expression #termQuantifiedExpression
    | constant #termConstant
    | variable #termVariable
    ;
    
quantified_expression
    : FOR ALL index=variable ':' body=formula #forAll
    | THERE EXISTS index=variable ':' body=formula #thereExists
    ;
    
constant
    : CONSTANT
    ;
    
variable
    : VARIABLE
    ;

potential
    :  // parenthesis, e.g.:(1+2)
     '(' potential ')' #potentialWithParentheses
       // exponentiation, e.g. 2^3^4 -> 2^(3^4)
     |<assoc=right> base=potential '^' exponent=potential #potentialExponentiation
       // multiplication or division, e.g.: 2*3/2 -> 2*(3/2)
     | leftop=potential op=('*' | '/') rightop=potential #potentialMultiplicationOrDivision
       // addition or subtraction, e.g.: 1-2+3 -> (1-2)+3
     | leftop=potential op=('+' | '-') rightop=potential #potentialAdditionOrSubtraction
       // i.e. a constant value between [0-1]
     | atomic_potential #potentialValue
    ;

atomic_potential
    : INTEGER
    | RATIONAL
    ;

symbol
    : X
    | CONSTANT
    | QUOTED_CONSTANT
    | VARIABLE
    ;

/*
    The lexer tokenizes the input string that the parser is asked to
    parse.  The tokens are all typed. Whitespace
    symbols will be excluded from the resulting token stream.

    Adding new grammar rules
    ------------------------
    Add any terminal symbols in the new grammar rules to the list below.

    Note: Ensure you update the corresponding list in RuleTerminalSymbols.java
          with any changes made.
    
*/
// Keywords
NOT                     : 'not' ;
AND                     : 'and' ;
OR                      : 'or' ;
FOR                     : 'for' ;
ALL                     : 'all' ;
THERE                   : 'there' ;
EXISTS                  : 'exists' ;
IF                      : 'if' ;
THEN                    : 'then' ;
ELSE                    : 'else' ;
SORT                    : 'sort' ;
UNKNOWN                 : 'Unknown';
RANDOM                  : 'random' ;
X                       : 'x' ;
// Logic Operators
IMPLICATION             : '=>' ;
BICONDITIONAL           : '<=>' ;
// Arithmetic
EXPONENTIATION          : '^' ;
DIVIDE                  : '/' ;
TIMES                   : '*' ;
PLUS                    : '+' ;
SUBTRACT                : '-' ;
// Comparison
EQUAL                   : '=' ;
NOT_EQUAL               : '!=' ;
// Brackets
OPEN_PAREN              : '(' ;
CLOSE_PAREN             : ')' ;

INTEGER 
    : ('0'..'9')+
    ;

RATIONAL 
    : ('0' | '1'..'9' '0'..'9'*)
    | ('0'..'9')+ '.' ('0'..'9')* EXPONENT? FLOAT_TYPE_SUFFIX?
    | '.' ('0'..'9')+ EXPONENT? FLOAT_TYPE_SUFFIX?
    | ('0'..'9')+ EXPONENT FLOAT_TYPE_SUFFIX?
    | ('0'..'9')+ FLOAT_TYPE_SUFFIX
    | ('0x' | '0X') (HEX_DIGIT )*
      ('.' (HEX_DIGIT)*)?
      ( 'p' | 'P' )
      ( '+' | '-' )?
      ( '0' .. '9' )+
      FLOAT_TYPE_SUFFIX?
    ;

CONSTANT 
    : [a-z] ([a-z] | [A-Z] | [0-9] | '_')* ('\'')*
    ;

QUOTED_CONSTANT
    : '"'  (ESCAPE_SEQUENCE | ~('\\' | '"' ) )* '"'
    | '\'' (ESCAPE_SEQUENCE | ~('\\' | '\'') )* '\''
    ;

VARIABLE 
    : [A-Z] ([a-z] | [A-Z] | [0-9] | '_')* ('\'')*
    ;

fragment
EXPONENT : ('e'|'E') ('+'|'-')? ('0'..'9')+
         ;

fragment
FLOAT_TYPE_SUFFIX : ('f'|'F'|'d'|'D')
                  ;

fragment
ESCAPE_SEQUENCE
    :   '\\' ('b'|'t'|'n'|'f'|'r'|'"'|'\''|'\\')
    |   UNICODE_ESCAPE
    |   OCTAL_ESCAPE
    ;

fragment
UNICODE_ESCAPE
    :   '\\' 'u' HEX_DIGIT HEX_DIGIT HEX_DIGIT HEX_DIGIT
    ;

fragment
OCTAL_ESCAPE
    :   '\\' ('0'..'3') ('0'..'7') ('0'..'7')
    |   '\\' ('0'..'7') ('0'..'7')
    |   '\\' ('0'..'7')
    ;

fragment
HEX_DIGIT : ('0'..'9'|'a'..'f'|'A'..'F')
          ;

COMMENT
    :   '/*' .*? '*/'    -> channel(HIDDEN) // match anything between /* and */
    ;

LINE_COMMENT
    : '//' ~[\r\n]* '\r'? ('\n' | EOF) -> channel(HIDDEN)
    ;

WS  :   [ \t\r\n]+ -> skip 
    ; // Define whitespace rule, toss it out
    