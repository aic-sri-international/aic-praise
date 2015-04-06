grammar HOGM;

model
    : statements+=statement* EOF
    ;
    
aterm
    : term EOF
    ;
    
statement
    : declaration
    | term SEMICOLON
    ;

declaration
    : sort_decl
    | random_variable_decl
    ;
    
sort_decl // sort Name [ ":" ( number | "Unknown" ) [ ", " constant+ ] ]
    : SORT name=sort_name (COLON size=(INTEGER | UNKNOWN) (COMMA constants+=constant_name)*)? (SEMICOLON)?
    ;

// "random" constantName : SortName
// "random" constantName ":" SortName ('x' SortName)* '->' SortName
random_variable_decl 
    : RANDOM name=constant_name COLON range=sort_name (SEMICOLON)? #propositionalRandomVariableDeclaration
    | RANDOM name=constant_name COLON parameters+=sort_name (X parameters+=sort_name)* MAPPING_RIGHT_ARROW range=sort_name (SEMICOLON)? #relationalRandomVariableDeclaration
    ;
    
term
      // parenthesis, e.g.:(1+2)
    : OPEN_PAREN term CLOSE_PAREN #parentheses
      // function application, e.g.: f(X)
    | function_application #functionApplication
      // not, e.g.: not A and B ---> (not(A)) and B
    | NOT term #not
      // negative, e.g.: 2 * -1 ---> 2 * (-1)
    | SUBTRACT term #unaryMinus // We set the unary minus to higher precedence
      // NOTE:  P)arentheses, E)xponents, ( M)ultiplication, D)ivision ), ( A)ddition, S)ubtraction )
      // see: http://en.wikipedia.org/wiki/Order_of_operations
	  // exponentiation, e.g. 2^3^4 ---> 2^(3^4)
	|  <assoc=right> base=term EXPONENTIATION exponent=term #exponentiation
	  // multiplication or division, e.g.: 2*3/2 ---> 2*(3/2)
	| leftop=term op=(TIMES | DIVIDE) rightop=term #multiplicationOrDivision
	  // addition or subtraction, e.g.: 1-2+3 ---> (1-2)+3
	| leftop=term op=(PLUS | SUBTRACT) rightop=term #additionOrSubtraction
      // comparison operators, e.g.: X = Y, 2 < 3
    | leftop=term op=(LESS_THAN | LESS_THAN_EQUAL | EQUAL | NOT_EQUAL | GREATER_THAN_EQUAL | GREATER_THAN) rightop=term #comparison
      // conjunction, e.g.: A or B and C ---> A or (B and C)
    | leftconj=term AND rightconj=term #conjunction
      // disjunction, e.g.: A => B or C ---> A => (B or C)
    | leftdisj=term OR rightdisj=term #disjunction
      // implication, e.g.: A = B => C = D
    |<assoc=right> antecedent=term IMPLICATION consequent=term #implication
      // biconditional, e.g.: A = B <=> C = D
    |<assoc=right> leftop=term BICONDITIONAL rightop=term #biconditional
      // universal quantification, e.g.: for all X : X != a
    | FOR ALL index=quantifier_index COLON body=term #forAll
      // existential quantification, e.g.: there exists X : X = a
    | THERE EXISTS index=quantifier_index COLON body=term #thereExists
      // condition=term(0) potential=term(1) ---> if condition then potential else 1-potential
    | term term #shorthandConditionedPotential 
      // conditional, e.g.: if X = Y then 1 else 2
    | IF condition=term THEN thenbranch=term ELSE elsebranch=term #conditional
    | symbol #atomicTerm
    ;
    
function_application
    : functor=functor_name OPEN_PAREN ( args+=term (COMMA args+=term)* )? CLOSE_PAREN
    ;
    
quantifier_index
    : indexes+=quantifier_index_term (COMMA indexes+=quantifier_index_term)*
    ;
    
quantifier_index_term
    : function_application #quantifierIndexTermFunctionApplication
    | VARIABLE #quantifierIndexTermVariable
    | variable=VARIABLE IN sort=sort_name #quantifierIndexTermVariableInSort
    ;

sort_name
    : IN_BUILT_SORT_BOOLEAN
    | IN_BUILT_SORT_NUMBER
    | VARIABLE
    ;
       
functor_name
    : VARIABLE
    | constant_name
    ;
        
symbol
    : VARIABLE
    | constant_name
    | constant_number
    ;
    
constant_name
    : X
    | CONSTANT
    | QUOTED_CONSTANT
    ;
    
constant_number
    : INTEGER
    | RATIONAL
    ;

/*
    The lexer tokenizes the input string that the parser is asked to
    parse.  The tokens are all typed. Whitespace
    symbols will be excluded from the resulting token stream.

    Adding new grammar rules
    ------------------------
    Add any terminal symbols in the new grammar rules to the list below.

    Note: Ensure you update the corresponding list in HOGMTerminalSymbols.java
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
IN                      : 'in' ;
IN_BUILT_SORT_BOOLEAN   : 'Boolean' ;
IN_BUILT_SORT_NUMBER    : 'Number' ;
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
LESS_THAN               : '<' ;
LESS_THAN_EQUAL         : '<=' ;
EQUAL                   : '=' ;
NOT_EQUAL               : '!=' ;
GREATER_THAN_EQUAL      : '>=' ;
GREATER_THAN            : '>' ;
// Punctuation
OPEN_PAREN              : '(' ;
CLOSE_PAREN             : ')' ;
SEMICOLON               : ';' ; 
COLON                   : ':' ;
COMMA                   : ',' ;
// Misc
MAPPING_RIGHT_ARROW     : '->' ;

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
    