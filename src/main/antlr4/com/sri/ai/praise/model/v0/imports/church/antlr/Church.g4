/*
The Church programming language (N. Goodman, Mansinghka, Roy, Bonawitz, & Tenenbaum, 2008), 
named in honor of Alonzo Church, is a generalization of Scheme which introduces the notion 
of probabilistic computation to the language (see - https://probmods.org/generative-models.html).

As we are only planning to support a subset of the Church language, we will only
be defining the required subsets of Church/Scheme that we require for the kinds of
Church programs that we intend to support.

Some useful resources related to the Scheme Grammar:

R6RS Lexical Syntax - 
http://www.r6rs.org/final/html/r6rs/r6rs-Z-H-7.html
Scheme R5RS grammar using Antlr 3 - 
http://stackoverflow.com/questions/6344766/antlr-resolving-non-ll-problems-and-syntactic-predicates/6349009#6349009
EBNF of Scheme R5R5 -
http://rose-r5rs.googlecode.com/hg/doc/r5rs-grammar.html

Minimal parse support to be provided is as follows:
----
(define IDENTIFIER VALUE)*   (query BASIC_VALUE)

(the star above is the Kleene star operator)

VALUE is either a BASIC_VALUE or a FUNCTION_DEFINITION.

BASIC_VALUE is one of:
- NUMBER or SYMBOL (a constant)
- (flip)
- (flip NUMBER)   (number needs to be a numeric constant between 0 and 1, inclusive)
- (CONNECTIVE BASIC_VALUE+), where CONNECTIVE is "and", "or", "not", "if"

FUNCTION_DEFINITION is
(mem (lambda (PARAM*) BASIC_VALUE)) 
----
*/

grammar Church;

/*
=============
GRAMMAR RULES
=============
*/

parse
  :  churchQuery EOF
  ;
  
churchQuery
  : '(' 'query' model=churchModelDefinition query=command condition=churchQueryCondition? ')'
  ;
  
churchModelDefinition
  : definition+
  ;

churchQueryCondition
  : '(' 'condition' churchEvidenceCommand ')'
  | churchEvidenceCommand
  ;
  
churchEvidenceCommand
  : specialUniversallyQuantifiedCommand
  | command
  ;
  
specialUniversallyQuantifiedCommand
  : '(' 'forall' universals=specialUniversalFormals  bodyLogic=command ')'
  ;
  
specialUniversalFormals
  : '(' variable+ ')'
  ;

definition
  :  defineChurchMemoization
  |  defineBinding
  |  defineProcedure
  ;

defineChurchMemoization
  :  '(' DEFINE name=variable '(' MEM procedure=expression ')' ')'
  ;

defineBinding
  : '(' DEFINE name=variable binding=expression ')'
  ;

defineProcedure
  :  '(' DEFINE '(' name=variable arguments=defFormals ')' bodyLogic=body ')'
  ;

defFormals
  :  variable* ('.' variable)?
  ;

keyword
  :  identifier
  ;

command
  :  expression
  ;

identifier
  :  syntacticKeyword
  |  variable
  ;

syntacticKeyword
  :  expressionKeyword
  |  DEFINE
  ;  

expressionKeyword
  :  LAMBDA
  |  IF
  |  AND
  |  OR
  |  NOT
  |  MEM
  |  FLIP
  ; 

expression
  :  variable
  |  literal
  |  lambdaExpression
  |  conditional
  |  flip
  |  logicalOperatorExpression
  |  procedureCall
  ;

variable
  :  VARIABLE
  |  ELLIPSIS
  ;

literal
  :  quotation
  |  selfEvaluating
  ;

quotation
  :  '\'' datum
  |  '(' QUOTE datum ')'
  ;

selfEvaluating
  :  bool
  |  number
  |  (CHARACTER | STRING)
  ;

lambdaExpression
  :  '(' LAMBDA formals body ')'
  ;

formals
  :  '(' (variable+ ('.' variable)?)? ')'
  |  variable
  ;

conditional
  :  '(' IF test consequent alternate? ')'
  ;

test 
  :  expression
  ;

consequent  
  :  expression
  ;

alternate 
  :  expression
  ;

flip
  :  '(' FLIP (value=expression)? ')'
  ;

logicalOperatorExpression
  : '(' AND test* ')'
  | '(' OR test* ')'
  | '(' NOT test ')'
  ;

procedureCall
  :  '(' operator operand* ')'
  ;

operator
  :  expression
  ;

operand
  :  expression
  ;

body
  :  (definition)* sequence
  ;

sequence
  :  expression+
  ;

datum
  :  simpleDatum
  |  compoundDatum
  ;

simpleDatum
  :  bool
  |  number
  |  (CHARACTER | STRING)
  |  identifier
  ;

compoundDatum
  :  list
  |  vector
  ;

list
  :  '(' (datum+ ('.' datum)?)? ')'
  |  abbreviation
  ;

abbreviation
  :  abbrevPrefix datum
  ;

abbrevPrefix
  :  '\'' | '`' | ',@' | ','
  ;

vector
  :  '#(' datum* ')'
  ;

number
  :  NUM_2
  |  NUM_8
  |  NUM_10
  |  NUM_16
  ;

bool
  :  TRUE
  |  FALSE
  ;

/*
===========
LEXER RULES
===========
*/

// Scheme syntactic keywords
DEFINE           : 'define';

// Scheme expression keywords
QUOTE            : 'quote';
LAMBDA           : 'lambda';
IF               : 'if';
AND              : 'and';
OR               : 'or';

// Additional Scheme keywords for in-built operators
// (i.e. predefined variables whose values are procedures
//  as opposed to operators defined by syntax (e.g. if))
NOT              : 'not';

// Church expression keywords (i.e. Church primitive)
MEM              : 'mem';
FLIP             : 'flip';

NUM_2  : PREFIX_2 COMPLEX_2;
NUM_8  : PREFIX_8 COMPLEX_8;
NUM_10 : PREFIX_10? COMPLEX_10;
NUM_16 : PREFIX_16 COMPLEX_16;

ELLIPSIS : '...';

VARIABLE 
  :  INITIAL SUBSEQUENT* 
  |  PECULIAR_IDENTIFIER
  ;

STRING : '"' STRING_ELEMENT* '"';

CHARACTER : '#\\' (~(' ' | '\n') | CHARACTER_NAME);

TRUE  : '#' ('t' | 'T');
FALSE : '#' ('f' | 'F');

// to ignore
SPACE   : (' ' | '\t' | '\r' | '\n') -> channel(HIDDEN);
COMMENT
    :   '#|' .*? '|#'    -> channel(HIDDEN) // match anything between #| and |#
    ;
LINE_COMMENT : ';' ~('\r' | '\n')* -> channel(HIDDEN);

// fragments  
fragment INITIAL : LETTER | SPECIAL_INITIAL;
fragment LETTER : 'a'..'z' | 'A'..'Z';
fragment SPECIAL_INITIAL : '!' | '$' | '%' | '&' | '*' | '/' | ':' | '<' | '=' | '>' | '?' | '^' | '_' | '~';
fragment SUBSEQUENT : INITIAL | DIGIT | SPECIAL_SUBSEQUENT;
fragment DIGIT : '0'..'9';
fragment SPECIAL_SUBSEQUENT : '.' | '+' | '-' | '@';
fragment PECULIAR_IDENTIFIER : '+' | '-';
fragment STRING_ELEMENT : ~('"' | '\\') | '\\' ('"' | '\\');
fragment CHARACTER_NAME : 'space' | 'newline';

fragment COMPLEX_2 
  :  REAL_2 ('@' REAL_2)?
  |  REAL_2? SIGN UREAL_2? ('i' | 'I')
  ;

fragment COMPLEX_8 
  :  REAL_8 ('@' REAL_8)?
  |  REAL_8? SIGN UREAL_8? ('i' | 'I')
  ;

fragment COMPLEX_10 
  :  REAL_10 ('@' REAL_10)?
  |  REAL_10? SIGN UREAL_10? ('i' | 'I')
  ;

fragment COMPLEX_16 
  :  REAL_16 ('@' REAL_16)?
  |  REAL_16? SIGN UREAL_16? ('i' | 'I')
  ;

fragment REAL_2 : SIGN? UREAL_2;
fragment REAL_8 : SIGN? UREAL_8;
fragment REAL_10 : SIGN? UREAL_10;
fragment REAL_16 : SIGN? UREAL_16;
fragment UREAL_2 : UINTEGER_2 ('/' UINTEGER_2)?;
fragment UREAL_8 : UINTEGER_8 ('/' UINTEGER_8)?;
fragment UREAL_10 : UINTEGER_10 ('/' UINTEGER_10)? | DECIMAL_10;
fragment UREAL_16 : UINTEGER_16 ('/' UINTEGER_16)?;

fragment DECIMAL_10 
  :  UINTEGER_10 SUFFIX
  |  '.' DIGIT+ '#'* SUFFIX?
  |  DIGIT+ '.' DIGIT* '#'* SUFFIX?
  |  DIGIT+ '#'+ '.' '#'* SUFFIX?
  ;

fragment UINTEGER_2 : DIGIT_2+ '#'*;
fragment UINTEGER_8 : DIGIT_8+ '#'*;
fragment UINTEGER_10 : DIGIT+ '#'*;
fragment UINTEGER_16 : DIGIT_16+ '#'*;
fragment PREFIX_2 : RADIX_2 EXACTNESS? | EXACTNESS RADIX_2;
fragment PREFIX_8 : RADIX_8 EXACTNESS? | EXACTNESS RADIX_8;
fragment PREFIX_10 : RADIX_10 EXACTNESS? | EXACTNESS RADIX_10;
fragment PREFIX_16 : RADIX_16 EXACTNESS? | EXACTNESS RADIX_16;
fragment SUFFIX : EXPONENT_MARKER SIGN? DIGIT+;
fragment EXPONENT_MARKER : 'e' | 's' | 'f' | 'd' | 'l' | 'E' | 'S' | 'F' | 'D' | 'L';
fragment SIGN : '+' | '-';
fragment EXACTNESS : '#' ('i' | 'e' | 'I' | 'E');
fragment RADIX_2 : '#' ('b' | 'B');
fragment RADIX_8 : '#' ('o' | 'O');
fragment RADIX_10 : '#' ('d' | 'D');
fragment RADIX_16 : '#' ('x' | 'X');
fragment DIGIT_2 : '0' | '1';
fragment DIGIT_8 : '0'..'7';
fragment DIGIT_16 : DIGIT | 'a'..'f' | 'A'..'F';
