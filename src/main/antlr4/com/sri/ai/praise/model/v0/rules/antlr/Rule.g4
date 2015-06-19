grammar Rule;

model 
    : elements+=model_element* EOF
    ;

aformula
    : formula EOF
    ;

model_element
    : sort_decl
    | propositional_random_variable_decl
    | relational_random_variable_decl
    | root_atomic_rule
    | prolog_rule
    | root_probability_notation_rule
    | root_causal_effect_rule
    | root_conditional_rule
    | root_conjunction_of_rules
    ;

sort_decl // sort symbol [ ":" ( number | "Unknown" ) [ ", " constant+ ] ]
    : SORT name=symbol (':' size=(INTEGER | UNKNOWN) (',' constants+=CONSTANT)*)? ';'
    ;

propositional_random_variable_decl // "random" symbol : symbol
    : RANDOM name=symbol ':' range=symbol ';'
    ;

relational_random_variable_decl // "random" symbol ":" [ ( symbol | symbol "x" )+ ] "->" symbol
    : RANDOM name=symbol ':' parameters+=symbol (X parameters+=symbol)* '->' range=symbol ';'
    ;

root_atomic_rule
    : atomic_rule ';'
    ;

atomic_rule
    : a=formula (p=potential)? 
    ;

prolog_rule
    : (p=potential)? head=formula (':-' rhs=formula)? '.'
    ;

root_probability_notation_rule
    : probability_notation_rule ';'
    ;

probability_notation_rule
    : P '(' formula1=formula '|' formula2=formula ')' '=' p=potential
    ;

root_causal_effect_rule 
    : causal_effect_rule ';'
    ;

causal_effect_rule
    : cause=formula '->' effect=sub_rule
    ;

root_conditional_rule
    : conditional_rule ';'
    ;

conditional_rule
    : IF condition=formula THEN thenrule=conditional_branch (ELSE elserule=conditional_branch)?
    ;

conditional_branch
    : sub_rule
    | potential
    ;

root_conjunction_of_rules
    : conjunction_of_rules ';'
    ;
    
// Note: the first rule must not parse as a formula
conjunction_of_rules
    : initialrule=initial_conj_sub_rule AND subsequentrules+=conj_sub_rule (AND subsequentrules+=conj_sub_rule)*
    ;

initial_conj_sub_rule
    : '(' an_initial_conj_sub_rule ')'
    | an_initial_conj_sub_rule
    ;

an_initial_conj_sub_rule
    : complete_atomic_rule
    | prolog_rule
    | probability_notation_rule
    | causal_effect_rule
    | conditional_rule
    ;

conj_sub_rule
    : '(' a_conj_sub_rule ')'
    | a_conj_sub_rule
    ;

a_conj_sub_rule
    : atomic_rule
    | prolog_rule
    | probability_notation_rule
    | causal_effect_rule
    | conditional_rule
    ;

sub_rule
    : '(' a_sub_rule ')'
    | a_sub_rule
    ;
    
a_sub_rule
    : conditional_rule
    | conjunction_of_rules
    | atomic_rule
    | prolog_rule
    | probability_notation_rule
    | causal_effect_rule
    ;

complete_atomic_rule
    : a=formula p=potential
    ;

formula
    :  // parenthesis, e.g.:(1+2)
     '(' formula ')' #formulaWithParentheses
       // function application, e.g.: f(X)
     | functor=symbol '(' ( args+=formula (',' args+=formula)* )? ')' #formulaFunctionApplication
       // not, e.g.: not A and B -> (not(A)) and B
     | NOT formula #formulaNot
       // X may be same as Y
     | leftop=symbol MAY BE SAME AS rightop=symbol #formulaMayBeSameAs
       // exponentiation, e.g. 2^3^4 -> 2^(3^4)
     |<assoc=right> base=formula '^' exponent=formula #formulaExponentiation
       // multiplication or division, e.g.: 2*3/2 -> 2*(3/2)
     | leftop=formula op=('*' | '/') rightop=formula #formulaMultiplicationOrDivision
       // addition or subtraction, e.g.: 1-2+3 -> (1-2)+3
     | leftop=formula op=('+' | '-') rightop=formula #formulaAdditionOrSubtraction
       // comparison operators, e.g.: X = Y, X != Y
     | leftop=formula op=('=' | '!=') rightop=formula #formulaComparison
      // conjunction, e.g.: A or B and C -> A or (B and C)
     | leftconj=formula AND rightconj=formula #formulaAnd
       // disjunction, e.g.: A => B or C -> A => (B or C)
     | leftdisj=formula OR rightdisj=formula #formulaOr
       // implication, e.g.: A = B => C = D
     |<assoc=right> antecedent=formula IMPLICATION consequent=formula #formulaImplication
       // biconditional, e.g.: A = B <=> C = D
     |<assoc=right> leftop=formula BICONDITIONAL rightop=formula #formulaBiconditional
       // universal quantification, e.g.: for all X : X != a
     | FOR ALL index=formula ':' body=formula #formulaForAll
       // existential quantification, e.g.: there exists X : X = a
     | THERE EXISTS index=formula ':' body=formula #formulaThereExists
       // a symbol
     | symbol #formulaSymbol
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
    : P
    | X
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
MAY                     : 'may' ;
BE                      : 'be' ;
SAME                    : 'same' ;
AS                      : 'as' ;
P                       : 'P' ;
X                       : 'x' ;
// Logic Operators
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
// Misc
COLON                   : ':' ;
COLON_DASH              : ':-';
SEMICOLON               : ';' ;
SINGLE_ARROW            : '->';
VERT_BAR                : '|' ;
COMMA                   : ',' ;
UNDERSCORE              : '_' ;
PERIOD                  : '.' ;

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



