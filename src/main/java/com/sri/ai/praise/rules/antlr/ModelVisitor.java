package com.sri.ai.praise.rules.antlr;

import java.util.ArrayList;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.Map;

import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.misc.NotNull;
import org.apache.commons.lang3.StringEscapeUtils;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.grinder.library.boole.And;
import com.sri.ai.grinder.library.boole.ForAll;
import com.sri.ai.grinder.library.boole.Not;
import com.sri.ai.grinder.library.boole.Or;
import com.sri.ai.grinder.library.boole.ThereExists;
import com.sri.ai.grinder.library.set.extensional.ExtensionalSet;
import com.sri.ai.grinder.library.set.tuple.Tuple;
import com.sri.ai.praise.model.RandomVariableDeclaration;
import com.sri.ai.praise.model.SortDeclaration;
import com.sri.ai.praise.rules.RuleConverter;

public class ModelVisitor extends RuleBaseVisitor<Expression> {
	
	// Note track bracketed expressions based on identity to ensure no accidental overal by value.
	private Map<Expression, Expression> parenthesizedExpressions = new IdentityHashMap<Expression, Expression>(); 

	// elements+=model_element* EOF
	@Override
	public Expression visitModel(RuleParser.ModelContext ctx) {
		// The syntax tree of a model is model(<elements>), where its arguments
		// "elements"
		// are the syntax trees of the declarations and rules found.
		Expression result = Expressions
				.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("model", expressions(ctx.elements));
		return result;
	}
	
	// aformula : formula EOF ;
	@Override
	public Expression visitAformula(RuleParser.AformulaContext ctx) {
		Expression result = visit(ctx.formula());
		return result;
	}

	// SORT name=atomic_symbol (':' size=(INTEGER | UNKNOWN) (','
	// constants+=CONSTANT)*)? ';'
	@Override
	public Expression visitSort_decl(RuleParser.Sort_declContext ctx) {

		Expression name = newSymbol(ctx.name.getText());
		Expression size = SortDeclaration.UNKNOWN_SIZE;
		if (ctx.size != null) {
			size = newSymbol(ctx.size.getText());
		}
		List<Expression> constants = new ArrayList<Expression>();
		for (Token constant : ctx.constants) {
			constants.add(newSymbol(constant.getText()));
		}

		Expression result = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(
				SortDeclaration.FUNCTOR_SORT_DECLARATION, name, size,
				ExtensionalSet.makeUniSet(constants));
		return result;
	}

	// RANDOM name=symbol ':' range=symbol ';'
	@Override 
	public Expression visitPropositional_random_variable_decl(@NotNull RuleParser.Propositional_random_variable_declContext ctx) { 
		Expression name  = newSymbol(ctx.name.getText());
		Expression arity = Expressions.ZERO;
		Expression range = newSymbol(ctx.range.getText());

		List<Expression> declarationArgs = new ArrayList<Expression>();
		declarationArgs.add(name);
		declarationArgs.add(arity);
		declarationArgs.add(range);

		Expression result = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(
				RandomVariableDeclaration.FUNCTOR_RANDOM_VARIABLE_DECLARATION,
				declarationArgs.toArray());

		return result;
	}
	
	// RANDOM name=symbol ':' parameters+=symbol (X parameters+=symbol)* '->' range=symbol ';'
	@Override
	public Expression visitRelational_random_variable_decl(@NotNull RuleParser.Relational_random_variable_declContext ctx) {
		Expression name = newSymbol(ctx.name.getText());
		List<Expression> parameters = expressionsList(ctx.parameters);
		Expression arity = Expressions.makeSymbol(parameters.size());
		Expression range = newSymbol(ctx.range.getText());

		List<Expression> declarationArgs = new ArrayList<Expression>();
		declarationArgs.add(name);
		declarationArgs.add(arity);
		declarationArgs.addAll(parameters);
		declarationArgs.add(range);

		Expression result = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(
				RandomVariableDeclaration.FUNCTOR_RANDOM_VARIABLE_DECLARATION,
				declarationArgs.toArray());

		return result;
	}
	
	// root_atomic_rule : atomic_rule ';' ;
	@Override
	public Expression visitRoot_atomic_rule(
			RuleParser.Root_atomic_ruleContext ctx) {
		Expression result = visit(ctx.atomic_rule());
		return result;
	}

	// a=formula (p=potential)?
	@Override
	public Expression visitAtomic_rule(RuleParser.Atomic_ruleContext ctx) {
		Expression formula = visit(ctx.a);
		Expression potential = Expressions.ONE;
		if (ctx.p != null) {
			potential = visit(ctx.p);
		}

		Expression result = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(RuleConverter.FUNCTOR_ATOMIC_RULE,
				formula, potential);

		return result;
	}
	
	// (p=potential)? head=formula (':-' rhs=formula)? '.'
	@Override
	public Expression visitProlog_rule(RuleParser.Prolog_ruleContext ctx) {
		Expression potential = Expressions.ONE;
		if (ctx.p != null) {
			potential = visit(ctx.p);
		}
		Expression head = visit(ctx.head);
		Expression rhs  = null;
		if (ctx.rhs != null) {
			rhs = visit(ctx.rhs);
		}
		
		Expression result = null;
		if (rhs != null) {
			result = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(RuleConverter.FUNCTOR_PROLOG_RULE, potential, head, rhs);
		}
		else {
			result = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(RuleConverter.FUNCTOR_PROLOG_RULE, potential, head);
		}
		return result;
	}
	
	// root_probability_notation_rule : probability_notation_rule ';' ;
	@Override
	public Expression visitRoot_probability_notation_rule(
			RuleParser.Root_probability_notation_ruleContext ctx) {
		Expression result = visit(ctx.probability_notation_rule());
		return result;
	}

	// P '(' formula1=formula '|' formula2=formula ')' '=' p=potential
	@Override
	public Expression visitProbability_notation_rule(
			RuleParser.Probability_notation_ruleContext ctx) {
		Expression formula1  = visit(ctx.formula1);
		Expression formula2  = visit(ctx.formula2);
		Expression potential = visit(ctx.p);
		
		Expression result = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(RuleConverter.FUNCTOR_STANDARD_PROB_RULE, formula1, formula2, potential);
		return result;
	}

	// root_causal_effect_rule : causal_effect_rule ';' ;
	@Override
	public Expression visitRoot_causal_effect_rule(
			RuleParser.Root_causal_effect_ruleContext ctx) {
		Expression result = visit(ctx.causal_effect_rule());
		return result;
	}

	// cause=formula '->' effect=sub_rule
	@Override
	public Expression visitCausal_effect_rule(
			RuleParser.Causal_effect_ruleContext ctx) {
		Expression cause  = visit(ctx.cause);
		Expression effect = visit(ctx.effect);
		
		Expression result = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(RuleConverter.FUNCTOR_CAUSAL_RULE, cause, effect);
		return result;
	}
	
	// root_conditional_rule: conditional_rule ';' ;
	@Override
	public Expression visitRoot_conditional_rule(
			RuleParser.Root_conditional_ruleContext ctx) {
		Expression result = visit(ctx.conditional_rule());
		return result;
	}
	
	// IF condition=formula THEN thenrule=conditional_sub_rule (ELSE elserule=conditional_sub_rule)?
	@Override
	public Expression visitConditional_rule(
			RuleParser.Conditional_ruleContext ctx) {
		Expression condition = visit(ctx.condition);
		Expression thenRule  = visit(ctx.thenrule);
		Expression elseRule  = null;
		if (ctx.elserule != null) {
			elseRule = visit(ctx.elserule);
		}
		
		Expression result = null;
		if (elseRule != null) {
			result = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(RuleConverter.FUNCTOR_CONDITIONAL_RULE, condition, thenRule, elseRule);
		}
		else {
			result = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(RuleConverter.FUNCTOR_CONDITIONAL_RULE, condition, thenRule);
		}
		
		return result;
	}
	
	// root_conjunction_of_rules : conjunction_of_rules ';' ;
	@Override
	public Expression visitRoot_conjunction_of_rules(
			RuleParser.Root_conjunction_of_rulesContext ctx) {
		Expression result = visit(ctx.conjunction_of_rules());
		return result;
	}

	// subrules+=sub_rule AND subrules+=sub_rule (AND subrules+=sub_rule)?
	@Override
	public Expression visitConjunction_of_rules(
			RuleParser.Conjunction_of_rulesContext ctx) {
		List<Expression> rules = new ArrayList<Expression>();
		rules.add(visit(ctx.initialrule));
		rules.addAll(expressionsList(ctx.subsequentrules));
		Expression result = Tuple.make(rules.toArray());
		return result;
	}
	
	// initial_conj_sub_rule: '(' an_initial_conj_sub_rule ')' | an_initial_conj_sub_rule ;
	@Override
	public Expression visitInitial_conj_sub_rule(RuleParser.Initial_conj_sub_ruleContext ctx) {
		Expression result = visit(ctx.an_initial_conj_sub_rule());
		return result;
	}
	
	// conj_sub_rule : '(' a_conj_sub_rule ')' | a_conj_sub_rule ;
	@Override
	public Expression visitConj_sub_rule(RuleParser.Conj_sub_ruleContext ctx) {
		Expression result = visit(ctx.a_conj_sub_rule());
		return result;
	}
	
	// sub_rule: '(' a_sub_rule ')' | a_sub_rule ;
	@Override
	public Expression visitSub_rule(RuleParser.Sub_ruleContext ctx) {
		Expression result = visit(ctx.a_sub_rule());
		return result;
	}
	
	// a=formula p=potential
	@Override
	public Expression visitComplete_atomic_rule(RuleParser.Complete_atomic_ruleContext ctx) {
		Expression formula   = visit(ctx.a);
		Expression potential = visit(ctx.p);

		Expression result = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(RuleConverter.FUNCTOR_ATOMIC_RULE, formula, potential);

		return result;
	}

	// parenthesis, e.g.:(1+2)
	// '(' formula ')' #formulaWithParentheses
	@Override
	public Expression visitFormulaWithParentheses(
			RuleParser.FormulaWithParenthesesContext ctx) {
		Expression result = visit(ctx.formula());
		
		// Keep track of explicitly bracketed expressions
		// so that the are not flattened as part of the 
		// possiblyFlatten()
		// call for some expressions, e.g.: 1 + 2 + 3.
		parenthesizedExpressions.put(result, result);
				
		return result;
	}

	// function application, e.g.: f(X)
    // functor=symbol '(' ( args+=formula (',' args+=formula)* )? ')' #formulaFunctionApplication
	@Override
	public Expression visitFormulaFunctionApplication(
			RuleParser.FormulaFunctionApplicationContext ctx) {
		Expression result = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(visit(ctx.functor), expressions(ctx.args));
		
		return result;
	}

	// not, e.g.: not A and B -> (not(A)) and B
    // NOT formula #formulaNot
	@Override
	public Expression visitFormulaNot(RuleParser.FormulaNotContext ctx) {
		Expression result = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(Not.FUNCTOR, visit(ctx.formula()));
		return result;
	}

	// X may be same as Y
    // leftop=symbol MAY BE SAME AS rightop=symbol #formulaMayBeSameAs
	@Override
	public Expression visitFormulaMayBeSameAs(
			RuleParser.FormulaMayBeSameAsContext ctx) {
		Expression result = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(RuleConverter.FUNCTOR_MAY_BE_SAME_AS, visit(ctx.leftop), visit(ctx.rightop));
		return result;
	}

	// exponentiation, e.g. 2^3^4 -> 2^(3^4)
    // base=formula '^'<assoc=right> exponent=formula #formulaExponentiation
	@Override
	public Expression visitFormulaExponentiation(
			RuleParser.FormulaExponentiationContext ctx) {
		Expression result = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(FunctorConstants.EXPONENTIATION, visit(ctx.base), visit(ctx.exponent));
		return result;
	}

	// multiplication or division, e.g.: 2*3/2 -> 2*(3/2)
    // leftop=formula op=('*' | '/') rightop=formula #formulaMultiplicationOrDivision
	@Override
	public Expression visitFormulaMultiplicationOrDivision(
			RuleParser.FormulaMultiplicationOrDivisionContext ctx) {
		Expression result = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(ctx.op.getText(), visit(ctx.leftop), visit(ctx.rightop));
		result = possiblyFlatten(result);
		return result;
	}

	// addition or subtraction, e.g.: 1-2+3 -> (1-2)+3
    // leftop=formula op=('+' | '-') rightop=formula #formulaAdditionOrSubtraction
	@Override
	public Expression visitFormulaAdditionOrSubtraction(
			RuleParser.FormulaAdditionOrSubtractionContext ctx) {
		Expression result = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(ctx.op.getText(), visit(ctx.leftop), visit(ctx.rightop));
		result = possiblyFlatten(result);
		return result;
	}

	// comparison operators, e.g.: X = Y, X != Y
    // leftop=formula op=('=' | '!=') rightop=formula #formulaComparison
	@Override
	public Expression visitFormulaComparison(
			RuleParser.FormulaComparisonContext ctx) {
		Expression result = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(ctx.op.getText(), visit(ctx.leftop), visit(ctx.rightop));
		result = possiblyFlatten(result);
		return result;
	}

	// conjunction, e.g.: A or B and C -> A or (B and C)
    // leftconj=formula AND rightconj=formula #formulaAnd
	@Override
	public Expression visitFormulaAnd(RuleParser.FormulaAndContext ctx) {
		Expression result = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(And.FUNCTOR, visit(ctx.leftconj), visit(ctx.rightconj));
		result = possiblyFlatten(result);
		return result;
	}

	// disjunction, e.g.: A => B or C -> A => (B or C)
    // leftdisj=formula OR rightdisj=formula #formulaOr
	@Override
	public Expression visitFormulaOr(RuleParser.FormulaOrContext ctx) {
		Expression result = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(Or.FUNCTOR, visit(ctx.leftdisj), visit(ctx.rightdisj));
		result = possiblyFlatten(result);
		return result;
	}

	// implication, e.g.: A = B => C = D
    // antecedent=formula IMPLICATION<assoc=right> consequent=formula #formulaImplication
	@Override
	public Expression visitFormulaImplication(
			RuleParser.FormulaImplicationContext ctx) {
		Expression result = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(FunctorConstants.IMPLICATION, visit(ctx.antecedent), visit(ctx.consequent));
		return result;
	}

	// biconditional, e.g.: A = B <=> C = D
    // leftop=formula BICONDITIONAL<assoc=right> rightop=formula #formulaBiconditional
	@Override
	public Expression visitFormulaBiconditional(
			RuleParser.FormulaBiconditionalContext ctx) {
		Expression result = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(FunctorConstants.EQUIVALENCE, visit(ctx.leftop), visit(ctx.rightop));
		return result;
	}

	// universal quantification, e.g.: for all X : X != a
    // FOR ALL index=formula ':' body=formula #formulaForAll
	@Override
	public Expression visitFormulaForAll(RuleParser.FormulaForAllContext ctx) {
		Expression result = ForAll.make(visit(ctx.index), visit(ctx.body));
		return result;
	}

	// existential quantification, e.g.: there exists X : X = a
    // THERE EXISTS index=formula ':' body=formula #formulaThereExists
	@Override
	public Expression visitFormulaThereExists(
			RuleParser.FormulaThereExistsContext ctx) {
		Expression result = ThereExists.make(visit(ctx.index), visit(ctx.body));
		return result;
	}
	
	// a symbol
    // symbol #formulaSymbol
	@Override
	public Expression visitFormulaSymbol(RuleParser.FormulaSymbolContext ctx) {
		Expression result = visit(ctx.symbol());
		return result;
	}

	// parenthesis, e.g.:(1+2)
    // '(' potential ')' #potentialWithParentheses
	@Override
	public Expression visitPotentialWithParentheses(
			RuleParser.PotentialWithParenthesesContext ctx) {
		Expression result = visit(ctx.potential());
		
		// Keep track of explicitly bracketed expressions
		// so that the are not flattened as part of the 
		// possiblyFlatten()
		// call for some expressions, e.g.: 1 + 2 + 3.
		parenthesizedExpressions.put(result, result);
		return result;
	}

	// exponentiation, e.g. 2^3^4 -> 2^(3^4)
    // base=potential '^'<assoc=right> exponent=potential #potentialExponentiation
	@Override
	public Expression visitPotentialExponentiation(
			RuleParser.PotentialExponentiationContext ctx) {
		Expression result = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(FunctorConstants.EXPONENTIATION, visit(ctx.base), visit(ctx.exponent));
		return result;
	}

	// multiplication or division, e.g.: 2*3/2 -> 2*(3/2)
    // leftop=potential op=('*' | '/') rightop=potential #potentialMultiplicationOrDivision
	@Override
	public Expression visitPotentialMultiplicationOrDivision(
			RuleParser.PotentialMultiplicationOrDivisionContext ctx) {
		Expression result = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(ctx.op.getText(), visit(ctx.leftop), visit(ctx.rightop));
		result = possiblyFlatten(result);
		return result;
	}

	// addition or subtraction, e.g.: 1-2+3 -> (1-2)+3
    // leftop=potential op=('+' | '-') rightop=potential #potentialAdditionOrSubtraction
	@Override
	public Expression visitPotentialAdditionOrSubtraction(
			RuleParser.PotentialAdditionOrSubtractionContext ctx) {
		Expression result = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(ctx.op.getText(), visit(ctx.leftop), visit(ctx.rightop));
		result = possiblyFlatten(result);
		return result;
	}
	
	// i.e. a constant value between [0-1]
	// atomic_potential #potentialValue
	@Override
	public Expression visitPotentialValue(RuleParser.PotentialValueContext ctx) {
		Expression result = visit(ctx.atomic_potential());
		return result;
	}

	// atomic_potential : RATIONAL;
	@Override
	public Expression visitAtomic_potential(
			RuleParser.Atomic_potentialContext ctx) {
		Expression result = newSymbol(ctx.getText());
		return result;
	}

	@Override
	public Expression visitSymbol(RuleParser.SymbolContext ctx) {
		Expression result = newSymbol(ctx.getText());
		return result;
	}

	//
	// PROTECTED
	//
	protected Expression newSymbol(String text) {
		// Remove quotes from around quoted strings
		if ((text.startsWith("'") && text.endsWith("'"))
				|| (text.startsWith("\"") && text.endsWith("\""))) {
			text = text.substring(1, text.length() - 1);
		}

		// Ensure escapes are applied.
		text = StringEscapeUtils.unescapeJava(text);

		text = new String(text);

		Expression result = Expressions.makeSymbol(text);
		return result;
	}

	protected Object[] expressions(
			List<? extends ParserRuleContext> exprContexts) {
		List<Expression> result = new ArrayList<Expression>();
		for (ParserRuleContext exprContext : exprContexts) {
			result.add(visit(exprContext));
		}
		return result.toArray();
	}

	protected List<Expression> symbolList(List<Token> tokens) {
		List<Expression> result = new ArrayList<Expression>();
		for (Token token : tokens) {
			result.add(newSymbol(token.getText()));
		}
		return result;
	}

	protected List<Expression> expressionsList(
			List<? extends ParserRuleContext> exprContexts) {
		List<Expression> result = new ArrayList<Expression>();
		for (ParserRuleContext exprContext : exprContexts) {
			result.add(visit(exprContext));
		}
		return result;
	}
	
	protected Expression possiblyFlatten(Expression expression) {
		Expression result = expression;
		
		Object functor = expression.getFunctor();
		if (functor != null) {
			if (functor.equals(FunctorConstants.TIMES) || functor.equals(FunctorConstants.PLUS) || 
			    functor.equals(FunctorConstants.EQUAL) ||
			    functor.equals(FunctorConstants.AND) || functor.equals(FunctorConstants.OR)) {
				List<Expression> args = new ArrayList<Expression>();
				for (Expression arg : expression.getArguments()) {
					if (arg.getFunctor() != null && functor.equals(arg.getFunctor()) && !parenthesizedExpressions.containsKey(arg)) {
						args.addAll(arg.getArguments());
					}
					else {
						args.add(arg);
					}
				}
				result = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(functor, args.toArray());
			}
		}
		
		// Clear in order manage memory
		parenthesizedExpressions.clear();
		
		return result;
	}
}
