package com.sri.ai.praise.core.representation.classbased.expressionbased.api;

import java.util.List;
import java.util.function.Predicate;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.Context;

/**
 * Represents a (always marginalization, for now) problem using {@link Expression}.
 * 
 * @author braz
 *
 */
public interface ExpressionBasedProblem {

	/** 
	 * Replaces variable query ("query") by original query in given expression, 
	 * and simplifies it with original model's context.
	 */
	Expression replaceQuerySymbolByQueryExpressionIfNeeded(Expression expression);

	Expression getQuerySymbol();

	List<Expression> getFactorExpressionsIncludingQueryDefinitionIfAny();

	List<Expression> getRandomVariablesExcludingQuerySymbol();

	Expression getQueryExpression();

	boolean modelIsKnownToBeBayesianNetwork();

	boolean getQueryIsCompound();

	Context getContext();

	Predicate<Expression> getIsParameterPredicate();

}