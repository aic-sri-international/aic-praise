package com.sri.ai.praise.core.model.classbased.expressionbased.api;

import java.util.List;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.Context;

public interface ExpressionBasedQuery {

	/** 
	 * Replaces variable query ("query") by original query in given expression, 
	 * and simplifies it with original model's context.
	 */
	Expression replaceQuerySymbolByQueryExpressionIfNeeded(Expression expression);

	Expression getQuerySymbol();

	List<Expression> getFactorExpressionsIncludingQueryDefinitionIfAny();

	List<Expression> getRandomVariablesExcludingQuerySymbol();

	Expression getQueryExpression();

	boolean isKnownToBeBayesianNetwork();

	boolean getQueryIsCompound();

	Context getContext();

}