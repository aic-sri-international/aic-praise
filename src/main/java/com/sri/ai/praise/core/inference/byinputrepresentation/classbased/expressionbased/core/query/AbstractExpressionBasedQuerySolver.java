package com.sri.ai.praise.core.inference.byinputrepresentation.classbased.expressionbased.core.query;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.praise.core.inference.byinputrepresentation.classbased.expressionbased.api.query.ExpressionBasedQuerySolver;
import com.sri.ai.praise.core.representation.classbased.expressionbased.api.ExpressionBasedQuery;

public abstract class AbstractExpressionBasedQuerySolver implements ExpressionBasedQuerySolver {

	protected abstract Expression computeNormalizedMarginal(ExpressionBasedQuery query);

	@Override
	public Expression solve(ExpressionBasedQuery query) {
		Expression normalizedMarginal = computeNormalizedMarginal(query);
		Expression result = query.replaceQuerySymbolByQueryExpressionIfNeeded(normalizedMarginal);
		return result;
	}

}