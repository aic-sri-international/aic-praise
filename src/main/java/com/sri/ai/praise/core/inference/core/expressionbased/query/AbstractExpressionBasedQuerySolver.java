package com.sri.ai.praise.core.inference.core.expressionbased.query;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.praise.core.inference.api.ExpressionBasedQuerySolver;
import com.sri.ai.praise.core.model.classbased.expressionbased.api.ExpressionBasedQuery;

public abstract class AbstractExpressionBasedQuerySolver implements ExpressionBasedQuerySolver{

	protected abstract Expression computeNormalizedMarginal(ExpressionBasedQuery query);

	@Override
	public Expression solve(ExpressionBasedQuery query) {
		Expression normalizedMarginal = computeNormalizedMarginal(query);
		Expression result = query.replaceQuerySymbolByQueryExpressionIfNeeded(normalizedMarginal);
		return result;
	}

	@Override
	public void interrupt() {
		System.err.println("interrupt() not yet implemented for " + getClass().getSimpleName());
	}

}