package com.sri.ai.praise.core.inference.api;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.praise.core.model.classbased.expressionbased.api.ExpressionBasedQuery;

/**
 * A probabilistic solver for an {@link ExpressionBasedQuery}.
 * 
 * @author braz
 */
public interface ExpressionBasedQuerySolver {

	void interrupt();

	Expression solve(ExpressionBasedQuery query);

}