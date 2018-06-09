package com.sri.ai.praise.core.inference.byinputrepresentation.classbased.expressionbased.api.query;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.praise.core.representation.classbased.expressionbased.api.ExpressionBasedQuery;

/**
 * A probabilistic solver for an {@link ExpressionBasedQuery}.
 * 
 * @author braz
 */
public interface ExpressionBasedQuerySolver {

	void interrupt();

	Expression solve(ExpressionBasedQuery query);

}