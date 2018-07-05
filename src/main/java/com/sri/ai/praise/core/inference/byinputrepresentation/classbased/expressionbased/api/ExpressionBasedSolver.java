package com.sri.ai.praise.core.inference.byinputrepresentation.classbased.expressionbased.api;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.praise.core.representation.classbased.expressionbased.api.ExpressionBasedModel;
import com.sri.ai.praise.core.representation.classbased.expressionbased.api.ExpressionBasedProblem;
import com.sri.ai.praise.core.representation.classbased.expressionbased.core.DefaultExpressionBasedProblem;

/**
 * A probabilistic solver for an {@link ExpressionBasedProblem}.
 * 
 * @author braz
 */
public interface ExpressionBasedSolver {

	void interrupt();

	Expression solve(ExpressionBasedProblem problem);

	default Expression solve(Expression queryExpression, ExpressionBasedModel expressionBasedModel) {
		ExpressionBasedProblem problem = new DefaultExpressionBasedProblem(queryExpression, expressionBasedModel);
		Expression result = solve(problem);
		return result;
	}
}