package com.sri.ai.praise.core.inference.byinputrepresentation.classbased.expressionbased.core;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.praise.core.inference.byinputrepresentation.classbased.expressionbased.api.ExpressionBasedSolver;
import com.sri.ai.praise.core.representation.classbased.expressionbased.api.ExpressionBasedProblem;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.expression.api.ExpressionFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.expression.core.DefaultExpressionFactor;

public abstract class AbstractExpressionBasedSolver implements ExpressionBasedSolver {

	protected abstract Expression solveForQuerySymbolDefinedByExpressionBasedProblem(ExpressionBasedProblem problem);

	@Override
	public Expression solve(ExpressionBasedProblem problem) {
		Expression normalizedMarginal = solveForQuerySymbolDefinedByExpressionBasedProblem(problem);
		ExpressionFactor expressionFactor = (ExpressionFactor)normalizedMarginal;
		ExpressionFactor result = new DefaultExpressionFactor(problem.replaceQuerySymbolByQueryExpressionIfNeeded(normalizedMarginal), expressionFactor.getContext());
		result.setExplanation(expressionFactor.getExplanation());
		return result;
	}
	
	@Override
	public String toString() {
		return getClass().getSimpleName();
	}

}