package com.sri.ai.praise.core.inference.api;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.Context;

public interface ExpressionBasedSolver {

	void interrupt();

	Context getContext();

	Expression solve(Expression queryExpression);

}