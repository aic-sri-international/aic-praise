package com.sri.ai.praise.core.inference.api;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.praise.core.model.classbased.expressionbased.ExpressionBasedModel;

public interface ExpressionBasedSolver {

	void interrupt();

	ExpressionBasedModel getModel();
	
	Context getContext();

	Expression solve(Expression queryExpression);

}