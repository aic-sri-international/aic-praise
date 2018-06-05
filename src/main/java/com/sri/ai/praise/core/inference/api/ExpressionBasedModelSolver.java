package com.sri.ai.praise.core.inference.api;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.praise.core.model.classbased.expressionbased.api.ExpressionBasedModel;

public interface ExpressionBasedModelSolver {

	void interrupt();

	ExpressionBasedModel getModel();
	
	Context getContext();

	Expression solve(Expression queryExpression);

}