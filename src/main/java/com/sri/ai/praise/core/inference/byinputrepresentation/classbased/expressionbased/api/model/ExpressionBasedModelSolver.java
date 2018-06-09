package com.sri.ai.praise.core.inference.byinputrepresentation.classbased.expressionbased.api.model;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.praise.core.representation.classbased.expressionbased.api.ExpressionBasedModel;

public interface ExpressionBasedModelSolver {

	void interrupt();

	ExpressionBasedModel getModel();
	
	Context getContext();

	Expression solve(Expression queryExpression);

}