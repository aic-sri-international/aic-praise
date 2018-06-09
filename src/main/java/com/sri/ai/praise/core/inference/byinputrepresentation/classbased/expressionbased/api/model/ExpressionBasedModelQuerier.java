package com.sri.ai.praise.core.inference.byinputrepresentation.classbased.expressionbased.api.model;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.praise.core.representation.classbased.expressionbased.api.ExpressionBasedModel;

public interface ExpressionBasedModelQuerier {

	void interrupt();

	ExpressionBasedModel getModel();
	
	Expression answer(Expression queryExpression);

}