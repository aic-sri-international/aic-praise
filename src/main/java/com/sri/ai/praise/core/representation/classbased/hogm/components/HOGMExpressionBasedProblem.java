package com.sri.ai.praise.core.representation.classbased.hogm.components;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.praise.core.representation.classbased.expressionbased.core.DefaultExpressionBasedProblem;

public class HOGMExpressionBasedProblem extends DefaultExpressionBasedProblem {

	HOGMExpressionBasedModel hogmExpressionBasedModel;
	
	public HOGMExpressionBasedProblem(Expression queryExpression, HOGMExpressionBasedModel model) {
		super(queryExpression, model);
		this.hogmExpressionBasedModel = model;
	}

	public HOGMExpressionBasedModel getHOGMExpressionBasedModel() {
		return hogmExpressionBasedModel;
	}

}
