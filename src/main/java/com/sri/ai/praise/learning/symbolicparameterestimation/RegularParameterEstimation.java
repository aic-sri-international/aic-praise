package com.sri.ai.praise.learning.symbolicparameterestimation;

import java.util.List;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.praise.core.representation.classbased.expressionbased.api.ExpressionBasedModel;

public class RegularParameterEstimation {
	
	public ExpressionBasedModel expressionBasedModel;
	public List<Expression> evidences;
	
	public RegularParameterEstimation(ExpressionBasedModel expressionBasedModel, List<Expression> evidences) {
		this.expressionBasedModel = expressionBasedModel;
		this.evidences = evidences;
	}
	
	public int countOccurencesGivenParameter(Expression parameter) {
		int result = 0;
		Context firstContext = expressionBasedModel.getContext().clone();
		for (Expression evidence : this.evidences) {
			Context newContext = firstContext.conjoin(evidence);
			for (Expression factor : this.expressionBasedModel.getFactors()) { 
				if (newContext.evaluate(factor)==parameter){
					result++;
				}
			}
			newContext = firstContext;
		}
		return result;
	}

}
