package com.sri.ai.praise.learning.parameterlearning.representation.expression;

import java.util.LinkedHashSet;

import com.sri.ai.expresso.api.Expression;

/**
 * Family (for datapoints) determined by a Expression condition over the parents of a Bayesian node
 * It also keeps track of the parameters that can be generated on the initial Expression given that the condition for this family is true
 * 
 * @author Roger Leite Lucena
 *
 */

public class Family {

	public Expression condition;
	public LinkedHashSet<Expression> parametersThatCanBeGenerated;
	
	public Family(Expression condition, LinkedHashSet<Expression> parametersThatCanBeGenerated) {
		this.condition = condition;
		this.parametersThatCanBeGenerated = parametersThatCanBeGenerated;
	}
	
	public void addParameters(LinkedHashSet<Expression> moreParameters) {
		parametersThatCanBeGenerated.addAll(moreParameters);
	}
	
	@Override
	public String toString() {
		String string = "[Condition: " + condition + ", Parameters: " + parametersThatCanBeGenerated + "]";
		return string;
	}
	
}
