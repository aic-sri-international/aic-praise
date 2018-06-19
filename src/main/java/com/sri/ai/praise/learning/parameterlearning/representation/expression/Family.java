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
	public LinkedHashSet<Expression> parameters;
	
	public Family(Expression condition, LinkedHashSet<Expression> parameters) {
		this.condition = condition;
		this.parameters = parameters;
	}
	
}
