package com.sri.ai.praise.core.representation.interfacebased.factor.core.expressionsampling;

import com.sri.ai.expresso.api.Expression;

public interface ExpressionWithProbabilityFunction extends Expression {

	com.sri.ai.util.function.api.functions.Function getDiscretizedConditionalProbabilityDistributionFunction();

}