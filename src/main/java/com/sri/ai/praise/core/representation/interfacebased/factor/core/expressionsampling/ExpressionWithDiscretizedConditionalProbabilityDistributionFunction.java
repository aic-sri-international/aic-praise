package com.sri.ai.praise.core.representation.interfacebased.factor.core.expressionsampling;

import com.sri.ai.util.distribution.DiscretizedConditionalProbabilityDistributionFunction;

public interface ExpressionWithDiscretizedConditionalProbabilityDistributionFunction {

	DiscretizedConditionalProbabilityDistributionFunction getDiscretizedConditionalProbabilityDistributionFunction();

}