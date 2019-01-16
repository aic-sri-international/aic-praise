package com.sri.ai.praise.core.representation.translation.rodrigoframework.samplinggraph2d;

import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.factor.SamplingFactor;
import com.sri.ai.util.distribution.DiscretizedConditionalProbabilityDistributionFunction;
import com.sri.ai.util.function.api.variables.Assignment;
import com.sri.ai.util.function.api.variables.SetOfVariables;
import com.sri.ai.util.graph2d.api.GraphSetMaker;

/**
 * Adds {@link com.sri.ai.util.function.api.functions.Function} functionality
 * to {@link SamplingFactorDiscretizedProbabilityDistribution},
 * equipping it with {@link #evaluate(Assignment)} method.
 * Among other things, this allows it to be used by {@link GraphSetMaker}.
 * <p>
 * It also defines a {@link #sample()} method that further samples the inner {@link SamplingFactorDiscretizedProbabilityDistribution}.
 */ 
public class SamplingFactorDiscretizedProbabilityDistributionFunction extends DiscretizedConditionalProbabilityDistributionFunction {
	
	public SamplingFactorDiscretizedProbabilityDistributionFunction(
			SamplingFactor samplingFactor,
			SetOfVariables inputVariablesWithRange,
			int queryVariableIndex) {
		
		super(new SamplingFactorDiscretizedProbabilityDistribution(samplingFactor, inputVariablesWithRange, queryVariableIndex));
	}
	
	public void sample() {
		((SamplingFactorDiscretizedProbabilityDistribution) getConditionalDistribution()).sample();
	}
}
