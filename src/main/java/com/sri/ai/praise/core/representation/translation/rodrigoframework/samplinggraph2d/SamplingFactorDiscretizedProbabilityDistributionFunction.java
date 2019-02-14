package com.sri.ai.praise.core.representation.translation.rodrigoframework.samplinggraph2d;

import java.util.ArrayList;

import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.factor.SamplingFactor;
import com.sri.ai.util.function.api.functions.Function;
import com.sri.ai.util.function.api.values.Value;

public interface SamplingFactorDiscretizedProbabilityDistributionFunction
extends Function, java.util.function.Function<ArrayList<Object>, Value> {

	SamplingFactor getSamplingFactor();

	SamplingFactorDiscretizedProbabilityDistribution getConditionalDistribution();

	void sample();

	boolean averageWeightIsZero();
	
	int getNumberOfSamples();
	
	double getTotalWeight();

//	@Override
//	SamplingFactorDiscretizedProbabilityDistributionSingleInputFunction 
//	project(Variable variable, Assignment assignmentToRemainingVariables);

//	SamplingFactorDiscretizedProbabilityDistributionFunction condition(Sample conditioningSample);

}