package com.sri.ai.praise.core.representation.translation.rodrigoframework.samplinggraph2d;

import com.sri.ai.util.function.api.functions.SingleInputFunction;
import com.sri.ai.util.function.api.variables.Assignment;
import com.sri.ai.util.function.api.variables.Variable;

public interface SamplingFactorDiscretizedProbabilityDistributionSingleInputFunction
		extends SamplingFactorDiscretizedProbabilityDistributionFunction, SingleInputFunction {

	@Override
	SamplingFactorDiscretizedProbabilityDistributionSingleInputFunction 
	project(Variable variable, Assignment assignmentToRemainingVariables);
}
