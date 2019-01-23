package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor;

import java.util.Collection;

import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.schedule.SamplingGoal;

public class SpecificationForFunctionResultSamplingRule {

	public Collection<? extends Integer> argumentsIndices;
	public Collection<? extends SamplingGoal> goalsBesidesArgumentsBeingDefined;
	public double estimatedSuccessWeight;
	
	public SpecificationForFunctionResultSamplingRule(
			Collection<? extends Integer> argumentsIndices,
			Collection<? extends SamplingGoal> goalsBesidesArgumentsBeingDefined,
			double estimatedSuccessWeight) {
		
		this.argumentsIndices = argumentsIndices;
		this.goalsBesidesArgumentsBeingDefined = goalsBesidesArgumentsBeingDefined;
		this.estimatedSuccessWeight = estimatedSuccessWeight;
	}
}
