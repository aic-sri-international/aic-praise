package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor;

import java.util.Collection;

import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.factor.SamplingFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.schedule.SamplingGoal;

public class SpecificationForFunctionResultSamplingRule {

	public SamplingFactor sampler;
	public Collection<? extends Integer> argumentsIndices;
	public Collection<? extends SamplingGoal> goalsBesidesArgumentsBeingDefined;
	public double estimatedSuccessWeight;
	
	public SpecificationForFunctionResultSamplingRule(
			SamplingFactor sampler,
			Collection<? extends Integer> argumentsIndices,
			Collection<? extends SamplingGoal> goalsBesidesArgumentsBeingDefined,
			double estimatedSuccessWeight) {
		
		this.sampler = sampler;
		this.argumentsIndices = argumentsIndices;
		this.goalsBesidesArgumentsBeingDefined = goalsBesidesArgumentsBeingDefined;
		this.estimatedSuccessWeight = estimatedSuccessWeight;
	}
}
