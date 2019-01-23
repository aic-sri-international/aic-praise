package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample.Sample;

public class DynamicSamplingFailureError extends Error {

	private static final long serialVersionUID = 1L;

	public DynamicSamplingFailureError(Variable variable, Sample sample, DynamicSamplingProductFactor dynamicSamplingProductFactor) {
		super("Could not sample " + variable + " from sample " + sample + " using sampling rules " + dynamicSamplingProductFactor.getSamplingRuleSet());
	}

}
