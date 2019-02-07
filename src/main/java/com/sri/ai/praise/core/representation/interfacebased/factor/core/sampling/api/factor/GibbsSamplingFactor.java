package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.factor;

import java.util.Collection;
import java.util.List;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample.Sample;

public interface GibbsSamplingFactor extends SamplingFactor {
	
	/**
	 * Takes a complete sample and performs an iteration of Gibbs sampling on it.
	 * @param variables TODO
	 * @param sample
	 * @param conditionedVariables TODO
	 */
	void gibbs(List<? extends Variable> variables, Sample sample, Collection<? extends Variable> conditionedVariables);

}
