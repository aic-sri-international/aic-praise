package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.factor;

import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample.Sample;

/**
 * An object that either samples values or evaluates sampled values in a sample.
 * 
 * @author braz
 *
 */
@FunctionalInterface
public interface Sampler {
	
	/**
	 * Attempts to fill in the current sample and update its potential and importance.
	 * Sampling factors are allowed to do no sampling if the given initial sample does not have enough information.
	 * For example, a sampling factor of a normal distribution will not produce any sampling if
	 * neither the mean nor the dependent (normally distributed) variable are assigned a value.
	 * @param sample
	 */
	void sampleOrWeigh(Sample sample);
}
