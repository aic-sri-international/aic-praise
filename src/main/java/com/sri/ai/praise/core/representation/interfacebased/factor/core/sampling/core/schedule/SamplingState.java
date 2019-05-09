package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.schedule;

import java.util.Collection;
import java.util.Random;

import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.factor.SamplingFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample.Sample;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.sample.UnmodifiableSample;
import com.sri.ai.util.planning.api.State;

public class SamplingState implements State {

	private Sample sample;
	@SuppressWarnings("unused")
	private Collection<? extends SamplingFactor> factors;
	private Random random;

	public SamplingState(Sample sample, Collection<? extends SamplingFactor> factors, Random random) {
		this.sample = sample;
		this.factors = factors;
		this.random = random;
	}
	
	/**
	 * Returns the state's sample in unmodified form; only the state itself is allowed to modify the sample
	 * in order to do proper bookkeeping on which factors have been used.
	 * @return
	 */
	public Sample getUnmodifiableSample() {
		return new UnmodifiableSample(sample);
	}

	@Override
	public Random getRandom() {
		return random;
	}

}
