package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.schedule;

import java.util.Random;

import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample.Sample;
import com.sri.ai.util.planning.api.State;

public class SamplingState implements State {

	private Sample sample;
	private Random random;

	public SamplingState(Sample sample, Random random) {
		this.sample = sample;
		this.random = random;
	}
	
	public Sample getSample() {
		return sample;
	}

	@Override
	public Random getRandom() {
		return random;
	}

}
