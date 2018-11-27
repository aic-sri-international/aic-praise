package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.schedule;

import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample.Sample;
import com.sri.ai.util.planning.api.State;

public class SampleState implements State {

	private Sample sample;

	public SampleState(Sample sample) {
		this.sample = sample;
	}
	
	public Sample getSample() {
		return sample;
	}

}
