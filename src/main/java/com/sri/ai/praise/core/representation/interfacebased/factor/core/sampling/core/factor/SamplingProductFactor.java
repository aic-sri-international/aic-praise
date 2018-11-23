package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor;

import java.util.ArrayList;
import java.util.Random;

import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.factor.SamplingFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample.Sample;

public class SamplingProductFactor extends AbstractCompoundSamplingFactor {
	
	public SamplingProductFactor(ArrayList<? extends SamplingFactor> multipliedFactors, Random random) {
		super(multipliedFactors, random);
	}
	
	@Override
	public void sample(Sample sampleToComplete) {
		for (SamplingFactor factor: getInputFactors()) {
			factor.sample(sampleToComplete);
		}
	}

}
