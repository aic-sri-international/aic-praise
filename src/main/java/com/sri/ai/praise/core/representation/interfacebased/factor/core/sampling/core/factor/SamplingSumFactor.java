package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor;

import java.util.ArrayList;
import java.util.Random;

import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.factor.SamplingFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample.Sample;

public class SamplingSumFactor extends AbstractCompoundSamplingFactor {
	
	public SamplingSumFactor(ArrayList<? extends SamplingFactor> addedFactors, Random random) {
		super(addedFactors, random);
	}

	@Override
	public void sample(Sample sampleToComplete) {
		int i = getRandom().nextInt(getInputFactors().size());
		getInputFactors().get(i).sample(sampleToComplete);
	}

}
