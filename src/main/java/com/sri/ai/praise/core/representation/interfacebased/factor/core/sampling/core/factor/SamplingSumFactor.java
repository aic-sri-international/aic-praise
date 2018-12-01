package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor;

import static com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.schedule.SamplingRuleSet.union;
import static com.sri.ai.util.Util.mapIntoList;

import java.util.ArrayList;
import java.util.Random;

import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.factor.SamplingFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample.Sample;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.schedule.SamplingRuleSet;

public class SamplingSumFactor extends AbstractCompoundSamplingFactor {
	
	public SamplingSumFactor(ArrayList<? extends SamplingFactor> addedFactors, Random random) {
		super(addedFactors, random);
	}

	@Override
	public void sampleOrWeigh(Sample sampleToComplete) {
		int i = getRandom().nextInt(getInputFactors().size());
		getInputFactors().get(i).sampleOrWeigh(sampleToComplete);
	}

	@Override
	protected SamplingRuleSet makeSamplingRules() {
		SamplingRuleSet samplingRules = union(mapIntoList(getInputFactors(), SamplingFactor::getSamplingRuleSet));
		return samplingRules;
	}

	@Override
	public String operatorName() {
		return "addition";
	}

}
