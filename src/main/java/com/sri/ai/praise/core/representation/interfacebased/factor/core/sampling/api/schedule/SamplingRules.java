package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.schedule;

import static com.sri.ai.util.Util.unionArrayList;
import static com.sri.ai.util.collect.FunctionIterator.functionIterator;

import java.util.ArrayList;
import java.util.List;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.factor.SamplingFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.schedule.DefaultSamplingRules;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.schedule.SamplingRule;

public interface SamplingRules {

	ArrayList<? extends SamplingRule> getSamplingRules();
	
	public SamplingRules replaceFactor(SamplingFactor samplingFactor);

	SamplingRules sumOut(List<? extends Variable> variables, SamplingFactor factorOnResultingRules);

	static SamplingRules union(List<? extends SamplingRules> samplingRulesSet) {
		ArrayList<? extends SamplingRule> union = unionArrayList(functionIterator(samplingRulesSet, SamplingRules::getSamplingRules));
		return new DefaultSamplingRules(union);
	}
	
}
