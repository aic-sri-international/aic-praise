package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.schedule;

import static com.sri.ai.util.Util.unionArrayList;
import static com.sri.ai.util.collect.FunctionIterator.functionIterator;

import java.util.ArrayList;
import java.util.List;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.factor.SamplingFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.schedule.DefaultSamplingRules;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.schedule.SamplingRule;

public interface SamplingRuleSet {

	ArrayList<? extends SamplingRule> getSamplingRules();
	
	public SamplingRuleSet replaceFactor(SamplingFactor samplingFactor);

	SamplingRuleSet sumOut(List<? extends Variable> variables, SamplingFactor factorOnResultingRules);

	static SamplingRuleSet union(List<? extends SamplingRuleSet> samplingRulesSet) {
		ArrayList<? extends SamplingRule> union = unionArrayList(functionIterator(samplingRulesSet, SamplingRuleSet::getSamplingRules));
		return new DefaultSamplingRules(union);
	}
	
}
