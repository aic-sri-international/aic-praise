package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.schedule;

import static com.sri.ai.util.Util.unionArrayList;
import static com.sri.ai.util.collect.FunctionIterator.functionIterator;

import java.util.ArrayList;
import java.util.List;

import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.factor.SamplingFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.schedule.DefaultSamplingRuleSet;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.schedule.SamplingRule;
import com.sri.ai.util.planning.api.Goal;

public interface SamplingRuleSet {

	ArrayList<? extends SamplingRule> getSamplingRules();

	List<? extends Goal> getAllGoals();
	
	public SamplingRuleSet replaceFactor(SamplingFactor samplingFactor);

	SamplingRuleSet project(List<? extends Goal> remainingGoals, SamplingFactor factorOnProjectedSamplingRules);

	static SamplingRuleSet union(List<? extends SamplingRuleSet> samplingRulesSet) {
		List<? extends Goal> unionOfVariables = unionArrayList(functionIterator(samplingRulesSet, SamplingRuleSet::getAllGoals));
		ArrayList<? extends SamplingRule> unionOfRules = unionArrayList(functionIterator(samplingRulesSet, SamplingRuleSet::getSamplingRules));
		return new DefaultSamplingRuleSet(unionOfVariables, unionOfRules);
	}
	
}
