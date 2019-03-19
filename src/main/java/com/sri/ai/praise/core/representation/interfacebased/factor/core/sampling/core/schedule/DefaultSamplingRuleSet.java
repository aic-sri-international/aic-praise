package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.schedule;

import static com.sri.ai.util.Util.join;
import static com.sri.ai.util.Util.set;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Set;

import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.schedule.SamplingGoal;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.schedule.SamplingRuleSet;

public class DefaultSamplingRuleSet implements SamplingRuleSet {

	private Collection<? extends SamplingRule> samplingRules;

	private Set<SamplingGoal> allGoals;
	
	public static DefaultSamplingRuleSet samplingRuleSet(Collection<? extends SamplingRule> samplingRules) {
		return new DefaultSamplingRuleSet(samplingRules);
	}

	public DefaultSamplingRuleSet(SamplingRule... samplingRules) {
		this(new ArrayList<>(Arrays.asList(samplingRules)));
	}

	public DefaultSamplingRuleSet(Collection<? extends SamplingRule> samplingRules) {
		this.samplingRules = samplingRules;
		this.allGoals = null;
	}

	@Override
	public Collection<? extends SamplingRule> getSamplingRules() {
		return samplingRules;
	}

	@Override
	public Set<? extends SamplingGoal> getAllGoals() {
		if (allGoals == null) {
			makeAllGoals();
		}
		return allGoals;
	}

	private void makeAllGoals() {
		allGoals = set();
		getSamplingRules().forEach(r -> allGoals.addAll(r.getAntecendents()));
		getSamplingRules().forEach(r -> allGoals.addAll(r.getConsequents()));
	}

	@Override
	public String toString() {
		return join(getSamplingRules());
	}
}
