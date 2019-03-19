package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor;

import static com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.schedule.ProjectionOfSamplingRuleSet.project;
import static com.sri.ai.util.Util.collectToList;
import static com.sri.ai.util.Util.fill;
import static com.sri.ai.util.Util.intersect;
import static com.sri.ai.util.Util.join;
import static com.sri.ai.util.Util.subtract;

import java.util.List;
import java.util.Random;
import java.util.Set;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.factor.SamplingFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample.Sample;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.schedule.SamplingGoal;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.schedule.SamplingRuleSet;

public class SamplingMarginalizingFactor extends AbstractSamplingFactor {

	private List<? extends Variable> marginalizedVariables;
	
	private SamplingFactor marginalizedFactor;
	
	public SamplingMarginalizingFactor(List<? extends Variable> marginalizedVariables, SamplingFactor marginalizedFactor, Random random) {
		super(subtract(marginalizedFactor.getVariables(), marginalizedVariables), random);
		this.marginalizedVariables = marginalizedVariables;
		this.marginalizedFactor = marginalizedFactor;
	}

	public List<? extends Variable> getMarginalizedVariables() {
		return marginalizedVariables;
	}

	public SamplingFactor getMarginalizedFactor() {
		return marginalizedFactor;
	}

	@Override
	public void sampleOrWeigh(Sample sampleToComplete) {
		if (marginalizedFactor instanceof DynamicSamplingProductFactor) { // TODO: too hard-wired; create interface for factors that can sample a subset of its variables
			((DynamicSamplingProductFactor) marginalizedFactor).sampleOrWeigh(getVariables(), sampleToComplete);
		}
		else {
			marginalizedFactor.sampleOrWeigh(sampleToComplete);
		}
	}

	@Override
	public SamplingRuleSet makeSamplingRules() {
		SamplingRuleSet samplingRuleSet = getMarginalizedFactor().getSamplingRuleSet();
		List<? extends SamplingGoal> remainingGoals = computeRemainingGoals(samplingRuleSet);
		SamplingRuleSet marginalSamplingRules = project(remainingGoals, this, getMarginalizedFactor());
		return marginalSamplingRules;
	}

	private List<? extends SamplingGoal> computeRemainingGoals(SamplingRuleSet samplingRuleSet) {
		Set<? extends SamplingGoal> allGoals = samplingRuleSet.getAllGoals();
		List<? extends SamplingGoal> remainingGoals = collectToList(allGoals, this::doesNotDependOnMarginalizedVariables);
		return remainingGoals;
	}

	private boolean doesNotDependOnMarginalizedVariables(SamplingGoal goal) {
		return !intersect(goal.getVariables(), marginalizedVariables);
	}

	@Override
	public String toString() {
		return "sum_{" + join(getMarginalizedVariables()) + "} " + getMarginalizedFactor();
	}

	@Override
	public	String nestedString(int level, boolean rules) {
		String tab = fill(level*4, ' ');
		return
				tab + "sum_{" + join(getMarginalizedVariables()) + "}\n"
				+ getMarginalizedFactor().nestedString(level + 1, rules)
				+ rulesString(level, rules);
	}

}
