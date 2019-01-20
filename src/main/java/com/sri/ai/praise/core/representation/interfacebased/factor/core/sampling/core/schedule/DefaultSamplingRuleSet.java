package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.schedule;

import static com.sri.ai.util.Util.arrayList;
import static com.sri.ai.util.Util.arrayListFrom;
import static com.sri.ai.util.Util.join;
import static com.sri.ai.util.Util.set;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Set;

import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.factor.SamplingFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.schedule.SamplingGoal;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.schedule.SamplingRuleSet;
import com.sri.ai.util.base.BinaryFunction;
import com.sri.ai.util.planning.core.ProjectionOfSetOfRules;

public class DefaultSamplingRuleSet implements SamplingRuleSet {

	private ArrayList<? extends SamplingRule> samplingRules;

	private Set<SamplingGoal> allGoals;
	
	public static DefaultSamplingRuleSet samplingRuleSet(ArrayList<? extends SamplingRule> samplingRules) {
		return new DefaultSamplingRuleSet(samplingRules);
	}

	public DefaultSamplingRuleSet(SamplingRule... samplingRules) {
		this(new ArrayList<>(Arrays.asList(samplingRules)));
	}

	public DefaultSamplingRuleSet(ArrayList<? extends SamplingRule> samplingRules) {
		this.samplingRules = samplingRules;
		this.allGoals = null;
	}

	@Override
	public ArrayList<? extends SamplingRule> getSamplingRules() {
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
	public SamplingRuleSet project(List<? extends SamplingGoal> remainingGoals, SamplingFactor projectedFactor) {
		Set<? extends SamplingRule> projectedSamplingRules = projectSamplingRules(remainingGoals, projectedFactor);
		DefaultSamplingRuleSet result = new DefaultSamplingRuleSet(new ArrayList<>(projectedSamplingRules));
		return result;
	}

	private Set<? extends SamplingRule> projectSamplingRules(List<? extends SamplingGoal> remainingGoals, SamplingFactor projectedFactor) {
		ProjectionOfSetOfRules<SamplingRule, SamplingGoal> projector = getProjector(remainingGoals, projectedFactor);
		Set<? extends SamplingRule> projectedSamplingRules = projector.getProjectedSetOfRules();
		return projectedSamplingRules;
	}

	private ProjectionOfSetOfRules<SamplingRule, SamplingGoal> getProjector(List<? extends SamplingGoal> remainingVariablesAsGoals, SamplingFactor projectedFactor) {
		return new ProjectionOfSetOfRules<>(getSamplingRules(), remainingVariablesAsGoals, makeSamplingRuleFactory(projectedFactor));
	}

	private BinaryFunction<SamplingGoal, Set<? extends SamplingGoal>, SamplingRule> makeSamplingRuleFactory(SamplingFactor projectedFactor) {
		return (SamplingGoal consequent, Set<? extends SamplingGoal> antecedents) -> {
			return new SamplingRule(projectedFactor, arrayList(consequent), arrayListFrom(antecedents), 0.5);
		};
	}

	@Override
	public String toString() {
		return join(getSamplingRules());
	}
}
