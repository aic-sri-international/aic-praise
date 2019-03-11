package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor.library.logic;

import static com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.schedule.SamplingRule.samplingRuleFromVariables;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.listFrom;
import static com.sri.ai.util.Util.setFrom;
import static com.sri.ai.util.Util.union;
import static com.sri.ai.util.collect.NestedIterator.nestedIterator;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Random;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.factor.SamplingFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample.Potential;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample.Sample;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.schedule.SamplingGoal;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.schedule.SamplingRuleSet;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor.AbstractSamplingFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.schedule.DefaultSamplingRuleSet;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.schedule.SamplingRule;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.schedule.goal.VariableEqualsGoal;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.schedule.goal.VariableIsDefinedGoal;

public class IfThenElseSamplingFactor extends AbstractSamplingFactor {

	private Variable condition;
	private SamplingFactor thenSamplingFactor;
	private SamplingFactor elseSamplingFactor;

	public IfThenElseSamplingFactor(Variable condition, SamplingFactor thenSamplingFactor, SamplingFactor elseSamplingFactor, Random random) {
		super(listFrom(setFrom(nestedIterator(condition, thenSamplingFactor.getVariables(), elseSamplingFactor.getVariables()))), random);
		this.condition = condition;
		this.thenSamplingFactor = thenSamplingFactor;
		this.elseSamplingFactor = elseSamplingFactor;
	}

	///////////////// SAMPLE OR WEIGHT

	@Override
	public void sampleOrWeigh(Sample sample) {
		if (sample.instantiates(condition)) {
			sampleOrWeightGivenCondition(sample);
		}
		else {
			sampleOrWeightGivenBranches(sample);
		}

	}

	private void sampleOrWeightGivenCondition(Sample sample) {
		if (((Boolean)sample.get(condition))) {
			thenSamplingFactor.sampleOrWeigh(sample);
		}
		else {
			elseSamplingFactor.sampleOrWeigh(sample);
		}
	}

	private void sampleOrWeightGivenBranches(Sample sample) {
		Potential probability = computeConditionProbability(sample);
		sampleOrWeighConditionWithGivenProbability(sample, probability);
	}

	private Potential computeConditionProbability(Sample sample) {
		Potential thenPotential = thenSamplingFactor.weight(sample);
		Potential elsePotential = elseSamplingFactor.weight(sample);
		Potential probability = thenPotential.divide(thenPotential.add(elsePotential));
		return probability;
	}

	private void sampleOrWeighConditionWithGivenProbability(Sample sample, Potential probability) {
		if (sample.instantiates(condition)) {
			sample.updatePotential(probability);
		}
		else {
			boolean conditionValue = getRandom().nextDouble() < probability.doubleValue();
			sample.set(condition, conditionValue);
		}
	}
	
	///////////////// SAMPLING RULES

	@Override
	protected SamplingRuleSet makeSamplingRules() {
		List<SamplingRule> samplingRules = list();
		collectConditionBasedSamplingRules(samplingRules);
		collectThenAndElseFactorsBasedSamplingRules(samplingRules);
		return new DefaultSamplingRuleSet(samplingRules);
	}

	private void collectConditionBasedSamplingRules(List<SamplingRule> samplingRules) {
		collectSamplingRuleAddingTestForCondition(thenSamplingFactor, true,  samplingRules);
		collectSamplingRuleAddingTestForCondition(elseSamplingFactor, false, samplingRules);
	}

	private void collectSamplingRuleAddingTestForCondition(SamplingFactor samplingFactor, boolean conditionValue, List<SamplingRule> samplingRules) {
		samplingFactor.getSamplingRuleSet().getSamplingRules()
		.forEach(samplingRule -> collectSamplingRuleWithAntecedentsPrefixedByCondition(samplingRule, conditionValue, samplingRules));
	}

	private void collectSamplingRuleWithAntecedentsPrefixedByCondition(
			SamplingRule samplingRule,
			boolean conditionValue,
			List<SamplingRule> samplingRules) {
		ArrayList<SamplingGoal> newAntecendents = makeNewAntecedentsPrefixedByCondition(samplingRule.getAntecendents(), conditionValue);
		SamplingRule newRule = samplingRule.copyWithNewSamplerAndAntecedents(this, newAntecendents);
		samplingRules.add(newRule);
	}

	private ArrayList<SamplingGoal> makeNewAntecedentsPrefixedByCondition(Collection<? extends SamplingGoal> antecendents, boolean conditionValue) {
		ArrayList<SamplingGoal> newAntecedents = new ArrayList<>(antecendents.size() + 2);
		newAntecedents.add(new VariableIsDefinedGoal(condition));
		newAntecedents.add(new VariableEqualsGoal(condition, conditionValue));
		newAntecedents.addAll(antecendents);
		return newAntecedents;
	}

	private void collectThenAndElseFactorsBasedSamplingRules(List<SamplingRule> samplingRules) {
		Collection<Variable> antecedentVariables = union(thenSamplingFactor.getVariables(), elseSamplingFactor.getVariables());
		SamplingRule samplingRule = samplingRuleFromVariables(this, list(condition), antecedentVariables, 1.0);
		samplingRules.add(samplingRule);
	}

	@Override
	public String toString() {
		return "if (" + condition + ") then " + thenSamplingFactor + " else " + elseSamplingFactor;
	}

}
