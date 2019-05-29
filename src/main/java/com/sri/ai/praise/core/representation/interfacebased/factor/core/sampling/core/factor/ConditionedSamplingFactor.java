package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor;

import static com.sri.ai.util.Util.arrayList;
import static com.sri.ai.util.Util.collectToArrayList;
import static com.sri.ai.util.Util.fill;
import static java.util.stream.Collectors.toCollection;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Map.Entry;
import java.util.Random;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.factor.SamplingFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample.Sample;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.schedule.SamplingGoal;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.schedule.SamplingRuleSet;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.schedule.DefaultSamplingRuleSet;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.schedule.SamplingRule;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.schedule.goal.VariableIsDefinedGoal;
import com.sri.ai.util.Util;

/**
 * A conditioned sampling factor, that is, the result of conditioning a sampling factor by a set of assignments to some of its variables.
 * 
 * @author braz
 *
 */
public class ConditionedSamplingFactor extends AbstractConditionedFactor implements SamplingFactor {

	/**
	 * A protected constructor which, like {@link AbstractConditionedFactor}'s constructor,
	 * assumes that the conditions are not empty and the given factor is not zero;
	 * this one, furthermore, also requires the factor is a sampling factor.
	 * @param conditioningSample
	 * @param factor
	 */
	protected ConditionedSamplingFactor(Sample conditioningSample, SamplingFactor factor) {
		super(conditioningSample, factor);
	}

	@Override
	public void sampleOrWeigh(Sample sample) {
		for(Entry<Variable, Object> entry : getConditioningSample().getAssignment().mapValue().entrySet()) {
			sample.set(entry.getKey(), entry.getValue());
		}
		
		getFactor().sampleOrWeigh(sample);
		
		for(Entry<Variable, Object> entry : getConditioningSample().getAssignment().mapValue().entrySet()) {
			sample.remove(entry.getKey());
		}
	}

	///////// BASIC GETTERS
	
	@Override
	public SamplingFactor getFactor() {
		return (SamplingFactor) super.getFactor();
	}

	@Override
	public Random getRandom() {
		return getFactor().getRandom();
	}


	///////// BUILDING

	private static class ConditionedSamplingFactorBuildingPolicy implements BuildingPolicy {

		@Override
		public Factor makeConditionedNonZeroAndNonConditionedFactor(Sample conditioningSample, Factor factor) {
			SamplingFactor samplingFactor = Util.assertType(factor, SamplingFactor.class, this);
			return new ConditionedSamplingFactor(conditioningSample, samplingFactor);
		}
		
	}
	
	@Override
	protected BuildingPolicy getBuildingPolicy() {
		return new ConditionedSamplingFactorBuildingPolicy();
	}
	
	public static SamplingFactor build(Sample conditioningSample, SamplingFactor factor) {
		return (SamplingFactor) build(conditioningSample, factor, new ConditionedSamplingFactorBuildingPolicy());
	}

	///////// NORMALIZATION

	@Override
	public Factor normalize() {
		return this; // nothing to do because sampling factors are normalized in the process of generating samples
	}

	///////// GETTER FOR SAMPLING RULES
	
	private SamplingRuleSet conditionedSamplingRuleSet;

	@Override
	public SamplingRuleSet getSamplingRuleSet() {
		if (conditionedSamplingRuleSet == null) {
			conditionedSamplingRuleSet = makeConditionedSamplingRuleSet();
		}
		return conditionedSamplingRuleSet;
	}
	
	public SamplingRuleSet makeConditionedSamplingRuleSet() {
		ArrayList<SamplingRule> rules = 
				getOriginalSamplingRules().stream()
				.map(this::conditionOrNull)
				.filter(c -> c != null)
				.collect(toCollection(() -> arrayList()));
		return new DefaultSamplingRuleSet(rules);
	}

	private Collection<? extends SamplingRule> getOriginalSamplingRules() {
		return getFactor().getSamplingRuleSet().getSamplingRules();
	}
	
	private SamplingRule conditionOrNull(SamplingRule rule) {
		ArrayList<SamplingGoal> conditionedConsequents = getConditionedConsequents(rule);
		if (conditionedConsequents.isEmpty()) {
			return null; // there is no point in keeping a rule that has no consequents
		}
		else {
			ArrayList<SamplingGoal> conditionedAntecedents = getConditionedAntecedents(rule);
			return new SamplingRule(this, conditionedConsequents, conditionedAntecedents, rule.getEstimatedSuccessWeight());
		}
	}

	private ArrayList<SamplingGoal> getConditionedConsequents(SamplingRule rule) {
		return collectToArrayList(rule.getConsequents(), g -> ! getConditioningSample().instantiates(getVariable(g)));
	}

	private ArrayList<SamplingGoal> getConditionedAntecedents(SamplingRule rule) {
		return collectToArrayList(rule.getAntecendents(), g -> ! g.isSatisfiedBySampleWithoutModifyingIt(getConditioningSample()));
	}

	private Variable getVariable(SamplingGoal variableIsDefinedGoal) {
		return ((VariableIsDefinedGoal) variableIsDefinedGoal).getVariable();
	}

	///////////// NESTED STRING
	
	@Override
	public String nestedString(int level, boolean showSamplingRules) {
		return 
				fill(LEVEL_INDENTATION, ' ') + "Factor conditioned by " + getConditioningSample() + "\n" +
				getFactor().nestedString(level + 1, showSamplingRules);
	}

}
