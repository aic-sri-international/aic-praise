package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor;

import static com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.schedule.SamplingRuleSet.union;
import static com.sri.ai.util.Util.arrayList;
import static com.sri.ai.util.Util.flattenOneLevelToArrayList;
import static com.sri.ai.util.Util.forAll;
import static com.sri.ai.util.Util.getValuePossiblyCreatingIt;
import static com.sri.ai.util.Util.intersection;
import static com.sri.ai.util.Util.join;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.map;
import static com.sri.ai.util.Util.mapIntoList;
import static com.sri.ai.util.Util.mapIntoSet;
import static com.sri.ai.util.Util.myAssert;
import static com.sri.ai.util.Util.subtract;
import static com.sri.ai.util.planning.core.PlannerUsingEachRuleAtMostOnce.planUsingEachRuleAtMostOnce;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.factor.SamplingFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample.Sample;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.schedule.SamplingRuleSet;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.schedule.SamplingRule;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.schedule.SamplingState;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.schedule.VariableGoal;
import com.sri.ai.util.planning.api.Plan;

public class SamplingProductFactor extends AbstractCompoundSamplingFactor {
	
	/**
	 * If true, this flag uses the weights of obtained samples to reward plans that generated them
	 * so that they are more likely picked for future samples.
	 */
	public static boolean adaptiveSampling = true;
	
	private Map<Set<VariableGoal>, Plan> fromSatisfiedGoalsToPlan;

	public SamplingProductFactor(ArrayList<? extends SamplingFactor> multipliedFactors, Random random) {
		super(flattenOneLevel(multipliedFactors), random);
		// it is important that product factors be flat because we must be sure that all incoming messages of a variable in Exact BP
		// are present in the same product factor if we are to limit the search for samplers to a single product factor.
		this.fromSatisfiedGoalsToPlan = map();
	}

	@Override
	protected SamplingRuleSet makeSamplingRules() {
		SamplingRuleSet samplingRules = union(mapIntoList(getInputFactors(), SamplingFactor::getSamplingRuleSet));
		return samplingRules;
	}

	public Plan getSamplingPlan(Sample sampleToComplete) {
		Set<VariableGoal> satisfiedGoals = getSatisfiedGoals(sampleToComplete);
		return getSamplingPlan(satisfiedGoals);
	}

	private Set<VariableGoal> getSatisfiedGoals(Sample sample) {
		Set<Variable> variablesDefinedInSample = sample.getAssignment().mapValue().keySet();
		Set<Variable> factorVariablesDefinedInSample = intersection(getVariables(), variablesDefinedInSample);
		Set<VariableGoal> result = mapIntoSet(factorVariablesDefinedInSample, v -> new VariableGoal(v));
		return result;
	}

	public Plan getSamplingPlan(Set<VariableGoal> satisfiedGoals) {
		Plan samplingPlan = getValuePossiblyCreatingIt(fromSatisfiedGoalsToPlan, satisfiedGoals, this::makePlan);
		return samplingPlan;
	}
	
	private Plan makePlan(Set<VariableGoal> satisfiedGoals) {
		List<VariableGoal> allGoals = mapIntoList(getVariables(), v -> new VariableGoal(v));
		ArrayList<? extends SamplingRule> samplingRules = getSamplingRuleSet().getSamplingRules();
		Plan plan = planUsingEachRuleAtMostOnce(allGoals, satisfiedGoals, samplingRules);
		return plan;
	}

	@Override
	public void sampleOrWeigh(Sample sampleToComplete) {
		List<? extends SamplingFactor> factorsThatFired = executeSamplingPlanIfAny(sampleToComplete);
		myAssert(complete(sampleToComplete), () -> "Factor was not able to complete sample.\nSample: " + sampleToComplete + "\nFactor: " + this + "\nSampling plan: " + (getSamplingPlan(sampleToComplete) == null? "none" : getSamplingPlan(sampleToComplete).nestedString()));
		makeSureToConsultAllInputFactors(sampleToComplete, factorsThatFired);
		rewardIfNeeded(sampleToComplete);
	}

	private List<? extends SamplingFactor> executeSamplingPlanIfAny(Sample sampleToComplete) {
		List<? extends SamplingFactor> factorsThatFired;
		Plan samplingPlan = getSamplingPlan(sampleToComplete);
		if (samplingPlan != null) {
			factorsThatFired = executeSamplingPlan(samplingPlan, sampleToComplete);
		}
		else {
			factorsThatFired = list();
		}
		return factorsThatFired;
	}
	
	private List<? extends SamplingFactor> executeSamplingPlan(Plan samplingPlan, Sample sampleToComplete) {
		
		getSamplingRuleSet().getSamplingRules().forEach(SamplingRule::reset);
		samplingPlan.execute(new SamplingState(sampleToComplete, getRandom()));
		List<? extends SamplingFactor> factorsThatFired = 
				getSamplingRuleSet()
				.getSamplingRules()
				.stream()
				.filter(SamplingRule::hasFired)
				.map(SamplingRule::getSamplingFactor)
				.collect(Collectors.toList());
		return factorsThatFired;
	}

	private void makeSureToConsultAllInputFactors(
			Sample sampleToComplete,
			List<? extends SamplingFactor> factorsThatFired) {
		
		List<SamplingFactor> factorsThatDidNotFire = subtract(getInputFactors(), factorsThatFired);
		for (SamplingFactor factor : factorsThatDidNotFire) {
			factor.sampleOrWeigh(sampleToComplete);
		}
	}

	private void rewardIfNeeded(Sample sampleToComplete) {
		if (adaptiveSampling && getSamplingPlan(sampleToComplete) != null) {
			getSamplingPlan(sampleToComplete).reward(sampleToComplete.getPotential().doubleValue());
		}
	}

	private boolean complete(Sample sampleToComplete) {
		// TODO: this can be made better than checking every variable at every iteration. We can instead update it according to factor used
		boolean result = forAll(getVariables(), v -> sampleToComplete.getAssignment().contains(v));
		return result;
	}

	@Override
	public String operatorName() {
		return "product";
	}

	private static ArrayList<? extends SamplingFactor> flattenOneLevel(ArrayList<? extends SamplingFactor> list) {
		Function<SamplingFactor, ArrayList<? extends SamplingFactor>> 
		expand = f -> f instanceof SamplingProductFactor? getInputFactors(f) : arrayList(f);
		return flattenOneLevelToArrayList(list, expand);
		// Note we are relying on the fact that previous sampling product factors are already flattened.
	}

	private static ArrayList<? extends SamplingFactor> getInputFactors(
			SamplingFactor factorKnownToBeSamplingProductFactor) {
		return ((SamplingProductFactor) factorKnownToBeSamplingProductFactor).getInputFactors();
	}

	@Override
	public String toString() {
		return "product(" + join(getInputFactors()) + ")";
	}
}
