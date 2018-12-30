package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor;

import static com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.schedule.SamplingRuleSet.union;
import static com.sri.ai.util.Util.arrayList;
import static com.sri.ai.util.Util.flattenOneLevelToArrayList;
import static com.sri.ai.util.Util.forAll;
import static com.sri.ai.util.Util.join;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.mapIntoList;
import static com.sri.ai.util.Util.myAssert;
import static com.sri.ai.util.Util.set;
import static com.sri.ai.util.Util.subtract;
import static com.sri.ai.util.planning.core.PlannerUsingEachRuleAtMostOnce.planUsingEachRuleAtMostOnce;

import java.util.ArrayList;
import java.util.List;
import java.util.Random;
import java.util.function.Function;
import java.util.stream.Collectors;

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
	
	private Plan samplingPlan;

	public SamplingProductFactor(ArrayList<? extends SamplingFactor> multipliedFactors, Random random) {
		super(flattenOneLevel(multipliedFactors), random);
		// it is important that product factors be flat because we must be sure that all incoming messages of a variable in Exact BP
		// are present in the same product factor if we are to limit the search for samplers to a single product factor.
		this.samplingPlan = makePlan();
	}

	@Override
	protected SamplingRuleSet makeSamplingRules() {
		SamplingRuleSet samplingRules = union(mapIntoList(getInputFactors(), SamplingFactor::getSamplingRuleSet));
		return samplingRules;
	}

	private Plan makePlan() {
		List<VariableGoal> variableGoals = mapIntoList(getVariables(), v -> new VariableGoal(v));
		ArrayList<? extends SamplingRule> samplingRules = getSamplingRuleSet().getSamplingRules();
		Plan plan = planUsingEachRuleAtMostOnce(variableGoals, set(), samplingRules);
		return plan;
	}

	public Plan getSamplingPlan() {
		return samplingPlan;
	}
	
	@Override
	public void sampleOrWeigh(Sample sampleToComplete) {
		List<? extends SamplingFactor> factorsThatFired = executePlanIfAny(sampleToComplete);
		makeSureToConsultAllInputFactors(sampleToComplete, factorsThatFired);
		rewardIfNeeded(sampleToComplete);
		myAssert(complete(sampleToComplete), () -> "Factor was not able to complete sample.\nSample: " + sampleToComplete + "\nFactor: " + this + "\nSampling plan: " + (samplingPlan == null? "none" : samplingPlan.nestedString()));
	}

	private List<? extends SamplingFactor> executePlanIfAny(Sample sampleToComplete) {
		getSamplingRuleSet().getSamplingRules().forEach(SamplingRule::reset);
		List<? extends SamplingFactor> factorsThatFired;
		if (samplingPlan != null) {
			factorsThatFired = executeSamplingPlan(sampleToComplete);
		}
		else {
			factorsThatFired = list();
		}
		return factorsThatFired;
	}

	private List<? extends SamplingFactor> executeSamplingPlan(Sample sampleToComplete) {
		
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
		if (adaptiveSampling && samplingPlan != null) {
			samplingPlan.reward(sampleToComplete.getPotential().doubleValue());
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
