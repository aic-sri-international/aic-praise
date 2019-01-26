package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor;

import static com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.schedule.SamplingRuleSet.union;
import static com.sri.ai.util.Util.arrayList;
import static com.sri.ai.util.Util.collectToList;
import static com.sri.ai.util.Util.collectToSet;
import static com.sri.ai.util.Util.flattenOneLevelToArrayList;
import static com.sri.ai.util.Util.forAll;
import static com.sri.ai.util.Util.getValuePossiblyCreatingIt;
import static com.sri.ai.util.Util.join;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.map;
import static com.sri.ai.util.Util.mapIntoList;
import static com.sri.ai.util.Util.myAssert;
import static com.sri.ai.util.Util.set;
import static com.sri.ai.util.Util.subtract;
import static com.sri.ai.util.planning.core.PlannerUsingEachRuleAtMostOnce.planUsingEachRuleAtMostOnce;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.Set;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;

import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.factor.Sampler;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.factor.SamplingFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample.Sample;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.schedule.SamplingGoal;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.schedule.SamplingRuleSet;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.schedule.SamplingRule;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.schedule.SamplingState;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.schedule.goal.VariableIsDefinedGoal;
import com.sri.ai.util.planning.api.Plan;

public class SamplingProductFactor extends AbstractCompoundSamplingFactor {
	
	/**
	 * If true, this flag uses the weights of obtained samples to reward plans that generated them
	 * so that they are more likely picked for future samples.
	 */
	public static boolean adaptiveSampling = true;
	
	private Map<Set<SamplingGoal>, Plan> fromSatisfiedGoalsToPlan;

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
		Set<SamplingGoal> satisfiedGoals = getSatisfiedGoals(sampleToComplete);
		return getSamplingPlan(satisfiedGoals);
	}

	private Set<SamplingGoal> getSatisfiedGoals(Sample sample) {
		Set<? extends SamplingGoal> allGoals = getSamplingRuleSet().getAllGoals();
		Set<SamplingGoal> result = collectToSet(allGoals, g -> g.isSatisfied(sample));
		return result;
	}

	public Plan getSamplingPlan(Set<SamplingGoal> satisfiedGoals) {
		Plan samplingPlan = getValuePossiblyCreatingIt(fromSatisfiedGoalsToPlan, satisfiedGoals, this::makePlan);
		return samplingPlan;
	}
	
	private Plan makePlan(Set<SamplingGoal> satisfiedGoals) {
		Predicate<SamplingGoal> isEffectivelyStaticGoal = g -> g instanceof VariableIsDefinedGoal;
		Set<? extends SamplingGoal> allRequiredGoals = collectToSet(getSamplingRuleSet().getAllGoals(), g -> isEffectivelyStaticGoal.test(g));
		ArrayList<? extends SamplingRule> samplingRules = getSamplingRuleSet().getSamplingRules();
		Plan plan = planUsingEachRuleAtMostOnce(allRequiredGoals, satisfiedGoals, set() /* TODO can do better */, isEffectivelyStaticGoal, samplingRules);
		myAssert(!plan.isFailedPlan(), () -> "Plan for sampling product factor has failed: " + this);
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
		List<? extends Sampler> samplersThatFired = 
				getSamplingRuleSet()
				.getSamplingRules()
				.stream()
				.filter(SamplingRule::hasFired)
				.map(SamplingRule::getSampler) // only samplers that are sampling factors will reach here
				.collect(Collectors.toList());
		@SuppressWarnings("unchecked")
		List<? extends SamplingFactor> factorsThatFired = (List<? extends SamplingFactor>) collectToList(samplersThatFired, s -> s instanceof SamplingFactor);
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
