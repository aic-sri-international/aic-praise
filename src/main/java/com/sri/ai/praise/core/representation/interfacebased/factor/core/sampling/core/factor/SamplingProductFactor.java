package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor;

import static com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.schedule.DefaultSamplingRuleSet.samplingRuleSet;
import static com.sri.ai.util.Util.arrayList;
import static com.sri.ai.util.Util.arrayListFrom;
import static com.sri.ai.util.Util.collectToSet;
import static com.sri.ai.util.Util.flattenOneLevelToArrayList;
import static com.sri.ai.util.Util.forAll;
import static com.sri.ai.util.Util.getValuePossiblyCreatingIt;
import static com.sri.ai.util.Util.join;
import static com.sri.ai.util.Util.map;
import static com.sri.ai.util.Util.mapIntoSet;
import static com.sri.ai.util.Util.myAssert;
import static com.sri.ai.util.Util.println;
import static com.sri.ai.util.Util.set;
import static com.sri.ai.util.Util.union;
import static com.sri.ai.util.Util.unionArrayList;
import static com.sri.ai.util.Util.valueOrDefaultIfNull;
import static com.sri.ai.util.base.Pair.pair;
import static com.sri.ai.util.collect.FunctionIterator.functionIterator;
import static com.sri.ai.util.planning.core.PlannerUsingEachRuleAtMostOnce.planUsingEachRuleAtMostOnce;
import static com.sri.ai.util.planning.core.ProjectionOfSetOfRules.project;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.Set;
import java.util.function.Function;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.factor.SamplingFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample.Sample;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.schedule.SamplingGoal;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.schedule.SamplingRuleSet;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.schedule.DefaultSamplingRuleSet;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.schedule.SamplingRule;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.schedule.SamplingState;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.schedule.goal.VariableIsDefinedGoal;
import com.sri.ai.util.base.Pair;
import com.sri.ai.util.planning.api.Plan;

public class SamplingProductFactor extends AbstractCompoundSamplingFactor {
	
	/**
	 * If true, this flag uses the weights of obtained samples to reward plans that generated them
	 * so that they are more likely picked for future samples.
	 */
	public static boolean adaptiveSampling = true;
	
	private Map<Pair<Set<? extends SamplingGoal>, Set<? extends SamplingGoal>>, Plan> fromRequiredGoalsAndSatisfiedGoalsToPlan;

	public SamplingProductFactor(ArrayList<? extends SamplingFactor> multipliedFactors, Random random) {
		super(flattenOneLevel(multipliedFactors), random);
		// it is important that product factors be flat because we must be sure that all incoming messages of a variable in Exact BP
		// are present in the same product factor if we are to limit the search for samplers to a single product factor.
		this.fromRequiredGoalsAndSatisfiedGoalsToPlan = map();
	}

	@Override
	protected SamplingRuleSet makeSamplingRules() {
		// We must provide a set of sampling rules that describe which goals can
		// be achieved from which sets of antecedents concerning the *entire* set of multiplied factors,
		// including antecedents from distinct factors.
		// This requires projecting the set of rules on the total set of goals.
		var allSamplingRulesArrayList = unionArrayList(functionIterator(getInputFactors(), f -> f.getSamplingRuleSet().getSamplingRules()));
		println("Projecting sampling rules for sampling product factor " + this);
		Set<? extends SamplingRule> projectionOfSetOfSamplingRules =  
				project(
						allSamplingRulesArrayList,
						samplingRuleSet(allSamplingRulesArrayList).getAllGoals(),
						(consequent, antecedents) -> 
						new SamplingRule(this, arrayList(consequent), arrayListFrom(antecedents), 0.5));
		// TODO: can we do better than just use 0.5 here?

		SamplingRuleSet result = new DefaultSamplingRuleSet(projectionOfSetOfSamplingRules);
		return result;
	}

	public Plan getSamplingPlan(List<? extends Variable> variablesToSample, Sample sample) {
		Set<? extends SamplingGoal> satisfiedGoals = getSatisfiedGoals(sample);
		return getSamplingPlan(variablesToSample, satisfiedGoals);
	}

	private Set<SamplingGoal> getSatisfiedGoals(Sample sample) {
		Set<? extends SamplingGoal> allGoals = 
				union(
						functionIterator(
								this.getInputFactors(), 
								f -> f.getSamplingRuleSet().getAllGoals()));
		Set<SamplingGoal> result = collectToSet(allGoals, g -> g.isSatisfiedBySampleWithoutModifyingIt(sample));
		return result;
	}

	public Plan getSamplingPlan(Set<? extends SamplingGoal> satisfiedGoals) {
		return getSamplingPlan(getVariables(), satisfiedGoals);
	}
	
	public Plan getSamplingPlan(List<? extends Variable> variablesToSample, Set<? extends SamplingGoal> satisfiedGoals) {
		Set<? extends SamplingGoal> requiredGoals = mapIntoSet(variablesToSample, v -> new VariableIsDefinedGoal(v));
		Plan samplingPlan = getValuePossiblyCreatingIt(fromRequiredGoalsAndSatisfiedGoalsToPlan, pair(requiredGoals, satisfiedGoals), this::makePlan);
		return samplingPlan;
	}
	
	private Plan makePlan(Pair<Set<? extends SamplingGoal>, Set<? extends SamplingGoal>> requiredGoalsAndSatisfiedGoals) {
		Set<? extends SamplingGoal> requiredGoals = requiredGoalsAndSatisfiedGoals.first;
		Set<? extends SamplingGoal> satisfiedGoals = requiredGoalsAndSatisfiedGoals.second;
		Collection<? extends SamplingRule> samplingRules = getInputFactorsSamplingRulesUnion();
		Plan plan = planUsingEachRuleAtMostOnce(requiredGoals, satisfiedGoals, set() /* TODO can do better */, samplingRules);
		myAssert(!plan.isFailedPlan(), () -> "Plan for sampling product factor has failed: " + this);
		return plan;
	}
	
	private Collection<? extends SamplingRule> inputFactorsSamplingRulesUnion;
	
	private Collection<? extends SamplingRule> getInputFactorsSamplingRulesUnion() {
		return valueOrDefaultIfNull(inputFactorsSamplingRulesUnion, () -> makeInputFactorsSamplingRulesUnion()); 
	}

	private Collection<? extends SamplingRule> makeInputFactorsSamplingRulesUnion() {
		var samplingRules = union(functionIterator(getInputFactors(), f -> f.getSamplingRuleSet().getSamplingRules()));
		return samplingRules;
	}

	@Override
	public void sampleOrWeigh(Sample sample) {
		sampleOrWeigh(getVariables(), sample);
	}

	public void sampleOrWeigh(List<? extends Variable> variablesToSample, Sample sample) {
		SamplingState samplingState = new SamplingState(sample, getInputFactors(), getRandom());
		Plan samplingPlan = getSamplingPlan(variablesToSample, sample);
		println("Executing sampling plan");
		samplingPlan.execute(samplingState);
		myAssert(complete(sample), () -> "Factor was not able to complete sample.\nSample: " + sample + "\nFactors: " + this + "\nSampling plan: " + (samplingPlan == null? "none" : samplingPlan.nestedString()));
		samplingState.makeSureToConsultAllRelevantInputFactors();
		rewardIfNeeded(sample);
	}

	private void rewardIfNeeded(Sample sample) {
		// Code because is incorrect because it uses the plan for the complete sample, that is, not the sample plan that was used to generate the sample!
//		if (adaptiveSampling && getSamplingPlan(sample) != null) {
//			getSamplingPlan(sample).reward(sample.getPotential().doubleValue());
//		}
	}

	private boolean complete(Sample sample) {
		// TODO: this can be made better than checking every variable at every iteration. We can instead update it according to factor used
		boolean result = forAll(getVariables(), v -> sample.getAssignment().contains(v));
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

	private static ArrayList<? extends SamplingFactor> getInputFactors(SamplingFactor factorKnownToBeSamplingProductFactor) {
		return ((SamplingProductFactor) factorKnownToBeSamplingProductFactor).getInputFactors();
	}

	@Override
	public String toString() {
		return "product(" + join(getInputFactors()) + ")";
	}
}
