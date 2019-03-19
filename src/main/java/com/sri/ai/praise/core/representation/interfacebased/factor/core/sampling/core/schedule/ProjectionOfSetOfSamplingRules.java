package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.schedule;

import static com.sri.ai.util.Util.arrayList;
import static com.sri.ai.util.Util.arrayListFrom;
import static com.sri.ai.util.Util.collectToList;
import static com.sri.ai.util.Util.mapIntoArrayList;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Set;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.factor.SamplingFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.schedule.SamplingGoal;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.schedule.SamplingRuleSet;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.schedule.goal.LuckySamplingGoal;
import com.sri.ai.util.planning.api.ContingentGoal;
import com.sri.ai.util.planning.core.ProjectionOfSetOfRules;

/**
 * A utility class with a method for projecting a set of sampling rules into a sub set of their goals.
 * 
 * @author braz
 *
 */
public class ProjectionOfSetOfSamplingRules {

	// High-level explanation of strategy in this class:
	// We replace the original sampling rule set in the original factor
	// by a set of sampling rules in which each contingent goal that depends on marginalized (eliminated) variables
	// is replaced by a {@link LuckyGoal}.
	// This is needed because the eliminated variables must not be apparent since they will be eliminated by marginalization
	// even if at run time they are still important to decide whether to use a sub-plan.
	// These goals obtained after this transformation are called "final goals".
	// We also define remaining final goals to be the contingent ones plus the static goals on the remaining variables.
	// Then we project the set of rules (they are sampling rules but this step applies to any rules, not just sampling ones)
	// on the set of remaining final goals.
	// This set of sampling rules on remaining final goals is then wrapped up in a sampling rule set and returned.
	
	/**
	 * Projects this set of sampling rules into a new one restricted to a set of remaining goals
	 * @param factorToBeUsedAsSamplerOfProjectedSamplingRules
	 * @param originalFactor the sampling factor from which the original set of sampling rules comes from
	 * @return
	 */
	public static SamplingRuleSet project(
			List<? extends Variable> remainingVariables,
			SamplingFactor factorToBeUsedAsSamplerOfProjectedSamplingRules,
			SamplingFactor originalFactor) {
		
		Set<? extends SamplingRule> projectedSamplingRules =
				projectSamplingRules(remainingVariables, factorToBeUsedAsSamplerOfProjectedSamplingRules, originalFactor);
		DefaultSamplingRuleSet result = new DefaultSamplingRuleSet(new ArrayList<>(projectedSamplingRules));
		return result;
	}

	private static Set<? extends SamplingRule> projectSamplingRules(
			List<? extends Variable> remainingVariables,
			SamplingFactor factorToBeUsedAsSamplerOfProjectedSamplingRules, 
			SamplingFactor originalFactor) {
		
		ArrayList<? extends SamplingRule> samplingRulesWithFinalGoals = getSamplingRulesWithFinalGoals(remainingVariables, originalFactor);
		List<? extends SamplingGoal> remainingFinalGoals = getRemainingFinalGoals(remainingVariables, originalFactor);
		Set<? extends SamplingRule> projectedSamplingRulesWithFinalGoals =
				getProjectedSamplingRules(
						remainingFinalGoals,
						samplingRulesWithFinalGoals,
						factorToBeUsedAsSamplerOfProjectedSamplingRules);
		return projectedSamplingRulesWithFinalGoals;
	}

	///////////////////// COMPUTE SAMPLING RULES WITH FINAL GOALS
	
	private static ArrayList<? extends SamplingRule> 
	getSamplingRulesWithFinalGoals(
			List<? extends Variable> remainingVariables, 
			SamplingFactor originalFactor) {
		
		Collection<? extends SamplingRule> samplingRules = originalFactor.getSamplingRuleSet().getSamplingRules();
		ArrayList<? extends SamplingRule> result = mapIntoArrayList(samplingRules, r -> replaceByLucky(r, remainingVariables, originalFactor));
		return result;
	}
	
	private static SamplingRule
	replaceByLucky(
			SamplingRule rule, 
			List<? extends Variable> remainingVariables, 
			SamplingFactor originalFactor) {
		
		SamplingRule result = rule.replaceGoals(g -> replaceByLucky(g, remainingVariables, originalFactor));
		return result;
	}

	private static SamplingGoal 
	replaceByLucky(
			SamplingGoal goal, 
			List<? extends Variable> remainingVariables, 
			SamplingFactor originalFactor) {
		
		SamplingGoal result;
		if (isContingent(goal) && ! isCompletelyContainedInRemainingVariables(goal, remainingVariables)) {
			result = new LuckySamplingGoal(goal, originalFactor);
		}
		else {
			result = goal;
		}
		return result;
	}

	private static boolean isContingent(SamplingGoal goal) {
		return goal instanceof ContingentGoal;
	}
	
	///////////////////// COMPUTE REMAINING FINAL GOALS
	
	private static 
	List<? extends SamplingGoal> 
	getRemainingFinalGoals(List<? extends Variable> remainingVariables, SamplingFactor originalFactor) {
		return collectToList(
				originalFactor.getSamplingRuleSet().getAllGoals(), 
				g -> (g instanceof ContingentGoal) || isCompletelyContainedInRemainingVariables(g, remainingVariables));
	}

	///////////////////// PROJECT
	
	private static Set<? extends SamplingRule> getProjectedSamplingRules(
			List<? extends SamplingGoal> remainingFinalGoals,
			ArrayList<? extends SamplingRule> samplingRulesWithFinalGoals,
			SamplingFactor factorToBeUsedAsSamplerOfProjectedSamplingRules) {
		
		ProjectionOfSetOfRules<SamplingRule, SamplingGoal> projectionOfSetOfRules = 
				new ProjectionOfSetOfRules<>(
						samplingRulesWithFinalGoals,
						remainingFinalGoals,
						(consequent, antecedents) -> 
						new SamplingRule(
								factorToBeUsedAsSamplerOfProjectedSamplingRules, 
								arrayList(consequent), 
								arrayListFrom(antecedents), 
								0.5)); // TODO: can we do better than just use 0.5 here?
		
		Set<? extends SamplingRule> projectedSamplingRules = projectionOfSetOfRules.getProjectedSetOfRules();
		return projectedSamplingRules;
	}
	
	////////////////////// UTIL
	
	private static boolean isCompletelyContainedInRemainingVariables(
			SamplingGoal goal, 
			List<? extends Variable> remainingVariables) {
		
		return remainingVariables.containsAll(goal.getVariables());
	}

}
