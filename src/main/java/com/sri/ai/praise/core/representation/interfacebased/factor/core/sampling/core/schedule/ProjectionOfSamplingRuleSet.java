package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.schedule;

import static com.sri.ai.util.Util.arrayList;
import static com.sri.ai.util.Util.arrayListFrom;
import static com.sri.ai.util.Util.intersect;
import static com.sri.ai.util.Util.mapIntoArrayList;
import static com.sri.ai.util.Util.mapIntoList;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Set;

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
public class ProjectionOfSamplingRuleSet {

	
	// TODO:
	// Disregard remainingGoals as given.
	// Take all goals from all sampling rules,
	// then use goal-projection on remaining goals that include all contingent goals,
	// that is, that excludes VariableIsDefined for eliminated variables.
	// The idea is that the lower-level projection algorithm should have contingent goals in the resulting rules
	// even if they depend on marginalized variables, since they cannot be be assumed to always be obtainable.
	// Then we replace them in the projection rules by lucky variables so that they don't depend on the instantiation
	// of marginalized variables, which won't be available when projected rules are employed.
	// This is not a simple algorithm; there is a careful argument to be made about its correctness.
	
	/**
	 * Projects this set of sampling rules into a new one restricted to a set of remaining goals
	 * @param remainingGoals the goals to remain in the final set of sampling rules
	 * @param factorToBeUsedAsSamplerOfProjectedSamplingRules
	 * @param originalFactor the sampling factor from which the original set of sampling rules comes from
	 * @return
	 */
	public static SamplingRuleSet project(
			List<? extends SamplingGoal> remainingGoals,
			SamplingFactor factorToBeUsedAsSamplerOfProjectedSamplingRules,
			SamplingFactor originalFactor) {
		
		Set<? extends SamplingRule> projectedSamplingRules =
				projectSamplingRules(remainingGoals, factorToBeUsedAsSamplerOfProjectedSamplingRules, originalFactor);
		DefaultSamplingRuleSet result = new DefaultSamplingRuleSet(new ArrayList<>(projectedSamplingRules));
		return result;
	}

	private static Set<? extends SamplingRule> projectSamplingRules(
			List<? extends SamplingGoal> remainingGoals,
			SamplingFactor factorToBeUsedAsSamplerOfProjectedSamplingRules,
			SamplingFactor originalFactor) {
		
		// TODO: this needs to be:
		// get all sampling rules from original factor
		// select remaining goals as all goals minus the VariableIsDefined goals for the marginalized variables
		// transform contingent goals into lucky goals
		
		ArrayList<? extends SamplingRule> samplingRulesForProjection = getSamplingRulesForProjection(originalFactor);
		List<? extends SamplingGoal> remainingGoalsForProjection = getRemainingGoalsForProjection(remainingGoals, originalFactor);
		Set<? extends SamplingRule> projectedSamplingRules =
				getProjectedSamplingRules(
						remainingGoalsForProjection,
						samplingRulesForProjection,
						factorToBeUsedAsSamplerOfProjectedSamplingRules);
		return projectedSamplingRules;
	}

	///////////////////// PROJECT
	
	private static Set<? extends SamplingRule> getProjectedSamplingRules(
			List<? extends SamplingGoal> remainingGoalsForProjection,
			ArrayList<? extends SamplingRule> samplingRulesForProjection,
			SamplingFactor factorToBeUsedAsSamplerOfProjectedSamplingRules) {
		
		ProjectionOfSetOfRules<SamplingRule, SamplingGoal> projectionOfSetOfRules = 
				new ProjectionOfSetOfRules<>(
						samplingRulesForProjection,
						remainingGoalsForProjection,
						(consequent, antecedents) -> 
						new SamplingRule(factorToBeUsedAsSamplerOfProjectedSamplingRules, arrayList(consequent), arrayListFrom(antecedents), 0.5));
		Set<? extends SamplingRule> projectedSamplingRules = projectionOfSetOfRules.getProjectedSetOfRules();
		return projectedSamplingRules;
	}
	
	///////////////////// COMPUTE SAMPLING RULES TO BE PROJECTED
	
	private static ArrayList<? extends SamplingRule> getSamplingRulesForProjection(SamplingFactor originalFactor) {
		Collection<? extends SamplingRule> samplingRules = originalFactor.getSamplingRuleSet().getSamplingRules();
		ArrayList<? extends SamplingRule> result = mapIntoArrayList(samplingRules, g -> replaceByLucky(g, originalFactor));
		return result;
	}
	
	private static SamplingRule replaceByLucky(SamplingRule rule, SamplingFactor originalFactor) {
		SamplingRule result = rule.replaceGoals(g -> replaceByLucky(g, originalFactor));
		return result;
	}
	
	///////////////////// COMPUTE REMAINING GOALS
	
	private static List<? extends SamplingGoal> getRemainingGoalsForProjection(List<? extends SamplingGoal> remainingGoals, SamplingFactor originalFactor) {
		return mapIntoList(remainingGoals, g -> replaceByLucky(g, originalFactor));
	}

	private static SamplingGoal replaceByLucky(SamplingGoal goal, SamplingFactor originalFactor) {
		SamplingGoal result;
		if (isContingent(goal) && intersect(goal.getVariables(), originalFactor.getVariables())) {
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

}
