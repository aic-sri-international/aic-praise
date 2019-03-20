package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.schedule;

import static com.sri.ai.util.Util.arrayList;
import static com.sri.ai.util.Util.arrayListFrom;
import static com.sri.ai.util.Util.collectToList;
import static com.sri.ai.util.Util.mapIntoArrayList;
import static com.sri.ai.util.explanation.logging.api.ThreadExplanationLogger.RESULT;
import static com.sri.ai.util.explanation.logging.api.ThreadExplanationLogger.code;
import static com.sri.ai.util.explanation.logging.api.ThreadExplanationLogger.explain;
import static com.sri.ai.util.explanation.logging.api.ThreadExplanationLogger.explainList;
import static com.sri.ai.util.explanation.logging.api.ThreadExplanationLogger.explanationBlock;

import java.util.ArrayList;
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
	 * @param newFactor
	 * @param originalFactor the sampling factor from which the original set of sampling rules comes from
	 * @return
	 */
	public static SamplingRuleSet project(
			List<? extends Variable> remainingVariables,
			SamplingFactor newFactor,
			SamplingFactor originalFactor) {
		
		return explanationBlock("Computing projection of sampling rules (expand to see arguments)", code(() -> {
			
			explain("Remaining variables: ", remainingVariables);
			explain("New factor         : ", newFactor);
			explain("Original factor    : ", originalFactor);
			explain("Remaining variables: ", remainingVariables);
			explainList("Original sampling rules", originalFactor.getSamplingRuleSet().getSamplingRules());

			var setOfSamplingRulesWithFinalGoals = getSamplingRulesWithFinalGoals(remainingVariables, originalFactor);
			var remainingFinalGoals = getRemainingFinalGoals(remainingVariables, setOfSamplingRulesWithFinalGoals);
			var projectedSamplingRulesWithFinalGoals = getProjectedSamplingRules(remainingFinalGoals, setOfSamplingRulesWithFinalGoals, newFactor);
			var result = new DefaultSamplingRuleSet(new ArrayList<>(projectedSamplingRulesWithFinalGoals));

			explainList("Final sampling rules", result.getSamplingRules());
			
			return result;

		}), "Computed projection of sampling rules (open block to see result)");
	}

	///////////////////// COMPUTE SAMPLING RULES WITH FINAL GOALS
	
	private static SamplingRuleSet
	getSamplingRulesWithFinalGoals(
			List<? extends Variable> remainingVariables, 
			SamplingFactor originalFactor) {
		
		return explanationBlock("Computing sampling rules with final goals", code(() -> {

			explainList("Original sampling rules", originalFactor.getSamplingRuleSet().getSamplingRules());

			var samplingRules = originalFactor.getSamplingRuleSet().getSamplingRules();
			var result = mapIntoArrayList(samplingRules, r -> replaceByLucky(r, remainingVariables, originalFactor));

			explainList("Sampling rules with final goals", result);

			return new DefaultSamplingRuleSet(result);

		}), "Computed sampling rules with final goals (open block to see result)");
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
	getRemainingFinalGoals(List<? extends Variable> remainingVariables, SamplingRuleSet setOfSamplingRulesWithFinalGoals) {
		
		return explanationBlock("Computing remaining goals", code(() -> {

			explainList("All goals", setOfSamplingRulesWithFinalGoals.getAllGoals());
			
			var result = 
					collectToList(
							setOfSamplingRulesWithFinalGoals.getAllGoals(), 
							g -> (g instanceof ContingentGoal) || isCompletelyContainedInRemainingVariables(g, remainingVariables));
			return result;

		}), "Remaining goals are ", RESULT);

	}

	///////////////////// PROJECT
	
	private static Set<? extends SamplingRule> getProjectedSamplingRules(
			List<? extends SamplingGoal> remainingFinalGoals,
			SamplingRuleSet setOfSamplingRulesWithFinalGoals,
			SamplingFactor newFactor) {

		return explanationBlock("Computing projection of rules with final goals (expand to see arguments)", code(() -> {

			explainList("Remaining final goals", remainingFinalGoals);
			explainList("Sampling rules with final goals", setOfSamplingRulesWithFinalGoals.getSamplingRules());

			var samplingRulesArrayList = new ArrayList<>(setOfSamplingRulesWithFinalGoals.getSamplingRules());
			ProjectionOfSetOfRules<SamplingRule, SamplingGoal> projectionOfSetOfRules = 
					new ProjectionOfSetOfRules<>(
							samplingRulesArrayList,
							remainingFinalGoals,
							(consequent, antecedents) -> 
							new SamplingRule(newFactor, arrayList(consequent), arrayListFrom(antecedents), 0.5));
			// TODO: can we do better than just use 0.5 here?

			Set<? extends SamplingRule> projectedSamplingRules = projectionOfSetOfRules.getProjectedSetOfRules();

			explainList("Projection of sampling rules with final goals", projectedSamplingRules);

			return projectedSamplingRules;

		}), "Computed projection of rules with final goals (open block to see result)");
	}
	
	////////////////////// UTIL
	
	private static boolean isCompletelyContainedInRemainingVariables(SamplingGoal goal, List<? extends Variable> remainingVariables) {
		return remainingVariables.containsAll(goal.getVariables());
	}

}
