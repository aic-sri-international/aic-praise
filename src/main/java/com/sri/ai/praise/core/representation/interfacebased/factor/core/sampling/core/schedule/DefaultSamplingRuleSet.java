package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.schedule;

import static com.sri.ai.util.Util.arrayList;
import static com.sri.ai.util.Util.arrayListFrom;
import static com.sri.ai.util.Util.forAll;
import static com.sri.ai.util.Util.join;
import static com.sri.ai.util.Util.mapIntoArrayList;
import static com.sri.ai.util.Util.subtract;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Set;

import com.google.common.base.Function;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.factor.SamplingFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.schedule.SamplingRuleSet;
import com.sri.ai.util.base.BinaryFunction;
import com.sri.ai.util.planning.api.Goal;
import com.sri.ai.util.planning.core.ProjectionOfSetOfRules;

public class DefaultSamplingRuleSet implements SamplingRuleSet {

	private ArrayList<? extends SamplingRule> samplingRules;

	private List<? extends Goal> allGoals;
	
	public static DefaultSamplingRuleSet samplingRuleSet(List<? extends Variable> allVariables, ArrayList<? extends SamplingRule> samplingRules) {
		return new DefaultSamplingRuleSet(makeSureItsListOfVariableGoals(allVariables), samplingRules);
	}

	public DefaultSamplingRuleSet(List<? extends Variable> allVariables, SamplingRule... samplingRules) {
		this(makeSureItsListOfVariableGoals(allVariables), new ArrayList<>(Arrays.asList(samplingRules)));
	}

	public DefaultSamplingRuleSet(List<? extends Goal> allVariables, ArrayList<? extends SamplingRule> samplingRules) {
		this.samplingRules = samplingRules;
		this.allGoals = allVariables;
	}

	@Override
	public ArrayList<? extends SamplingRule> getSamplingRules() {
		return samplingRules;
	}

	@Override
	public List<? extends Goal> getAllGoals() {
		return allGoals;
	}

	@Override
	public DefaultSamplingRuleSet replaceFactor(SamplingFactor samplingFactor) {
		ArrayList<? extends SamplingRule> newRules = mapIntoArrayList(getSamplingRules(), r -> r.replaceFactor(samplingFactor));
		DefaultSamplingRuleSet result = new DefaultSamplingRuleSet(getAllGoals(), newRules);
		return result;
	}

	@Override
	public SamplingRuleSet sumOut(List<? extends Variable> summedOutVariables, SamplingFactor factorOnResultingRules) {

		List<? extends Goal> summedOutVariableGoals = makeSureItsListOfVariableGoals(summedOutVariables);
		
		List<Goal> remainingVariablesAsGoals = subtract(getAllGoals(), summedOutVariableGoals);
		
		DefaultSamplingRuleSet result = makeMarginalizedSamplingRules(summedOutVariableGoals,  remainingVariablesAsGoals, factorOnResultingRules);
		
		return result;
	}

	@SuppressWarnings("unchecked")
	private static List<? extends Goal> makeSureItsListOfVariableGoals(List<? extends Variable> variables) {
		if (forAll(variables, v -> v instanceof Goal)) {
			return (List<? extends Goal>) variables;
		}
		else {
			List<Goal> variablesAsGoals = mapIntoArrayList(variables, wrapAsGoal());
			return variablesAsGoals;
		}
	}

	private static Function<Variable, Goal> wrapAsGoal() {
		return v -> v instanceof Goal? (Goal) v : new VariableIsDefinedGoal(v);
	}

	private ProjectionOfSetOfRules<SamplingRule, Goal> getMarginalizer(List<? extends Goal> remainingVariablesAsGoals) {
		return new ProjectionOfSetOfRules<>(getSamplingRules(), remainingVariablesAsGoals, makeSamplingRuleFactory());
	}

	private DefaultSamplingRuleSet makeMarginalizedSamplingRules(
			List<? extends Goal> summedOutVariableGoals,
			List<Goal> remainingVariablesAsGoals, 
			SamplingFactor factorOnResultingRules) {
		
		ProjectionOfSetOfRules<SamplingRule, Goal> marginalizer = getMarginalizer(remainingVariablesAsGoals);
		Set<? extends SamplingRule> marginalizedSamplingRules = marginalizer.getProjectedSetOfRules();
		DefaultSamplingRuleSet result = new DefaultSamplingRuleSet(remainingVariablesAsGoals, new ArrayList<>(marginalizedSamplingRules));
		result = result.replaceFactor(factorOnResultingRules);
		return result;
	}

	private BinaryFunction<Goal, Set<? extends Goal>, SamplingRule> makeSamplingRuleFactory() {
		return (Goal consequent, Set<? extends Goal> antecedents) -> {
			return new SamplingRule(null, arrayList(consequent), arrayListFrom(antecedents), 0.5);
		};
	}

	@Override
	public String toString() {
		return join(getSamplingRules());
	}
}
