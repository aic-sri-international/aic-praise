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

	private List<? extends VariableGoal> allVariables;
	
	public static DefaultSamplingRuleSet samplingRuleSet(List<? extends Variable> allVariables, ArrayList<? extends SamplingRule> samplingRules) {
		return new DefaultSamplingRuleSet(makeSureItsListOfVariableGoals(allVariables), samplingRules);
	}

	public DefaultSamplingRuleSet(List<? extends Variable> allVariables, SamplingRule... samplingRules) {
		this(makeSureItsListOfVariableGoals(allVariables), new ArrayList<>(Arrays.asList(samplingRules)));
	}

	public DefaultSamplingRuleSet(List<? extends VariableGoal> allVariables, ArrayList<? extends SamplingRule> samplingRules) {
		this.samplingRules = samplingRules;
		this.allVariables = allVariables;
	}

	@Override
	public ArrayList<? extends SamplingRule> getSamplingRules() {
		return samplingRules;
	}

	@Override
	public List<? extends VariableGoal> getVariables() {
		return allVariables;
	}

	@Override
	public DefaultSamplingRuleSet replaceFactor(SamplingFactor samplingFactor) {
		ArrayList<? extends SamplingRule> newRules = mapIntoArrayList(getSamplingRules(), r -> r.replaceFactor(samplingFactor));
		DefaultSamplingRuleSet result = new DefaultSamplingRuleSet(getVariables(), newRules);
		return result;
	}

	@Override
	public SamplingRuleSet sumOut(List<? extends Variable> summedOutVariables, SamplingFactor factorOnResultingRules) {

		List<? extends VariableGoal> summedOutVariableGoals = makeSureItsListOfVariableGoals(summedOutVariables);
		
		List<VariableGoal> remainingVariablesAsGoals = subtract(getVariables(), summedOutVariableGoals);
		
		DefaultSamplingRuleSet result = makeMarginalizedSamplingRules(summedOutVariableGoals,  remainingVariablesAsGoals, factorOnResultingRules);
		
		return result;
	}

	@SuppressWarnings("unchecked")
	private static List<? extends VariableGoal> makeSureItsListOfVariableGoals(List<? extends Variable> variables) {
		if (forAll(variables, v -> v instanceof Goal)) {
			return (List<? extends VariableGoal>) variables;
		}
		else {
			List<VariableGoal> variablesAsGoals = mapIntoArrayList(variables, wrapAsGoal());
			return variablesAsGoals;
		}
	}

	private static Function<Variable, VariableGoal> wrapAsGoal() {
		return v -> v instanceof Goal? (VariableGoal) v : new VariableGoal(v);
	}

	private ProjectionOfSetOfRules<SamplingRule, VariableGoal> getMarginalizer(List<? extends VariableGoal> remainingVariablesAsGoals) {
		return new ProjectionOfSetOfRules<>(getSamplingRules(), remainingVariablesAsGoals, makeSamplingRuleFactory());
	}

	private DefaultSamplingRuleSet makeMarginalizedSamplingRules(
			List<? extends VariableGoal> summedOutVariableGoals,
			List<VariableGoal> remainingVariablesAsGoals, 
			SamplingFactor factorOnResultingRules) {
		
		ProjectionOfSetOfRules<SamplingRule, VariableGoal> marginalizer = getMarginalizer(remainingVariablesAsGoals);
		Set<? extends SamplingRule> marginalizedSamplingRules = marginalizer.getProjectedSetOfRules();
		DefaultSamplingRuleSet result = new DefaultSamplingRuleSet(remainingVariablesAsGoals, new ArrayList<>(marginalizedSamplingRules));
		result = result.replaceFactor(factorOnResultingRules);
		return result;
	}

	private BinaryFunction<VariableGoal, Set<? extends VariableGoal>, SamplingRule> makeSamplingRuleFactory() {
		return (VariableGoal consequent, Set<? extends VariableGoal> antecedents) -> {
			return new SamplingRule(null, arrayList(consequent), arrayListFrom(antecedents), 0.5);
		};
	}

	@Override
	public String toString() {
		return join(getSamplingRules());
	}
}
