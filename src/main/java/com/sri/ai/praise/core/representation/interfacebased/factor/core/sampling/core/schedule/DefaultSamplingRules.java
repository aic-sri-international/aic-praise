package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.schedule;

import static com.sri.ai.util.Util.arrayList;
import static com.sri.ai.util.Util.arrayListFrom;
import static com.sri.ai.util.Util.forAll;
import static com.sri.ai.util.Util.mapIntoArrayList;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Set;

import com.google.common.base.Function;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.factor.SamplingFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.schedule.SamplingRules;
import com.sri.ai.util.base.BinaryFunction;
import com.sri.ai.util.planning.api.Goal;
import com.sri.ai.util.planning.core.RuleMarginalizer;

public class DefaultSamplingRules implements SamplingRules {

	private ArrayList<? extends SamplingRule> samplingRules;
	
	public DefaultSamplingRules(SamplingRule... samplingRules) {
		this(new ArrayList<>(Arrays.asList(samplingRules)));
	}

	public DefaultSamplingRules(ArrayList<? extends SamplingRule> samplingRules) {
		this.samplingRules = samplingRules;
	}

	@Override
	public ArrayList<? extends SamplingRule> getSamplingRules() {
		return samplingRules;
	}

	@Override
	public SamplingRules replaceFactor(SamplingFactor samplingFactor) {
		ArrayList<? extends SamplingRule> newRules = mapIntoArrayList(getSamplingRules(), r -> r.replaceFactor(samplingFactor));
		DefaultSamplingRules result = new DefaultSamplingRules(newRules);
		return result;
	}

	@Override
	public SamplingRules sumOut(List<? extends Variable> variables, SamplingFactor factorOnResultingRules) {

		List<VariableGoal> variablesAsGoals = makeSureItsListOfVariableGoals(variables);
		
		RuleMarginalizer<SamplingRule, VariableGoal> marginalizer =  
				new RuleMarginalizer<>(getSamplingRules(), variablesAsGoals, makeSamplingRuleFactory());
		
		Set<? extends SamplingRule> marginalized = marginalizer.marginalize();
		DefaultSamplingRules result = new DefaultSamplingRules(new ArrayList<>(marginalized));
		return result;
	}

	@SuppressWarnings("unchecked")
	private List<VariableGoal> makeSureItsListOfVariableGoals(List<? extends Variable> variables) {
		if (forAll(variables, v -> v instanceof Goal)) {
			return (List<VariableGoal>) variables;
		}
		else {
			List<VariableGoal> variablesAsGoals = mapIntoArrayList(variables, wrapAsGoal());
			return variablesAsGoals;
		}
	}

	public Function<Variable, VariableGoal> wrapAsGoal() {
		return v -> v instanceof Goal? (VariableGoal) v : new VariableGoal(v);
	}

	private BinaryFunction<VariableGoal, Set<? extends VariableGoal>, SamplingRule> makeSamplingRuleFactory() {
		return (VariableGoal consequent, Set<? extends VariableGoal> antecedents) -> {
			return new SamplingRule(null, arrayList(consequent), arrayListFrom(antecedents), 0.5);
		};
	}

}
