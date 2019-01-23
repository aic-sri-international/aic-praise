package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor;

import static com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.schedule.SamplingRuleSet.union;
import static com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.schedule.SamplingRule.SUCCESS_COMPARATOR;
import static com.sri.ai.util.Util.collectToArrayList;
import static com.sri.ai.util.Util.collectToList;
import static com.sri.ai.util.Util.forAll;
import static com.sri.ai.util.Util.getValuePossiblyCreatingIt;
import static com.sri.ai.util.Util.intersect;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.map;
import static com.sri.ai.util.Util.mapIntoList;
import static com.sri.ai.util.Util.setFrom;
import static com.sri.ai.util.Util.whileDo;
import static com.sri.ai.util.collect.PredicateIterator.predicateIterator;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Deque;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.Set;

import com.google.common.base.Function;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.factor.SamplingFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample.Sample;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.schedule.SamplingGoal;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.schedule.SamplingRuleSet;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.schedule.SamplingRule;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.schedule.goal.VariableIsDefinedGoal;
import com.sri.ai.util.collect.PredicateIterator;
import com.sri.ai.util.planning.api.Goal;

public class DynamicSamplingProductFactor extends AbstractCompoundSamplingFactor {

	private Deque<Variable> stackOfVariablesWeAreTryingToInstantiate;
	private Set<SamplingFactor> inputFactorsYetToUse;
	
	/////////////////////////////
	
	public DynamicSamplingProductFactor(ArrayList<? extends SamplingFactor> multipliedFactors, Random random) {
		super(multipliedFactors, random);
	}

	/////////////////////////////
	
	@Override
	public void sampleOrWeigh(Sample sample) {
		stackOfVariablesWeAreTryingToInstantiate = list();
		inputFactorsYetToUse = setFrom(getInputFactors());
		makeSureToInstantiate(getVariables(), sample);
		makeSureAllInputFactorsAreExecuted(sample);
	}

	private void makeSureAllInputFactorsAreExecuted(Sample sample) {
		inputFactorsYetToUse.forEach(f -> f.sampleOrWeigh(sample));
	}

	private void makeSureToInstantiate(Collection<? extends Variable> variables, Sample sample) {
		variables.forEach(v -> makeSureToInstantiate(v, sample));
	}

	private void makeSureToInstantiate(Variable variable, Sample sample) throws DynamicSamplingFailureError {
		if ( ! sample.instantiates(variable)) {
			tryToInstantiate(variable, sample);
			if ( ! sample.instantiates(variable)) {
				throw new DynamicSamplingFailureError(variable, sample, this);
			}
		}
	}

	private void tryToInstantiate(Variable variable, Sample sample) {
		
		stackOfVariablesWeAreTryingToInstantiate.push(variable);
		
		whileDo(
				getPrioritizedSamplingRules(variable), 
				/* while */ ( ) -> ! sample.instantiates(variable), 
				/* do    */ (r) -> tryToInstantiateWithRule(r, sample));
		
		stackOfVariablesWeAreTryingToInstantiate.pop();
		
	}

	private void tryToInstantiateWithRule(SamplingRule rule, Sample sample) {
		
		List<Goal> antecedentsRequiringAVariableToBeDefined = getAntecedentVariableGoals(rule);
		
		List<Variable> variablesRequiredByRule = getVariablesCorrespondingToGoals(antecedentsRequiringAVariableToBeDefined);
		
		if (areNonCycleVariables(variablesRequiredByRule)) {
			List<Variable> requiredButUninstantiatedVariables = selectUninstantiatedVariables(variablesRequiredByRule, sample);
			boolean successfullyInstantiated = tryToInstantiate(requiredButUninstantiatedVariables, sample);
			if (successfullyInstantiated) {
				tryToExecuteRuleWithInstantiatedAntecedentVariables(rule, sample);
			}
		}
		
	}

	private boolean areNonCycleVariables(List<Variable> variablesToTryToInstantiate) {
		return ! intersect(variablesToTryToInstantiate, stackOfVariablesWeAreTryingToInstantiate);
	}

	private PredicateIterator<? extends SamplingGoal> getNonVariableIsDefinedGoalAntecedents(SamplingRule rule) {
		return predicateIterator(rule.getAntecendents(), a -> !(a instanceof VariableIsDefinedGoal));
	}

	private List<Variable> selectUninstantiatedVariables(List<Variable> variables, Sample sample) {
		List<Variable> uninstantiatedNonCycleVariables = collectToList(variables, v -> !sample.instantiates(v));
		return uninstantiatedNonCycleVariables;
	}

	private boolean tryToInstantiate(List<Variable> variablesToTryToInstantiate, Sample sample) {
		boolean successfullyInstantiated;
		try {
			makeSureToInstantiate(variablesToTryToInstantiate, sample);
			successfullyInstantiated = true;
		} catch (DynamicSamplingFailureError error) {
			successfullyInstantiated = false;
		}
		return successfullyInstantiated;
	}

	private void tryToExecuteRuleWithInstantiatedAntecedentVariables(SamplingRule rule, Sample sample) {
		boolean remainingAntecedentsAreSatisfied = checkNonVariableIsDefinedGoalAntecedents(rule, sample);
		if (remainingAntecedentsAreSatisfied) {
			executeSamplingRule(rule, sample);
		}
	}

	private boolean checkNonVariableIsDefinedGoalAntecedents(SamplingRule rule, Sample sample) {
		boolean result = forAll(getNonVariableIsDefinedGoalAntecedents(rule), g -> isSatisfied(sample, g));
		return result;
	}

	private void executeSamplingRule(SamplingRule rule, Sample sample) {
		rule.getSamplingFactor().sampleOrWeigh(sample);
		inputFactorsYetToUse.remove(rule.getSamplingFactor());
	}

	///////////////////// Auxiliary
	
	@Override
	public String operatorName() {
		return "product";
	}

	private Map<Variable, Collection<? extends SamplingRule>> fromVariableToPrioritizedSamplingRules = map();

	private Collection<? extends SamplingRule> getPrioritizedSamplingRules(Variable variable) {
		return getValuePossiblyCreatingIt(fromVariableToPrioritizedSamplingRules, variable, this::makePrioritizedSamplingRules);
	}

	private Collection<? extends SamplingRule> makePrioritizedSamplingRules(Variable variable) {
		ArrayList<SamplingRule> prioritizedSamplingRulesForVariable = 
				collectToArrayList(getSamplingRules(), r -> containsAntecedentOn(r, variable));
		prioritizedSamplingRulesForVariable.sort(SUCCESS_COMPARATOR);
		return prioritizedSamplingRulesForVariable;
	}

	private boolean containsAntecedentOn(SamplingRule rule, Variable variable) {
		boolean result = 
				rule.getConsequents().stream()
				.map(c -> ((VariableIsDefinedGoal)c).getVariable())
				.anyMatch(v -> v.equals(variable));
		return result;
	}

	private ArrayList<? extends SamplingRule> getSamplingRules() {
		return getSamplingRuleSet().getSamplingRules();
	}

	@Override
	protected SamplingRuleSet makeSamplingRules() {
		SamplingRuleSet samplingRules = union(mapIntoList(getInputFactors(), SamplingFactor::getSamplingRuleSet));
		return samplingRules;
	}

	private List<Goal> getAntecedentVariableGoals(SamplingRule rule) {
		List<Goal> antecedentsRequiringAVariableToBeDefined = collectToList(rule.getAntecendents(), g -> g instanceof VariableIsDefinedGoal);
		return antecedentsRequiringAVariableToBeDefined;
	}

	private List<Variable> getVariablesCorrespondingToGoals(List<Goal> antecedentsRequiringAVariableToBeDefined) {
		List<Variable> variablesRequiredByRule = mapIntoList(antecedentsRequiringAVariableToBeDefined, fromGoalToVariable());
		return variablesRequiredByRule;
	}

	private Function<Goal, Variable> fromGoalToVariable() {
		return a -> ((VariableIsDefinedGoal)a).getVariable();
	}

	private boolean isSatisfied(Sample sample, SamplingGoal g) {
		boolean result = g.isSatisfied(sample);
		return result;
	}

}
