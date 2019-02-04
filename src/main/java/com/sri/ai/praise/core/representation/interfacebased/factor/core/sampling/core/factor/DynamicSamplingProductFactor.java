package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor;

import static com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.schedule.SamplingRuleSet.union;
import static com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.schedule.SamplingRule.SUCCESS_COMPARATOR;
import static com.sri.ai.util.Util.collectToArrayList;
import static com.sri.ai.util.Util.collectToList;
import static com.sri.ai.util.Util.forAll;
import static com.sri.ai.util.Util.getFirst;
import static com.sri.ai.util.Util.getFirstSatisfyingPredicateOrNull;
import static com.sri.ai.util.Util.getValuePossiblyCreatingIt;
import static com.sri.ai.util.Util.intersect;
import static com.sri.ai.util.Util.join;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.map;
import static com.sri.ai.util.Util.mapIntoList;
import static com.sri.ai.util.Util.println;
import static com.sri.ai.util.Util.set;
import static com.sri.ai.util.Util.setFrom;
import static com.sri.ai.util.Util.thereExists;
import static com.sri.ai.util.Util.whileDo;
import static com.sri.ai.util.collect.PredicateIterator.predicateIterator;
import static com.sri.ai.util.explanation.logging.api.ThreadExplanationLogger.code;
import static com.sri.ai.util.explanation.logging.api.ThreadExplanationLogger.explain;
import static com.sri.ai.util.explanation.logging.api.ThreadExplanationLogger.explanationBlock;
import static com.sri.ai.util.explanation.logging.api.ThreadExplanationLogger.getThreadExplanationLogger;

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
	private Set<Variable> uninstantiableWithCurrentSample;
	
	/////////////////////////////
	
	public DynamicSamplingProductFactor(ArrayList<? extends SamplingFactor> multipliedFactors, Random random) {
		super(multipliedFactors, random);
	}

	/////////////////////////////
	
	@Override
	public void sampleOrWeigh(Sample sample) {
		sampleOrWeigh(getVariables(), sample);
	}

	/**
	 * Samples or weight only a subset of variables (others may be sampled for supporting requested variables).
	 * @param variablesToSample
	 * @param sample
	 */
	public void sampleOrWeigh(List<? extends Variable> variablesToSample, Sample sample) {
		boolean debug = false;
		long initialTime = System.currentTimeMillis();
		if (debug) {
			println("Sampling DynamicSamplingProductFactor for " + join(variablesToSample) + " starting with " + sample);
			getThreadExplanationLogger().setIsActive(join(variablesToSample).equals("neighbor__bob_1993"));
		}
		stackOfVariablesWeAreTryingToInstantiate = list();
		uninstantiableWithCurrentSample = set();
		inputFactorsYetToUse = setFrom(getInputFactors());
		makeSureToInstantiate(variablesToSample, sample);
		makeSureAllRelevantInputFactorsAreExecuted(sample);
		if (debug) {
			println("DynamicSamplingProductFactor time: " + (System.currentTimeMillis() - initialTime) + " ms");
		}
	}

	private void makeSureAllRelevantInputFactorsAreExecuted(Sample sample) {

		explanationBlock("Making sure all relevant factors are executed", code( () -> {
		
			while ( ! inputFactorsYetToUse.isEmpty()) {
				SamplingFactor factor = getFirst(inputFactorsYetToUse);
				inputFactorsYetToUse.remove(factor);
				Variable commonVariable = getFirstSatisfyingPredicateOrNull(factor.getVariables(), sample::instantiates);
				if (commonVariable != null) {
					explain("Common variable " + commonVariable + " between sample and factor " + factor);
					tryToInstantiate(factor.getVariables(), sample);
					// the factor may need a variable that was not requested to be instantiated
					// so we try to instantiate them all here in case it needs it
					// Note that we are just trying because it may not be possible to instantiate the variable
					// AND it may not be needed.
					// In cases when they are both needed but there is no way to instantiate,
					// there will be a justified error.
					// One inefficiency here is that we may be instantiating more than
					// is needed.
					// TODO improve sampling factors API so that the factor requests instantiation itself if needed


					factor.sampleOrWeigh(sample);
				}
			}

		}), "Sample now is ", sample);
	}

	private void tryToInstantiate(Collection<? extends Variable> variables, Sample sample) {

		explanationBlock("Trying to instantiate ", variables, " given ", sample, code( () -> {

			variables.forEach(v -> tryToInstantiate(v, sample));

		}), "Sample now is ", sample);
	}

	private void makeSureToInstantiate(Collection<? extends Variable> variables, Sample sample) {

		explanationBlock("Make sure to instantiate ", variables, " given ", sample, code( () -> {

			variables.forEach(v -> makeSureToInstantiate(v, sample));

		}), "Sample now is ", sample);
	}

	private void makeSureToInstantiate(Variable variable, Sample sample) throws DynamicSamplingFailureError {

		explanationBlock("Making sure to instantiate ", variable, " given ", sample, code( () -> {

			if ( ! sample.instantiates(variable)) {
				tryToInstantiate(variable, sample);
				if ( ! sample.instantiates(variable)) {
					throw new DynamicSamplingFailureError(variable, sample, this);
				}
			}

		}), "Sample now is ", sample);
	}

	private void tryToInstantiate(Variable variable, Sample sample) {

		explanationBlock("Trying to instantiate ", variable, " given ", sample, code( () -> {
			
			if (uninstantiableWithCurrentSample.contains(variable)) {
				explain("Already found to not be instantiable with current sample");
				return;
			}

			stackOfVariablesWeAreTryingToInstantiate.push(variable);

			whileDo(
					getPrioritizedSamplingRules(variable), 
					/* while */ ( ) -> ! sample.instantiates(variable), 
					/* do    */ (r) -> tryToInstantiateWithRule(variable, r, sample));

			stackOfVariablesWeAreTryingToInstantiate.pop();
			
			if ( ! sample.instantiates(variable)) {
				explain(variable + " recorded as not instantiable under current sample");
				uninstantiableWithCurrentSample.add(variable);
			}
			else {
				// sample has changed, so discard memory of uninstantiable variables
				uninstantiableWithCurrentSample = set();
			}

		}), "Sample now is ", sample);
	}

	private void tryToInstantiateWithRule(Variable variable, SamplingRule rule, Sample sample) {

		explanationBlock("Trying to instantiate ", variable, " with rule ", rule, " given ", sample, code( () -> {

			List<Goal> antecedentsRequiringAVariableToBeDefined = getAntecedentVariableGoals(rule);

			List<Variable> variablesRequiredByRule = getVariablesCorrespondingToGoals(antecedentsRequiringAVariableToBeDefined);

			if (areNonCycleVariables(variablesRequiredByRule)) {
				List<Variable> requiredButUninstantiatedVariables = selectUninstantiatedVariables(variablesRequiredByRule, sample);
				boolean successfullyInstantiated = tryToInstantiate(requiredButUninstantiatedVariables, sample);
				if (successfullyInstantiated) {
					tryToExecuteRuleWithInstantiatedAntecedentVariables(rule, sample);
					if (thereExists(sample.getAssignment().mapValue().values(), v -> v instanceof Double && (Double.isNaN((double) v) || Double.isInfinite((double) v)))) {
						throw new Error("Invalid double");
					}
				}
				else {
					explain("Could not instantiate required variables ", requiredButUninstantiatedVariables);
				}
			}

		}), "Sample now is ", sample);
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

		explanationBlock("Rule has required instantiated variables: ", rule, " given ", sample, code( () -> {

			boolean remainingAntecedentsAreSatisfied = checkNonVariableIsDefinedGoalAntecedents(rule, sample);
			if (remainingAntecedentsAreSatisfied) {
				executeSamplingRule(rule, sample);
			}
			else {
				explain("Conditions not satisfied for rule " + rule);
			}

		}), "Sample now is ", sample);
	}

	private boolean checkNonVariableIsDefinedGoalAntecedents(SamplingRule rule, Sample sample) {
		boolean result = forAll(getNonVariableIsDefinedGoalAntecedents(rule), g -> isSatisfied(sample, g));
		return result;
	}

	private void executeSamplingRule(SamplingRule rule, Sample sample) {

		explanationBlock("Executing rule ", rule, " given ", sample, code( () -> {

			rule.getSampler().sampleOrWeigh(sample);
			if (rule.getSampler() instanceof SamplingFactor) {
				inputFactorsYetToUse.remove(rule.getSampler());
			}
			// the above is the reason for why samplers that are not sampling factors may not
			// change the potential of the sample;
			// We need to make sure each factor changes the potential only once,
			// but more than one sampling rule in it could potentially fire,
			// so they are not supposed to do that.
			// TODO: change class hierarchy so that sampling rule samplers
			// becomes some type of object that does not even see the sample potential.

		}), "Sample now is ", sample);
	}

	///////////////////// Auxiliary
	
	@Override
	public String operatorName() {
		return "product";
	}

	private Map<Variable, Collection<? extends SamplingRule>> fromVariableToPrioritizedSamplingRules = map();

	private Collection<? extends SamplingRule> getPrioritizedSamplingRules(Variable variable) {
		// println("Size of sampling rules map: " + fromVariableToPrioritizedSamplingRules.size());
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
