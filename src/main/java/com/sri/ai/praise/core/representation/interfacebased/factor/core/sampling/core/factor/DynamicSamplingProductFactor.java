package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor;

import static com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.schedule.SamplingRuleSet.union;
import static com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.schedule.SamplingRule.SUCCESS_COMPARATOR;
import static com.sri.ai.util.Util.collectToArrayList;
import static com.sri.ai.util.Util.collectToList;
import static com.sri.ai.util.Util.fold;
import static com.sri.ai.util.Util.forAll;
import static com.sri.ai.util.Util.get;
import static com.sri.ai.util.Util.getFirst;
import static com.sri.ai.util.Util.getValuePossiblyCreatingIt;
import static com.sri.ai.util.Util.intersect;
import static com.sri.ai.util.Util.join;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.map;
import static com.sri.ai.util.Util.mapIntoList;
import static com.sri.ai.util.Util.myAssert;
import static com.sri.ai.util.Util.notContainedBy;
import static com.sri.ai.util.Util.println;
import static com.sri.ai.util.Util.set;
import static com.sri.ai.util.Util.setFrom;
import static com.sri.ai.util.Util.subtract;
import static com.sri.ai.util.Util.thereExists;
import static com.sri.ai.util.Util.union;
import static com.sri.ai.util.Util.whileDo;
import static com.sri.ai.util.collect.FunctionIterator.functionIterator;
import static com.sri.ai.util.collect.PredicateIterator.predicateIterator;
import static com.sri.ai.util.explanation.logging.api.ThreadExplanationLogger.RESULT;
import static com.sri.ai.util.explanation.logging.api.ThreadExplanationLogger.code;
import static com.sri.ai.util.explanation.logging.api.ThreadExplanationLogger.explain;
import static com.sri.ai.util.explanation.logging.api.ThreadExplanationLogger.explainList;
import static com.sri.ai.util.explanation.logging.api.ThreadExplanationLogger.explanationBlock;
import static com.sri.ai.util.explanation.logging.api.ThreadExplanationLogger.getThreadExplanationLogger;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Deque;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.Set;

import com.google.common.base.Function;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.factor.GibbsSamplingFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.factor.SamplingFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample.Potential;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample.Sample;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.schedule.SamplingGoal;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.schedule.SamplingRuleSet;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.sample.DoublePotential;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.schedule.SamplingRule;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.schedule.goal.VariableIsDefinedGoal;
import com.sri.ai.util.collect.PredicateIterator;
import com.sri.ai.util.graph.Graphs;
import com.sri.ai.util.planning.api.Goal;

public class DynamicSamplingProductFactor extends AbstractCompoundSamplingFactor implements GibbsSamplingFactor {

	public static boolean gibbs = false;
	
	// Gibbs sampling variables
	private Potential p;
	private Map<SamplingFactor, Potential> fromFactorToP;
	private Potential q;
	private Map<Variable, Potential> fromVariableToQ;
	
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
		
		if (gibbs) {
			gibbsSampleOrWeight(variablesToSample, sample);
		}
		else {
			monteCarloSampleOrWeigh(variablesToSample, sample);
		}
	}

	public void monteCarloSampleOrWeigh(List<? extends Variable> variablesToSample, Sample sample) {
		
		boolean debug = false;
		long initialTime = System.currentTimeMillis();
		if (debug) {
			println("Sampling DynamicSamplingProductFactor for " + join(variablesToSample) + " starting with " + sample);
			getThreadExplanationLogger().setIsActive(join(variablesToSample).equals("neighbor__person4_1995"));
		}
		
		
		inputFactorsYetToUse = setFrom(getInputFactors());
		
		p = sample.getPotential().one();
		fromFactorToP = map();
		q = sample.getPotential().one();
		// sample potential will be one on first complete sampling
		fromVariableToQ = map();
		
		monteCarloSampleOrWeighWithFactorsYetToUse(variablesToSample, sample);
		
		
		if (debug) {
			println("DynamicSamplingProductFactor time: " + (System.currentTimeMillis() - initialTime) + " ms");
		}
	}

	private void monteCarloSampleOrWeighWithFactorsYetToUse(List<? extends Variable> variablesToSample, Sample sample) {
		stackOfVariablesWeAreTryingToInstantiate = list();
		uninstantiableWithCurrentSample = set();
		makeSureToInstantiate(variablesToSample, sample);
		makeSureAllRelevantInputFactorsAreExecuted(sample);
	}

	private void makeSureAllRelevantInputFactorsAreExecuted(Sample sample) {

		explanationBlock("Making sure all relevant factors are executed", code( () -> {
			
			explanationBlock("Factors yet to use: ", code( () -> {
				for (SamplingFactor factor : inputFactorsYetToUse) {
					explain(factor);
				}
			}));
		
			SamplingFactor factor;
			while ((factor = getSamplingFactorNotUsedYetAndIntersectingSample(sample)) != null) {
				explain("Factor not used yet and relevant to sample:" + factor);
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

				explain("Instantiated factor variables " + factor.getVariables());

				// we need to check again if factor has still not been used, because it might have been used to instantiate one of its variables
				if (inputFactorsYetToUse.contains(factor)) {
					final SamplingFactor finalFactor = factor;
					explanationBlock("Going to evaluate its assignment according to ", factor, code( () -> {

						Potential initialPotential = sample.getPotential();
						sample.setPotential(initialPotential.one());
						finalFactor.sampleOrWeigh(sample); // updates sample weight
						Potential factorPotential = sample.getPotential();
						sample.setPotential(initialPotential);
						sample.updatePotential(factorPotential);

						inputFactorsYetToUse.remove(finalFactor);
						
						explain("Factor potential is ", factorPotential);
						explain("p was     ", p);
						p = p.multiply(factorPotential);
						fromFactorToP.put(finalFactor, factorPotential);
						explain("p now is  ", p);

						// q is not updated because this is not about the proposal distribution being used to general samples
						explain("q remains ", q);

//						myAssert(factorPotential.doubleValue() > 0, () -> "Factor " + finalFactor + " has probability zero for sample " + sample);

					}), "Sample is now evaluated at ", sample);
				}
				else {
					explain("Factor was used to instantiate one of its variables, so we do not need to use its potential");
				}
			}

		}), "Sample now is ", sample);
	}
	
	private SamplingFactor getSamplingFactorNotUsedYetAndIntersectingSample(Sample sample) {
		return getFirst(inputFactorsYetToUse, f -> thereExists(f.getVariables(), sample::instantiates));
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
					tryToExecuteRuleWithInstantiatedAntecedentVariables(rule, variable, sample);
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

	private void tryToExecuteRuleWithInstantiatedAntecedentVariables(SamplingRule rule, Variable variable, Sample sample) {

		explanationBlock("Rule has required instantiated variables: ", rule, " given ", sample, code( () -> {

			boolean remainingAntecedentsAreSatisfied = checkNonVariableIsDefinedGoalAntecedents(rule, sample);
			if (remainingAntecedentsAreSatisfied) {
				executeSamplingRule(rule, variable, sample);
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
	
	private void executeSamplingRule(SamplingRule rule, Variable variable, Sample sample) {

		explanationBlock("Executing rule ", rule, " given ", sample, code( () -> {

			Potential initialPotential = sample.getPotential();
			sample.setPotential(initialPotential.one());
			rule.getSamplingFactor().sampleOrWeigh(sample); // updates sample weight
			Potential factorPotential = sample.getPotential();
			sample.setPotential(initialPotential);
			sample.updatePotential(factorPotential);

			explain("Factor potential is ", factorPotential);
			explain("p was   : ", p);
			explain("q was   : ", q);
			p = p.multiply(factorPotential);
			fromFactorToP.put(rule.getSamplingFactor(), factorPotential);
			q = q.multiply(factorPotential);
			explain("p now is: ", p);
			explain("q now is: ", q);
			// no need to update sample potential as p and q cancel each other
			fromVariableToQ.put(variable, factorPotential);
			explain("Registering q(v) for ", variable, " as ", factorPotential);
			
			inputFactorsYetToUse.remove(rule.getSamplingFactor());

		}), "Sample now is ", sample);
	}

	///////////////////// Auxiliary
	
	@Override
	public String operatorName() {
		return "product";
	}

	private Map<Variable, Collection<? extends SamplingRule>> fromVariableToPrioritizedSamplingRules = map();

	private Collection<? extends SamplingRule> getPrioritizedSamplingRules(Variable variable) {
		
		Collection<? extends SamplingRule> prioritizedSamplingRules = 
				getValuePossiblyCreatingIt(fromVariableToPrioritizedSamplingRules, variable, this::makePrioritizedSamplingRules);

		explainList(
				"Prioritized rules for " + variable,
				mapIntoList(prioritizedSamplingRules, r -> r + " with weight " + r.getEstimatedSuccessWeight()));
		
		return prioritizedSamplingRules;
	}
	
	private Collection<? extends SamplingRule> makePrioritizedSamplingRules(Variable variable) {
		ArrayList<SamplingRule> prioritizedSamplingRulesForVariable = 
				collectToArrayList(getSamplingRules(), r -> containsConsequentOn(r, variable));
		prioritizedSamplingRulesForVariable.sort(SUCCESS_COMPARATOR);
		return prioritizedSamplingRulesForVariable;
	}

	private boolean containsConsequentOn(SamplingRule rule, Variable variable) {
		boolean result = 
				rule.getConsequents().stream()
				.map(c -> ((VariableIsDefinedGoal)c).getVariable())
				.anyMatch(v -> v.equals(variable));
		return result;
	}

	private Collection<? extends SamplingRule> getSamplingRules() {
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
	
	////////////////////////// Gibbs sampling
	
	private Sample conditioningSample;
	private Sample previousSample = null;
	private Collection<Variable> conditionedVariables = list();

	private void gibbsSampleOrWeight(List<? extends Variable> variables, Sample sample) {
		if (previousSample == null || !sample.equals(conditioningSample)) {
			explain("Going to generate first Gibbs sample, starting with sample ", sample);
			conditionedVariables = collectToList(sample.getVariables(), sample::instantiates);
			conditioningSample = sample.copy();
			monteCarloSampleOrWeigh(variables, sample);
			previousSample = sample.copy();
			explain("Generated first Gibbs sample ", sample);
			explain("Conditioned variables are ", conditionedVariables);
		}
		else {
			gibbs(variables, previousSample, conditionedVariables);
			sample.copyToSameInstance(previousSample); // TODO: get rid of this hideousness
		}
	}
	
//	private ArrayList<Double> histo = Util.fill(25, 0.0);
//	private double totalWeight = 0;

	int counter = 0;
	
	@Override
	public void gibbs(List<? extends Variable> variables, Sample sample, Collection<? extends Variable> conditionedVariables) {
		explanationBlock("Gibbs step with previous sample ", sample, " and conditioned variables ", join(conditionedVariables), code( () -> {
			
			Variable variable = pickNonConditionedVariableAtRandom(sample, conditionedVariables);
			
			int initialNumberOfVariables = sample.size();
			uninstantiateVariableAndItsDeterminers(variable, sample, conditionedVariables);
			int finalNumberOfVariables = sample.size();
			explain("Uninstantiated " + (initialNumberOfVariables - finalNumberOfVariables) + " out of " + initialNumberOfVariables + " variables");
			if (finalNumberOfVariables != 0) {
				explain("Got a good case!!!");
			}
			monteCarloSampleOrWeighWithFactorsYetToUse(variables, sample);
			
//			if (counter % 1000 == 0) {
//				println("sample weight: " + sample.getPotential());
//			}
			
			if (p.divide(q).subtract(sample.getPotential()).pow(new DoublePotential(2.0)).doubleValue() > 0.0000000001) {
				println("Disagreement between p/q and sample potential");
				println("p     : " + p);
				println("q     : " + q);
				println("p/q   : " + p.divide(q));
				println("sample: " + sample.getPotential());
				System.exit(0);
			}
			
//			println(sample);
//			
//			double value_5 = (double) sample.get(DefaultExpressionVariable.expressionVariable(parse("food_availability__5")));
//			println("value: " + value_5);
//			if (value_5 > -50.0 && value_5 < 50.0) { 
//				long index = (long) Math.floor((value_5 - (-50.0)) / (100.0/4.0));
//				println("Dynamic  index: " + index);
//				double weight = sample.getPotential().doubleValue();
//				println("Dynamic  weight: " + weight);
//				histo.set((int) index,  histo.get((int) index) + weight);
//				totalWeight += weight;
//				println("totalWeight: " + totalWeight);
//				println("Dynamic:  " + mapIntoList(histo, h -> h/totalWeight));
//			}
			
		}), "After Gibbs step, sample is ", sample);
	}

	private Variable pickNonConditionedVariableAtRandom(Sample sample, Collection<? extends Variable> conditionedVariables) {
		return explanationBlock("Picking non conditioned variable", code( () -> {

			// TODO: this is potentially picking non-conditioned variables that are effectively conditioned if they are a deterministic function of conditioned variables.
			// So we can improve this by detecting those in advance.
			int i = pickNonConditionedVariableIndex(sample, conditionedVariables);
			Variable variable = get(nonConditionedVariables(conditionedVariables, sample), i);
			myAssert(variable != null, () -> "Gibbs sampling needs to uninstantiate some variable in sample but they are all declared to be conditioned (fixed): " + sample);
			return variable;

		}), "Picked ", RESULT);
	}

	private int pickNonConditionedVariableIndex(Sample sample, Collection<? extends Variable> conditionedVariables) {
		int numberOfNonConditionedVariables = sample.size() - conditionedVariables.size();
//		List<Integer> selected = Util.collectIndices(sample.getVariables(), v -> list("food_availability__2", "food_availability__1").contains(v.toString()));
//		int i = Util.pickUniformly(selected, getRandom());
		int i = getRandom().nextInt(numberOfNonConditionedVariables);
		explain("Number of non-conditioned variables: " + numberOfNonConditionedVariables);
		explain("Picked index: " + i);
		explain("Random: " + getRandom());
		return i;
	}

	private PredicateIterator<? extends Variable> nonConditionedVariables(
			Collection<? extends Variable> conditionedVariables, Sample sample) {
		return predicateIterator(sample.getVariables(), notContainedBy(conditionedVariables));
	}

	/////////////////// Uninstantiation
	
	private void uninstantiateVariableAndItsDeterminers(Variable variable, Sample sample, Collection<? extends Variable> conditionedVariables) {
		if ( ! conditionedVariables.contains(variable) && sample.instantiates(variable)) {
			Collection<? extends Variable> determiners = getVariableAndItsDeterministicNeighbors(variable, sample, conditionedVariables);
			explain("Determiners of ", variable, ": ", determiners);
			determiners.forEach(d -> backtrackFactorsForRemoving(d, sample));
			determiners.forEach(d -> backtrackVariable(d, sample));
		}
	}
	
	private Set<Variable> getVariableAndItsDeterministicNeighbors(Variable variable, Sample sample, Collection<? extends Variable> conditionedVariables) {
		return Graphs.component(variable, v -> makeDeterministicNeighborsIterator(v, sample, conditionedVariables)); 
	}

	private Iterator<Variable> makeDeterministicNeighborsIterator(Variable variable, Sample sample, Collection<? extends Variable> conditionedVariables) {
		Collection<? extends SamplingRule> samplingRules = getDeterministicSamplingRulesInvolvingVariable(variable);
		explain("Deterministic rules involving ", variable, ": ", samplingRules);
		Collection<Variable> variables = union(functionIterator(samplingRules, r -> r.getSamplingFactor().getVariables()));
		explain("Union of variables of these rules: ", variables);
		Collection<Variable> variablesMinusConditioned = subtract(variables, conditionedVariables);
		return variablesMinusConditioned.iterator();
	}

	private void backtrackFactorsForRemoving(Variable variable, Sample sample) {
		explanationBlock("Uninstantiating factors for removing ", variable, code( () -> {

		for (SamplingFactor factor : factorsOn(variable)) {
			
			if (factorInfluenceStillRepresentedInSample(factor)) {
				backtrackFactorForRemoving(factor, variable, sample);
			}
		}

		}), "After removal of factors, sample is ", sample);
	}

	private boolean factorInfluenceStillRepresentedInSample(SamplingFactor factor) {
		return ! inputFactorsYetToUse.contains(factor);
	}

	private void backtrackFactorForRemoving(SamplingFactor factor, Variable variable, Sample sample) {
		explanationBlock("Uninstantiating factor ", factor, " for removing ", variable, code( () -> {

			removeFactorPotential(factor, variable, sample);
			inputFactorsYetToUse.add(factor);

		}), "After removal of factor, sample is ", sample);
	}

	public void removeFactorPotential(SamplingFactor factor, Variable variable, Sample sample) {
		explanationBlock("Removing potential of ", factor, " for removing ", variable, code( () -> {

			Potential factorPotential = fromFactorToP.get(factor);
			fromFactorToP.remove(factor);
			explain("Factor has potential ", factorPotential);
			explain("p was    : ", p);
			if (factorPotential.equals(factorPotential.zero())) {
				explain("Multiplying all remaining p's of each factor to obtain total p");
				explain("p's for each factor are ", fromFactorToP);
				p = fold(fromFactorToP.values(), Potential::multiply, factorPotential.one());
				sample.setPotential(p.divide(q));
				explain("Total p is ", p);
				explain("Sample now is ", sample);
			}
			else {
				p = p.divide(factorPotential);
				sample.removePotential(factorPotential);
			}
			explain("p now is : ", p);
			explain("q remains: ", q);

		}), "After removal of potential, sample is ", sample);
	}

	private void backtrackVariable(Variable variable, Sample sample) {
		explanationBlock("Removing variable ", variable, " from sample", code( () -> {

			sample.remove(variable);
			
			Potential qForVariable = fromVariableToQ.get(variable);
			explain("q(v) for ", variable, " is ", qForVariable);
			explain("q was    : ", q);
			q = q.divide(qForVariable);
			explain("q now is : ", q);
			explain("p remains: ", p);
			sample.removePotential(qForVariable.inverse()); // removing inverse (double division) because q is a divisor of sample weight
			fromVariableToQ.remove(variable);
			explain("Unregistering q(v) for ", variable);

		}), "After removal of variable, sample is ", sample);
	}

	private Map<Variable, Collection<? extends SamplingFactor>> fromVariableToSamplingFactors = map();
	
	private Collection<? extends SamplingFactor> factorsOn(Variable variable) {
		return getValuePossiblyCreatingIt(fromVariableToSamplingFactors, variable, this::makeFactorsOnVariable);
	}
	
	private Collection<? extends SamplingFactor> makeFactorsOnVariable(Variable variable) {
		return collectToList(getInputFactors(), f -> f.getVariables().contains(variable));
	}

	private Map<Variable, Collection<? extends SamplingRule>> fromVariableToDeterministicSamplingRulesInvolvingIt = map();
	
	private Collection<? extends SamplingRule> getDeterministicSamplingRulesInvolvingVariable(Variable variable) {
		return getValuePossiblyCreatingIt(fromVariableToDeterministicSamplingRulesInvolvingIt, variable, this::makeDeterministicSamplingRulesInvolvingVariable);
	}
	
	private Collection<? extends SamplingRule> makeDeterministicSamplingRulesInvolvingVariable(Variable variable) {
		ArrayList<SamplingRule> deterministicSamplingRulesInvolvingVariable = 
				collectToArrayList(getSamplingRules(), r -> isDeterministicAndInvolves(r, variable));
		return deterministicSamplingRulesInvolvingVariable;
	}

	private boolean isDeterministicAndInvolves(SamplingRule rule, Variable variable) {
		boolean result =
				rule.isDeterministic()
				&&
				(containsAsAntecedent(rule, variable)
				||
				containsAsConsequent(rule, variable));
		return result;
	}

	private boolean containsAsAntecedent(SamplingRule rule, Variable variable) {
		return rule.getAntecendents().stream()
				.filter(a -> a instanceof VariableIsDefinedGoal)
				.map(c -> ((VariableIsDefinedGoal)c).getVariable())
				.anyMatch(v -> v.equals(variable));
	}

	private boolean containsAsConsequent(SamplingRule rule, Variable variable) {
		return rule.getConsequents().stream()
				.map(c -> ((VariableIsDefinedGoal)c).getVariable())
				.anyMatch(v -> v.equals(variable));
	}

}

