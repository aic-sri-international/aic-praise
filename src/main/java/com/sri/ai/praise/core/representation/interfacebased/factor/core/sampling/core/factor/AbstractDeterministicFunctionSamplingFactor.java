package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor;

import static com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.schedule.DefaultSamplingRuleSet.samplingRuleSet;
import static com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.schedule.SamplingRule.deterministicSamplingRuleFromGoals;
import static com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.schedule.SamplingRule.deterministicSamplingRuleFromVariables;
import static com.sri.ai.util.Util.arrayList;
import static com.sri.ai.util.Util.collectThoseWhoseIndexSatisfy;
import static com.sri.ai.util.Util.flatList;
import static com.sri.ai.util.Util.forAll;
import static com.sri.ai.util.Util.iterator;
import static com.sri.ai.util.Util.join;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.mapIntoList;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Random;
import java.util.function.Function;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.factor.SamplingFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample.Sample;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.schedule.SamplingGoal;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.schedule.SamplingRuleSet;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.schedule.SamplingRule;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.schedule.goal.VariableIsDefinedGoal;

/**
 * An abstract implementation of a {@link SamplingFactor} based on a deterministic function.
 * <p>
 * To use it, implement method {@link #evaluateFunction(Function<Variable, Object> fromVariableToValue)}
 * by using the values for the arguments provided by the given function.
 * <p>
 * Additionally, if the function has inverse forms for some arguments, this can be provided
 * by overriding methods {@link #argumentsWithInverseFunctionIterator()} to provide the indices
 * of these arguments, then {@link #computeMissingArgumentValue(Function<Variable, Object> fromVariableToValue, int missingArgumentIndex)}
 * to compute its value based on the values stored in the given sample.
 * <p>
 * Even though it is possible to have inverses that compute multiple variables
 * based on the instantiations of the others, this class does not currently support that.
 * It only supports inverses on a single argument.
 * 
 * @author braz
 *
 */
public abstract class AbstractDeterministicFunctionSamplingFactor extends AbstractSamplingFactor {

	protected abstract Object evaluateFunctionFromAllArguments(Function<Variable, Object> fromVariableToValue);

	/**
	 * A protected method that gives extensions a chance to define further ways of computing the function result
	 * other than just plain computing it from all arguments.
	 * For example, z = x*y can be computed from x alone if x is zero.
	 * In such case, this method can return a specification indicating the index of x, the condition that it is zero,
	 * and the estimated success weight of such a sampling operation.
	 * @return
	 */
	protected Iterator<SpecificationForFunctionResultSamplingRule> specificationsForFunctionResultSamplingRules() {
		return iterator();
	}

	protected abstract Iterator<? extends Integer> argumentsWithInverseFunctionIterator();

	protected abstract Object computeMissingArgumentValue(Function<Variable, Object> fromVariableToValue, int missingArgumentIndex);

	protected abstract String getFunctionName();

	/**
	 * Provides the contingent conditions for application of the inverse for a given argument,
	 * in addition to the other variables being defined.
	 * For example, in multiplication w = x*y*z, we can compute z from w, z, y but only if the product x*y is not zero.
	 * Default is empty list. 
	 * @param i
	 * @return
	 */
	protected Collection<? extends SamplingGoal> conditionsForInverseOfArgument(int i) {
		return list();
	}

	//////////////////////
	
	private Variable functionResult;

	private List<? extends Variable> arguments;

	//////////////////////
	
	public AbstractDeterministicFunctionSamplingFactor(Variable result, List<? extends Variable> arguments, Random random) {
		super(flatList(result, arguments), random);
		this.functionResult = result;
		this.arguments = arguments;
	}

	//////////////////////
	
	@Override
	public void sampleOrWeigh(Sample sample) {
		
		// completeFunctionResultFromPossiblyPartialAssignmentIfPossible(sample);
	
		int missingArgumentIndex = analyzeMissingArguments(sample);
		
		if (missingArgumentIndex == -1) {
			allArgumentsDefined(sample);
		}
		else if (missingArgumentIndex == -2) {
			moreThanOneArgumentIsUndefined();
		}
		else {
			exactlyOneMissingArgument(sample, missingArgumentIndex);
		}
	}

	/**
	 * Returns -1 if all arguments are defined, -2 if more than one argument is undefined,
	 * and the index of the unique undefined argument otherwise.
	 * @param sample
	 * @return
	 */
	private int analyzeMissingArguments(Sample sample) {
		int result = -1; // all arguments so far are defined
		int i = 0;
		for (Variable argument : arguments) {
			if (isNotDefined(argument, sample)) {
				if (result == -1) { // this is the first undefined argument, so write down its index
					result = i; 
				}
				else { // we have seen an undefined argument before, so there is more than one, break and return -2
					result = -2;
					break;
				}
			}
			i++;
		}
		return result;
	}

	private void allArgumentsDefined(Sample sample) {
		if (isDefined(functionResult, sample)) {
			checkConsistencyOfFullyDefinedVariables(sample);
		}
		else {
			completeFunctionResultWhenAllArgumentsAreDefined(sample);
		}
	}

	private void moreThanOneArgumentIsUndefined() {
		// do nothing; problem is underspecified
	}

	private void exactlyOneMissingArgument(Sample sample, int missingArgumentIndex) {
		if (isDefined(functionResult, sample)) {
			completeMissingArgument(sample, missingArgumentIndex);
		}
		else {
			// do nothing; problem is underspecified
		}
	}

	private void checkConsistencyOfFullyDefinedVariables(Sample sample) {
		Object functionOnArguments = evaluateFunctionFromAllArguments(fromSampleToValueFunction(sample));
		if ( ! functionOnArguments.equals(getValue(functionResult, sample))) {
			sample.updatePotential(sample.getPotential().make(0.0));
		}
	}

	private void completeFunctionResultWhenAllArgumentsAreDefined(Sample sample) {
		Object functionResultValue = evaluateFunctionFromAllArguments(fromSampleToValueFunction(sample));
		setValue(functionResult, functionResultValue, sample);
	}
	
	private void completeMissingArgument(Sample sample, int missingArgumentIndex) {
		if (forAll(conditionsForInverseOfArgument(missingArgumentIndex), g -> g.isSatisfied(sample))) {
			Object missingArgumentValue = computeMissingArgumentValue(fromSampleToValueFunction(sample), missingArgumentIndex);
			setMissingArgumentValue(sample, missingArgumentIndex, missingArgumentValue);
		}
	}

	private void setMissingArgumentValue(Sample sample, int missingArgumentIndex, Object missingArgumentValue) {
		Variable missingArgument = arguments.get(missingArgumentIndex);
		setValue(missingArgument, missingArgumentValue, sample);
	}

	private boolean isDefined(Variable variable, Sample sample) {
		boolean result = getValue(variable, sample) != null;
		return result;
	}

	private boolean isNotDefined(Variable variable, Sample sample) {
		boolean result = getValue(variable, sample) == null;
		return result;
	}

	private void setValue(Variable variable, Object value, Sample sample) {
		sample.getAssignment().set(variable, value);
	}

	@Override
	protected SamplingRuleSet makeSamplingRules() {
		ArrayList<? extends SamplingRule> samplingRulesArrayList = makeSamplingRulesArrayList();
		SamplingRuleSet result = samplingRuleSet(samplingRulesArrayList);
		return result;
	}

	private ArrayList<SamplingRule> makeSamplingRulesArrayList() {
		ArrayList<SamplingRule> samplingRules = arrayList();
		addSamplingRulesForFunctionResult(samplingRules);
		addSamplingRulesForArguments(samplingRules);
		return samplingRules;
	}

	private void addSamplingRulesForFunctionResult(ArrayList<SamplingRule> samplingRules) {
		samplingRules.add(makeSamplingRuleForFunctionResultBasedOnAllArguments());
		mapIntoList(specificationsForFunctionResultSamplingRules(), this::makeSamplingRuleFromSpecificationForFunctionResultSamplingRule, samplingRules);
	}

	private SamplingRule makeSamplingRuleForFunctionResultBasedOnAllArguments() {
		return deterministicSamplingRuleFromVariables(this, list(this.functionResult), arguments);
	}

	private SamplingRule makeSamplingRuleFromSpecificationForFunctionResultSamplingRule(SpecificationForFunctionResultSamplingRule specification) {
		List<SamplingGoal> antecedents = list();
		mapIntoList(specification.argumentsIndices, i -> new VariableIsDefinedGoal(getArguments().get(i)), antecedents);
		antecedents.addAll(specification.goalsBesidesArgumentsBeingDefined);
		LinkedList<VariableIsDefinedGoal> consequents = list(new VariableIsDefinedGoal(getFunctionResult()));
		SamplingRule result = new SamplingRule(specification.sampler, consequents, antecedents, specification.estimatedSuccessWeight);
		return result;
	}

	private void addSamplingRulesForArguments(ArrayList<SamplingRule> samplingRules) {
		mapIntoList(argumentsWithInverseFunctionIterator(), this::makeSamplingRuleForArgumentAt, samplingRules);
	}

	private SamplingRule makeSamplingRuleForArgumentAt(int i) {
		List<SamplingGoal> antecedents = getAntecedentsForSamplingRuleForArgument(i);
		SamplingGoal iThArgumentIsDefined = getConsequentForSamplingRuleForArgument(i);
		SamplingRule samplingRule = deterministicSamplingRuleFromGoals(this, list(iThArgumentIsDefined), antecedents);
		return samplingRule;
	}

	private List<SamplingGoal> getAntecedentsForSamplingRuleForArgument(int i) {
		List<SamplingGoal> antecedents = list();
		antecedents.addAll(makeGoalsForFunctionResultAndOtherArgumentsBeingDefined(i));
		antecedents.addAll(conditionsForInverseOfArgument(i));
		return antecedents;
	}

	private List<SamplingGoal> makeGoalsForFunctionResultAndOtherArgumentsBeingDefined(int i) {
		List<Variable> functionResultAndArgumentsOtherThanI = makeListOfFunctionResultAndArgumentsOtherThan(i);
		List<SamplingGoal> goalsForFunctionResultAndArgumentsOtherThanI = mapIntoList(functionResultAndArgumentsOtherThanI, v -> new VariableIsDefinedGoal(v));
		return goalsForFunctionResultAndArgumentsOtherThanI;
	}

	private SamplingGoal getConsequentForSamplingRuleForArgument(int i) {
		SamplingGoal iThArgumentIsDefined = new VariableIsDefinedGoal(arguments.get(i));
		return iThArgumentIsDefined;
	}

	//////////////////////

	/**
	 * A convenience protected method for extensions that have sampling rules
	 * for single arguments given all other variables are defined.
	 * @param i
	 * @return
	 */
	protected List<Variable> makeListOfFunctionResultAndArgumentsOtherThan(int i) {
		List<Variable> otherArgumentsAndFunctionResult = new ArrayList<Variable>(arguments);
		otherArgumentsAndFunctionResult.set(i, functionResult);
		return otherArgumentsAndFunctionResult;
	}
	
	/**
	 * A convenience protected method providing all arguments but the one with given index.
	 * @param i
	 * @return
	 */
	protected List<Variable> getArgumentsOtherThan(int i) {
		List<Variable> otherArguments = collectThoseWhoseIndexSatisfy(getArguments(), j -> 
		{ 
			return j != i;
		});
		return otherArguments;
	}
	
	//////////////////////

	private Function<Variable, Object> fromSampleToValueFunction(Sample sample) {
		return v -> sample.getAssignment().get(v);
	}

	public Variable getFunctionResult() {
		return functionResult;
	}

	public List<? extends Variable> getArguments() {
		return arguments;
	}
	
	public Variable getArgument(int i) {
		return getArguments().get(i);
	}
	
	//////////////////////

	@Override
	public String toString() {
		return getFunctionResult() + " = " + getFunctionName() + "(" + join(getArguments()) + ")";
	}
}