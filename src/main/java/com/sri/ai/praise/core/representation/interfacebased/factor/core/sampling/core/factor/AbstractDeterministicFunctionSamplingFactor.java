package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor;

import static com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.schedule.DefaultSamplingRuleSet.samplingRuleSet;
import static com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.schedule.SamplingRule.deterministicSamplingRuleFromVariables;
import static com.sri.ai.util.Util.arrayList;
import static com.sri.ai.util.Util.flatList;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.mapIntoList;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Random;
import java.util.function.Function;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.factor.SamplingFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample.Sample;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.schedule.SamplingRuleSet;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.schedule.SamplingRule;

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

	protected abstract Object evaluateFunction(Function<Variable, Object> fromVariableToValue);

	protected abstract Iterator<? extends Integer> argumentsWithInverseFunctionIterator();

	protected abstract Object computeMissingArgumentValue(Function<Variable, Object> fromVariableToValue, int missingArgumentIndex);

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
			completeFunctionResult(sample);
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
		Object functionOnArguments = evaluateFunction(fromSampleToValueFunction(sample));
		if ( ! functionOnArguments.equals(getValue(functionResult, sample))) {
			sample.updatePotential(sample.getPotential().make(0.0));
		}
	}

	private void completeFunctionResult(Sample sample) {
		Object functionResultValue = evaluateFunction(fromSampleToValueFunction(sample));
		setValue(functionResult, functionResultValue, sample);
	}
	
	private void completeMissingArgument(Sample sample, int missingArgumentIndex) {
		Object missingArgumentValue = computeMissingArgumentValue(fromSampleToValueFunction(sample), missingArgumentIndex);
		setMissingArgumentValue(sample, missingArgumentIndex, missingArgumentValue);
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

	private Object getValue(Variable variable, Sample sample) {
		Object result = sample.getAssignment().get(variable);
		return result;
	}

	@Override
	protected SamplingRuleSet makeSamplingRules() {
		ArrayList<? extends SamplingRule> samplingRulesArrayList = makeSamplingRulesArrayList();
		SamplingRuleSet result = samplingRuleSet(samplingRulesArrayList);
		return result;
	}

	private ArrayList<SamplingRule> makeSamplingRulesArrayList() {
		ArrayList<SamplingRule> samplingRules = arrayList(makeSamplingRuleForFunctionResult());
		mapIntoList(argumentsWithInverseFunctionIterator(), this::makeSamplingRuleForArgumentAt, samplingRules);
		return samplingRules;
	}

	private SamplingRule makeSamplingRuleForFunctionResult() {
		return deterministicSamplingRuleFromVariables(this, list(this.functionResult), arguments);
	}

	private SamplingRule makeSamplingRuleForArgumentAt(int i) {
		List<Variable> otherArgumentsAndFunctionResult = makeListOfOtherArgumentsAndFunctionResult(i);
		SamplingRule samplingRule = deterministicSamplingRuleFromVariables(this, list(arguments.get(i)), otherArgumentsAndFunctionResult);
		return samplingRule;
	}

	//////////////////////

	/**
	 * A convenience protected method for extensions that have sampling rules
	 * for single arguments given all other variables are defined.
	 * @param i
	 * @return
	 */
	protected List<Variable> makeListOfOtherArgumentsAndFunctionResult(int i) {
		List<Variable> otherArgumentsAndFunctionResult = new ArrayList<Variable>(arguments);
		otherArgumentsAndFunctionResult.set(i, functionResult);
		return otherArgumentsAndFunctionResult;
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

}