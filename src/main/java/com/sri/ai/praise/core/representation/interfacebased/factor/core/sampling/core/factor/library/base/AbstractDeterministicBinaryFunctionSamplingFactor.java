package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor.library.base;

import java.util.List;
import java.util.Random;
import java.util.function.Function;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor.AbstractDeterministicFunctionSamplingFactor;
import com.sri.ai.util.base.NullaryFunction;

/**
 * A specialization of {@link AbstractDeterministicFunctionSamplingFactor} for operators with two arguments.
 * 
 * @author braz
 *
 */
public abstract class AbstractDeterministicBinaryFunctionSamplingFactor<A, B, R>
		extends AbstractDeterministicFunctionSamplingFactor {
	
	protected abstract R fromSampleValueToFunctionAppropriateResultValue(Object resultSampleValueObject);

	protected abstract A fromSampleValueToFunctionAppropriateFirstValue(Object firstArgumentSampleValueObject);

	protected abstract B fromSampleValueToFunctionAppropriateSecondValue(Object secondArgumentSampleValueObject);

	protected abstract R operation(A firstValue, B secondValue);

	protected abstract A computeFirstFromOthers(B secondValue, R functionResultValue);

	protected abstract B computeSecondFromOthers(A firstValue, R functionResultValue);

	abstract protected boolean isInvalidFunctionResult(R value);

	abstract protected boolean isInvalidFirstArgument(A value);

	abstract protected boolean isInvalidSecondArgument(B value);

	////////////////////
	
	private Variable first;
	private Variable second;

	////////////////////
	
	public AbstractDeterministicBinaryFunctionSamplingFactor(Variable result, List<? extends Variable> arguments, Random random) {
		super(result, arguments, random);
		this.first = arguments.get(0);
		this.second = arguments.get(1);
	}

	////////////////////
	
	public Variable getFirst() {
		return first;
	}

	public Variable getSecond() {
		return second;
	}

	////////////////////
	
	@SuppressWarnings("unchecked")
	@Override
	protected Object evaluateFunctionFromAllArguments(Function<Variable, Object> fromVariableToValue) {
		A firstValue = getFirstValue(fromVariableToValue);
		B secondValue = getSecondValue(fromVariableToValue);
		Object result = computeWithErrorChecking(fromVariableToValue, () -> operation(firstValue, secondValue));
		check(isInvalidFunctionResult((R)result), getFunctionResultVariable(), result, fromVariableToValue);
		return result;
	}

	@Override
	protected Object computeMissingArgumentValue(Function<Variable, Object> fromVariableToValue, int missingArgumentIndex) {
		Object result = 
				computeWithErrorChecking(
						fromVariableToValue, 
						() -> computeMissingArgumentValueWithoutOperationErrorChecking(
								fromVariableToValue, 
								missingArgumentIndex));
		
		checkArgument(missingArgumentIndex, result, fromVariableToValue);
		
		return result;
	}

	private Object computeMissingArgumentValueWithoutOperationErrorChecking(Function<Variable, Object> fromVariableToValue, int missingArgumentIndex) throws Error {
		if (missingArgumentIndex == 0) {
			return computeFirstFromOthers(fromVariableToValue);
		}
		else if (missingArgumentIndex == 1) {
			return computeSecondFromOthers(fromVariableToValue);
		}
		else {
			throw new Error("computeMissingArgumentValue got invalid argument index " + missingArgumentIndex + " while solving" + problemDescription(fromVariableToValue));
		}
	}

	private Object computeFirstFromOthers(Function<Variable, Object> fromVariableToValue) {
		B secondValue = getSecondValue(fromVariableToValue);
		R functionResultValue = getResultValue(fromVariableToValue);
		A result = computeFirstFromOthers(secondValue, functionResultValue);
		return result;
	}

	private Object computeSecondFromOthers(Function<Variable, Object> fromVariableToValue) {
		A firstValue = getFirstValue(fromVariableToValue);
		R functionResultValue = getResultValue(fromVariableToValue);
		B result = computeSecondFromOthers(firstValue, functionResultValue);
		return result;
	}

	private Object computeWithErrorChecking(Function<Variable, Object> fromVariableToValue, NullaryFunction<Object> calculation) throws Error {
		Object result;
		try {
			result = calculation.apply();
		}
		catch (Throwable e) {
			throw new Error("Error solving " + problemDescription(fromVariableToValue) + ": " + e.getMessage(), e);
		}
		
		return result;
	}

	////////////////////
	
	private R getResultValue(Function<Variable, Object> fromVariableToValue) {
		Object sampleValueObject = fromVariableToValue.apply(getFunctionResultVariable());
		R resultValue = fromSampleValueToFunctionAppropriateResultValue(sampleValueObject);
		return resultValue;
	}

	private A getFirstValue(Function<Variable, Object> fromVariableToValue) {
		Object sampleValueObject = fromVariableToValue.apply(getFirst());
		A firstValue = fromSampleValueToFunctionAppropriateFirstValue(sampleValueObject);
		return firstValue;
	}

	private B getSecondValue(Function<Variable, Object> fromVariableToValue) {
		Object sampleValueObject = fromVariableToValue.apply(getSecond());
		B secondValue = fromSampleValueToFunctionAppropriateSecondValue(sampleValueObject);
		return secondValue;
	}

	////////////////////
	
	private String problemDescription(Function<Variable, Object> fromVariableToValue) {
		return 
				valueOrVariable(getFunctionResultVariable(), fromVariableToValue) 
				+ " = " 
				+ valueOrVariable(first, fromVariableToValue) 
				+ getFunctionName() 
				+ valueOrVariable(second, fromVariableToValue);
	}

	private Object valueOrVariable(Variable variable, Function<Variable, Object> fromVariableToValue) {
		Object value = fromVariableToValue.apply(variable);
		if (value == null) {
			return variable;
		}
		else {
			return value;
		}
	}

	@SuppressWarnings("unchecked")
	private void checkArgument(int missingArgumentIndex, Object value, Function<Variable, Object> fromVariableToValue) {
		if (missingArgumentIndex == 0) {
			check(isInvalidFirstArgument((A) value), getArguments().get(0), value, fromVariableToValue);
		}
		else {
			check(isInvalidSecondArgument((B) value), getArguments().get(1), value, fromVariableToValue);
		}
	}

	private void check(boolean isInvalid, Variable variable, Object checkedValue, Function<Variable, Object> fromVariableToValue) {
		if (isInvalid) {
			throw new Error("Error solving " + problemDescription(fromVariableToValue) + ": illegal arguments resulting in " + variable + " = " + checkedValue);
		}
	}

	@Override
	public String toString() {
		return getFunctionResultVariable() + " = (" + getFirst() + " " + getFunctionName() + " " + getSecond() + ")";
	}
}