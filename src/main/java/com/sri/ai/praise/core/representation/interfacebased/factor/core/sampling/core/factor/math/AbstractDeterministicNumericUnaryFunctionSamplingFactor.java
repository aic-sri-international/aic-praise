package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor.math;

import static com.sri.ai.util.Util.iterator;
import static com.sri.ai.util.Util.list;

import java.util.Iterator;
import java.util.Random;
import java.util.function.Function;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor.AbstractDeterministicFunctionSamplingFactor;
import com.sri.ai.util.base.NullaryFunction;

/**
 * A specialization of {@link AbstractDeterministicFunctionSamplingFactor} for numeric operators with one argument.
 * 
 * @author braz
 *
 */
public abstract class AbstractDeterministicNumericUnaryFunctionSamplingFactor
		extends AbstractDeterministicFunctionSamplingFactor {

	protected abstract String operatorSymbol();

	protected abstract double operation(Double argumentValue);

	protected abstract double computeArgumentFromResult(Double functionResultValue);

	////////////////////
	
	private Variable argument;

	////////////////////
	
	public AbstractDeterministicNumericUnaryFunctionSamplingFactor(Variable result, Variable argument, Random random) {
		super(result, list(argument), random);
		this.argument = argument;
	}

	////////////////////
	
	@Override
	protected Object evaluateFunctionFromAllArguments(Function<Variable, Object> fromVariableToValue) {
		Double argumentValue = getArgumentValue(fromVariableToValue);
		Object result = computeWithErrorChecking(fromVariableToValue, () -> operation(argumentValue));
		return result;
	}

	@Override
	protected Iterator<? extends Integer> argumentsWithInverseFunctionIterator() {
		return iterator(0); 
	}

	@Override
	protected Object computeMissingArgumentValue(Function<Variable, Object> fromVariableToValue, int missingArgumentIndex) {
		Object result = 
				computeWithErrorChecking(
						fromVariableToValue, 
						() -> computeMissingArgumentValueWithoutOperationErrorChecking(
								fromVariableToValue, 
								missingArgumentIndex));
		return result;
	}

	private Object computeMissingArgumentValueWithoutOperationErrorChecking(Function<Variable, Object> fromVariableToValue, int missingArgumentIndex) throws Error {
		if (missingArgumentIndex == 0) {
			return computeArgumentFromResult(fromVariableToValue);
		}
		else {
			throw new Error("computeMissingArgumentValue got invalid argument index " + missingArgumentIndex + " while solving" + problemDescription(fromVariableToValue));
		}
	}

	private Object computeArgumentFromResult(Function<Variable, Object> fromVariableToValue) {
		Double functionResultValue = getResultValue(fromVariableToValue);
		double result = computeArgumentFromResult(functionResultValue);
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
		
		if (Double.isNaN((Double) result) || Double.isInfinite((Double) result)) {
			throw new Error("Error solving " + problemDescription(fromVariableToValue) + ": illegal arguments resulting in " + result);
		}
		
		return result;
	}

	////////////////////
	
	private Double getResultValue(Function<Variable, Object> fromVariableToValue) {
		Double resultValue = (Double) fromVariableToValue.apply(getFunctionResult());
		return resultValue;
	}

	private Double getArgumentValue(Function<Variable, Object> fromVariableToValue) {
		Double baseValue = (Double) fromVariableToValue.apply(argument);
		return baseValue;
	}

	////////////////////
	
	private String problemDescription(Function<Variable, Object> fromVariableToValue) {
		return 
				valueOrVariable(getFunctionResult(), fromVariableToValue) 
				+ " = " 
				+ operatorSymbol()
				+ valueOrVariable(argument, fromVariableToValue); 
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

}