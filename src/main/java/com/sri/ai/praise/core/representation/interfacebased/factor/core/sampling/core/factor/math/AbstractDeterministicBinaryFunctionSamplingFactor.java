package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor.math;

import static com.sri.ai.util.Util.iterator;

import java.util.Iterator;
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
public abstract class AbstractDeterministicBinaryFunctionSamplingFactor
		extends AbstractDeterministicFunctionSamplingFactor {

	protected abstract String operatorSymbol();

	protected abstract double operation(Double firstValue, Double secondValue);

	protected abstract double computeFirstFromOthers(Double secondValue, Double functionResultValue);

	protected abstract double computeSecondFromOthers(Double firstValue, Double functionResultValue);

	////////////////////
	
	private Variable first;
	private Variable second;

	////////////////////
	
	public AbstractDeterministicBinaryFunctionSamplingFactor(Variable result, List<? extends Variable> arguments,
			Random random) {
		super(result, arguments, random);
		this.first = arguments.get(0);
		this.second = arguments.get(1);
	}

	////////////////////
	
	@Override
	protected Object evaluateFunction(Function<Variable, Object> fromVariableToValue) {
		Double firstValue = getFirstValue(fromVariableToValue);
		Double secondValue = getSecondValue(fromVariableToValue);
		Object result = computeWithErrorChecking(fromVariableToValue, () -> operation(firstValue, secondValue));
		return result;
	}

	@Override
	protected Iterator<? extends Integer> argumentsWithInverseFunctionIterator() {
		return iterator(0, 1); 
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
		Double secondValue = getSecondValue(fromVariableToValue);
		Double functionResultValue = getResultValue(fromVariableToValue);
		double result = computeFirstFromOthers(secondValue, functionResultValue);
		return result;
	}

	private Object computeSecondFromOthers(Function<Variable, Object> fromVariableToValue) {
		Double firstValue = getFirstValue(fromVariableToValue);
		Double functionResultValue = getResultValue(fromVariableToValue);
		double result = computeSecondFromOthers(firstValue, functionResultValue);
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
		
		if (Double.isNaN((Double) result)) {
			throw new Error("Error solving " + problemDescription(fromVariableToValue) + ": illegal arguments resulting in NaN.");
		}
		return result;
	}

	////////////////////
	
	private Double getResultValue(Function<Variable, Object> fromVariableToValue) {
		Double resultValue = (Double) fromVariableToValue.apply(getFunctionResult());
		return resultValue;
	}

	private Double getFirstValue(Function<Variable, Object> fromVariableToValue) {
		Double baseValue = (Double) fromVariableToValue.apply(first);
		return baseValue;
	}

	private Double getSecondValue(Function<Variable, Object> fromVariableToValue) {
		Double exponentValue = (Double) fromVariableToValue.apply(second);
		return exponentValue;
	}

	////////////////////
	
	private String problemDescription(Function<Variable, Object> fromVariableToValue) {
		return 
				valueOrVariable(getFunctionResult(), fromVariableToValue) 
				+ " = " 
				+ valueOrVariable(first, fromVariableToValue) 
				+ operatorSymbol() 
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

}