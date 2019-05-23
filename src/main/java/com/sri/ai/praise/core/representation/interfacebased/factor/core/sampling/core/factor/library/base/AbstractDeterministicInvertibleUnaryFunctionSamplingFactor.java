package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor.library.base;

import static com.sri.ai.util.Util.iterator;
import static com.sri.ai.util.Util.list;

import java.util.Collection;
import java.util.Iterator;
import java.util.Random;
import java.util.function.Function;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.schedule.SamplingGoal;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor.AbstractDeterministicFunctionSamplingFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor.SpecificationForFunctionResultSamplingRule;
import com.sri.ai.util.base.NullaryFunction;

/**
 * A specialization of {@link AbstractDeterministicFunctionSamplingFactor} for numeric operators with one argument.
 * 
 * @author braz
 *
 */
public abstract class AbstractDeterministicInvertibleUnaryFunctionSamplingFactor<T> extends AbstractDeterministicFunctionSamplingFactor {

	protected abstract String operatorSymbol();

	protected abstract T operation(T argumentValue);

	protected abstract T computeArgumentFromResult(T functionResultValue);

	protected abstract boolean isValidValue(T value);
	
	////////////////////
	
	private Variable argument;

	////////////////////
	
	public AbstractDeterministicInvertibleUnaryFunctionSamplingFactor(Variable result, Variable argument, Random random) {
		super(result, list(argument), random);
		this.argument = argument;
	}

	////////////////////
	
	@Override
	protected Object evaluateFunctionFromAllArguments(Function<Variable, Object> fromVariableToValue) {
		T argumentValue = getArgumentValue(fromVariableToValue);
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
		T functionResultValue = getResultValue(fromVariableToValue);
		T result = computeArgumentFromResult(functionResultValue);
		return result;
	}

	@SuppressWarnings("unchecked")
	private Object computeWithErrorChecking(Function<Variable, Object> fromVariableToValue, NullaryFunction<Object> calculation) throws Error {
		Object result;
		try {
			result = calculation.apply();
		}
		catch (Throwable e) {
			throw new Error("Error solving " + problemDescription(fromVariableToValue) + ": " + e.getMessage(), e);
		}
		
		if ( ! isValidValue((T) result)) {
			throw new Error("Error solving " + problemDescription(fromVariableToValue) + ": illegal arguments resulting in " + result);
		}
		
		return result;
	}

	////////////////////
	
	private T getResultValue(Function<Variable, Object> fromVariableToValue) {
		@SuppressWarnings("unchecked")
		T resultValue = (T) fromVariableToValue.apply(getFunctionResultVariable());
		return resultValue;
	}

	private T getArgumentValue(Function<Variable, Object> fromVariableToValue) {
		@SuppressWarnings("unchecked")
		T baseValue = (T) fromVariableToValue.apply(argument);
		return baseValue;
	}

	////////////////////
	
	protected String problemDescription(Function<Variable, Object> fromVariableToValue) {
		return 
				valueOrVariable(getFunctionResultVariable(), fromVariableToValue) 
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

	@Override
	protected Iterator<SpecificationForFunctionResultSamplingRule> specificationsForShortCircuitingSamplingRules() {
		return iterator();
	}

	@Override
	protected Collection<? extends SamplingGoal> conditionsForInverseOfArgument(int i) {
		return list();
	}
}