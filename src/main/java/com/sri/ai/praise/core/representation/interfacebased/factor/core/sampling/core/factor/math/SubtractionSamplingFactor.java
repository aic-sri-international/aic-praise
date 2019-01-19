package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor.math;

import static com.sri.ai.util.Util.iterator;
import static com.sri.ai.util.Util.list;

import java.util.Iterator;
import java.util.List;
import java.util.Random;
import java.util.function.Function;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor.AbstractDeterministicFunctionSamplingFactor;

/**
 * An extension of {@link AbstractDeterministicFunctionSamplingFactor} for subtraction.
 *
 * @author braz
 *
 */
public class SubtractionSamplingFactor extends AbstractDeterministicFunctionSamplingFactor {

	private Variable minuend;
	private Variable subtrahend;
	
	public SubtractionSamplingFactor(Variable result, List<? extends Variable> arguments, Random random) {
		super(result, arguments, random);
		this.minuend = arguments.get(0);
		this.subtrahend = arguments.get(1);
	}

	public SubtractionSamplingFactor(Variable result, Variable minuend, Variable subtrahend, Random random) {
		this(result, list(minuend, subtrahend), random);
	}

	@Override
	protected Object evaluateFunction(Function<Variable, Object> fromVariableToValue) {
		Double minuendValue = getMinuendValue(fromVariableToValue);
		Double subtrahendValue = getSubtrahendValue(fromVariableToValue);
		Double result = minuendValue - subtrahendValue;
		return result;
	}

	@Override
	protected Iterator<? extends Integer> argumentsWithInverseFunctionIterator() {
		return iterator(0, 1); 
	}

	@Override
	protected Object computeMissingArgumentValue(Function<Variable, Object> fromVariableToValue, int missingArgumentIndex) {
		if (missingArgumentIndex == 0) {
			return computeMinuendFromOthers(fromVariableToValue);
		}
		else if (missingArgumentIndex == 1) {
			return computeSubtrahendFromOthers(fromVariableToValue);
		}
		else {
			throw new Error("computeMissingArgumentValue got invalid index " + missingArgumentIndex + " for subtraction argument.");
		}
	}

	private Object computeMinuendFromOthers(Function<Variable, Object> fromVariableToValue) {
		Double subtrahendValue = getSubtrahendValue(fromVariableToValue);
		Double resultValue = getResultValue(fromVariableToValue);
		return resultValue + subtrahendValue;
	}

	private Object computeSubtrahendFromOthers(Function<Variable, Object> fromVariableToValue) {
		Double minuendValue = getMinuendValue(fromVariableToValue);
		Double resultValue = getResultValue(fromVariableToValue);
		return resultValue + minuendValue;
	}

	/////////////////////
	
	private Double getResultValue(Function<Variable, Object> fromVariableToValue) {
		Double resultValue = (Double) fromVariableToValue.apply(getFunctionResult());
		return resultValue;
	}

	private Double getMinuendValue(Function<Variable, Object> fromVariableToValue) {
		Double minuendValue = (Double) fromVariableToValue.apply(minuend);
		return minuendValue;
	}

	private Double getSubtrahendValue(Function<Variable, Object> fromVariableToValue) {
		Double subtrahendValue = (Double) fromVariableToValue.apply(subtrahend);
		return subtrahendValue;
	}

}