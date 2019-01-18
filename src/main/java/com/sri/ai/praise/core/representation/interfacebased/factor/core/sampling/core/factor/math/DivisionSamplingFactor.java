package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor.math;

import static com.sri.ai.util.Util.iterator;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.myAssert;

import java.util.Iterator;
import java.util.List;
import java.util.Random;
import java.util.function.Function;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor.AbstractDeterministicFunctionSamplingFactor;

/**
 * An extension of {@link AbstractDeterministicFunctionSamplingFactor} for division.
 *
 * @author braz
 *
 */
public class DivisionSamplingFactor extends AbstractDeterministicFunctionSamplingFactor {

	private Variable dividend;
	private Variable divisor;
	
	public DivisionSamplingFactor(Variable result, List<? extends Variable> arguments, Random random) {
		super(result, arguments, random);
		this.dividend = arguments.get(0);
		this.divisor = arguments.get(1);
	}

	public DivisionSamplingFactor(Variable result, Variable dividend, Variable divisor, Random random) {
		this(result, list(dividend, divisor), random);
	}

	@Override
	protected Object evaluateFunction(Function<Variable, Object> fromVariableToValue) {
		Double dividendValue = getDividendValue(fromVariableToValue);
		Double divisorValue = getDivisorValue(fromVariableToValue);
		Double result = dividendValue/divisorValue;
		return result;
	}

	@Override
	protected Iterator<? extends Integer> argumentsWithInverseFunctionIterator() {
		return iterator(0, 1); 
	}

	@Override
	protected Object computeMissingArgumentValue(Function<Variable, Object> fromVariableToValue, int missingArgumentIndex) {
		if (missingArgumentIndex == 0) {
			return computeDividendFromOthers(fromVariableToValue);
		}
		else if (missingArgumentIndex == 1) {
			return computeDivisorFromOthers(fromVariableToValue);
		}
		else {
			throw new Error("computeMissingArgumentValue got invalid index " + missingArgumentIndex + " for division argument.");
		}
	}

	private Object computeDividendFromOthers(Function<Variable, Object> fromVariableToValue) {
		Double divisorValue = getDivisorValue(fromVariableToValue);
		Double resultValue = getResultValue(fromVariableToValue);
		return resultValue * divisorValue;
	}

	private Object computeDivisorFromOthers(Function<Variable, Object> fromVariableToValue) {
		Double dividendValue = getDividendValue(fromVariableToValue);
		Double resultValue = getResultValue(fromVariableToValue);
		return resultValue * dividendValue;
	}

	/////////////////////
	
	private Double getResultValue(Function<Variable, Object> fromVariableToValue) {
		Double resultValue = (Double) fromVariableToValue.apply(getFunctionResult());
		return resultValue;
	}

	private Double getDividendValue(Function<Variable, Object> fromVariableToValue) {
		Double dividendValue = (Double) fromVariableToValue.apply(dividend);
		return dividendValue;
	}

	private Double getDivisorValue(Function<Variable, Object> fromVariableToValue) {
		Double divisorValue = (Double) fromVariableToValue.apply(divisor);
		myAssert(divisorValue != 0.0, () -> "Division of " + dividend + " by " + divisor + " with value zero.");
		return divisorValue;
	}

}