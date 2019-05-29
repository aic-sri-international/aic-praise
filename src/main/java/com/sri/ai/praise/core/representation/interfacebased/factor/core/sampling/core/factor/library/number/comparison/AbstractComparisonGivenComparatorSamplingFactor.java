package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor.library.number.comparison;

import static com.sri.ai.util.Util.getDoubleValueWithDoubleZeroNormalizedToPositive;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.Random;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor.library.AbstractDeterministicBinaryFunctionWithoutInversesSamplingFactor;

public class AbstractComparisonGivenComparatorSamplingFactor<T>
extends AbstractDeterministicBinaryFunctionWithoutInversesSamplingFactor<T, T, Boolean> {

	private Comparator<T> comparator;
	private ArrayList<Integer> comparisonResults;
	private String functionName;
	
	public AbstractComparisonGivenComparatorSamplingFactor(
			String functionName,
			Variable functionResult, 
			ArrayList<? extends Variable> arguments, 
			Comparator<T> comparator,
			ArrayList<Integer> comparisonResults,
			Random random) {
		
		super(functionResult, arguments, random);
		this.comparator = comparator;
		this.comparisonResults = comparisonResults;
		this.functionName = functionName;
	}
	
	@Override
	protected Boolean operation(T firstValue, T secondValue) {
		int comparisonResult;
		// TODO this should be cleaner, and taken care of somewhere else
		if (firstValue instanceof Integer && secondValue instanceof Double) {
			comparisonResult = Double.valueOf(((Integer) firstValue).doubleValue()).compareTo((Double) secondValue);
		}
		else if (secondValue instanceof Integer && firstValue instanceof Double) {
			comparisonResult = ((Double) firstValue).compareTo(Double.valueOf(((Integer) secondValue).doubleValue()));
		}
		else {
			comparisonResult = comparator.compare(firstValue, secondValue);
		}
		return comparisonResults.contains(comparisonResult);
	}

	@Override
	protected boolean isInvalidFunctionResult(Boolean value) {
		return false;
	}

	@Override
	protected String getFunctionName() {
		return functionName;
	}

	@Override
	protected Boolean fromSampleValueToFunctionAppropriateResultValue(Object resultSampleValueObject) {
		return (Boolean) resultSampleValueObject;
	}

	@SuppressWarnings("unchecked")
	@Override
	protected T fromSampleValueToFunctionAppropriateFirstValue(Object firstArgumentSampleValueObject) {
		return (T) getDoubleValueWithDoubleZeroNormalizedToPositive(firstArgumentSampleValueObject);
	}

	@SuppressWarnings("unchecked")
	@Override
	protected T fromSampleValueToFunctionAppropriateSecondValue(Object secondArgumentSampleValueObject) {
		return (T) getDoubleValueWithDoubleZeroNormalizedToPositive(secondArgumentSampleValueObject);
	}
}