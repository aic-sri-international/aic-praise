package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor.library.number.comparison;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.Random;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor.library.AbstractDeterministicBinaryFunctionWithoutInversesSamplingFactor;

public class AbstractComparisonGivenComparatorSamplingFactor<T> extends AbstractDeterministicBinaryFunctionWithoutInversesSamplingFactor<T, T, Boolean> {

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
		return comparisonResults.contains(comparator.compare(firstValue, secondValue));
	}

	@Override
	protected boolean isInvalidFunctionResult(Boolean value) {
		return false;
	}

	@Override
	protected String getFunctionName() {
		return functionName;
	}
}