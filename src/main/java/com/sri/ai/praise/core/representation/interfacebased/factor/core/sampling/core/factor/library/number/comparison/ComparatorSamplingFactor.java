package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor.library.number.comparison;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.Random;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor.library.AbstractDeterministicBinaryFunctionWithoutInversesSamplingFactor;

public class ComparatorSamplingFactor<T> extends AbstractDeterministicBinaryFunctionWithoutInversesSamplingFactor<T, T, Integer> {

	private Comparator<T> comparator;
	
	public ComparatorSamplingFactor(
			Variable functionResult, 
			ArrayList<? extends Variable> arguments, 
			Comparator<T> comparator, 
			Random random) {
		
		super(functionResult, arguments, random);
		this.comparator = comparator;
	}
	
	@Override
	protected Integer operation(T firstValue, T secondValue) {
		return comparator.compare(firstValue, secondValue);
	}

	@Override
	protected boolean isInvalidFunctionResult(Integer value) {
		return false;
	}

	@Override
	protected String getFunctionName() {
		return "compare";
	}
}