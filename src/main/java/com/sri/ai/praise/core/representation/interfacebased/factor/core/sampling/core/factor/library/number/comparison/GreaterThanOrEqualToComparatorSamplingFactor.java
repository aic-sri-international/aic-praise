package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor.library.number.comparison;

import static com.sri.ai.util.Util.arrayList;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.Random;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;

public class GreaterThanOrEqualToComparatorSamplingFactor<T> extends AbstractComparisonGivenComparatorSamplingFactor<T> {

	public GreaterThanOrEqualToComparatorSamplingFactor(
			Variable functionResult, 
			ArrayList<? extends Variable> arguments, 
			Comparator<T> comparator, 
			Random random) {
		
		super("greaterThanOrEqualTo", functionResult, arguments, comparator, arrayList(0, +1), random);
	}

}