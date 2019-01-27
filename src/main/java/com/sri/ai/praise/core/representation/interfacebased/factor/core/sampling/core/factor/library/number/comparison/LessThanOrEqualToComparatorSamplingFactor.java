package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor.library.number.comparison;

import static com.sri.ai.util.Util.arrayList;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.Random;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;

public class LessThanOrEqualToComparatorSamplingFactor<T> extends AbstractComparisonGivenComparatorSamplingFactor<T> {

	public LessThanOrEqualToComparatorSamplingFactor(
			Variable functionResult, 
			ArrayList<? extends Variable> arguments, 
			Comparator<T> comparator, 
			Random random) {
		
		super("lessThanOrEqualTo", functionResult, arguments, comparator, arrayList(-1, 0), random);
	}

}