package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor.library.number.comparison;

import static com.sri.ai.util.Util.arrayList;

import java.util.ArrayList;
import java.util.Random;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;

public class LessThanSamplingFactor<T extends Comparable<T>> extends AbstractComparisonGivenComparatorSamplingFactor<T> {

	public LessThanSamplingFactor(
			Variable functionResult, 
			ArrayList<? extends Variable> arguments, 
			Random random) {
		
		super("lessThan", functionResult, arguments, (v1, v2) -> v1.compareTo(v2), arrayList(-1), random);
	}

}