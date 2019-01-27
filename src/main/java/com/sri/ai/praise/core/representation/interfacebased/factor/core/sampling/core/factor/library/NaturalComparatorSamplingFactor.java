package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor.library;

import java.util.ArrayList;
import java.util.Random;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;

public class NaturalComparatorSamplingFactor<T extends Comparable<T>> extends ComparatorSamplingFactor<T> {

	public NaturalComparatorSamplingFactor(
			Variable functionResult, 
			ArrayList<? extends Variable> arguments, 
			Random random) {
		
		super(functionResult, arguments, (v1, v2) -> v1.compareTo(v2), random);
	}

}