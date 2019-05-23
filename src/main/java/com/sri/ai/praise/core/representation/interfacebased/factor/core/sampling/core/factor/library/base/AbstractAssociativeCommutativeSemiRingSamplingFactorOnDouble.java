package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor.library.base;

import java.util.List;
import java.util.Random;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;

public abstract class AbstractAssociativeCommutativeSemiRingSamplingFactorOnDouble
		extends AbstractAssociativeCommutativeSemiRingSamplingFactor<Double> {

	public AbstractAssociativeCommutativeSemiRingSamplingFactorOnDouble(
			Variable result,
			List<? extends Variable> arguments,
			Random random) {
		
		super(result, arguments, random);
	}

	@Override
	protected Double fromSampleValueToFunctionAppropriateValue(Object value) {
		if (value instanceof Number) {
			return ((Number) value).doubleValue();
		}
		else {
			throw new Error(getClass() + " requires numeric values but got " + value);
		}
	}
}
