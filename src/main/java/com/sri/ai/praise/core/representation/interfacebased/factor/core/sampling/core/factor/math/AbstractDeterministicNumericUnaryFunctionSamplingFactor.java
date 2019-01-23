package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor.math;

import java.util.Random;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;

/**
 * A specialization of {@link AbstractDeterministicUnaryFunctionSamplingFactor} for numeric values,
 * defining a validity check against NaN and infinite values.
 * 
 * @author braz
 *
 */
public abstract class AbstractDeterministicNumericUnaryFunctionSamplingFactor
		extends AbstractDeterministicUnaryFunctionSamplingFactor<Double> {

	public AbstractDeterministicNumericUnaryFunctionSamplingFactor(Variable result, Variable argument, Random random) {
		super(result, argument, random);
	}

	////////////////////
	
	@Override
	protected boolean isValidValue(Double value) {
		return ! Double.isNaN(value) && ! Double.isInfinite(value);
	}
}