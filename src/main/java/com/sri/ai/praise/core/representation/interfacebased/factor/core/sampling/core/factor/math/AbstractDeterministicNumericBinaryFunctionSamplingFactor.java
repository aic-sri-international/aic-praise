package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor.math;

import java.util.List;
import java.util.Random;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor.AbstractDeterministicFunctionSamplingFactor;

/**
 * A specialization of {@link AbstractDeterministicFunctionSamplingFactor} for numeric operators with two arguments.
 * 
 * @author braz
 *
 */
public abstract class AbstractDeterministicNumericBinaryFunctionSamplingFactor
		extends AbstractDeterministicBinaryFunctionSamplingFactor<Double, Double, Double> {

	public AbstractDeterministicNumericBinaryFunctionSamplingFactor(Variable result, List<? extends Variable> arguments,
			Random random) {
		super(result, arguments, random);
	}

	////////////////////
	
	@Override
	protected boolean isValidResult(Double value) {
		return Double.isNaN(value) || Double.isInfinite(value);
	}

	@Override
	protected boolean isValidFirstArgument(Double value) {
		return Double.isNaN(value) || Double.isInfinite(value);
	}

	@Override
	protected boolean isValidSecondArgument(Double value) {
		return Double.isNaN(value) || Double.isInfinite(value);
	}

}