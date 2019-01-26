package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor.library.number;

import java.util.List;
import java.util.Random;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor.AbstractDeterministicFunctionSamplingFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor.library.base.AbstractDeterministicBinaryFunctionSamplingFactor;

/**
 * A specialization of {@link AbstractDeterministicFunctionSamplingFactor} for numeric operators with two arguments.
 * 
 * @author braz
 *
 */
public abstract class AbstractDeterministicNumericBinaryFunctionSamplingFactor
		extends AbstractDeterministicBinaryFunctionSamplingFactor<Double, Double, Double> {

	public AbstractDeterministicNumericBinaryFunctionSamplingFactor(Variable result, List<? extends Variable> arguments, Random random) {
		super(result, arguments, random);
	}

	////////////////////
	
	@Override
	protected boolean isInvalidFunctionResult(Double value) {
		return Double.isNaN(value) || Double.isInfinite(value);
	}

	@Override
	protected boolean isInvalidFirstArgument(Double value) {
		return Double.isNaN(value) || Double.isInfinite(value);
	}

	@Override
	protected boolean isInvalidSecondArgument(Double value) {
		return Double.isNaN(value) || Double.isInfinite(value);
	}

}