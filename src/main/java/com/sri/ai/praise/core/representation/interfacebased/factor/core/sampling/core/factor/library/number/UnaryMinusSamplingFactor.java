package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor.library.number;

import static com.sri.ai.util.Util.normalizeDoubleZeroToPositiveZero;

import java.util.Random;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;

/**
 * An extension of {@link AbstractDeterministicNumericUnaryFunctionSamplingFactor} for unary minus.
 *
 * @author braz
 *
 */
public class UnaryMinusSamplingFactor extends AbstractDeterministicNumericUnaryFunctionSamplingFactor {

	public UnaryMinusSamplingFactor(Variable result, Variable argument, Random random) {
		super(result, argument, random);
	}

	////////////////////

	@Override
	protected Double operation(Double argumentValue) {
		return normalizeDoubleZeroToPositiveZero(-argumentValue);
	}

	@Override
	protected String operatorSymbol() {
		return "-";
	}

	@Override
	protected Double computeArgumentFromResult(Double functionResultValue) {
		return normalizeDoubleZeroToPositiveZero(-functionResultValue);
	}

	@Override
	protected String getFunctionName() {
		return "-";
	}

}