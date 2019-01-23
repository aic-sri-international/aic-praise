package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor.math.number;

import java.util.Random;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor.math.AbstractDeterministicNumericUnaryFunctionSamplingFactor;

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
	protected double operation(Double argumentValue) {
		return -argumentValue;
	}

	@Override
	protected String operatorSymbol() {
		return "-";
	}

	@Override
	protected double computeArgumentFromResult(Double functionResultValue) {
		return -functionResultValue;
	}

	@Override
	protected String getFunctionName() {
		return "-";
	}

}