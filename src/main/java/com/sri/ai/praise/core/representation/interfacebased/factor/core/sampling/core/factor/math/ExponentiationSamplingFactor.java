package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor.math;

import static com.sri.ai.util.Util.list;

import java.util.List;
import java.util.Random;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;

/**
 * An extension of {@link AbstractDeterministicBinaryFunctionSamplingFactor} for exponentiation.
 *
 * @author braz
 *
 */
public class ExponentiationSamplingFactor extends AbstractDeterministicBinaryFunctionSamplingFactor {

	public ExponentiationSamplingFactor(Variable result, List<? extends Variable> arguments, Random random) {
		super(result, arguments, random);
	}

	public ExponentiationSamplingFactor(Variable result, Variable base, Variable exponent, Random random) {
		this(result, list(base, exponent), random);
	}

	////////////////////

	@Override
	protected double operation(Double firstValue, Double secondValue) {
		return Math.pow(firstValue, secondValue);
	}

	@Override
	protected String operatorSymbol() {
		return "^";
	}

	@Override
	protected double computeFirstFromOthers(Double secondValue, Double functionResultValue) {
		return Math.pow(functionResultValue, 1/secondValue);
	}

	@Override
	protected double computeSecondFromOthers(Double firstValue, Double functionResultValue) {
		return Math.log(functionResultValue) / Math.log(firstValue);
	}

}