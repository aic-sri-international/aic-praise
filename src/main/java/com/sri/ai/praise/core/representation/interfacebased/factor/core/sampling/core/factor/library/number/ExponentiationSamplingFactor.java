package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor.library.number;

import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.normalizeDoubleZeroToPositiveZero;

import java.util.List;
import java.util.Random;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;

/**
 * An extension of {@link AbstractDeterministicNumericBinaryFunctionSamplingFactor} for exponentiation.
 *
 * @author braz
 *
 */
public class ExponentiationSamplingFactor extends AbstractDeterministicNumericBinaryFunctionWithGuaranteedArgumentInversesSamplingFactor {

	public ExponentiationSamplingFactor(Variable result, List<? extends Variable> arguments, Random random) {
		super(result, arguments, random);
	}

	public ExponentiationSamplingFactor(Variable result, Variable base, Variable exponent, Random random) {
		this(result, list(base, exponent), random);
	}

	////////////////////

	@Override
	protected Double operation(Double firstValue, Double secondValue) {
		return normalizeDoubleZeroToPositiveZero(Math.pow(firstValue, secondValue));
	}

	@Override
	protected String operatorSymbol() {
		return "^";
	}

	@Override
	protected Double computeFirstFromOthers(Double secondValue, Double functionResultValue) {
		return normalizeDoubleZeroToPositiveZero(Math.pow(functionResultValue, 1/secondValue));
	}

	@Override
	protected Double computeSecondFromOthers(Double firstValue, Double functionResultValue) {
		return normalizeDoubleZeroToPositiveZero(Math.log(functionResultValue) / Math.log(firstValue));
	}

	@Override
	protected String getFunctionName() {
		return "^";
	}

}