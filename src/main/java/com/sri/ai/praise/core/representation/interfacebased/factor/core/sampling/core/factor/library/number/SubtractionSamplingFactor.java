package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor.library.number;

import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.normalizeDoubleZeroToPositiveZero;

import java.util.List;
import java.util.Random;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;

/**
 * An extension of {@link AbstractDeterministicNumericBinaryFunctionSamplingFactor} for subtraction.
 *
 * @author braz
 *
 */
public class SubtractionSamplingFactor extends AbstractDeterministicNumericBinaryFunctionWithGuaranteedArgumentInversesSamplingFactor {

	public SubtractionSamplingFactor(Variable result, List<? extends Variable> arguments, Random random) {
		super(result, arguments, random);
	}

	public SubtractionSamplingFactor(Variable result, Variable base, Variable exponent, Random random) {
		this(result, list(base, exponent), random);
	}

	////////////////////

	@Override
	protected Double operation(Double firstValue, Double secondValue) {
		return normalizeDoubleZeroToPositiveZero(firstValue - secondValue);
	}

	@Override
	protected Double computeFirstFromOthers(Double secondValue, Double functionResultValue) {
		return normalizeDoubleZeroToPositiveZero(functionResultValue + secondValue);
	}

	@Override
	protected Double computeSecondFromOthers(Double firstValue, Double functionResultValue) {
		return normalizeDoubleZeroToPositiveZero(functionResultValue + firstValue);
	}

	@Override
	protected String getFunctionName() {
		return "-";
	}

}