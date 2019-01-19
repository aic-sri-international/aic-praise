package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor.math;

import static com.sri.ai.util.Util.list;

import java.util.List;
import java.util.Random;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;

/**
 * An extension of {@link AbstractDeterministicNumericBinaryFunctionSamplingFactor} for subtraction.
 *
 * @author braz
 *
 */
public class SubtractionSamplingFactor extends AbstractDeterministicNumericBinaryFunctionSamplingFactor {

	public SubtractionSamplingFactor(Variable result, List<? extends Variable> arguments, Random random) {
		super(result, arguments, random);
	}

	public SubtractionSamplingFactor(Variable result, Variable base, Variable exponent, Random random) {
		this(result, list(base, exponent), random);
	}

	////////////////////

	@Override
	protected double operation(Double firstValue, Double secondValue) {
		return firstValue - secondValue;
	}

	@Override
	protected String operatorSymbol() {
		return "-";
	}

	@Override
	protected double computeFirstFromOthers(Double secondValue, Double functionResultValue) {
		return functionResultValue + secondValue;
	}

	@Override
	protected double computeSecondFromOthers(Double firstValue, Double functionResultValue) {
		return functionResultValue + firstValue;
	}

}