package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.distribution;

import static com.sri.ai.util.Util.arrayList;
import static com.sri.ai.util.Util.myAssert;

import java.util.Random;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor.math.AbstractDeterministicBinaryFunctionSamplingFactor;

public class EqualitySamplingFactor2 extends AbstractDeterministicBinaryFunctionSamplingFactor<Object, Object, Boolean> {

	public EqualitySamplingFactor2(Variable functionResult, Variable variable1, Variable variable2, Random random) {
		super(functionResult, arrayList(variable1, variable2), random);
	}
	
	@Override
	protected String operatorSymbol() {
		return "=";
	}

	@Override
	protected Boolean operation(Object firstValue, Object secondValue) {
		return firstValue == secondValue;
	}

	@Override
	protected Object computeFirstFromOthers(Object secondValue, Boolean functionResultValue) {
		myAssert(functionResultValue, () -> getClass().getSimpleName() + " asked to set first argument " + getFirst() + " equal to " + getSecond() + " but " + getFunctionResult() + " is set to false");
		return secondValue;
	}

	@Override
	protected Object computeSecondFromOthers(Object firstValue, Boolean functionResultValue) {
		myAssert(functionResultValue, () -> getClass().getSimpleName() + " asked to set second argument " + getSecond() + " equal to " + getFirst() + " but " + getFunctionResult() + " is set to false");
		return firstValue;
	}

	@Override
	protected boolean isValidResult(Boolean value) {
		return true;
	}

	@Override
	protected boolean isValidFirstArgument(Object value) {
		return true;
	}

	@Override
	protected boolean isValidSecondArgument(Object value) {
		return true;
	}

	@Override
	protected String getFunctionName() {
		return "equality";
	}

	@Override
	public String toString() {
		return getFunctionResult() + " = (" + getFirst() + " = " + getSecond() + ")";
	}
}