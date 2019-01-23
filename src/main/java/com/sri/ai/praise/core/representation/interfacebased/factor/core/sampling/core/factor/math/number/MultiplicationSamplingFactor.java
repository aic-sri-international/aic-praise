package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor.math.number;

import java.util.List;
import java.util.Random;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor.math.AbstractAssociativeCommutativeSemiRingSamplingFactor;

/**
 * A {@link AbstractAssociativeCommutativeSemiRingSamplingFactor} instantiated for multiplication.  
 * @author braz
 *
 */
public class MultiplicationSamplingFactor extends AbstractAssociativeCommutativeSemiRingWithInverseDependingOnOperationOnOtherArgumentBeingDifferentFromAbsorbingElementSamplingFactor<Double> {

	public MultiplicationSamplingFactor(Variable result, List<? extends Variable> arguments, Random random) {
		super(result, arguments, random);
	}

	@Override
	protected Double computeMissingArgument(Double functionResultValue, Double definedArgumentsOperatorApplication, int missingArgumentIndex) {
		return functionResultValue / definedArgumentsOperatorApplication;
	}

	@Override
	protected Double getIdentityElement() {
		return 1.0;
	}

	@Override
	protected Double getAbsorbingElement() {
		return 0.0;
	}

	@Override
	protected boolean isAbsorbingElement(Double value) {
		return value.equals(getAbsorbingElement()) || value.equals(-0.0);
	}

	@Override
	protected Double apply(Double v1, Double v2) {
		return v1 * v2;
	}

	@Override
	protected Class<Double> getValueClass() {
		return Double.class;
	}

	@Override
	protected String getFunctionName() {
		return "*";
	}
}
