package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor.ring;

import java.util.Iterator;
import java.util.List;
import java.util.Random;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor.AbstractCommutativeAssociativeRingSamplingFactor;
import com.sri.ai.util.collect.IntegerIterator;

/**
 * A {@link AbstractCommutativeAssociativeRingSamplingFactor} instantiated for multiplication.  
 * @author braz
 *
 */
public class MultiplicationSamplingFactor extends AbstractCommutativeAssociativeRingSamplingFactor<Double> {

	public MultiplicationSamplingFactor(Variable result, List<? extends Variable> arguments, Random random) {
		super(result, arguments, random);
	}

	@Override
	protected Double computeMissingArgument(Double functionResultValue, Double definedArgumentsOperatorApplication) {
		throw new Error("computeMissingArgument should never be invoked for " + getClass());
	}

	@Override
	protected Iterator<? extends Integer> argumentsWithInverseFunctionIterator() {
		return new IntegerIterator(0, 0); // none
	}

	@Override
	protected Double getIdentityElement() {
		return 1.0;
	}

	@Override
	protected Double apply(Double v1, Double v2) {
		return v1 * v2;
	}

	@Override
	protected Class<Double> getValueClass() {
		return Double.class;
	}

}
