package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor.math;

import java.util.Iterator;
import java.util.List;
import java.util.Random;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.util.collect.IntegerIterator;

/**
 * A {@link AbstractCommutativeAssociativeRingSamplingFactor} instantiated for summation.  
 * @author braz
 *
 */
public class SumSamplingFactor extends AbstractCommutativeAssociativeRingSamplingFactor<Double> {

	public SumSamplingFactor(Variable result, List<? extends Variable> arguments, Random random) {
		super(result, arguments, random);
	}

	@Override
	protected Double computeMissingArgument(Double functionResultValue, Double definedArgumentsOperatorApplication) {
		return functionResultValue - definedArgumentsOperatorApplication;
	}

	@Override
	protected Iterator<? extends Integer> argumentsWithInverseFunctionIterator() {
		return new IntegerIterator(0, getArguments().size());
	}

	@Override
	protected Double getIdentityElement() {
		return 0.0;
	}

	@Override
	protected Double apply(Double v1, Double v2) {
		return v1 + v2;
	}

	@Override
	protected Class<Double> getValueClass() {
		return Double.class;
	}

}
