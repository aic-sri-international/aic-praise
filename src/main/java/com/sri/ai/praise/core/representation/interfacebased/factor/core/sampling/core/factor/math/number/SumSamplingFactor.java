package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor.math.number;

import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.normalizeDoubleZeroToPositiveZero;

import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Random;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.schedule.SamplingGoal;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor.math.AbstractAssociativeCommutativeSemiRingSamplingFactor;
import com.sri.ai.util.collect.IntegerIterator;

/**
 * A {@link AbstractAssociativeCommutativeSemiRingSamplingFactor} instantiated for summation.  
 * @author braz
 *
 */
public class SumSamplingFactor extends AbstractAssociativeCommutativeSemiRingSamplingFactor<Double> {

	public SumSamplingFactor(Variable result, List<? extends Variable> arguments, Random random) {
		super(result, arguments, random);
	}

	@Override
	protected Double computeMissingArgument(Double functionResultValue, Double definedArgumentsOperatorApplication, int missingArgumentIndex) {
		return normalizeDoubleZeroToPositiveZero(functionResultValue - definedArgumentsOperatorApplication);
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
	protected Double getAbsorbingElement() {
		return null;
	}

	@Override
	protected boolean isAbsorbingElement(Double value) {
		return false;
	}

	@Override
	protected Double apply(Double v1, Double v2) {
		return normalizeDoubleZeroToPositiveZero(v1 + v2);
	}

	@Override
	protected Class<Double> getValueClass() {
		return Double.class;
	}

	@Override
	protected String getFunctionName() {
		return "+";
	}

	@Override
	protected Collection<? extends SamplingGoal> conditionsForInverseOfArgument(int i) {
		return list();
	}

}
