package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor.math;

import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.myAssert;

import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Random;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.schedule.SamplingGoal;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.schedule.FunctionOnSetOfVariablesWillNotBeEqualTogGoal;
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
	protected Double computeMissingArgument(Double functionResultValue, Double definedArgumentsOperatorApplication, int missingArgumentIndex) {
		myAssert(definedArgumentsOperatorApplication != 0.0, () -> "Sampling rule for computing " + getArguments().get(missingArgumentIndex) + " out of " + getFunctionResult() + " and " + getArgumentsOtherThan(missingArgumentIndex) + " in " + this + " was invoked, but product of " + getArgumentsOtherThan(missingArgumentIndex) + " is zero. This was supposed to have been checked at this point.");
		double result = functionResultValue / definedArgumentsOperatorApplication;
		return result;
	}

	@Override
	protected Iterator<? extends Integer> argumentsWithInverseFunctionIterator() {
		return new IntegerIterator(0, getArguments().size()); // none
	}

	@Override
	protected Collection<? extends SamplingGoal> conditionsForInverseOfArgument(int i) {
		FunctionOnSetOfVariablesWillNotBeEqualTogGoal<Double> 
		productOfOtherArgumentsIsNotZero = 
		new FunctionOnSetOfVariablesWillNotBeEqualTogGoal<Double>(
				"productIsNotZero",
				getArgumentsOtherThan(i), 
				c -> evaluateFunction(c.iterator()), 
				0.0);
		return list(productOfOtherArgumentsIsNotZero);
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

	@Override
	protected String getFunctionName() {
		return "*";
	}

}
