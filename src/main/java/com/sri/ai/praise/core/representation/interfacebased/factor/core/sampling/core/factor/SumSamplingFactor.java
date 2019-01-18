package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor;

import static com.sri.ai.util.Util.fold;
import static com.sri.ai.util.collect.FunctionIterator.functionIterator;

import java.util.Iterator;
import java.util.List;
import java.util.Random;
import java.util.function.Function;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.util.collect.IntegerIterator;
import com.sri.ai.util.collect.PredicateIterator;

/**
 * A sampling factor respecting a sum relationship of the type <code>s = x_1 + x_2 + ... + x_n</code>.
 * It will instantiate any of the variables <code>s, x_1, x_2, ..., x_n</code> if the others are instantiated.
 * If all are instantiated, it will update the weight of the sample to 1 if the values are consistent, or 0 if not.
 * If more than one are uninstantiated, it does nothing.
 * <p>
 * Note that this does not solve equations. Given an equation <code>x = y + y</code> with instantiated <code>x</code>,
 * it will <b>not</not> instantiate <code>y</code> even though that would be possible in principle.
 * This choice was made because variables can be arbitrarily complex (consider <code>x = y + z</code> where <code>z</code>
 * is defined somewhere else to be <code>x * y</code>), and going this route is out of scope.
 * Instead, this type of factor guarantees to define one variable (the sum) as a function of others,
 * and the ability to instantiate variables other than then sum is seen as a sampling bonus that may or may not be available.
 *  
 * @author braz
 *
 */
public class SumSamplingFactor extends DeterministicFunctionSamplingFactor {

	public SumSamplingFactor(Variable result, List<? extends Variable> arguments, Random random) {
		super(result, arguments, random);
	}

	@Override
	protected Object evaluateFunction(Function<Variable, Object> fromVariableToValue) {
		Iterator<Double> doubleValues = functionIterator(getArguments(), v -> (Double) fromVariableToValue.apply(v));
		Double result = evaluateFunction(doubleValues);
		return result;
	}
	
	protected Double evaluateFunction(Iterator<Double> values) {
		Double result = fold(values, (d1, d2) -> d1 + d2, 0.0);
		return result;
	}
	
	@Override
	protected Double computeMissingArgumentValue(Function<Variable, Object> fromVariableToValue, int missingArgumentIndex) {
		Iterator<Double> argumentsButMissingOne = otherArgumentsIterator(fromVariableToValue, missingArgumentIndex);
		Double definedArgumentsSum = evaluateFunction(argumentsButMissingOne);
		Double functionResultValue = (Double) fromVariableToValue.apply(getFunctionResult());
		Double missingArgumentValue = functionResultValue - definedArgumentsSum;
		return missingArgumentValue;
	}

	private Iterator<Double> otherArgumentsIterator(Function<Variable, Object> fromVariableToValue, int missingArgumentIndex) {
		IntegerIterator argumentIndices = new IntegerIterator(0, getArguments().size());
		PredicateIterator<Integer> otherArgumentsIndices = new PredicateIterator<>(argumentIndices, i -> i != missingArgumentIndex);
		return functionIterator(otherArgumentsIndices, i -> (Double) fromVariableToValue.apply(getArguments().get(i)));
	}
	
	@Override
	protected Iterator<? extends Integer> argumentsWithInverseFunctionIterator() {
		return new IntegerIterator(0, getArguments().size());
	}

}
