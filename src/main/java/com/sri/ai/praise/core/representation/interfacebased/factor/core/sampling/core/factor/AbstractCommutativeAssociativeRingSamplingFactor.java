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
 * An extension of {@link AbstractDeterministicFunctionSamplingFactor} for commutative associative ring operators.
 * Implement abstract methods to specify the specific operator.
 * <p>
 * The sampling factor works by respecting a relationship of the type <code>s = x_1 + x_2 + ... + x_n</code>,
 * where here we use + to stand for the ring's operator.
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
public abstract class AbstractCommutativeAssociativeRingSamplingFactor<T> extends AbstractDeterministicFunctionSamplingFactor {

	/**
	 * Specifies the indices of arguments for which there is an inverse operation given any values of all the other variables, including the result. 
	 */
	@Override
	protected abstract Iterator<? extends Integer> argumentsWithInverseFunctionIterator();

	/**
	 * Define how to compute the missing value of an argument based on the result and
	 * the application of the operator to all remaining, defined arguments.
	 * @param functionResultValue
	 * @param definedArgumentsOperatorApplication
	 * @return
	 */
	abstract protected T computeMissingArgument(T functionResultValue, T definedArgumentsOperatorApplication);

	/**
	 * Specifies the identity element of the operator.
	 * @return
	 */
	abstract protected T getIdentityElement();

	/**
	 * Specifies how to compute the operator.
	 * @return
	 */
	abstract protected T apply(T v1, T v2);

	/**
	 * Provides the {@link Class} object for the type of values in this ring.
	 * @return
	 */
	abstract protected Class<T> getValueClass();

	//////////////////////

	public AbstractCommutativeAssociativeRingSamplingFactor(Variable result, List<? extends Variable> arguments, Random random) {
		super(result, arguments, random);
	}

	//////////////////////
	
	@Override
	protected T computeMissingArgumentValue(Function<Variable, Object> fromVariableToValue, int missingArgumentIndex) {
		Iterator<T> argumentsButMissingOne = otherArgumentsIterator(fromVariableToValue, missingArgumentIndex);
		T definedArgumentsOperatorApplication = evaluateFunction(argumentsButMissingOne);
		T functionResultValue = getValue(fromVariableToValue, getFunctionResult());
		T missingArgumentValue = computeMissingArgument(functionResultValue, definedArgumentsOperatorApplication);
		return missingArgumentValue;
	}

	@Override
	protected Object evaluateFunction(Function<Variable, Object> fromVariableToValue) {
		Iterator<T> doubleValues = functionIterator(getArguments(), v -> getValue(fromVariableToValue, v));
		T result = evaluateFunction(doubleValues);
		return result;
	}

	protected T evaluateFunction(Iterator<T> values) {
		T result = fold(values, (v1, v2) -> apply(v1, v2), getIdentityElement());
		return result;
	}

	protected Iterator<T> otherArgumentsIterator(Function<Variable, Object> fromVariableToValue, int missingArgumentIndex) {
		IntegerIterator argumentIndices = new IntegerIterator(0, getArguments().size());
		PredicateIterator<Integer> otherArgumentsIndices = new PredicateIterator<>(argumentIndices, i -> i != missingArgumentIndex);
		return functionIterator(otherArgumentsIndices, i -> getValue(fromVariableToValue, getArguments().get(i)));
	}

	@SuppressWarnings("unchecked")
	private T getValue(Function<Variable, Object> fromVariableToValue, Variable variable) {
		try {
			return (T) fromVariableToValue.apply(variable);
		} catch(ClassCastException e) {
			throw new Error("All arguments of " + getClass() + " must be of type " + getValueClass());
		}
	}

}