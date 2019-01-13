package com.sri.ai.praise.core.representation.interfacebased.factor.core.expressionsampling;

import static com.sri.ai.util.Util.mapIntoArrayList;
import static com.sri.ai.util.Util.myAssert;

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.math.BigDecimal;
import java.math.MathContext;
import java.util.ArrayList;
import java.util.function.Function;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Type;
import com.sri.ai.expresso.type.Categorical;
import com.sri.ai.expresso.type.IntegerInterval;
import com.sri.ai.expresso.type.RealInterval;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.expression.api.ExpressionVariable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.factor.SamplingFactor;
import com.sri.ai.util.function.api.variables.SetOfVariables;
import com.sri.ai.util.function.api.variables.Unit;
import com.sri.ai.util.function.api.variables.Variable;
import com.sri.ai.util.function.core.values.SetOfRealValues;
import com.sri.ai.util.function.core.variables.DefaultSetOfVariables;
import com.sri.ai.util.function.core.variables.RealVariable;

public interface ExpressionSamplingFactor extends Expression, SamplingFactor {

	public static ExpressionSamplingFactor newInstance(SamplingFactor samplingFactor, Context context) {
	     return (ExpressionSamplingFactor) java.lang.reflect.Proxy.newProxyInstance(
	             samplingFactor.getClass().getClassLoader(),
	             new Class[] { ExpressionSamplingFactor.class },
	             new ExpressionSamplingFactorProxyInvocationHandler(samplingFactor, context));		
	}

	public static class ExpressionSamplingFactorProxyInvocationHandler implements InvocationHandler {

		private SamplingFactor samplingFactor;
		private Context context;
		
		public ExpressionSamplingFactorProxyInvocationHandler(SamplingFactor samplingFactor, Context context) {
			this.samplingFactor = samplingFactor;
			this.context = context;
		}
		
		@Override
		public Object invoke(Object proxy, Method method, Object[] args) throws Throwable {
			if (method.getDeclaringClass().isAssignableFrom(Expression.class)) {
				return method.invoke(getExpression(), args);
			}
			else if (method.getDeclaringClass().isAssignableFrom(SamplingFactor.class)) {
				Object returnValue = method.invoke(samplingFactor, args);
				if (returnValue instanceof SamplingFactor) {
					return newInstance((SamplingFactor) returnValue, context);
				}
				else {
					return returnValue;
				}
			}
			else {
				throw new Error(getClass() + " received method of interface " + method.getDeclaringClass() + " which it is not prepared to execute.");
			}
		}

		private Object getExpression() {
			// TODO Auto-generated method stub
			return null;
		}

	}
	
	/**
	 * Prepare a {@link SetOfVariables} specifying discretization for the {@link ExpressionVariable}s 
	 * in a given a factor and a function indicating the number of discrete variables specified for each expression variable.
	 * 
	 * @param expressionVariables
	 * @param numberOfDiscreteValues
	 * @param context
	 * @return
	 */
	public static SetOfVariables makeSetOfVariablesWithRanges(
			ArrayList<ExpressionVariable> expressionVariables, 
			Function<ExpressionVariable, Integer> numberOfDiscreteValues,
			Context context) {
		
		ArrayList<Variable> variables = mapIntoArrayList(expressionVariables,v -> makeVariableWithRange(v, numberOfDiscreteValues.apply(v), context));
		SetOfVariables result = new DefaultSetOfVariables(variables);
		return result;

	}

	public static Variable makeVariableWithRange(ExpressionVariable expressionVariable, int numberOfDiscreteValues, Context context) {
		Type type = context.getTypeOfRegisteredSymbol(expressionVariable);
		Variable result;
		if (type instanceof RealInterval) {
			result = makeRealVariableWithRange(expressionVariable, (RealInterval) type, numberOfDiscreteValues, context);
		}
		else if (type instanceof IntegerInterval) {
			result = makeIntegerVariableWithRange(expressionVariable, (IntegerInterval) type, numberOfDiscreteValues, context);
		}
		else if (type instanceof Categorical) {
			result = makeEnumVariableWithRange(expressionVariable, (Categorical) type, numberOfDiscreteValues, context);
		}
		else {
			throw new Error(ExpressionSamplingFactor.class + " only supports real, integer and enum types, but got variable " + expressionVariable + " of type " + type);
		}
		return result;
	}

	public static RealVariable makeRealVariableWithRange(ExpressionVariable expressionVariable, RealInterval type, int numberOfDiscreteValues, Context context) {
		
		myAssert(numberOfDiscreteValues > 0, () -> ExpressionSamplingFactor.class + " requires a positive number of discrete values but received " + numberOfDiscreteValues + " for variable " + expressionVariable);
		myAssert(type.boundsAreConstants(),  () -> ExpressionSamplingFactor.class + " requires real-valued variables to have constant bounds, but got " + expressionVariable + " in " + type);
		myAssert(!type.noLowerBound(),       () -> ExpressionSamplingFactor.class + " requires real-valued variables to be bounded, but got " + expressionVariable + " in " + type);
		myAssert(!type.noUpperBound(),       () -> ExpressionSamplingFactor.class + " requires real-valued variables to be bounded, but got " + expressionVariable + " in " + type);
		
		String name = expressionVariable.toString();
		
		// If the interval is closed on both ends,
		// we have the bounds to be the first and last discrete values,
		// and step to be the upper minus lower bound, divided by the number of gaps, which is the number of discrete values minus 1.
		// However, if a bound is open, we do not use that bound as a value,
		// and instead use lower bound plus step as the first discretized value, and upper bound minus step as the last discretized value.
		// However, doing so will create less discretized points than planned (we are throwing away the open bounds).
		// So to compensate for that think in terms of "delimiters", which are the discretized values plus the open bounds,
		// and use those to compute the step.
		
		BigDecimal lowerBound = new BigDecimal(type.getLowerBound().toString());
		BigDecimal upperBound = new BigDecimal(type.getUpperBound().toString());

		int numberOfDelimiters = numberOfDiscreteValues + (type.lowerBoundIsOpen()? 1 : 0) + (type.upperBoundIsOpen()? 1 : 0);
		BigDecimal numberOfGaps = new BigDecimal(numberOfDelimiters).subtract(new BigDecimal(1));
		BigDecimal step = upperBound.subtract(lowerBound).divide(numberOfGaps, MathContext.DECIMAL64);

		SetOfRealValues setOfRealValues;
		if (step.compareTo(BigDecimal.ZERO) <= 0) {
			if (!type.lowerBoundIsOpen() && !type.upperBoundIsOpen()) {
				// singleton set
				setOfRealValues = new SetOfRealValues(lowerBound, BigDecimal.ZERO, lowerBound);
			}
			else {
				// empty set of values, which means an upper bound smaller than the lower bound
				setOfRealValues = new SetOfRealValues(lowerBound, step, lowerBound.subtract(BigDecimal.ONE));
			}
		}
		else {
			BigDecimal firstValue = lowerBound;
			if (type.lowerBoundIsOpen()) {
				firstValue = firstValue.add(step);
			}

			BigDecimal lastValue = upperBound;
			if (type.upperBoundIsOpen()) {
				lastValue = lastValue.subtract(step);
			}

			setOfRealValues = new SetOfRealValues(firstValue, step, lastValue);
			setOfRealValues.setLowerBoundForDiscretizedValue(lowerBound);
			setOfRealValues.setUpperBoundForDiscretizedValue(upperBound);
		}
		
		RealVariable result = new RealVariable(name, Unit.NONE, setOfRealValues); // TODO: accept option specification of units for each of the factor's variables
		return result;
	}

	public static Variable makeIntegerVariableWithRange(ExpressionVariable expressionVariable, IntegerInterval type, int numberOfDiscreteValues, Context context) {
		// TODO Auto-generated method stub
		return null;
	}

	public static Variable makeEnumVariableWithRange(ExpressionVariable expressionVariable, Categorical type, int numberOfDiscreteValues, Context context) {
		// TODO Auto-generated method stub
		return null;
	}
}
