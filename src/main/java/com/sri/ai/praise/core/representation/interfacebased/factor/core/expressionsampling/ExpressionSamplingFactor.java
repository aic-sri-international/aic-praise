package com.sri.ai.praise.core.representation.interfacebased.factor.core.expressionsampling;

import static com.sri.ai.praise.core.representation.interfacebased.factor.core.expressionsampling.FromRealExpressionVariableToRealVariableWithRange.makeRealVariableWithRange;
import static com.sri.ai.util.Util.mapIntoArrayList;

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
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
import com.sri.ai.util.function.api.variables.Variable;
import com.sri.ai.util.function.core.variables.DefaultSetOfVariables;

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

	public static Variable makeIntegerVariableWithRange(ExpressionVariable expressionVariable, IntegerInterval type, int numberOfDiscreteValues, Context context) {
		// TODO Auto-generated method stub
		return null;
	}

	public static Variable makeEnumVariableWithRange(ExpressionVariable expressionVariable, Categorical type, int numberOfDiscreteValues, Context context) {
		// TODO Auto-generated method stub
		return null;
	}
}
