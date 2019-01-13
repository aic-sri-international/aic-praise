package com.sri.ai.praise.core.representation.interfacebased.factor.core.expressionsampling;

import static com.sri.ai.praise.core.representation.interfacebased.factor.core.expressionsampling.FromRealExpressionVariableToRealVariableWithRange.makeRealVariableWithRange;
import static com.sri.ai.util.Util.mapIntoArray;
import static com.sri.ai.util.Util.mapIntoArrayList;
import static com.sri.ai.util.Util.mapIntoList;
import static com.sri.ai.util.Util.myAssert;

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Type;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.expresso.type.Categorical;
import com.sri.ai.expresso.type.IntegerInterval;
import com.sri.ai.expresso.type.RealInterval;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.expression.api.ExpressionVariable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.factor.SamplingFactor;
import com.sri.ai.praise.core.representation.translation.rodrigoframework.samplinggraph2d.SamplingFactorDiscretizedProbabilityDistribution;
import com.sri.ai.util.function.api.variables.SetOfVariables;
import com.sri.ai.util.function.api.variables.Unit;
import com.sri.ai.util.function.api.variables.Variable;
import com.sri.ai.util.function.core.values.SetOfIntegerValues;
import com.sri.ai.util.function.core.variables.DefaultSetOfVariables;
import com.sri.ai.util.function.core.variables.EnumVariable;
import com.sri.ai.util.function.core.variables.IntegerVariable;

public interface ExpressionSamplingFactor extends Expression, SamplingFactor {
	
	SamplingFactorDiscretizedProbabilityDistribution getSamplingFactorDiscretizedProbabilityDistribution();

	public static ExpressionSamplingFactor newInstance(
			SamplingFactor samplingFactor, 
			int queryIndex, 
			Function<ExpressionVariable, Integer> fromVariableToNumberOfDiscreteValues,
			Context context) {
		
	     return (ExpressionSamplingFactor) java.lang.reflect.Proxy.newProxyInstance(
	             samplingFactor.getClass().getClassLoader(),
	             new Class[] { ExpressionSamplingFactor.class },
	             new ExpressionSamplingFactorProxyInvocationHandler(samplingFactor, queryIndex, fromVariableToNumberOfDiscreteValues, context));		
	}

	public static class ExpressionSamplingFactorProxyInvocationHandler implements InvocationHandler {

		private SamplingFactor samplingFactor;
		private int queryIndex;
		private Function<ExpressionVariable, Integer> fromVariableToNumberOfDiscreteValues;
		private Context context;
		private SamplingFactorDiscretizedProbabilityDistribution samplingFactorDiscretizedProbabilityDistribution;
		
		public ExpressionSamplingFactorProxyInvocationHandler(SamplingFactor samplingFactor, int queryIndex, Function<ExpressionVariable, Integer> fromVariableToNumberOfDiscreteValues, Context context) {
			this.samplingFactor = samplingFactor;
			this.queryIndex = queryIndex;
			this.fromVariableToNumberOfDiscreteValues = fromVariableToNumberOfDiscreteValues;
			this.context = context;
		}
		
		public Function<ExpressionVariable, Integer> getFromVariableToNumberOfDiscreteValues() {
			return fromVariableToNumberOfDiscreteValues;
		}

		public void setFromVariableToNumberOfDiscreteValues(Function<ExpressionVariable, Integer> fromVariableToNumberOfDiscreteValues) {
			this.fromVariableToNumberOfDiscreteValues = fromVariableToNumberOfDiscreteValues;
		}

		@Override
		public Object invoke(Object proxy, Method method, Object[] args) throws Throwable {
			if (method.getDeclaringClass().isAssignableFrom(Expression.class)) {
				return method.invoke(getExpression(), args);
			}
			else if (method.getDeclaringClass().isAssignableFrom(SamplingFactor.class)) {
				Object result = method.invoke(samplingFactor, args);
				return result;
			}
			else if (method.getName().equals("getSamplingFactorDiscretizedProbabilityDistribution")) {
				return getSamplingFactorDiscretizedProbabilityDistribution();
			}
			else {
				throw new Error(getClass() + " received method of interface " + method.getDeclaringClass() + " which it is not prepared to execute.");
			}
		}

		private Object getExpression() {
			// TODO Auto-generated method stub
			return null;
		}
		
		public SamplingFactorDiscretizedProbabilityDistribution getSamplingFactorDiscretizedProbabilityDistribution() {
			if (samplingFactorDiscretizedProbabilityDistribution == null) {
				samplingFactorDiscretizedProbabilityDistribution = makeSamplingFactorDiscretizedProbabilityDistribution();
			}
			return samplingFactorDiscretizedProbabilityDistribution;
		}
		
		private SamplingFactorDiscretizedProbabilityDistribution makeSamplingFactorDiscretizedProbabilityDistribution() {
			List<? extends ExpressionVariable> expressionVariables = mapIntoList(samplingFactor.getVariables(), v -> (ExpressionVariable) v);
			SetOfVariables setOfVariables = makeSetOfVariablesWithRanges(expressionVariables, fromVariableToNumberOfDiscreteValues, context);
			SamplingFactorDiscretizedProbabilityDistribution result = new SamplingFactorDiscretizedProbabilityDistribution(samplingFactor, setOfVariables, queryIndex);
			return result;
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
			List<? extends ExpressionVariable> expressionVariables, 
			Function<ExpressionVariable, Integer> numberOfDiscreteValues,
			Context context) {
		
		ArrayList<Variable> variables = mapIntoArrayList(expressionVariables,v -> makeVariableWithRange(v, numberOfDiscreteValues.apply(v), context));
		SetOfVariables result = new DefaultSetOfVariables(variables);
		return result;

	}

	public static Variable makeVariableWithRange(ExpressionVariable expression, Integer numberOfDiscreteValues, Context context) {
		Type type = context.getTypeOfRegisteredSymbol(expression);
		Variable result;
		String name = expression.toString();
		if (type instanceof RealInterval) {
			result = makeRealVariableWithRange(name, (RealInterval) type, numberOfDiscreteValues, context);
		}
		else if (type instanceof IntegerInterval) {
			result = makeIntegerVariableWithRange(name, (IntegerInterval) type, context);
		}
		else if (type instanceof Categorical) {
			result = makeEnumVariableWithRange(name, (Categorical) type, context);
		}
		else {
			throw new Error(ExpressionSamplingFactor.class + " only supports real, integer and enum types, but got variable " + expression + " of type " + type);
		}
		return result;
	}

	public static Variable makeIntegerVariableWithRange(String name, IntegerInterval type, Context context) {
		int first = type.getNonStrictLowerBound().intValue();
		int last = type.getNonStrictUpperBound().intValue();
		SetOfIntegerValues setOfIntegerValues = new SetOfIntegerValues(first, last);
		IntegerVariable integerVariable = new IntegerVariable(name, Unit.NONE, setOfIntegerValues);
		return integerVariable;
	}

	public static Variable makeEnumVariableWithRange(String name, Categorical type, Context context) {
		myAssert(Expressions.isNumber(type.cardinality()), () -> ExpressionSamplingFactor.class + " requires categorical types to have known finite cardinality, but got " + type);
		String[] values = mapIntoArray(String.class, type.cardinality().intValue(), type.iterator(), Expression::toString);
		return new EnumVariable(name, values);
	}
}
