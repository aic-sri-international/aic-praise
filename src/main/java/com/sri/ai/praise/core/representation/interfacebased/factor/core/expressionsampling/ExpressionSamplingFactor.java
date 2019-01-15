package com.sri.ai.praise.core.representation.interfacebased.factor.core.expressionsampling;

import static com.sri.ai.expresso.helper.Expressions.makeSymbol;
import static com.sri.ai.praise.core.representation.interfacebased.factor.core.expressionsampling.FromRealExpressionVariableToRealVariableWithRange.makeRealVariableWithRange;
import static com.sri.ai.util.Util.mapIntegersIntoArrayList;
import static com.sri.ai.util.Util.mapIntegersIntoList;
import static com.sri.ai.util.Util.mapIntoArray;
import static com.sri.ai.util.Util.mapIntoArrayList;
import static com.sri.ai.util.Util.mapIntoList;
import static com.sri.ai.util.Util.myAssert;

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.function.Function;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Type;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.expresso.type.Categorical;
import com.sri.ai.expresso.type.IntegerInterval;
import com.sri.ai.expresso.type.RealInterval;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.library.Equality;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.grinder.library.boole.And;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.expression.api.ExpressionVariable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.factor.SamplingFactor;
import com.sri.ai.praise.core.representation.translation.rodrigoframework.samplinggraph2d.SamplingFactorDiscretizedProbabilityDistributionFunction;
import com.sri.ai.util.base.Pair;
import com.sri.ai.util.collect.CartesianProductOnIntegersIterator;
import com.sri.ai.util.function.api.values.Value;
import com.sri.ai.util.function.api.variables.Assignment;
import com.sri.ai.util.function.api.variables.SetOfVariables;
import com.sri.ai.util.function.api.variables.Unit;
import com.sri.ai.util.function.api.variables.Variable;
import com.sri.ai.util.function.core.values.SetOfIntegerValues;
import com.sri.ai.util.function.core.values.SetOfRealValues;
import com.sri.ai.util.function.core.variables.DefaultAssignment;
import com.sri.ai.util.function.core.variables.DefaultSetOfVariables;
import com.sri.ai.util.function.core.variables.EnumVariable;
import com.sri.ai.util.function.core.variables.IntegerVariable;

public interface ExpressionSamplingFactor extends Expression, SamplingFactor {
	
	SamplingFactorDiscretizedProbabilityDistributionFunction getSamplingFactorDiscretizedProbabilityDistribution();

	public static ExpressionSamplingFactor newInstance(
			SamplingFactor samplingFactor, 
			int queryIndex, 
			Function<Expression, Integer> fromVariableToNumberOfDiscreteValues,
			Context context) {
		
	     return (ExpressionSamplingFactor) java.lang.reflect.Proxy.newProxyInstance(
	             samplingFactor.getClass().getClassLoader(),
	             new Class[] { ExpressionSamplingFactor.class },
	             new ExpressionSamplingFactorProxyInvocationHandler(samplingFactor, queryIndex, fromVariableToNumberOfDiscreteValues, context));		
	}

	public static class ExpressionSamplingFactorProxyInvocationHandler implements InvocationHandler {

		private SamplingFactor samplingFactor;
		private int queryIndex;
		private Function<Expression, Integer> fromVariableToNumberOfDiscreteValues;
		private Context context;
		private SamplingFactorDiscretizedProbabilityDistributionFunction samplingFactorDiscretizedProbabilityDistribution;
		
		public ExpressionSamplingFactorProxyInvocationHandler(SamplingFactor samplingFactor, int queryIndex, Function<Expression, Integer> fromVariableToNumberOfDiscreteValues, Context context) {
			this.samplingFactor = samplingFactor;
			this.queryIndex = queryIndex;
			this.fromVariableToNumberOfDiscreteValues = fromVariableToNumberOfDiscreteValues;
			this.context = context;
		}
		
		public Function<Expression, Integer> getFromVariableToNumberOfDiscreteValues() {
			return fromVariableToNumberOfDiscreteValues;
		}

		public void setFromVariableToNumberOfDiscreteValues(Function<Expression, Integer> fromVariableToNumberOfDiscreteValues) {
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
			List<Integer> numberOfValues = mapIntoList(getFunctionVariables(), v -> v.getSetOfValuesOrNull().size()); 
			Expression result = getExpressionFor(new CartesianProductOnIntegersIterator(numberOfValues));
			return result;
		}
		
		private Expression getExpressionFor(Iterator<ArrayList<Integer>> valueIndicesIterator) {
			Expression result;
			if (valueIndicesIterator.hasNext()) {
				result = getExpressionForNonEmptyDomain(valueIndicesIterator);
			}
			else {
				result = getExpressionForEmptyDomain();
			}
			return result;
		}

		private Expression getExpressionForNonEmptyDomain(Iterator<ArrayList<Integer>> valueIndicesIterator) {
			Expression result;
			ArrayList<Integer> valueIndices = valueIndicesIterator.next();
			Expression probabilityExpression = getProbabilityForAssignment(valueIndices);
			if (valueIndicesIterator.hasNext()) {
				result = getExpressionForNotLastAssignment(valueIndicesIterator, valueIndices, probabilityExpression);
			}
			else {
				result = probabilityExpression;
			}
			return result;
		}

		private Expression getProbabilityForAssignment(ArrayList<Integer> valueIndices) {
			Assignment assignment = getAssignmentFromValueIndices(valueIndices);
			double probability = getSamplingFactorDiscretizedProbabilityDistribution().evaluate(assignment).doubleValue();
			return makeSymbol(probability);
		}

		private Expression getExpressionForNotLastAssignment(
				Iterator<ArrayList<Integer>> valueIndicesIterator,
				ArrayList<Integer> valueIndices, 
				Expression probabilityExpression) {
			
			Expression condition = getExpressionFor(valueIndices);
			Expression remaining = getExpressionFor(valueIndicesIterator);
			Expression result = IfThenElse.make(condition, probabilityExpression, remaining);
			return result;
		}

		private Expression getExpressionForEmptyDomain() {
			return Expressions.ONE;
		}
		
		private Assignment getAssignmentFromValueIndices(ArrayList<Integer> valueIndices) {
			SetOfVariables setOfVariables = getSamplingFactorDiscretizedProbabilityDistribution().getSetOfVariablesWithRange();
			ArrayList<Value> values = mapIntegersIntoArrayList(setOfVariables.size(), i -> getValue(setOfVariables, valueIndices, i));
			Assignment assignment = new DefaultAssignment(setOfVariables, values);
			return assignment;
		}

		private Value getValue(SetOfVariables setOfVariables, ArrayList<Integer> valueIndices, int i) {
			Variable variable = setOfVariables.get(i);
			int indexOfValue = valueIndices.get(i);
			Value value = variable.getSetOfValuesOrNull().get(indexOfValue);
			return value;
		}

		private Expression getExpressionFor(ArrayList<Integer> valueIndices) {
			ArrayList<? extends Variable> variables = getFunctionVariables();
			List<Expression> conjuncts = mapIntegersIntoList(valueIndices.size(), i -> getConditionFor(variables.get(i), valueIndices.get(i)));
			Expression result = And.make(conjuncts);
			return result;
		}

		private Expression getConditionFor(Variable variable, int valueIndex) {
			if (variable instanceof IntegerVariable || variable instanceof EnumVariable) {
				return getConditionForDiscreteVariable(variable, valueIndex);
			}
			else {
				return getConditionForRealVariable(variable, valueIndex);
			}
		}

		private Expression getConditionForDiscreteVariable(Variable variable, int valueIndex) {
			Expression result = Equality.make(variable.getName(), variable.getSetOfValuesOrNull().get(valueIndex));
			return result;
		}

		private Expression getConditionForRealVariable(Variable variable, int valueIndex) {
			Pair<BigDecimal, BigDecimal> boundsForIndex = ((SetOfRealValues) variable.getSetOfValuesOrNull()).getBoundsForIndex(valueIndex);
			Expression result = Expressions.apply(FunctorConstants.LESS_THAN, variable.getName(), boundsForIndex.second.doubleValue());
			return result;
		}

		public SamplingFactorDiscretizedProbabilityDistributionFunction getSamplingFactorDiscretizedProbabilityDistribution() {
			if (samplingFactorDiscretizedProbabilityDistribution == null) {
				samplingFactorDiscretizedProbabilityDistribution = makeSamplingFactorDiscretizedProbabilityDistribution();
			}
			return samplingFactorDiscretizedProbabilityDistribution;
		}
		
		private SamplingFactorDiscretizedProbabilityDistributionFunction makeSamplingFactorDiscretizedProbabilityDistribution() {
			List<? extends ExpressionVariable> expressionVariables = mapIntoList(samplingFactor.getVariables(), v -> (ExpressionVariable) v);
			SetOfVariables setOfVariables = makeSetOfVariablesWithRanges(expressionVariables, fromVariableToNumberOfDiscreteValues, context);
			SamplingFactorDiscretizedProbabilityDistributionFunction result = new SamplingFactorDiscretizedProbabilityDistributionFunction(samplingFactor, setOfVariables, queryIndex);
			return result;
		}

		private ArrayList<? extends Variable> getFunctionVariables() {
			return getSamplingFactorDiscretizedProbabilityDistribution().getSetOfInputVariables().getVariables();
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
			Function<Expression, Integer> numberOfDiscreteValues,
			Context context) {
		
		ArrayList<Variable> variables = mapIntoArrayList(expressionVariables,v -> makeVariableWithRange(v, numberOfDiscreteValues.apply(v), context));
		SetOfVariables result = new DefaultSetOfVariables(variables);
		return result;

	}

	public static Variable makeVariableWithRange(ExpressionVariable expression, Integer numberOfDiscreteValues, Context context) {
		Variable result;
		String name = expression.toString();
		Type type = context.getTypeOfRegisteredSymbol(expression);
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
