package com.sri.ai.praise.core.representation.interfacebased.factor.core.expressionsampling;

import static com.sri.ai.expresso.helper.Expressions.makeSymbol;
import static com.sri.ai.grinder.library.FunctorConstants.LESS_THAN;
import static com.sri.ai.praise.core.representation.interfacebased.factor.core.expressionsampling.FromRealExpressionVariableToRealVariableWithRange.makeRealVariableWithRange;
import static com.sri.ai.util.Util.arrayList;
import static com.sri.ai.util.Util.makeProxy;
import static com.sri.ai.util.Util.mapIntegersIntoArrayList;
import static com.sri.ai.util.Util.mapIntegersIntoList;
import static com.sri.ai.util.Util.mapIntoArray;
import static com.sri.ai.util.Util.mapIntoArrayList;
import static com.sri.ai.util.Util.mapIntoList;
import static com.sri.ai.util.Util.myAssert;
import static com.sri.ai.util.Util.objectStringEqualsOneOf;
import static com.sri.ai.util.Util.product;
import static com.sri.ai.util.Util.repeat;
import static com.sri.ai.util.base.PairOf.makePairOf;
import static com.sri.ai.util.collect.FunctionIterator.functionIterator;

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;
import java.util.Random;
import java.util.function.Function;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Symbol;
import com.sri.ai.expresso.api.Type;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.expresso.type.Categorical;
import com.sri.ai.expresso.type.IntegerInterval;
import com.sri.ai.expresso.type.RealInterval;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.library.Equality;
import com.sri.ai.grinder.library.boole.And;
import com.sri.ai.grinder.library.boole.Not;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.expression.api.ExpressionVariable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.factor.SamplingFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample.Sample;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor.ConditionedSamplingFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor.UniformDiscreteSamplingFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor.UniformIntegerSamplingFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor.UniformRealSamplingFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor.UniformSamplingFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.sample.DoublePotentialFactory;
import com.sri.ai.praise.core.representation.translation.rodrigoframework.samplinggraph2d.DefaultSamplingFactorDiscretizedProbabilityDistributionFunction;
import com.sri.ai.praise.core.representation.translation.rodrigoframework.samplinggraph2d.SamplingFactorDiscretizedProbabilityDistributionFunction;
import com.sri.ai.util.base.Pair;
import com.sri.ai.util.base.PairOf;
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
	
	/**
	 * The maximum number of assignments to factor's discretized values
	 * that will allow generating an expression for it
	 * ({@link #TOO_LARGE_FOR_EXPRESSION_GENERATION} is used otherwise).
	 *
	 */
	public static int maximum_number_of_assignments_for_expression_generation = 100;
	
	/**
	 * The factor's expression if the number of assignments to factor's discretized values
	 * is greater than {@link #maximum_number_of_assignments_for_expression_generation}.
	 */
	public static final Symbol TOO_LARGE_FOR_EXPRESSION_GENERATION = makeSymbol("too large");

	DefaultSamplingFactorDiscretizedProbabilityDistributionFunction getSamplingFactorDiscretizedProbabilityDistributionFunction();

	ExpressionSamplingFactor condition(Sample conditioningSample);
	
	void sample();
	
	boolean averageWeightIsZero();
	
	int getNumberOfSamples();
	
	double getTotalWeight();

	/**
	 * If given expression is an instance of {@link ExpressionSamplingFactor}, samples it a given number of times.
	 * @param expression
	 * @param numberOfSamples
	 */
	public static void sample(Expression expression, int numberOfSamples) {
		if (expression instanceof ExpressionSamplingFactor) {
			repeat(numberOfSamples, () -> ((ExpressionSamplingFactor) expression).sample());
		}
	}

	public static ExpressionSamplingFactor expressionSamplingFactor(
			SamplingFactor samplingFactor, 
			int queryIndex, 
			Function<Expression, Integer> fromVariableToNumberOfDiscreteValues,
			int initialNumberOfSamples,
			Context context) {
		
		return makeProxy(
				ExpressionSamplingFactor.class, 
				samplingFactor, 
				queryIndex, 
				fromVariableToNumberOfDiscreteValues, 
				initialNumberOfSamples, 
				context);
	}

	public static class ExpressionSamplingFactorProxyInvocationHandler implements InvocationHandler {

		private SamplingFactor samplingFactor;
		private int queryIndex;
		private Function<Expression, Integer> fromVariableToNumberOfDiscreteValues;
		private int initialNumberOfSamples;
		private Context context;
		private DefaultSamplingFactorDiscretizedProbabilityDistributionFunction samplingFactorDiscretizedProbabilityDistribution;
		
		public ExpressionSamplingFactorProxyInvocationHandler(SamplingFactor samplingFactor, int queryIndex, Function<Expression, Integer> fromVariableToNumberOfDiscreteValues, int initialNumberOfSamples, Context context) {
			this.samplingFactor = samplingFactor;
			this.queryIndex = queryIndex;
			this.fromVariableToNumberOfDiscreteValues = fromVariableToNumberOfDiscreteValues;
			this.initialNumberOfSamples = initialNumberOfSamples;
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
				return method.invoke(getFactorExpression(), args);
			}
			else if (method.getDeclaringClass().isAssignableFrom(SamplingFactor.class)) {
				Object result = method.invoke(samplingFactor, args);
				return result;
			}
			else if (method.getName().equals("condition")) {
				return condition((Sample) args[0]);
			}
			else if (method.getName().equals("getSamplingFactorDiscretizedProbabilityDistributionFunction")) {
				return getSamplingFactorDiscretizedProbabilityDistributionFunction();
			}
			else if (method.getName().equals("sample")) {
				sample();
				return null;
			}
			else if (method.getName().equals("getNumberOfSamples")) {
				return getNumberOfSamples();
			}
			else if (method.getName().equals("averageWeightIsZero")) {
				return averageWeightIsZero();
			}
			else if (method.getName().equals("getTotalWeight")) {
				return getTotalWeight();
			}
			else {
				throw new Error(getClass() + " received method '" + method + "' of " + method.getDeclaringClass() + " which it is not prepared to execute.");
			}
		}

		public ExpressionSamplingFactor condition(Sample conditioningSample) {
			
			SamplingFactor conditionedSamplingFactor = 
					ConditionedSamplingFactor.condition(samplingFactor, conditioningSample);
			
			ExpressionSamplingFactor result = 
					expressionSamplingFactor(conditionedSamplingFactor, queryIndex, fromVariableToNumberOfDiscreteValues, initialNumberOfSamples, context);
			
			return result;
			
		}

		public void sample() {
			getSamplingFactorDiscretizedProbabilityDistributionFunction().sample();
		}
		
		public boolean averageWeightIsZero() {
			return getSamplingFactorDiscretizedProbabilityDistributionFunction().averageWeightIsZero();
		}

		public int getNumberOfSamples() {
			return getSamplingFactorDiscretizedProbabilityDistributionFunction().getNumberOfSamples();
		}

		public double getTotalWeight() {
			return getSamplingFactorDiscretizedProbabilityDistributionFunction().getTotalWeight();
		}
		
		private Expression expression;
		
		private Expression getFactorExpression() {
			if (expression == null) {
				makeFactorExpressionOrTooLarge();
			}
			return expression;
		}

		private void makeFactorExpressionOrTooLarge() {
			int numberOfAssignments = computeNumberOfAssignments();
			if (numberOfAssignments > maximum_number_of_assignments_for_expression_generation) {
				expression = TOO_LARGE_FOR_EXPRESSION_GENERATION;
			}
			else {
				makeFactorExpression(); 
			}
		}

		private int computeNumberOfAssignments() {
			return (int) product(functionIterator(getFunctionVariables(), v -> v.getSetOfValuesOrNull().size()));
		}

		private void makeFactorExpression() {
			Iterator<ArrayList<Integer>> valueIndicesIterator = makeValueIndicesIterator();
			if (valueIndicesIterator.hasNext()) {
				expression = getFactorExpressionForNonEmptyDomain(valueIndicesIterator);
			}
			else {
				expression = getFactorExpressionForEmptyDomain();
			}
		}

		private Iterator<ArrayList<Integer>> makeValueIndicesIterator() {
			List<Integer> fromVariableIndexToNumberOfValues = makeListOfVariableNumberOfValues();
			return makeValueIndicesIterator(fromVariableIndexToNumberOfValues);
		}

		private List<Integer> makeListOfVariableNumberOfValues() {
			List<Integer> result = mapIntoList(getFunctionVariables(), v -> v.getSetOfValuesOrNull().size());
			return result;
		}

		private CartesianProductOnIntegersIterator makeValueIndicesIterator(List<Integer> fromVariableIndexToNumberOfValues) {
			return new CartesianProductOnIntegersIterator(fromVariableIndexToNumberOfValues);
		}

		private Expression getFactorExpressionForNonEmptyDomain(Iterator<ArrayList<Integer>> valueIndicesIterator) {
			
			PairOf<ArrayList<Expression>> conditionsAndProbabilities = 
					gatherConditionsAndProbabilities(valueIndicesIterator);
			
			Expression result = 
					makeExpression(conditionsAndProbabilities.first, conditionsAndProbabilities.second);
			
			return result;
			
		}

		private 
		PairOf<ArrayList<Expression>>
		gatherConditionsAndProbabilities(Iterator<ArrayList<Integer>> valueIndicesIterator) {
			
			ArrayList<Expression> conditions = arrayList();
			ArrayList<Expression> probabilities = arrayList();
			
			while (valueIndicesIterator.hasNext()) {
				
				ArrayList<Integer> valueIndices = valueIndicesIterator.next();
				
				Expression condition = getConditionForAssignmentIndices(valueIndices);
				conditions.add(condition);

				Expression probability = getProbabilityForAssignmentIndices(valueIndices);
				probabilities.add(probability);
			}
			
			return makePairOf(conditions, probabilities);
		}

		private Expression makeExpression(ArrayList<Expression> conditions, ArrayList<Expression> probabilities) {
			ListIterator<Expression> conditionsIterator = conditions.listIterator(conditions.size());
			ListIterator<Expression> probabilitiesIterator = probabilities.listIterator(probabilities.size());

			conditionsIterator.previous(); // no need for last condition, as it is implied by all the others failing
			Expression current = probabilitiesIterator.previous();
			while (probabilitiesIterator.hasPrevious()) {
				Expression condition = conditionsIterator.previous();
				Expression probability = probabilitiesIterator.previous();
				current = IfThenElse.make(condition, probability, current);
			}
			
			return current;
		}

		private Expression getProbabilityForAssignmentIndices(ArrayList<Integer> valueIndices) {
			
			Assignment assignment = 
					getAssignmentFromValueIndices(valueIndices);
			
			SamplingFactorDiscretizedProbabilityDistributionFunction probabilityDistributionFunction = 
					getSamplingFactorDiscretizedProbabilityDistributionFunction();
			
			SamplingFactorDiscretizedProbabilityDistributionFunction 
			projectedProbabilityDistributionFunction = 
					projectIfNeeded(probabilityDistributionFunction, assignment);
			
			myAssert(!projectedProbabilityDistributionFunction.averageWeightIsZero(), () -> "Samples for distribution from " + samplingFactor + " on " + samplingFactor.getVariables() + " have zero weight");

			double probability = 
					projectedProbabilityDistributionFunction.evaluate(assignment).doubleValue();
			
			Symbol result = 
					makeSymbol(probability);
			
			return result;
		}

		/**
		 * We take the projection of the Function based on the assignment, but only if needed
		 * (that is, if the assignment contains any variables).
		 * If the assignment does not contain any variables, the projection is just the initial function.
		 * This is preferable because projection creates a new instance of Function that will
		 * throw away previous samples.
		 * @param probabilityDistributionFunction
		 * @param assignment
		 * @return
		 */
		private SamplingFactorDiscretizedProbabilityDistributionFunction projectIfNeeded(
				SamplingFactorDiscretizedProbabilityDistributionFunction probabilityDistributionFunction,
				Assignment assignment) {

			SamplingFactorDiscretizedProbabilityDistributionFunction result;
			
			Variable query = probabilityDistributionFunction.getSetOfInputVariables().get(queryIndex);

			if (assignment.size() > 1) {
				Assignment assignmentWithoutQuery = assignment.remove(query);
				result = probabilityDistributionFunction.project(query, assignmentWithoutQuery);
			}
			else if (assignment.size() == 1) {
				result = probabilityDistributionFunction;
			}
			else {
				throw new Error("Assignment should have at least query variable " + query + " but was " + assignment);
			}
			
			return result;
		}

		private Expression getFactorExpressionForEmptyDomain() {
			return Expressions.ONE;
		}
		
		private Assignment getAssignmentFromValueIndices(ArrayList<Integer> valueIndices) {
			SetOfVariables setOfVariables = getSamplingFactorDiscretizedProbabilityDistributionFunction().getSetOfVariablesWithRange();
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

		private Expression getConditionForAssignmentIndices(ArrayList<Integer> valueIndices) {
			ArrayList<? extends Variable> variables = getFunctionVariables();
			List<Expression> conjuncts = 
					mapIntegersIntoList(
							valueIndices.size(), 
							i -> getConditionForAssignment(variables.get(i), valueIndices.get(i)));
			Expression result = And.make(conjuncts);
			return result;
		}

		private Expression getConditionForAssignment(Variable variable, int valueIndex) {
			if (variable instanceof IntegerVariable || variable instanceof EnumVariable) {
				return getConditionForDiscreteVariableAssignment(variable, valueIndex);
			}
			else {
				return getConditionForRealVariableAssignment(variable, valueIndex);
			}
		}

		private Expression getConditionForDiscreteVariableAssignment(Variable variable, int valueIndex) {
			Expression result;
			Object value = variable.getSetOfValuesOrNull().get(valueIndex).objectValue();
			if (objectStringEqualsOneOf(value, "true", "false")) {
				result = getConditionForBooleanVariableAssignment(variable, value);
			}
			else {
				result = Equality.make(variable.getName(), value);
			}
			return result;
		}

		private Expression getConditionForBooleanVariableAssignment(Variable variable, Object value) {
			Expression result;
			Symbol variableExpression = Expressions.makeSymbol(variable.getName());
			if (value.toString().equals("true")) {
				result = variableExpression;
			}
			else {
				result = Not.make(variableExpression);
			}
			return result;
		}

		private Expression getConditionForRealVariableAssignment(Variable variable, int valueIndex) {
			Pair<BigDecimal, BigDecimal> boundsForIndex = ((SetOfRealValues) variable.getSetOfValuesOrNull()).getBoundsForIndex(valueIndex);
			Expression result = Expressions.apply(LESS_THAN, variable.getName(), boundsForIndex.second.doubleValue());
			return result;
		}

		public DefaultSamplingFactorDiscretizedProbabilityDistributionFunction getSamplingFactorDiscretizedProbabilityDistributionFunction() {
			if (samplingFactorDiscretizedProbabilityDistribution == null) {
				samplingFactorDiscretizedProbabilityDistribution = makeSamplingFactorDiscretizedProbabilityDistribution();
			}
			return samplingFactorDiscretizedProbabilityDistribution;
		}
		
		private DefaultSamplingFactorDiscretizedProbabilityDistributionFunction makeSamplingFactorDiscretizedProbabilityDistribution() {
			List<? extends ExpressionVariable> expressionVariables = mapIntoList(samplingFactor.getVariables(), v -> (ExpressionVariable) v);
			SetOfVariables setOfVariables = makeSetOfVariablesWithRanges(expressionVariables, fromVariableToNumberOfDiscreteValues, context);
			DefaultSamplingFactorDiscretizedProbabilityDistributionFunction result = new DefaultSamplingFactorDiscretizedProbabilityDistributionFunction(samplingFactor, setOfVariables, queryIndex, initialNumberOfSamples);
			return result;
		}

		private ArrayList<? extends Variable> getFunctionVariables() {
			return getSamplingFactorDiscretizedProbabilityDistributionFunction().getSetOfInputVariables().getVariables();
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
		
		ArrayList<Variable> variables = 
				mapIntoArrayList(
						expressionVariables,
						v -> makeVariableWithRange(v, numberOfDiscreteValues.apply(v), context));
		
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
	
	public static UniformSamplingFactor makeUniformSamplingFactor(ExpressionVariable variable, Random random, Context context) {
		UniformSamplingFactor result;
		Type type = context.getTypeOfRegisteredSymbol(variable);
		if (type instanceof RealInterval) {
			RealInterval realInterval = (RealInterval) type;
			result = 
					new UniformRealSamplingFactor(
							variable, 
							realInterval.getLowerBound().doubleValue(), 
							realInterval.getUpperBound().doubleValue(), 
							new DoublePotentialFactory(), 
							random);
		}
		else if (type instanceof IntegerInterval) {
			IntegerInterval integerInterval = (IntegerInterval) type;
			result = 
					new UniformIntegerSamplingFactor(
							variable, 
							integerInterval.getNonStrictLowerBound().intValue(), 
							integerInterval.getNonStrictUpperBound().intValue() + 1, 
							new DoublePotentialFactory(), 
							random);
		}
		else if (type instanceof Categorical) {
			Categorical categoricalType = (Categorical) type;
			result = 
					new UniformDiscreteSamplingFactor<String>(
							variable, 
							() -> categoricalType.sampleUniquelyNamedConstant(random).toString(), 
							categoricalType.cardinality().intValue(), 
							new DoublePotentialFactory(), 
							random);
		}
		else {
			throw new Error(ExpressionSamplingFactor.class + " only supports real, integer and enum types, but got variable " + variable + " of type " + type);
		}
		return result;
	}

}
