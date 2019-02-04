package com.sri.ai.praise.core.representation.interfacebased.factor.core.expressionsampling;

import static com.sri.ai.expresso.helper.Expressions.ifThenElse;
import static com.sri.ai.expresso.helper.Expressions.makeSymbol;
import static com.sri.ai.grinder.library.FunctorConstants.LESS_THAN;
import static com.sri.ai.util.Util.arrayList;
import static com.sri.ai.util.Util.mapIntegersIntoArrayList;
import static com.sri.ai.util.Util.mapIntegersIntoList;
import static com.sri.ai.util.Util.mapIntoList;
import static com.sri.ai.util.Util.objectStringEqualsOneOf;
import static com.sri.ai.util.Util.println;
import static com.sri.ai.util.Util.product;
import static com.sri.ai.util.base.PairOf.makePairOf;
import static com.sri.ai.util.collect.FunctionIterator.functionIterator;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Symbol;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.library.Equality;
import com.sri.ai.grinder.library.boole.And;
import com.sri.ai.grinder.library.boole.Not;
import com.sri.ai.util.base.Pair;
import com.sri.ai.util.base.PairOf;
import com.sri.ai.util.collect.CartesianProductOnIntegersIterator;
import com.sri.ai.util.function.api.values.Value;
import com.sri.ai.util.function.api.variables.Assignment;
import com.sri.ai.util.function.api.variables.SetOfVariables;
import com.sri.ai.util.function.api.variables.Variable;
import com.sri.ai.util.function.core.values.SetOfRealValues;
import com.sri.ai.util.function.core.variables.DefaultAssignment;
import com.sri.ai.util.function.core.variables.EnumVariable;
import com.sri.ai.util.function.core.variables.IntegerVariable;

public class FromFunctionToExpression {

	/**
	 * The maximum number of assignments to factor's discretized values
	 * that will allow generating an expression for it
	 * ({@link #TOO_LARGE_FOR_EXPRESSION_GENERATION} is used otherwise).
	 *
	 */
	public static int maximum_number_of_assignments_for_expression_generation = 200000;

	/**
	 * The factor's expression if the number of assignments to factor's discretized values
	 * is greater than {@link #maximum_number_of_assignments_for_expression_generation}.
	 */
	public static final Symbol TOO_LARGE_FOR_EXPRESSION_GENERATION = makeSymbol("too large");


	private int queryIndex;
	private com.sri.ai.util.function.api.functions.Function function;

	public FromFunctionToExpression(
			com.sri.ai.util.function.api.functions.Function function, 
			int queryIndex) {
		
		this.queryIndex = queryIndex;
		this.function = function;
	}

	private Expression expression;

	public Expression getFactorExpression() {
		if (expression == null) {
			expression = makeFactorExpressionOrTooLarge();
		}
		return expression;
	}

	private Expression makeFactorExpressionOrTooLarge() {
		Expression expression;
		int numberOfAssignments = computeNumberOfAssignments();
		if (numberOfAssignments > maximum_number_of_assignments_for_expression_generation) {
			expression = TOO_LARGE_FOR_EXPRESSION_GENERATION;
		}
		else {
			expression = makeFactorExpression(); 
		}
		return expression;
	}

	private int computeNumberOfAssignments() {
		int numberOfAssignments = (int) product(functionIterator(getFunctionVariables(), v -> {
			int size = v.getSetOfValuesOrNull().size();
			println("Variable " + v + " has " + size + " number Of assignments");
			return size;	
		}));
		println("Total number of assignments: " + numberOfAssignments);
		return numberOfAssignments;
	}

	private Expression makeFactorExpression() {
		Expression expression;
		Iterator<ArrayList<Integer>> valueIndicesIterator = makeValueIndicesIterator();
		if (valueIndicesIterator.hasNext()) {
			expression = getFactorExpressionForNonEmptyDomain(valueIndicesIterator);
		}
		else {
			expression = getFactorExpressionForEmptyDomain();
		}
		return expression;
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
		Expression expression = ifThenElse(conditions, probabilities);
		return expression;
	}

	private Expression getProbabilityForAssignmentIndices(ArrayList<Integer> valueIndices) {

		Assignment assignment = getAssignmentFromValueIndices(valueIndices);

		com.sri.ai.util.function.api.functions.Function 
		projectedFunction = projectIfNeeded(function, assignment);

		double probability = projectedFunction.evaluate(assignment).doubleValue();

		Symbol result = makeSymbol(probability);

		return result;
	}

	/**
	 * We take the projection of the Function based on the assignment, but only if needed
	 * (that is, if the assignment contains any variables).
	 * If the assignment does not contain any variables, the projection is just the initial function.
	 * This is preferable because projection may throw away information.
	 * @param function
	 * @param assignment
	 * @return
	 */
	private com.sri.ai.util.function.api.functions.Function projectIfNeeded(
			com.sri.ai.util.function.api.functions.Function function,
			Assignment assignment) {

		com.sri.ai.util.function.api.functions.Function result;

		Variable query = function.getSetOfInputVariables().get(queryIndex);

		if (assignment.size() > 1) {
			Assignment assignmentWithoutQuery = assignment.remove(query);
			result = function.project(query, assignmentWithoutQuery);
		}
		else {
			result = function;
		}

		return result;
	}

	private Expression getFactorExpressionForEmptyDomain() {
		return Expressions.ONE;
	}

	private Assignment getAssignmentFromValueIndices(ArrayList<Integer> valueIndices) {
		SetOfVariables setOfVariables = function.getSetOfInputVariables();
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

	private ArrayList<? extends Variable> getFunctionVariables() {
		return function.getSetOfInputVariables().getVariables();
	}

}
