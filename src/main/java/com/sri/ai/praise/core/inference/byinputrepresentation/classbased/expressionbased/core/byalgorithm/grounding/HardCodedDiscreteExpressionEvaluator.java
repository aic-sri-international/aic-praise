package com.sri.ai.praise.core.inference.byinputrepresentation.classbased.expressionbased.core.byalgorithm.grounding;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Symbol;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.core.TrueContext;
import com.sri.ai.grinder.helper.UniquelyNamedConstantIncludingBooleansAndNumbersPredicate;
import com.sri.ai.grinder.interpreter.ContextAssignmentLookup;
import com.sri.ai.util.Util;
import com.sri.ai.util.base.BinaryFunction;
import com.sri.ai.util.math.Rational;

import java.util.ArrayList;
import java.util.Map;
import java.util.function.Function;

import static com.sri.ai.expresso.helper.Expressions.makeSymbol;
import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.grinder.library.FunctorConstants.IF_THEN_ELSE;
import static com.sri.ai.util.Util.*;

/**
 * A hard-coded discrete evaluator.
 * Currently supports only a sub-set of operations.
 * 
 * @author braz
 *
 */
public class HardCodedDiscreteExpressionEvaluator
		implements
		BinaryFunction<Expression, Context, Object>,
		DiscreteExpressionEvaluator
{

	/**
	 * Gives a chance to extending classes to obtain result of expression without
	 * evaluating it (typically through some kind of caching).
	 * If it returns null, the result will be evaluated as usual.
	 *
	 * @param functorOperation
	 * @param expression
	 * @param context
	 * @return
	 */
	protected Object beforeEvaluate(
			Function<ArrayList<Object>, Object> functorOperation,
			Expression expression,
			Context context) {

		return null;
	}

	protected
	void
	afterEvaluate(
			Function<ArrayList<Object>, Object> functorOperation,
			Expression expression,
			Context context,
			Object resultingValue) {
	}

	protected final Expression expression;
	protected final Context context;
	private Map<String, Function<ArrayList<Object>, Object>> functorOperations;

	public HardCodedDiscreteExpressionEvaluator(Expression expression, Context context) {
		this.expression = expression;
		this.context = context;
		functorOperations = map();
		functorOperations.put("+", this::evaluateAddition);
		functorOperations.put("not", this::evaluateNot);
		functorOperations.put("and", this::evaluateAnd);
		functorOperations.put("or", this::evaluateOr);
		functorOperations.put("=>", this::evaluateImplication);
		functorOperations.put("<=>", this::evaluateEquivalence);
		functorOperations.put(IF_THEN_ELSE, this::evaluateIfThenElse);
		functorOperations.put("<", this::evaluateLessThan);
		functorOperations.put(">", this::evaluateGreaterThan);
		functorOperations.put("<=", this::evaluateLessThanOrEqualTo);
		functorOperations.put(">=", this::evaluateGreaterThanOrEqualTo);
		functorOperations.put("=", this::evaluateEqualTo);
		functorOperations.put("!=", this::evaluateNotEqualTo);
	}

	protected int[] currentAssignment;

	@Override
	public double evaluate(int[] assignment) {
		currentAssignment = assignment;
		return ((Rational) apply(expression, context)).doubleValue();
	}

	@Override
	public Object apply(Expression expression, Context context) {
		if (expression.getSyntacticFormType().equals("Symbol")) {
			return evaluateSymbol(expression, context);
		}
		else {
			return evaluateFunctionApplication(expression, context);
		}
	}

	private Object evaluateSymbol(Expression expression, Context context) {
		if (context.isVariable(expression)) {
			return evaluateVariable(expression, context);
		}
		else {
			return expression.getValue();
		}
	}

	protected Object evaluateVariable(Expression expression, Context context) {
		var value = ContextAssignmentLookup.getAssignedValue(expression, context);
		myAssert(value != null, () -> expression + " has no assigned value");
		return value.intValue();
	}

	private Object evaluateFunctionApplication(Expression expression, Context context) {
		var functorOperation = obtainFunctorOperation(expression);
		var result = evaluate(functorOperation, expression, context);
		return result;
	}

	private Function<ArrayList<Object>, Object> obtainFunctorOperation(Expression expression) {
		var functorString = expression.getFunctor().getValue();
		var functorOperation = functorOperations.get(functorString);
		myAssert(functorOperation != null, () -> "Undefined operator: " + functorString + " from " + expression);
		return functorOperation;
	}

	private
	Object
	evaluate(
			Function<ArrayList<Object>, Object> functorOperation,
			Expression expression,
			Context context) {

		var resultingValue = beforeEvaluate(functorOperation, expression, context);
		if (resultingValue == null) {
			var evaluatedArguments = mapIntoArrayList(expression.getArguments(), a -> apply(a, context));
			resultingValue = functorOperation.apply(evaluatedArguments);
		}
		afterEvaluate(functorOperation, expression, context, resultingValue);
		return resultingValue;
	}

	private Number evaluateAddition(ArrayList<Object> evaluatedArguments) {
		var numericValues = mapIntoList(evaluatedArguments, a -> (Number) a);
		var result = Util.sum(numericValues);
		return result;
	}

	private Boolean evaluateNot(ArrayList<Object> evaluatedArguments) {
		var booleanValues = mapIntoList(evaluatedArguments, a -> (Boolean) a);
		var result = Util.not(booleanValues);
		return result;
	}

	private Boolean evaluateAnd(ArrayList<Object> evaluatedArguments) {
		var booleanValues = mapIntoList(evaluatedArguments, a -> (Boolean) a);
		var result = Util.and(booleanValues);
		return result;
	}

	private Boolean evaluateOr(ArrayList<Object> evaluatedArguments) {
		var booleanValues = mapIntoList(evaluatedArguments, a -> (Boolean) a);
		var result = Util.or(booleanValues);
		return result;
	}

	private Boolean evaluateImplication(ArrayList<Object> evaluatedArguments) {
		var booleanValues = mapIntoList(evaluatedArguments, a -> (Boolean) a);
		var result = Util.implication(booleanValues);
		return result;
	}

	private Boolean evaluateEquivalence(ArrayList<Object> evaluatedArguments) {
		var booleanValues = mapIntoList(evaluatedArguments, a -> (Boolean) a);
		var result = Util.equivalence(booleanValues);
		return result;
	}

	private Object evaluateIfThenElse(ArrayList<Object> evaluatedArguments) {
		myAssert(evaluatedArguments.size() == 3, () -> "if then else requires exactly three arguments but got: " + evaluatedArguments);
		var branchIndex = ((Boolean) evaluatedArguments.get(0))? 1 : 2;
		var result = evaluatedArguments.get(branchIndex);
		return result;
	}
	
	private Boolean evaluateLessThan(ArrayList<Object> evaluatedArguments) {
		return evaluateRelationalOperatorOnNumbers((d1, d2) -> d1.doubleValue() < d2.doubleValue(), "<", evaluatedArguments);
	}

	private Boolean evaluateGreaterThan(ArrayList<Object> evaluatedArguments) {
		return evaluateRelationalOperatorOnNumbers((d1, d2) -> d1.doubleValue() > d2.doubleValue(), ">", evaluatedArguments);
	}

	private Boolean evaluateLessThanOrEqualTo(ArrayList<Object> evaluatedArguments) {
		return evaluateRelationalOperatorOnNumbers((d1, d2) -> d1.doubleValue() <= d2.doubleValue(), "<=", evaluatedArguments);
	}

	private Boolean evaluateGreaterThanOrEqualTo(ArrayList<Object> evaluatedArguments) {
		return evaluateRelationalOperatorOnNumbers((d1, d2) -> d1.doubleValue() >= d2.doubleValue(), ">=", evaluatedArguments);
	}

	private Boolean evaluateEqualTo(ArrayList<Object> evaluatedArguments) {
		return evaluateRelationalOperatorOnObjects(this::equalityOfPossiblyNumberObjects, "=", evaluatedArguments);
	}

	private Boolean evaluateNotEqualTo(ArrayList<Object> evaluatedArguments) {
		return evaluateRelationalOperatorOnObjects(this::disequalityOfPossiblyNumberObjects, "!=", evaluatedArguments);
	}

	private Boolean equalityOfPossiblyNumberObjects(Object o1, Object o2) {
		if (o1 instanceof Number) {
			if (o2 instanceof Number) {
				return ((Number) o1).doubleValue() == ((Number) o2).doubleValue();
			}
			else {
				throw new Error(HardCodedDiscreteExpressionEvaluator.class + " comparing two values but only one is a number: " + o1 + ", " + o2);
			}
		}
		else if (o2 instanceof Number) {
			throw new Error(HardCodedDiscreteExpressionEvaluator.class + " comparing two values but only one is a number: " + o1 + ", " + o2);
		}
		else {
			return o1 == o2;
		}
	}

	private Boolean disequalityOfPossiblyNumberObjects(Object o1, Object o2) {
		return ! equalityOfPossiblyNumberObjects(o1, o2);
	}

	private Boolean evaluateRelationalOperatorOnNumbers(
			BinaryFunction<Number, Number, Boolean> binaryOperator,
			String operatorName,
			ArrayList<Object> evaluatedArguments) {
		
		myAssert(evaluatedArguments.size() == 2, () -> operatorName + " applied to more than two arguments: " + evaluatedArguments);
		ArrayList<Number> numericValues = mapIntoArrayList(evaluatedArguments, a -> (Number) a);
		var result = binaryOperator.apply(numericValues.get(0), numericValues.get(1));
		return result;
	}

	private Boolean evaluateRelationalOperatorOnObjects(
			BinaryFunction<Object, Object, Boolean> binaryOperator,
			String operatorName,
			ArrayList<Object> evaluatedArguments) {

		myAssert(evaluatedArguments.size() == 2, () -> operatorName + " applied to more than two arguments: " + evaluatedArguments);
		var result = binaryOperator.apply(evaluatedArguments.get(0), evaluatedArguments.get(1));
		return result;
	}

	public static void main(String[] args) {
		Map<Expression, Expression> assignments = map(parse("y"), makeSymbol(10), parse("x"), makeSymbol(2));
		var expression = Expressions.parse(
				"x + (2 + y) + "
				+ "(if x = y then 0 else 100) + "
				+ "(if x < 4 and y > 8 and x <= 2 and y >= 10 and x != y then 1000 else 0)");
		Context context = new TrueContext();
		context = context.setIsUniquelyNamedConstantPredicate(new UniquelyNamedConstantIncludingBooleansAndNumbersPredicate());
		context = ContextAssignmentLookup.setAssignments(context, assignments);
		var fastInterpreter = new HardCodedDiscreteExpressionEvaluator(expression, context);
		var result = fastInterpreter.apply(expression, context);
		println(result);
	}
}
