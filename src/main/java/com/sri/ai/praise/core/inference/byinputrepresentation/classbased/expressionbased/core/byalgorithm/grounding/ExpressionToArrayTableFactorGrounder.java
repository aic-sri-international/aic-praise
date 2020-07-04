package com.sri.ai.praise.core.inference.byinputrepresentation.classbased.expressionbased.core.byalgorithm.grounding;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.interpreter.ContextAssignmentLookup;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.base.TableVariable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.bydatastructure.arraylist.ArrayTableFactor;
import com.sri.ai.util.base.BinaryFunction;

import java.util.ArrayList;
import java.util.function.Function;

import static com.sri.ai.expresso.api.Symbol.makeSymbol;
import static com.sri.ai.util.Util.arrayListFrom;
import static com.sri.ai.util.Util.mapIntoArrayList;

/**
 * A converter from an expression to an equivalent {@link ArrayTableFactor}.
 * The types of variables must be registered in the given context.
 * The interpreter must be able to take into account the variable assignments according to {@link ContextAssignmentLookup},
 * in the exact same order as variables appear in the expression
 * (the order they are returned in by {@link Expressions#getVariablesBeingReferenced(Expression, Context)}).
 * Assignments are guaranteed to be evaluated in lexicographical order, so the evaluator may rely on that for
 * incremental evaluation.
 * @author braz
 */
public class ExpressionToArrayTableFactorGrounder {

	private Function<Expression, DiscreteExpressionEvaluator> fromExpressionToEvaluator;
	private Context context;

	public ExpressionToArrayTableFactorGrounder(
			Function<Expression, DiscreteExpressionEvaluator> fromExpressionToEvaluator,
			Context context) {

		this.fromExpressionToEvaluator = fromExpressionToEvaluator;
		this.context = context;
	}

	public ArrayTableFactor ground(Expression expression) {
		var evaluator = fromExpressionToEvaluator.apply(expression);
		var variables = arrayListFrom(Expressions.getVariablesBeingReferenced(expression, context));
		var tableVariables = mapIntoArrayList(variables, this::makeTableVariable);
		var result = ArrayTableFactor.fromFunctionOnIndicesArray(tableVariables, evaluator::evaluate);
		return result;
	}

	public TableVariable makeTableVariable(Expression variable) {
		var cardinality = getCardinalityOfIntegerIntervalTypedRegisteredSymbol(variable);
		return new TableVariable(variable.toString(), cardinality);
	}

	private int getCardinalityOfIntegerIntervalTypedRegisteredSymbol(Expression symbol) {
		var cardinality = context.getCardinalityOfIntegerIntervalTypedRegisteredSymbol(symbol);
		return cardinality;
	}
}
