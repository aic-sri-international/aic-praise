package com.sri.ai.praise.core.inference.byinputrepresentation.classbased.expressionbased.core.byalgorithm.grounding;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.interpreter.ContextAssignmentLookup;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.base.TableVariable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.bydatastructure.arraylist.ArrayTableFactor;
import com.sri.ai.util.base.BinaryFunction;

import java.util.*;
import java.util.function.Function;

import static com.sri.ai.expresso.helper.Expressions.getMaxDepthOfOccurrences;
import static com.sri.ai.expresso.helper.Expressions.getVariablesBeingReferenced;
import static com.sri.ai.util.Util.*;
import static java.util.Comparator.comparing;

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

	private BinaryFunction<Expression, ArrayList<? extends Expression>, DiscreteExpressionEvaluator>
			fromExpressionAndVariablesToEvaluator;
	private Context context;

	public ExpressionToArrayTableFactorGrounder(
			BinaryFunction<Expression, ArrayList<? extends Expression>, DiscreteExpressionEvaluator>
					fromExpressionAndVariablesToEvaluator,
			Context context) {

		this.fromExpressionAndVariablesToEvaluator = fromExpressionAndVariablesToEvaluator;
		this.context = context;
	}

	public ArrayTableFactor ground(Expression expression) {
		var variables = extractVariablesInChosenOrder(expression);
		var evaluator = fromExpressionAndVariablesToEvaluator.apply(expression, variables);
		var tableVariables = mapIntoArrayList(variables, this::makeTableVariable);
		var result = ArrayTableFactor.fromFunctionOnIndicesArray(tableVariables, evaluator::evaluate);
		return result;
	}

	private ArrayList<Expression> extractVariablesInChosenOrder(Expression expression) {
		// We want the deeper variables to be the first ones in 'variables' (most significant)
		// so that they change less often as assignments are iterated.
		// This will make incremental evaluators more efficient as they will
		// need to recompute sub-expressions less often.
		var variables = arrayListFrom(getVariablesBeingReferenced(expression, context));
		var depthMap = mapIntoMap(variables, e -> getMaxDepthOfOccurrences(e, expression));
		variables.sort(comparing(depthMap::get).reversed());
		return variables;
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
