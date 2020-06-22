package com.sri.ai.praise.core.inference.byinputrepresentation.classbased.expressionbased.core.byalgorithm.grounding;

import static com.sri.ai.expresso.api.Symbol.makeSymbol;
import static com.sri.ai.util.Util.arrayListFrom;
import static com.sri.ai.util.Util.mapIntoArrayList;

import java.util.ArrayList;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.interpreter.BruteForceCommonInterpreter;
import com.sri.ai.grinder.interpreter.ContextAssignmentLookup;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.base.TableVariable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.bydatastructure.arraylist.ArrayTableFactor;
import com.sri.ai.util.base.BinaryFunction;

/**
 * A converter from an expression to an equivalent {@link ArrayTableFactor}.
 * The types of variables must be registered in the given context.
 * The interpreter must be able to take into account the variable assignments according to {@link ContextAssignmentLookup}.
 * @author braz
 */
public class ExpressionToArrayTableFactorGrounder {

	public static ArrayTableFactor ground(
			Expression expression,
			BinaryFunction<Expression, Context, Expression> interpreter,
			Context context) {
		
		var grounder = new ExpressionToArrayTableFactorGrounder(interpreter, context);
		var result = grounder.ground(expression);
		return result;
	}
	
	private Expression expression;
	private BinaryFunction<Expression, Context, Expression> interpreter = new BruteForceCommonInterpreter();
	private Context context;
	private ArrayList<? extends Expression> variables;
	
	public ExpressionToArrayTableFactorGrounder(
			BinaryFunction<Expression, Context, Expression> interpreter, 
			Context context) {
		
		this.interpreter = interpreter;
		this.context = context;
	}

	public ArrayTableFactor ground(Expression expression) {
		this.expression = expression;
		this.variables = arrayListFrom(Expressions.getVariablesBeingReferenced(expression, context));
		var tableVariables = mapIntoArrayList(variables, this::makeTableVariable);
		var result = ArrayTableFactor.fromFunctionOnIndicesArray(tableVariables, this::computeEntry);
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
	
	private double computeEntry(int[] indices) {
		setContextAssignment(indices);
		return interpreter.apply(expression, context).doubleValue();
	}

	private void setContextAssignment(int[] indices) {
		for (int i = 0; i != variables.size(); i++) {
			var valueExpression = makeSymbol(indices[i]);
			context = ContextAssignmentLookup.setAssignment(context, variables.get(i), valueExpression);
		}
	}
}
