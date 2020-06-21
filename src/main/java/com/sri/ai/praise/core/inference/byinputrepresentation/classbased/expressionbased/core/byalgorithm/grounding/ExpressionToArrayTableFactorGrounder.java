package com.sri.ai.praise.core.inference.byinputrepresentation.classbased.expressionbased.core.byalgorithm.grounding;

import static com.sri.ai.expresso.api.Symbol.makeSymbol;
import static com.sri.ai.util.Util.arrayListFrom;
import static com.sri.ai.util.Util.mapIntoArrayList;

import java.util.ArrayList;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.expresso.type.IntegerInterval;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.interpreter.BruteForceCommonInterpreter;
import com.sri.ai.grinder.interpreter.ContextAssignmentLookup;
import com.sri.ai.grinder.rewriter.api.Rewriter;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.base.TableVariable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.bydatastructure.arraylist.ArrayTableFactor;

public class ExpressionToArrayTableFactorGrounder {

	public static ArrayTableFactor ground(Expression expression, Context context) {
		var grounder = new ExpressionToArrayTableFactorGrounder(expression, context);
		return grounder.result;
	}
	
	private Expression expression;
	private Context context;
	private ArrayList<? extends Expression> variables;
	private ArrayList<? extends TableVariable> tableVariables;
	private ArrayTableFactor result;
	private Rewriter interpreter = new BruteForceCommonInterpreter();
	
	private ExpressionToArrayTableFactorGrounder(Expression expression, Context context) {
		this.expression = expression;
		this.context = context;
		this.variables = arrayListFrom(Expressions.getVariablesBeingReferenced(expression, context));
		this.tableVariables = mapIntoArrayList(variables, this::makeTableVariable);
		this.result = ArrayTableFactor.fromFunctionOnArray(tableVariables, this::computeEntry);
	}
	
	private TableVariable makeTableVariable(Expression variable) {
		var type = (IntegerInterval) context.getTypeOfRegisteredSymbol(variable);
		var cardinality = type.cardinality().intValue();
		return new TableVariable(variable.toString(), cardinality);
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
