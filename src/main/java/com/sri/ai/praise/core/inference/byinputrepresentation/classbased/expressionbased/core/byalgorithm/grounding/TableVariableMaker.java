package com.sri.ai.praise.core.inference.byinputrepresentation.classbased.expressionbased.core.byalgorithm.grounding;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.base.TableVariable;

import java.util.ArrayList;

import static com.sri.ai.util.Util.mapIntoArrayList;

public class TableVariableMaker {

	public static ArrayList<TableVariable>
	getTableVariables(
			ArrayList<? extends Expression> variables, Context context) {

		return mapIntoArrayList(variables, v -> makeTableVariable(v, context));
	}

	public static TableVariable makeTableVariable(Expression variable, Context context) {
		var cardinality = getCardinalityOfIntegerIntervalTypedRegisteredSymbol(variable, context);
		return new TableVariable(variable.toString(), cardinality);
	}

	private static int
	getCardinalityOfIntegerIntervalTypedRegisteredSymbol(
			Expression symbol, Context context) {

		var cardinality = context.getCardinalityOfIntegerIntervalTypedRegisteredSymbol(symbol);
		return cardinality;
	}
}
