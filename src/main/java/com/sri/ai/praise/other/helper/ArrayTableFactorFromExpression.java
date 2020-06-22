package com.sri.ai.praise.other.helper;

import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.praise.core.inference.byinputrepresentation.classbased.expressionbased.core.byalgorithm.grounding.ExpressionToArrayTableFactorGrounder.ground;
import static com.sri.ai.util.Util.join;
import static com.sri.ai.util.Util.mapIntoList;

import java.util.Map;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.praise.core.representation.classbased.hogm.components.HOGMExpressionBasedModel;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.bydatastructure.arraylist.ArrayTableFactor;
import com.sri.ai.util.base.BinaryFunction;

public class ArrayTableFactorFromExpression {

	public
	static
	ArrayTableFactor
	arrayTableFactorFrom(String expressionString, BinaryFunction<Expression, Context, Expression> interpreter, Map<String, Integer> variableDefinitions) {
		var expression = parse(expressionString);
		var hogModel = makeModel(variableDefinitions);
		var actualFactor = ground(expression, interpreter, hogModel.getContext());
		return actualFactor;
	}

	private static HOGMExpressionBasedModel makeModel(Map<String, Integer> variableDefinitions) {
		var definitionsString = mapIntoList(variableDefinitions.entrySet(), ArrayTableFactorFromExpression::fromMapEntryToString);
		var hogModelString = join("; ", definitionsString);
		var hogModel = new HOGMExpressionBasedModel(hogModelString);
		return hogModel;
	}

	private static String fromMapEntryToString(Map.Entry<String, Integer> entry) {
		return "random " + entry.getKey() + ": 0.." + (entry.getValue() - 1);
	}

}
