package com.sri.ai.test.praise.core.representation.interfacebased.expressionsampling;

import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.util.Util.map;
import static com.sri.ai.util.Util.println;
import static org.junit.Assert.assertEquals;

import org.junit.jupiter.api.Test;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.type.RealInterval;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.core.TrueContext;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.expression.api.ExpressionVariable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.expression.core.DefaultExpressionVariable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.expressionsampling.ExpressionSamplingFactor;
import com.sri.ai.util.function.core.variables.RealVariable;

class ExpressionSamplingFactorTest {

	@Test
	void makeSetOfVariablesWithRangesTest() {
		
		String expressionString;
		String typeString;
		int numberOfDiscreteValues;
		String expected;
		
		expressionString = "X";
		typeString = "[0;100]";
		numberOfDiscreteValues = 101;
		expected = "RealVariable [getName()=X, getUnit()=DefaultUnit{name='none', symbol=''}, getSetOfValuesOrNull()=SetOfRealValues from 0 to 100, step 1, lower bound 0, upperBoundForDiscretizedValue 100]";
		runMakeSetOfVariablesWithRangesTest(expressionString, typeString, numberOfDiscreteValues, expected);
		
		expressionString = "X";
		typeString = "]0;100[";
		numberOfDiscreteValues = 99;
		expected = "RealVariable [getName()=X, getUnit()=DefaultUnit{name='none', symbol=''}, getSetOfValuesOrNull()=SetOfRealValues from 1 to 99, step 1, lower bound 0, upperBoundForDiscretizedValue 100]";
		runMakeSetOfVariablesWithRangesTest(expressionString, typeString, numberOfDiscreteValues, expected);
	}

	private void runMakeSetOfVariablesWithRangesTest(String expressionString, String typeString, int numberOfDiscreteValues, String expected) {
		Expression expression = parse(expressionString);
		Context context = new TrueContext();
		context = context.setSymbolsAndTypes(map(expression, parse(typeString)));
		ExpressionVariable expressionVariable = new DefaultExpressionVariable(expression);
		RealInterval type = (RealInterval) context.getTypeOfRegisteredSymbol(expression);
		RealVariable realVariable = ExpressionSamplingFactor.makeRealVariableWithRange(expressionVariable, type, numberOfDiscreteValues, context);
		println(realVariable);
		assertEquals(expected, realVariable.toString());
	}

}
