package com.sri.ai.test.praise.core.representation.interfacebased.expressionsampling;

import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.praise.core.representation.interfacebased.factor.core.expressionsampling.FromRealExpressionVariableToRealVariableWithRange.makeRealVariableWithRange;
import static com.sri.ai.util.Util.map;
import static com.sri.ai.util.Util.println;
import static org.junit.Assert.assertEquals;

import org.junit.jupiter.api.Test;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.type.RealInterval;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.core.TrueContext;
import com.sri.ai.util.function.core.values.SetOfRealValues;
import com.sri.ai.util.function.core.variables.RealVariable;

class FromRealExpressionVariableToRealVariableWithRangeTest {

	@Test
	void makeSetOfVariablesWithRangesTest() {
		
		String expressionString;
		String typeString;
		int numberOfDiscreteValues;
		String expected;
		
		expressionString = "X";
		typeString = "[0;100]";
		numberOfDiscreteValues = 101;
		expected = "RealVariable [name=X, unit=DefaultUnit{name='none', symbol=''}, setOfValuesOrNull=SetOfRealValues from 0 to 100, step 1, lower bound 0, upperBoundForDiscretizedValue 100]";
		runMakeSetOfVariablesWithRangesTest(expressionString, typeString, numberOfDiscreteValues, expected);
		
		expressionString = "X";
		typeString = "]0;100[";
		numberOfDiscreteValues = 99;
		expected = "RealVariable [name=X, unit=DefaultUnit{name='none', symbol=''}, setOfValuesOrNull=SetOfRealValues from 1 to 99, step 1, lower bound 0, upperBoundForDiscretizedValue 100]";
		runMakeSetOfVariablesWithRangesTest(expressionString, typeString, numberOfDiscreteValues, expected);
		
		expressionString = "X";
		typeString = "[0;100[";
		numberOfDiscreteValues = 100;
		expected = "RealVariable [name=X, unit=DefaultUnit{name='none', symbol=''}, setOfValuesOrNull=SetOfRealValues from 0 to 99, step 1, lower bound 0, upperBoundForDiscretizedValue 100]";
		runMakeSetOfVariablesWithRangesTest(expressionString, typeString, numberOfDiscreteValues, expected);
		
		expressionString = "X";
		typeString = "]0;100]";
		numberOfDiscreteValues = 100;
		expected = "RealVariable [name=X, unit=DefaultUnit{name='none', symbol=''}, setOfValuesOrNull=SetOfRealValues from 1 to 100, step 1, lower bound 0, upperBoundForDiscretizedValue 100]";
		runMakeSetOfVariablesWithRangesTest(expressionString, typeString, numberOfDiscreteValues, expected);
		
		// Same as above but with one less number of discrete values in other to test inexact division
		expressionString = "X";
		typeString = "]0;100]";
		numberOfDiscreteValues = 99;
		expected = "RealVariable [name=X, unit=DefaultUnit{name='none', symbol=''}, setOfValuesOrNull=SetOfRealValues from 1.010101010101010 to 100, step 1.010101010101010, lower bound 0, upperBoundForDiscretizedValue 100]";
		runMakeSetOfVariablesWithRangesTest(expressionString, typeString, numberOfDiscreteValues, expected);
		
		expressionString = "X";
		typeString = "]100;0[";
		numberOfDiscreteValues = 99;
		expected = "RealVariable [name=X, unit=DefaultUnit{name='none', symbol=''}, setOfValuesOrNull=Empty set of real values]";
		runMakeSetOfVariablesWithRangesTest(expressionString, typeString, numberOfDiscreteValues, expected);
		
		expressionString = "X";
		typeString = "]0;0[";
		numberOfDiscreteValues = 99;
		expected = "RealVariable [name=X, unit=DefaultUnit{name='none', symbol=''}, setOfValuesOrNull=Empty set of real values]";
		runMakeSetOfVariablesWithRangesTest(expressionString, typeString, numberOfDiscreteValues, expected);
		
		expressionString = "X";
		typeString = "[0;0]";
		numberOfDiscreteValues = 99;
		expected = "RealVariable [name=X, unit=DefaultUnit{name='none', symbol=''}, setOfValuesOrNull=Singleton set of real 0]";
		runMakeSetOfVariablesWithRangesTest(expressionString, typeString, numberOfDiscreteValues, expected);

		// OVERREACHING STEP BUG (search for this to see where this is specifically tested)
		// Error found in debugging: step was approximated and first + step*(number of values + 1) would be greater than last.
		// This was fixed by setting the rounding mode of step to RondingMode.FLOOR rather than using the DECIMAL64 default of HALF_EVEN.
		expressionString = "X";
		typeString = "[-10;10]";
		numberOfDiscreteValues = 15;
		expected = "RealVariable [name=X, unit=DefaultUnit{name='none', symbol=''}, setOfValuesOrNull=SetOfRealValues from -10 to 10, step 1.428571428571428, lower bound -10, upperBoundForDiscretizedValue 10]";
		runMakeSetOfVariablesWithRangesTest(expressionString, typeString, numberOfDiscreteValues, expected);
	}

	private void runMakeSetOfVariablesWithRangesTest(String expressionString, String typeString, int numberOfDiscreteValues, String expected) {
		Expression expression = parse(expressionString);
		Context context = new TrueContext();
		context = context.setSymbolsAndTypes(map(expression, parse(typeString)));
		RealInterval type = (RealInterval) context.getTypeOfRegisteredSymbol(expression);
		RealVariable realVariable = makeRealVariableWithRange(expression.toString(), type, numberOfDiscreteValues, context);
		if (expected.equals(realVariable.toFullString())) {
			println(realVariable.toFullString());
		}
		else {
			println ("Expected: " + expected);
			println ("Actual  : " + realVariable.toFullString());
		}
		assertEquals(expected, realVariable.toFullString());
		
		// OVERREACHING STEP BUG (search for this to see where this is explained)
		SetOfRealValues setOfRealValues = realVariable.getSetOfValuesOrNull();
		if (!setOfRealValues.isEmpty()) {
			assertEquals(setOfRealValues.getLast().doubleValue(), setOfRealValues.get(numberOfDiscreteValues - 1).doubleValue(), 0.000001);
		}
	}

}
