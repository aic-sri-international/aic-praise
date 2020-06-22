package com.sri.ai.test.praise.core.inference.byinputrepresentation.classbased.expressionbased.core.byalgorithm.grounding;

import static com.sri.ai.praise.PRAiSEUtil.arrayTableFactorFrom;
import static com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.bydatastructure.arraylist.ArrayTableFactor.arrayTableFactor;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.map;
import static com.sri.ai.util.Util.mapIntoList;
import static com.sri.ai.util.Util.println;
import static com.sri.ai.util.Util.round;
import static org.junit.Assert.assertEquals;

import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.Test;

import com.sri.ai.grinder.interpreter.BruteForceCommonInterpreter;
import com.sri.ai.grinder.interpreter.FastInterpreter;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.base.TableVariable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.bydatastructure.arraylist.ArrayTableFactor;
import com.sri.ai.util.Timer;

class ExpressionToArrayTableFactorGrounderTest {

	@Test
	void test() {
		
		ArrayTableFactor.maximumNumberOfEntriesToShow = 50;
		
		Map<String, Integer> variableDefinitions = map("i", 5, "j", 5);
		var def = variableDefinitions;
		
		String expressionString;
		ArrayTableFactor expectedFactor;
		
		expressionString = "if i < 3 and j = 4 then 0.3 else 0.7";
		expectedFactor = arrayTableFactor(vars("i, j", def), (vi, vj) -> vi < 3 && vj == 4? 0.3 : 0.7); 
		runTest(expressionString, variableDefinitions, expectedFactor);
		
		expressionString = "1.0";
		expectedFactor = arrayTableFactor(list(), () -> 1.0); 
		runTest(expressionString, variableDefinitions, expectedFactor);
	}

	@Test
	void performanceTest() {
		
		ArrayTableFactor.maximumNumberOfEntriesToShow = 50;
		
		Map<String, Integer> variableDefinitions = map("i", 100, "j", 100);
		var def = variableDefinitions;
		
		String expressionString;
		ArrayTableFactor expectedFactor;
		
		expressionString = "if i < 3 and j = 4 then 0.3 else 0.7";
		expectedFactor = arrayTableFactor(vars("i, j", def), (vi, vj) -> vi < 3 && vj == 4? 0.3 : 0.7); 
		runTest(expressionString, variableDefinitions, expectedFactor);
	}

	private List<TableVariable> vars(String variables, Map<String, Integer> variableDefinitions) {
		var variableNamesArray = variables.split("\\s*,\\s*");
		return mapIntoList(variableNamesArray, name -> new TableVariable(name, variableDefinitions.get(name)));
	}
	
	private void runTest(String expressionString, Map<String, Integer> variableDefinitions, ArrayTableFactor expectedFactor) {
		var bruteForceActualFactorAndTime = 
				Timer.getResultAndTime(
						() -> arrayTableFactorFrom(expressionString, new BruteForceCommonInterpreter(), variableDefinitions));
		var fastActualFactorAndTime =
				Timer.getResultAndTime(
						() -> arrayTableFactorFrom(expressionString, new FastInterpreter(), variableDefinitions));
		println();
		println("Variables and cardinalities : ", variableDefinitions);
		println("Expression: ", expressionString);
		println("Expected factor: ", expectedFactor);
		println("Actual   factor by brute force interpreter: ", bruteForceActualFactorAndTime.first);
		println("Actual   factor by fast interpreter       : ", fastActualFactorAndTime.first);
		println("Time by brute force interpreter: ", round(bruteForceActualFactorAndTime.second/1000., 2) + " seconds");
		println("Time by fast interpreter       : ", round(fastActualFactorAndTime.second/1000., 2) + " seconds");
		assertEquals(expectedFactor, bruteForceActualFactorAndTime.first);
		assertEquals(expectedFactor, fastActualFactorAndTime.first);
	}

}
