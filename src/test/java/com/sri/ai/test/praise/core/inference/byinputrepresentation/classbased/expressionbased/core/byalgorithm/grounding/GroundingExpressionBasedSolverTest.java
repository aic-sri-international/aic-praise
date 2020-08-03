package com.sri.ai.test.praise.core.inference.byinputrepresentation.classbased.expressionbased.core.byalgorithm.grounding;

import static com.sri.ai.util.Timer.timeStringInSeconds;
import static com.sri.ai.util.Util.join;
import static com.sri.ai.util.Util.println;

import org.junit.jupiter.api.Test;

import com.sri.ai.expresso.ExpressoConfiguration;
import com.sri.ai.praise.core.inference.byinputrepresentation.classbased.expressionbased.core.byalgorithm.grounding.GroundingExpressionBasedSolver;
import com.sri.ai.praise.core.inference.byinputrepresentation.classbased.hogm.solver.HOGMMultiQueryProblemSolver;
import com.sri.ai.util.Timer;

class GroundingExpressionBasedSolverTest {

	@Test
	void test() {
		ExpressoConfiguration.setDisplayNumericsMostDecimalPlacesInExactRepresentationOfNumericalSymbols(2);
		
		String modelString;
		String queryString;

		modelString = ""
				+ "random day: 0..4;"
				+ "random temp: 0..3;"
				+ "if day = 0 "
				+ "    then if temp = 0 then 1 else 0"
				+ "    else 0.25;"
				+ "";
		queryString = "temp";
		runTest(modelString, queryString);
	}

	@Test
	void testLarger() {
		ExpressoConfiguration.setDisplayNumericsMostDecimalPlacesInExactRepresentationOfNumericalSymbols(2);

		String modelString;
		String queryString;

		modelString = ""
				+ "random day: 0..299;"
				+ "random temp: 0..129;"
				+ "if day < 50 "
				+ "    then if temp < 20 then 0.01 else 0.04"
				+ "    else if temp < 20 then 0.02 else 0.03;"
				+ "";
		queryString = "temp";
		runTest(modelString, queryString);
	}

	@Test
	void testCategorical() {
		ExpressoConfiguration.setDisplayNumericsMostDecimalPlacesInExactRepresentationOfNumericalSymbols(2);

		String modelString;
		String queryString;

		modelString = ""
				+ "sort Strength: 3, weak, medium, strong;"
				+ "random earthquake: Strength;"
				+ "random burglary: Boolean;"
				+ "random temp: 0..15;"
				+ "random alarm: Boolean;"
				+ "earthquake = strong or earthquake = medium;"
				+ "if burglary then alarm 0.9 else if earthquake = strong then alarm 0.7 else if temp > 10 then alarm 0.2 else alarm 0.1;"
				+ "";
		queryString = "alarm";
		runTest(modelString, queryString);
	}

	private void runTest(String modelString, String queryString) {
		println();
		var resultsAndTime = 
				Timer.timed(
						() -> 
						new HOGMMultiQueryProblemSolver(modelString, queryString, new GroundingExpressionBasedSolver())
						.getResults());
		println(join("\n", resultsAndTime.first));
		println("Time: " + timeStringInSeconds(resultsAndTime, 4));
	}

}
